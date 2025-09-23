local nio = require("nio")

local M = {}

local function apply_pyright_imports()
  nio.run(function()
    local buf = nio.api.nvim_get_current_buf()

    local client = nio.lsp.get_clients({ name = "basedpyright" })[1]
    if client == nil then
      vim.notify("No client found", vim.log.levels.WARN)
      return
    end

    local uri = vim.uri_from_bufnr(buf)

    local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, true)
    local diags = vim.lsp.diagnostic.from(vim.diagnostic.get(buf, {}))

    ---@type nio.lsp.types.TextEdit[]
    local edits = {}

    for _, diag in ipairs(diags) do
      if diag.code == "reportUndefinedVariable" then
        local err, response = client.request.textDocument_codeAction({
          textDocument = { uri = uri },
          context = {
            diagnostics = diags,
          },
          range = {
            start = diag.range.start,
            ["end"] = diag.range["end"],
          },
        })
        assert(not err, err and err.message)
        if response[1] then
          vim.list_extend(edits, response[1].edit.changes[uri])
        end
      end
    end
    local deduped = {}
    for _, edit in ipairs(edits) do
      deduped[edit.newText] = edit
    end
    vim.lsp.util.apply_text_edits(vim.tbl_values(deduped), buf, "utf-16")
  end)
end

local function configure_watching()
  local FSWATCH_EVENTS = {
    Created = 1,
    Updated = 2,
    Removed = 3,
    -- Renamed
    OwnerModified = 2,
    AttributeModified = 2,
    MovedFrom = 1,
    MovedTo = 3,
    -- IsFile
    -- IsDir
    -- IsSymLink
    -- Link
    -- Overflow
  }

  --- @param data string
  --- @param opts table
  --- @param callback fun(path: string, event: integer)
  local function fswatch_output_handler(data, opts, callback)
    local d = vim.split(data, "%s+")
    local cpath = d[1]

    for i = 2, #d do
      if d[i] == "IsDir" or d[i] == "IsSymLink" or d[i] == "PlatformSpecific" then
        return
      end
    end

    if opts.include_pattern and opts.include_pattern:match(cpath) == nil then
      return
    end

    if opts.exclude_pattern and opts.exclude_pattern:match(cpath) ~= nil then
      return
    end

    for i = 2, #d do
      local e = FSWATCH_EVENTS[d[i]]
      if e then
        callback(cpath, e)
      end
    end
  end

  local function fswatch(path, opts, callback)
    local obj = vim.system({
      "fswatch",
      "--recursive",
      "--event-flags",
      "--exclude",
      "/.git/",
      path,
    }, {
      stdout = function(error, data)
        if data then
          for line in vim.gsplit(data, "\n", { plain = true, trimempty = true }) do
            fswatch_output_handler(line, opts, callback)
          end
        elseif error then
          vim.notify(error, vim.log.levels.ERROR, { title = "LSP Watch" })
        end
      end,
    })

    return function()
      obj:kill(2)
    end
  end

  if vim.fn.executable("fswatch") == 1 then
    require("vim.lsp._watchfiles")._watchfunc = fswatch
  end
end

function M.post()
  configure_watching()
  local has_status, lsp_status = pcall(require, "lsp-status")
  if has_status then
    lsp_status.register_progress()
  end
  require("trouble").setup({})
  vim.diagnostic.config({
    signs = {
      priority = 5,
      text = {
        [vim.diagnostic.severity.ERROR] = "▶",
        [vim.diagnostic.severity.WARN] = "▶",
        [vim.diagnostic.severity.INFO] = "▶",
        [vim.diagnostic.severity.HINT] = "▶",
      },
    },
    underline = false,
    virtual_text = {
      prefix = "",
      source = "always",
    },
    float = {
      show_header = false,
      border = vim.g.border_chars,
    },
    severity_sort = true,
  })

  require("config.lsp.handlers").setup()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  if has_status then
    capabilities = vim.tbl_deep_extend("force", capabilities, lsp_status.capabilities)
  end
  -- pcall(function()
  --   capabilities =
  --       vim.tbl_deep_extend("keep", require("cmp_nvim_lsp").default_capabilities(), capabilities)
  -- end)
  capabilities.textDocument.foldingRange = {
    dynamicRegistration = false,
    lineFoldingOnly = true,
  }

  capabilities = vim.tbl_deep_extend(
    "force",
    vim.lsp.protocol.make_client_capabilities(),
    require("lsp-file-operations").default_capabilities()
  )
  require("lspsaga").setup({
    diagnostic = {
      on_insert = false,
      on_insert_follow = false,
      insert_winblend = 0,
      show_virt_line = false,
      show_code_action = true,
      show_source = true,
      jump_num_shortcut = true,
      --1 is max
      max_width = 0.7,
      custom_fix = nil,
      custom_msg = nil,
      text_hl_follow = false,
      border_follow = true,
      keys = {
        exec_action = "o",
        quit = "q",
        go_action = "g",
      },
    },
    ui = {
      -- This option only works in Neovim 0.9
      title = true,
      -- Border type can be single, double, rounded, solid, shadow.
      border = vim.g.border_chars,
      winblend = 0,
      expand = "",
      collapse = "",
      code_action = "",
      incoming = " ",
      outgoing = " ",
      hover = " ",
      kind = {},
    },
  })

  local on_attach = function(client, bufnr)
    if has_status then
      lsp_status.on_attach(client)
    end
    ---@type nio.lsp.types.ServerCapabilities
    local server_capabilities = client.server_capabilities

    if server_capabilities.inlayHintProvider then
      -- vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
    end

    if client.name == "ruff" then
      server_capabilities.hoverProvider = false
    end

    -- if client.server_capabilities.codeLensProvider and #vim.tbl_keys(client.server_capabilities.codeLensProvider) > 0 then
    --   vim.cmd("autocmd BufEnter,CursorHold,InsertLeave <buffer> lua vim.lsp.codelens.refresh()")
    -- end

    local lsp_util = require("config.lsp.util")
    local fzf = require("fzf-lua")
    local mappings = {
      n = {
        gd = vim.lsp.buf.definition,
        gt = vim.lsp.buf.type_definition,
        ge = function()
          vim.diagnostic.open_float(0, { scope = "line" })
        end,
        K = function()
          vim.lsp.buf.hover({
            silent = true,
          })
        end,
        gq = vim.lsp.buf.references,
        gr = lsp_util.rename,
        gD = function()
          lsp_util.preview("textDocument/definition")
        end,
        gC = fzf.lsp_outgoing_calls,
        gb = lsp_util.previous_win,

        gL = vim.lsp.codelens.run,
        ["]d"] = vim.diagnostic.goto_next,
        ["[d"] = vim.diagnostic.goto_prev,
        ["<C-s>"] = vim.lsp.buf.signature_help,
        ["<leader>la"] = function()
          vim.cmd("Lspsaga code_action")
        end,
        ["<leader>li"] = apply_pyright_imports,
        ["<leader>ls"] = fzf.lsp_document_symbols,
        ["<leader>lf"] = function()
          vim.lsp.buf.format({ timeout_ms = 5000 })
        end,
        ["<leader>lc"] = function()
          require("trouble").close()
        end,
        gh = function()
          local row = vim.api.nvim_win_get_cursor(0)[1] - 1
          local hint = vim.lsp.inlay_hint.get({
            bufnr = 0,
            range = {
              start = { line = row, character = 0 },
              ["end"] = { line = row + 1, character = 0 },
            },
          })[1]
          if hint == nil then
            vim.notify("No hint on current line", vim.log.levels.WARN, { title = "LSP" })
            return
          end

          local c = vim.lsp.get_client_by_id(hint.client_id)
          vim.lsp.util.apply_text_edits(hint.inlay_hint.textEdits, hint.bufnr, c.offset_encoding)

          c:request("textDocument/hover", {
            textDocument = { uri = vim.uri_from_bufnr(hint.bufnr) },
            position = hint.inlay_hint.position,
          })
        end,
      },
      i = {
        ["<C-s>"] = vim.lsp.buf.signature_help,
      },
      v = {
        ["<leader>lf"] = function()
          vim.lsp.buf.format({ timeout_ms = 5000 })
        end,
        K = function()
          vim.lsp.buf.hover()
        end,
      },
    }

    for mode, mode_mappings in pairs(mappings) do
      for keys, mapping in pairs(mode_mappings) do
        vim.api.nvim_buf_set_keymap(bufnr, mode, keys, "", { callback = mapping, noremap = true })
      end
    end
    vim.api.nvim_buf_set_keymap(
      bufnr,
      "x",
      "<leader>lf",
      "",
      { callback = vim.lsp.buf.range_formatting, noremap = true }
    )
  end

  require("config.lsp.settings").setup(on_attach, capabilities)
end

return M
