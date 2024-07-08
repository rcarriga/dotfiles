local M = {}

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
  pcall(function()
    capabilities =
        vim.tbl_deep_extend("keep", require("cmp_nvim_lsp").default_capabilities(), capabilities)
  end)
  capabilities.textDocument.foldingRange = {
    dynamicRegistration = false,
    lineFoldingOnly = true,
  }
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

  local aerial = require("aerial")
  aerial.setup({
    attach_mode = "global",
    backends = { "lsp", "treesitter", "markdown", "man" },
    layout = {
      min_width = 28,
    },
    show_guides = true,
    filter_kind = false,
    guides = {
      mid_item = "├ ",
      last_item = "└ ",
      nested_top = "│ ",
      whitespace = "  ",
    },
    keymaps = {
      ["[y"] = "actions.prev",
      ["]y"] = "actions.next",
      ["[Y"] = "actions.prev_up",
      ["]Y"] = "actions.next_up",
      ["{"] = false,
      ["}"] = false,
      ["[["] = false,
      ["]]"] = false,
    },
  })
  local on_attach = function(client, bufnr)
    if has_status then
      lsp_status.on_attach(client)
    end
    ---@type nio.lsp.types.ServerCapabilities
    local server_capabilities = client.server_capabilities

    vim.keymap.set("n", "<leader>a", aerial.toggle, { buffer = bufnr })

    if server_capabilities.inlayHintProvider then
      vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
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
        K = vim.lsp.buf.hover,
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
        ["<space>la"] = function()
          vim.cmd("Lspsaga code_action")
        end,
        ["<space>ls"] = fzf.lsp_document_symbols,
        ["<space>lf"] = function()
          vim.lsp.buf.format({ timeout_ms = 5000 })
        end,
        ["<space>lc"] = function()
          require("trouble").close()
        end,
      },
      i = {
        ["<C-s>"] = vim.lsp.buf.signature_help,
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
      "<space>lf",
      "",
      { callback = vim.lsp.buf.range_formatting, noremap = true }
    )
  end

  require("config.lsp.settings").setup(on_attach, capabilities)
end

return M
