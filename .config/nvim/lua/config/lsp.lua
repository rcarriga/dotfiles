local M = {}

function M.post()
  local has_status, lsp_status = pcall(require, "lsp-status")
  if has_status then
    lsp_status.register_progress()
  end
  vim.diagnostic.config({
    signs = {
      priority = 5,
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
  vim.cmd([[
    sign define DiagnosticSignError text=▶ texthl=DiagnosticError numhl=DiagnosticsError
    sign define DiagnosticSignWarn text=▶ texthl=DiagnosticWarn numhl=DiagnosticsWarning
    sign define DiagnosticSignInfo text=▶ texthl=DiagnosticInfo numhl=DiagnosticsInformation
    sign define DiagnosticSignHint text=▶ texthl=DiagnosticHint numhl=DiagnosticsHint
  ]])

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
  require("lsp-inlayhints").setup({
    inlay_hints = {
      parameter_hints = {
        show = true,
        prefix = "← ",
        separator = ", ",
        remove_colon_start = false,
        remove_colon_end = false,
      },
      type_hints = {
        -- type and other hints
        show = true,
        prefix = "",
        separator = ", ",
        remove_colon_start = false,
        remove_colon_end = false,
      },
      -- separator between types and parameter hints. Note that type hints are
      -- shown before parameter
      labels_separator = "",
      -- whether to align to the length of the longest line in the file
      max_len_align = false,
      -- padding from the left if max_len_align is true
      max_len_align_padding = 1,
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
    vim.keymap.set("n", "<leader>a", aerial.toggle, { buffer = bufnr })
    pcall(function()
      require("lsp-inlayhints").on_attach(client, bufnr)
    end)
    -- lsp_sig.on_attach({
    --   floating_window_above_cur_line = true,
    --   bind = true,
    --   hint_enable = false,
    --   hi_parameter = "LspSelectedParam",
    --   zindex = 50,
    --   handler_opts = {
    --     border = vim.g.border_chars,
    --   },
    -- })

    if client.server_capabilities.codeLensProvider then
      vim.cmd("autocmd BufEnter,CursorHold,InsertLeave <buffer> lua vim.lsp.codelens.refresh()")
    end

    local fold_win

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
          if fold_win and vim.api.nvim_win_is_valid(fold_win) then
            vim.api.nvim_set_current_win(fold_win)
          end
          fold_win = require("ufo").peekFoldedLinesUnderCursor()
          if not fold_win then
            vim.lsp.buf.hover()
          else
            vim.api.nvim_win_set_option(fold_win, "winhl", "Normal:Normal")
            vim.api.nvim_win_set_option(fold_win, "winblend", 0)
          end
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
        ["<space>la"] = function()
          vim.cmd("Lspsaga code_action")
        end,
        ["<space>ls"] = fzf.lsp_document_symbols,
        ["<space>lf"] = function()
          vim.lsp.buf.format({ timeout_ms = 5000 })
        end,
        ["<space>lc"] = function()
          vim.cmd("TroubleClose")
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
  require("ufo").setup()
end

return M
