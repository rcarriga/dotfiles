local M = {}
local util = require("util")
function M.pre()
  vim.g.symbols_outline = {
    highlight_hovered_item = true,
    show_guides = true,
    auto_preview = true,
    position = "right",
    relative_width = true,
    width = 25,
    auto_close = false,
    show_numbers = false,
    show_relative_numbers = false,
    show_symbol_details = true,
    preview_bg_highlight = "Normal",
    keymaps = { -- These keymaps can be a string or a table for multiple keys
      close = { "<Esc>", "q" },
      goto_location = "<Cr>",
      focus_location = "o",
      hover_symbol = "<C-space>",
      toggle_preview = "K",
      rename_symbol = "r",
      code_actions = "a",
    },
    lsp_blacklist = {},
    symbol_blacklist = {},
    symbols = {
      File = { icon = "", hl = "TSURI" },
      Module = { icon = "", hl = "TSNamespace" },
      Namespace = { icon = "", hl = "TSNamespace" },
      Package = { icon = "", hl = "TSNamespace" },
      Class = { icon = "ﴯ", hl = "TSType" },
      Method = { icon = "ƒ", hl = "TSMethod" },
      Property = { icon = "", hl = "TSMethod" },
      Field = { icon = "", hl = "TSField" },
      Constructor = { icon = "", hl = "TSConstructor" },
      Enum = { icon = "ℰ", hl = "TSType" },
      Interface = { icon = "ﰮ", hl = "TSType" },
      Function = { icon = "", hl = "TSFunction" },
      Variable = { icon = "", hl = "TSConstant" },
      Constant = { icon = "", hl = "TSConstant" },
      String = { icon = "𝓐", hl = "TSString" },
      Number = { icon = "#", hl = "TSNumber" },
      Boolean = { icon = "⊨", hl = "TSBoolean" },
      Array = { icon = "", hl = "TSConstant" },
      Object = { icon = "⦿", hl = "TSType" },
      Key = { icon = "🔐", hl = "TSType" },
      Null = { icon = "NULL", hl = "TSType" },
      EnumMember = { icon = "", hl = "TSField" },
      Struct = { icon = "𝓢", hl = "TSType" },
      Event = { icon = "🗲", hl = "TSType" },
      Operator = { icon = "+", hl = "TSOperator" },
      TypeParameter = { icon = "𝙏", hl = "TSParameter" },
    },
  }
end

function M.post()
  local lsp_status = require("lsp-status")
  lsp_status.register_progress()
  vim.diagnostic.config({
    signs = {
      priority = 5,
    },
    underline = false,
    virtual_text = {
      prefix = "●",
      source = "always",
    },
    float = {
      show_header = false,
      border = vim.g.border_chars,
    },
    severity_sort = true,
  })
  util.multilineCommand([[
    sign define DiagnosticSignError text=▶ texthl=DiagnosticsError numhl=DiagnosticsError
    sign define DiagnosticSignWarn text=▶ texthl=DiagnosticsWarning numhl=DiagnosticsWarning
    sign define DiagnosticSignInfo text=▶ texthl=DiagnosticsInformation numhl=DiagnosticsInformation
    sign define DiagnosticSignHint text=▶ texthl=DiagnosticsHint numhl=DiagnosticsHint
  ]])

  require("config.lsp.handlers").setup()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities = vim.tbl_deep_extend("force", capabilities, lsp_status.capabilities)
  capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

  local lsp_sig = require("lsp_signature")
  local on_attach = function(client, bufnr)
    if client.resolved_capabilities.document_highlight then
      vim.cmd([[
        augroup LspReferences
          au CursorHold  <buffer> lua vim.lsp.buf.document_highlight()
          au CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()
          au CursorMoved <buffer> lua vim.lsp.buf.clear_references()
        augroup END
      ]])
    end

    lsp_status.on_attach(client)
    lsp_sig.on_attach({
      floating_window_above_cur_line = true,
      bind = true,
      hint_enable = false,
      hi_parameter = "LspSelectedParam",
      zindex = 50,
      handler_opts = {
        border = vim.g.border_chars,
      },
    })

    if client.resolved_capabilities.code_lens then
      vim.cmd("autocmd BufEnter,CursorHold,InsertLeave <buffer> lua vim.lsp.codelens.refresh()")
    end

    local mappings = {
      gd = "vim.lsp.buf.definition()",
      gt = "vim.lsp.buf.type_definition()",
      ge = "vim.diagnostic.open_float(0, { scope = 'line' })",
      K = "vim.lsp.buf.hover()",
      gi = "vim.lsp.buf.implementation()",
      gq = "vim.lsp.buf.references()",
      gr = "require('config.lsp.util').rename()",
      gD = "require('config.lsp.util').preview('textDocument/definition')",
      gb = "require('config.lsp.util').previous_win()",
      gL = "vim.lsp.codelens.run()",
      ["]d"] = "vim.diagnostic.goto_next()",
      ["[d"] = "vim.diagnostic.goto_prev()",
      ["<C-s>"] = "vim.lsp.buf.signature_help()",
      ["<space>la"] = "vim.lsp.buf.code_action()",
      -- ["<space>lt"] = "vim.lsp.buf.type_definition()",
      ["<space>ls"] = "vim.lsp.buf.document_symbol()",
      ["<space>lf"] = "vim.lsp.buf.formatting_sync()",
      ["<space>lt"] = "vim.cmd[[SymbolsOutline]]",
    }

    for keys, mapping in pairs(mappings) do
      util.lua_map({ keys = keys, mapping = mapping, bufnr = bufnr })
    end
    util.lua_map({
      keys = "<space>lf",
      mapping = "vim.lsp.buf.range_formatting()",
      bufnr = bufnr,
      mode = "x",
    })
  end

  require("config.lsp.settings").setup(on_attach, capabilities)
end

return M
