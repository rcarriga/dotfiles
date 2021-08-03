local M = {}
local util = require("util")

function M.post()
  local lsp_status = require("lsp-status")
  lsp_status.register_progress()
  util.multilineCommand([[
    sign define LspDiagnosticsSignError text=▶ texthl=LspDiagnosticsDefaultError numhl=LspDiagnosticsDefaultError
    sign define LspDiagnosticsSignWarning text=▶ texthl=LspDiagnosticsDefaultWarning numhl=LspDiagnosticsDefaultWarning
    sign define LspDiagnosticsSignInformation text=▶ texthl=LspDiagnosticsDefaultInformation numhl=LspDiagnosticsDefaultInformation
    sign define LspDiagnosticsSignHint text=▶ texthl=LspDiagnosticsDefaultHint numhl=LspDiagnosticsDefaultHint
  ]])

  require("config.lsp.handlers").setup()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities.textDocument.completion.completionItem.snippetSupport = true
  capabilities = vim.tbl_extend("keep", capabilities or {}, lsp_status.capabilities)

  local on_attach = function(client, bufnr)
    lsp_status.on_attach(client)
    require("lsp_signature").on_attach({
      bind = true,
      hint_enable = false,
      hi_parameter = "LspSelectedParam",
      handler_opts = {
        border = vim.g.border_chars,
      },
    })
    local mappings = {
      gd = "vim.lsp.buf.definition()",
      ge = "require('config.lsp.util').line_diagnostics(" .. client.id .. ")",
      K = "vim.lsp.buf.hover()",
      gi = "vim.lsp.buf.implementation()",
      gq = "vim.lsp.buf.references()",
      gr = "require('config.lsp.util').rename()",
      gD = "require('config.lsp.util').preview('textDocument/definition')",
      gb = "require('config.lsp.util').previous_win()",
      ["]d"] = "vim.lsp.diagnostic.goto_next()",
      ["[d"] = "vim.lsp.diagnostic.goto_prev()",
      ["<C-s>"] = "vim.lsp.buf.signature_help()",
      ["<space>la"] = "vim.lsp.buf.code_action()",
      ["<space>lt"] = "vim.lsp.buf.type_definition()",
      ["<space>ls"] = "vim.lsp.buf.document_symbol()",
      ["<space>lf"] = "vim.lsp.buf.formatting()",
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
