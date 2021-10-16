local M = {}
local util = require("util")

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
	capabilities.textDocument.completion.completionItem.documentationFormat = { "markdown" }
	capabilities.textDocument.completion.completionItem.snippetSupport = true
	capabilities.textDocument.completion.completionItem.preselectSupport = true
	capabilities.textDocument.completion.completionItem.insertReplaceSupport = true
	capabilities.textDocument.completion.completionItem.labelDetailsSupport = true
	capabilities.textDocument.completion.completionItem.deprecatedSupport = true
	capabilities.textDocument.completion.completionItem.commitCharactersSupport = true
	capabilities.textDocument.completion.completionItem.tagSupport = { valueSet = { 1 } }
	capabilities.textDocument.completion.completionItem.resolveSupport = {
		properties = {
			"documentation",
			"detail",
			"additionalTextEdits",
		},
	}
  capabilities = vim.tbl_extend("keep", capabilities or {}, lsp_status.capabilities)

  local lsp_sig = require("lsp_signature")
  local on_attach = function(client, bufnr)
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
      ge = "require('config.lsp.util').line_diagnostics()",
      K = "vim.lsp.buf.hover()",
      gi = "vim.lsp.buf.implementation()",
      gq = "vim.lsp.buf.references()",
      gr = "require('config.lsp.util').rename()",
      gD = "require('config.lsp.util').preview('textDocument/definition')",
      gb = "require('config.lsp.util').previous_win()",
      gL = "vim.lsp.codelens.run()",
      ["]d"] = "vim.diagnostic.goto_next({popup_opts = { border = vim.g.border_chars }})",
      ["[d"] = "vim.diagnostic.goto_prev({popup_opts = { border = vim.g.border_chars }})",
      ["<C-s>"] = "vim.lsp.buf.signature_help()",
      ["<space>la"] = "vim.lsp.buf.code_action()",
      ["<space>lt"] = "vim.lsp.buf.type_definition()",
      ["<space>ls"] = "vim.lsp.buf.document_symbol()",
      ["<space>lf"] = "vim.lsp.buf.formatting_sync()",
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
