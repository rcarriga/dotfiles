local M = {}
local lsp_util = require("config.lsp.util")
local util = require("util")

vim.lsp.util.close_preview_autocmd = function(events, winnr)
  -- I prefer to keep the preview (especially for signature_help) open while typing in insert mode
  events =
    vim.tbl_filter(
    function(v)
      return v ~= "CursorMovedI" and v ~= "BufLeave"
    end,
    events
  )
  vim.api.nvim_command(
    "autocmd " ..
      table.concat(events, ",") .. " <buffer> ++once lua pcall(vim.api.nvim_win_close, " .. winnr .. ", true)"
  )
end

function M.post()
  local lsp_status = require("lsp-status")
  lsp_status.register_progress()
  util.multilineCommand [[
    sign define LspDiagnosticsSignError text=▶ texthl=LspDiagnosticsDefaultError numhl=LspDiagnosticsDefaultError
    sign define LspDiagnosticsSignWarning text=▶ texthl=LspDiagnosticsDefaultWarning numhl=LspDiagnosticsDefaultWarning
    sign define LspDiagnosticsSignInformation text=▶ texthl=LspDiagnosticsDefaultInformation numhl=LspDiagnosticsDefaultInformation
    sign define LspDiagnosticsSignHint text=▶ texthl=LspDiagnosticsDefaultHint numhl=LspDiagnosticsDefaultHint
  ]]

  require("config.lsp.handlers").setup()
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities.textDocument.completion.completionItem.snippetSupport = true
  capabilities = vim.tbl_extend("keep", capabilities or {}, lsp_status.capabilities)

  local lsp_sig = require("lsp_signature")
  local on_attach = function(client, bufnr)
    lsp_status.on_attach(client)
    lsp_sig.on_attach(
      {
        bind = true,
        hint_enable = false,
        hi_parameter = "LspSelectedParam",
        handler_opts = {
          border = vim.g.border_chars
        }
      }
    )
    local mappings = {
      gd = "vim.lsp.buf.definition()",
      ge = "require('config.lsp.util').line_diagnostics(" .. client.id .. ")",
      K = "vim.lsp.buf.hover()",
      gi = "vim.lsp.buf.implementation()",
      gq = "vim.lsp.buf.references()",
      gr = "require('config.lsp.util').rename()",
      gD = "require('config.lsp.util').preview('textDocument/definition')",
      gb = "require('config.lsp.util').previous_win()",
      ["<C-s>"] = "vim.lsp.buf.signature_help()",
      ["<space>la"] = "vim.lsp.buf.code_action()",
      ["<space>lt"] = "vim.lsp.buf.type_definition()",
      ["<space>ls"] = "vim.lsp.buf.document_symbol()",
      ["<space>lf"] = "vim.lsp.buf.formatting()"
    }

    for keys, mapping in pairs(mappings) do
      lsp_util.lua_map({keys = keys, mapping = mapping, bufnr = bufnr})
    end
    lsp_util.lua_map({keys = "<space>lf", mapping = "vim.lsp.buf.range_formatting()", bufnr = bufnr, mode = "x"})
  end

  require("config.lsp.settings").setup(on_attach, capabilities)
end

return M
