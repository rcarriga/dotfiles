local M = {}

function M.post()
  local lsp_status = require("lsp-status")
  lsp_status.register_progress()
  vim.fn.sign_define(
    "LspDiagnosticsSignError",
    {text = "", texthl = "LspDiagnosticsDefaultError", numhl = "LspDiagnosticsDefaultError"}
  )
  vim.fn.sign_define(
    "LspDiagnosticsSignWarning",
    {text = "", texthl = "LspDiagnosticsDefaultWarning", numhl = "LspDiagnosticsDefaultWarning"}
  )
  vim.fn.sign_define(
    "LspDiagnosticsSignInformation",
    {text = "", texthl = "LspDiagnosticsDefaultInformation", numhl = "LspDiagnosticsDefaultInformation"}
  )
  vim.fn.sign_define(
    "LspDiagnosticsSignHint",
    {text = "", texthl = "LspDiagnosticsDefaultHint", numhl = "LspDiagnosticsDefaultHint"}
  )
  require("lspsaga").init_lsp_saga(
    {
      border_style = "single",
      code_action_prompt = {
        virtual_text = false
      }
    }
  )

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
        handler_opts = {
          border = "single"
        }
      }
    )
    local function buf_set_keymap(...)
      vim.api.nvim_buf_set_keymap(bufnr, ...)
    end
    -- Mappings.
    local opts = {noremap = true, silent = true}
    buf_set_keymap("n", "gd", "<Cmd>lua vim.lsp.buf.definition()<CR>", opts)
    buf_set_keymap("n", "ge", "<cmd>lua require'lspsaga.diagnostic'.show_line_diagnostics()<CR>", opts)
    buf_set_keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
    buf_set_keymap("n", "<M-d>", "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<CR>", opts)
    buf_set_keymap("n", "<M-u>", "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<CR>", opts)
    buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
    buf_set_keymap("n", "gr", "<cmd>lua require('lspsaga.rename').rename()<CR>", opts)
    buf_set_keymap("n", "[d", "<cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_prev()<CR>", opts)
    buf_set_keymap("n", "]d", "<cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_next()<CR>", opts)
    buf_set_keymap("n", "<space>la", "<cmd>lua require('lspsaga.codeaction').code_action()<CR>", opts)
    buf_set_keymap("v", "<space>la", "<cmd>lua require('lspsaga.codeaction').range_code_action()<CR>", opts)
    buf_set_keymap("n", "<space>lt", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
    buf_set_keymap("n", "<space>ls", "<cmd>lua vim.lsp.buf.document_symbol()<CR>", opts)
    buf_set_keymap("n", "<space>lf", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
    buf_set_keymap("n", "<space>ln", "<cmd>lua require('config.lsp_codelens').run()<CR>", opts)
    buf_set_keymap("n", "gq", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
    require("util").multilineCommand [[
      augroup CodeLensRegfresh
        au!
        au BufWrite,BufEnter * lua require("config.lsp_codelens").refresh()
      augroup END

      hi link LspCodeLens Hidden
      hi link LspReferenceRead Error
      hi link LspReferenceText Error
      hi link LspReferenceWrite Error
    ]]
  end

  require("config.lsp.settings").setup(on_attach, capabilities)
  vim.split(package.path .. ";" .. vim.fn.expand("$HOME/.cache/nvim/site/packer/?.lua"), ";")
end

return M
