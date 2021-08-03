local M = {}

local function wrap_options(custom, handler)
  return function(opts)
    opts = opts and vim.tbl_extend(opts, custom) or custom
    if type(handler) == "string" then
      require("telescope.builtin")[handler](opts)
    else
      handler(opts)
    end
  end
end

function M.setup()
  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics,
    {
      signs = {
        priority = 5,
      },
      underline = false,
      virtual_text = {
        prefix = "●",
      },
    }
  )
  vim.lsp.handlers["textDocument/codeAction"] = wrap_options(
    { layout_strategy = "vertical", layout_config = { width = 100 } },
    "lsp_code_actions"
  )
  vim.lsp.handlers["textDocument/references"] = wrap_options(
    { layout_strategy = "vertical" },
    "lsp_references"
  )
  vim.lsp.handlers["textDocument/documentSymbol"] = function(opts)
    require("telescope.builtin").lsp_document_symbols(opts)
  end
  vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
    border = vim.g.border_chars,
  })
end
return M
