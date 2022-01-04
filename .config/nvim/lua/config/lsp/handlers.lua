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
  vim.lsp.handlers["textDocument/codeLens"] = vim.lsp.codelens.on_codelens
  vim.lsp.handlers["textDocument/definition"] = function(err, result, ctx, config)
    if err then
      vim.notify(err.message)
      return
    end
    if result == nil then
      vim.notify("Location not found")
      return
    end
    if vim.tbl_islist(result) then
      vim.lsp.util.jump_to_location(result[1])

      if #result > 1 then
        vim.fn.setqflist(vim.lsp.util.locations_to_items(result))
        vim.api.nvim_command("copen")
      end
    else
      vim.lsp.util.jump_to_location(result)
    end
  end
end
return M
