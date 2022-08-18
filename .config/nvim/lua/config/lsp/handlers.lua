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
  if pcall(require, "telescope") then
    vim.lsp.handlers["textDocument/documentSymbol"] =
      require("telescope.builtin").lsp_document_symbols
    vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
      border = vim.g.border_chars,
    })
  end
  vim.lsp.handlers["textDocument/codeLens"] = vim.lsp.codelens.on_codelens
  local severity = {
    "error",
    "warn",
    "info",
    "info", -- map both hint and info to info?
  }
  vim.lsp.handlers["window/showMessage"] = function(_, method, params, client_id)
    vim.notify(
      method.message,
      severity[params.type],
      { title = vim.lsp.get_client_by_id(client_id).name }
    )
  end
  local handle_locations = function(err, result, ctx, config)
    local client_encoding = vim.lsp.get_client_by_id(ctx.client_id).offset_encoding
    if err then
      vim.notify(err.message)
      return
    end
    if result == nil then
      vim.notify("Location not found", "LSP")
      return
    end
    if vim.tbl_islist(result) and result[1] then
      vim.lsp.util.jump_to_location(result[1], client_encoding)

      if #result > 1 then
        vim.fn.setqflist(vim.lsp.util.locations_to_items(result, client_encoding))
        vim.cmd("TroubleToggle quickfix")
      end
    else
      vim.lsp.util.jump_to_location(result, client_encoding)
    end
  end
  vim.lsp.handlers["textDocument/definition"] = handle_locations
  vim.lsp.handlers["textDocument/references"] = handle_locations
end
return M
