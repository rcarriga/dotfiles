local M = {}

function M.setup()
  vim.lsp.handlers["textDocument/publishDiagnostics"] =
      vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
        update_in_insert = false,
      })
  vim.lsp.handlers["textDocument/codeLens"] = vim.lsp.codelens.on_codelens

  local handle_locations = function(err, result, ctx, _)
    local client_encoding = vim.lsp.get_client_by_id(ctx.client_id).offset_encoding
    if err then
      vim.notify(err.message)
      return
    end
    if result == nil then
      vim.notify("Location not found", "LSP")
      return
    end
    if vim.islist(result) and result[1] then
      vim.lsp.util.jump_to_location(result[1], client_encoding)

      if #result > 1 then
        vim.fn.setqflist(vim.lsp.util.locations_to_items(result, client_encoding))
        require("trouble").open({ mode = "quickfix" })
      end
    else
      vim.lsp.util.jump_to_location(result, client_encoding)
    end
  end
  vim.lsp.handlers["textDocument/definition"] = handle_locations
  vim.lsp.handlers["textDocument/typeDefinition"] = handle_locations
  vim.lsp.handlers["textDocument/references"] = handle_locations
end

return M
