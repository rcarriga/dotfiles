local M = {}

function M.setup()
  require("noice").setup({
    cmdline = {
      enabled = false,
    },
    messages = {
      enabled = false,
    },
    popupmenu = {
      enabled = false,
    },
    commands = {},
    notify = {
      enabled = false,
    },
    lsp = {
      progress = {
        enabled = false,
      },
      override = {
        ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
        ["vim.lsp.util.stylize_markdown"] = true,
        ["cmp.entry.get_documentation"] = true,
      },
    },
    presets = {
      lsp_doc_border = true,
    },
  })
  vim.lsp.handlers["textDocument/codeLens"] = vim.lsp.codelens.on_codelens
  local severity = {
    "error",
    "warn",
    "info",
    "info", -- map both hint and info to info?
  }
  vim.lsp.handlers["window/showMessage"] = function(_, method, params, client_id)
    local client = vim.lsp.get_client_by_id(client_id)
    vim.notify(method.message, severity[params.type], { title = client and client.name })
  end

  local base_logger = vim.lsp.handlers["window/logMessage"]
  local protocol = vim.lsp.protocol
  local msg_types = {
    [protocol.MessageType.Error] = "ERROR",
    [protocol.MessageType.Warning] = "WARN",
    [protocol.MessageType.Info] = "INFO",
  }
  local client_logs = {}
  vim.lsp.handlers["window/logMessage"] = function(...)
    local _, result, ctx, _ = ...
    local client_id = ctx.client_id
    local msg_type = msg_types[result.type or protocol.MessageType.Info]
    client_logs[client_id] = client_logs[client_id] or {}
    client_logs[client_id][#client_logs[client_id] + 1] = {
      type = msg_type,
      message = result.message,
    }

    return base_logger(...)
  end

  vim.api.nvim_create_user_command("LspLogs", function()
    local buf = vim.api.nvim_create_buf(false, true)
    local lines = {}
    for client_id, logs in pairs(client_logs) do
      local client = vim.lsp.get_client_by_id(client_id)
      local client_name = client and client.name or string.format("id=%d", client_id)
      lines[#lines + 1] = string.format("%s logs:", client_name)
      for _, log in ipairs(logs) do
        lines[#lines + 1] = string.format("%s | %s", log.type, log.message)
      end
    end
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    vim.api.nvim_buf_set_option(buf, "filetype", "log")
    vim.api.nvim_win_set_buf(0, buf)
  end, {})

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
