local M = {}

local windows = {}

local function set_auto_close()
  vim.cmd([[ au CursorMoved * ++once lua require('config.lsp.util').remove_wins() ]])
end

local function fit_to_node(window)
  local node = require("nvim-treesitter.ts_utils").get_node_at_cursor()
  if node:type() == "identifier" then
    node = node:parent()
  end
  local start_row, _, end_row, _ = node:range()
  local new_height = math.min(math.max(end_row - start_row + 6, 15), 30)
  vim.api.nvim_win_set_height(window, new_height)
end

local open_preview_win = function(target, position)
  local buffer = vim.uri_to_bufnr(target)
  local win_opts = {
    relative = "cursor",
    row = 4,
    col = 4,
    width = 120,
    height = 15,
    border = vim.g.border_chars,
  }
  -- Don't jump immediately, we need the windows list to contain ID before autocmd
  windows[#windows + 1] = vim.api.nvim_open_win(buffer, false, win_opts)
  vim.api.nvim_set_current_win(windows[#windows])
  vim.api.nvim_buf_set_option(buffer, "bufhidden", "wipe")
  set_auto_close()
  vim.api.nvim_win_set_cursor(windows[#windows], position)
  fit_to_node(windows[#windows])
end

function M.remove_wins()
  local current = vim.api.nvim_get_current_win()
  for i = #windows, 1, -1 do
    if current == windows[i] then
      break
    end
    pcall(vim.api.nvim_win_close, windows[i], true)
    table.remove(windows, i)
  end
  if #windows > 0 then
    set_auto_close()
  end
end

function M.previous_win()
  if #windows > 1 then
    vim.api.nvim_set_current_win(windows[#windows - 1])
  elseif #windows == 1 then
    vim.api.nvim_win_close(windows[#windows], true)
  end
end

function M.rename()
  local bufnr = vim.api.nvim_get_current_buf()
  local params = vim.lsp.util.make_position_params()
  vim.lsp.buf.document_highlight()
  local initial = vim.fn.expand("<cword>")
  local prompt_buf = vim.api.nvim_create_buf(false, true)

  vim.api.nvim_buf_set_option(prompt_buf, "buftype", "prompt")

  local prompt_window = vim.api.nvim_open_win(prompt_buf, true, {
    relative = "cursor",
    row = 1,
    col = 1,
    width = 20,
    height = 1,
    border = vim.g.border_chars,
    style = "minimal",
  })
  vim.cmd(
    "au BufHidden <buffer> lua pcall(require('config.lsp.util')._close_rename,"
      .. prompt_window
      .. ","
      .. prompt_buf
      .. ","
      .. bufnr
      .. ")"
  )
  vim.fn.prompt_setprompt(prompt_buf, " → ")

  vim.fn.prompt_setcallback(prompt_buf, function(new_name)
    if not (new_name and #new_name > 0) then
      return
    end
    params.newName = new_name
    vim.cmd("norm <C-W>p")
    vim.lsp.buf_request(bufnr, "textDocument/rename", params)
    M._close_rename(prompt_window, prompt_buf, bufnr)
  end)

  vim.cmd("norm i" .. initial)
  vim.cmd("startinsert!")
end

function M._close_rename(prompt_window, prompt_buf, origin_buf)
  vim.schedule(function()
    pcall(vim.api.nvim_win_close, prompt_window, true)
    pcall(vim.api.nvim_buf_delete, prompt_buf, { force = true })
    vim.lsp.util.buf_clear_references(origin_buf)
    vim.cmd("stopinsert")
  end)
end

function M.preview(request)
  local params = vim.lsp.util.make_position_params()
  pcall(vim.lsp.buf_request, 0, request, params, function(_, result, _)
    if not result then
      return
    end
    local data = vim.tbl_islist(result) and result[1] or result
    local target = data.targetUri or data.uri
    local range = data.targetRange or data.range
    open_preview_win(target, { range.start.line + 1, range.start.character })
  end)
end

return M
