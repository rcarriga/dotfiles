local M = {}

function M.line_diagnostics(client_id)
  vim.lsp.diagnostic.show_line_diagnostics(
    {border = vim.g.border_chars},
    vim.fn.bufnr(),
    vim.fn.line(".") - 1,
    client_id
  )
end

local windows = {}

local function set_auto_close()
  vim.cmd [[ au CursorMoved * ++once lua require('config.lsp.util').remove_wins() ]]
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
    border = vim.g.border_chars
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

local function make_prompt(opts)
  local prompt_buf = vim.api.nvim_create_buf(false, true)

  vim.api.nvim_buf_set_option(prompt_buf, "buftype", "prompt")

  local prompt_window =
    vim.api.nvim_open_win(
    prompt_buf,
    true,
    {relative = "cursor", row = 1, col = 1, width = 20, height = 1, border = "single", style = "minimal"}
  )
  vim.fn.prompt_setprompt(prompt_buf, opts.prompt)

  vim.fn.prompt_setcallback(
    prompt_buf,
    function(text)
      if opts.callback(text) then
        vim.api.nvim_win_close(prompt_window, true)
        vim.api.nvim_buf_delete(prompt_buf, {force = true})
      end
    end
  )

  if opts.initial then
    vim.cmd("norm i" .. opts.initial)
  end
  vim.cmd("startinsert")

  return prompt_buf, prompt_window
end

function M.rename()
  local bufnr = vim.api.nvim_get_current_buf()
  local params = vim.lsp.util.make_position_params()
  make_prompt(
    {
      prompt = " → ",
      callback = function(new_name)
        if not (new_name and #new_name > 0) then
          return true
        end
        params.newName = new_name
        vim.lsp.buf_request(bufnr, "textDocument/rename", params)
        return true
      end
    }
  )
end

function M.preview(request)
  local params = vim.lsp.util.make_position_params()
  pcall(
    vim.lsp.buf_request,
    0,
    request,
    params,
    function(_, _, result)
      if not result then
        return
      end
      local data = result[1]
      local target = data.targetUri or data.uri
      local range = data.targetRange or data.range
      open_preview_win(target, {range.start.line + 1, range.start.character})
    end
  )
end

function M.jump_to_diagnostic(direction)
  local diagnostics = vim.lsp.diagnostic.get(0)
  local positions = vim.tbl_map(function (val)
    return {val.range.start.line + 1, val.range.start.character}
  end, diagnostics)
  local cur_pos = vim.fn.getcurpos()
  if direction == 1 then
    for _, position in ipairs(positions) do
      if position[1] > cur_pos[2] then
        vim.fn.setpos(".", {0, position[1], position[2], cur_pos[4]})
        return
      end
    end
  else
    for _, position in ipairs(vim.fn.reverse(positions)) do
      if position[1] < cur_pos[2] then
        vim.fn.setpos(".", {0, position[1], position[2], cur_pos[4]})
        return
      end
    end
  end

end

return M
