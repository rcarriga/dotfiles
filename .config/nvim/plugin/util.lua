P = function(...)
  local obj = select("#", ...) == 1 and select(1, ...) or { ... }
  local s = type(obj) == "string" and obj or vim.inspect(obj)
  if vim.in_fast_event() then
    vim.schedule(function()
      print(s)
    end)
  else
    print(s)
  end
end

PP = vim.schedule_wrap(function(...)
  local is_string = select("#", ...) == 1 and type(select(1, ...)) == "string"
  local buf = vim.api.nvim_create_buf(false, true)
  local lines = vim.split(is_string and select(1, ...) or vim.inspect(...), "\n", { plain = true })
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.cmd("vsplit")
  vim.api.nvim_win_set_buf(0, buf)
end)

-- Find highlight group under cursor for changing colorschemes
vim.cmd([[
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
]])
