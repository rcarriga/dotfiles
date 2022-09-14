local M = {}

function M.get_python_path(workspace)
  -- Use activated virtualenv.
  local util = require("lspconfig/util")

  local path = util.path
  if vim.env.VIRTUAL_ENV then
    return path.join(vim.env.VIRTUAL_ENV, "bin", "python")
  end

  -- Find and use virtualenv in workspace directory.
  for _, pattern in ipairs({ "*", ".*" }) do
    local match = vim.fn.glob(path.join(workspace or vim.fn.getcwd(), pattern, "pyvenv.cfg"))
    if match ~= "" then
      return path.join(path.dirname(match), "bin", "python")
    end
  end

  -- Fallback to system Python.
  return vim.fn.exepath("python3") or vim.fn.exepath("python") or "python"
end

function M.kitty_scrollback()
  vim.cmd("silent! write! /tmp/kitty_scrollback")

  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_set_current_buf(buf)
  local data = io.open("/tmp/kitty_scrollback", "r"):read("*a"):gsub("\n", "\r\n")
  local term = vim.api.nvim_open_term(0, {})
  vim.api.nvim_chan_send(term, data)
end

return M
