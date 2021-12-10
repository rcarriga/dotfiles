local M = {}

function M.multilineCommand(command)
  for line in vim.gsplit(command, "\n", true) do
    vim.cmd(vim.trim(line))
  end
end

function M.lua_map(args)
  local opts = { noremap = true, silent = true }
  if args.bufnr then
    vim.api.nvim_buf_set_keymap(
      args.bufnr,
      args.mode or "n",
      args.keys,
      "<cmd>lua " .. args.mapping .. "<CR>",
      opts
    )
  else
    vim.api.nvim_set_keymap(
      args.mode or "n",
      args.keys,
      "<cmd>lua " .. args.mapping .. "<CR>",
      opts
    )
  end
end
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

return M
