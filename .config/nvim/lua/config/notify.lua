local util = require("util")

local M = {}

function M.display()
  local buf = vim.api.nvim_create_buf(false, true)
  local notifications = require("notify").notifications
  local lines = {}
  for _, notif in pairs(notifications) do
  end
  util.lua_map({ keys = keys, mapping = mapping, bufnr = bufnr })
end
function M.post()
  require("notify").setup({ background_colour = "#121212" })
end
return M
