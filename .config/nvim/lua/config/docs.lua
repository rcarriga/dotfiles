local M = {}

function M.post()
  require("neogen").setup({
    enabled = true,
  })
end

return M

