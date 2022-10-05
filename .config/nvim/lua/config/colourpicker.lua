local M = {}

function M.post()
  require("ccc").setup({
    highlighter = {
      auto_enable = true,
    },
  })
end

return M
