local M = {}

function M.post()
  require("nvim-tree").setup({
    disable_netrw = false,
    auto_close = 1,
    view = {
      width = 45,
    },
    update_focused_file = {
      enable = true,
    },
  })
end

return M
