local M = {}

function M.post()
  vim.g.nvim_tree_git_hl = 1
  vim.g.nvim_tree_indent_markers = 1
  vim.g.nvim_tree_icons = {
    default = "",
    symlink = "",
    git = {
      unstaged = "✗",
      staged = "✓",
      unmerged = "",
      renamed = "➜",
      untracked = "★",
    },
    folder = {
      default = "▾",
      open = "▸",
    },
  }
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
