local M = {}

function M.post()
  vim.g.nvim_tree_git_hl = 1
  
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
      default = "▸",
      open = "▾",
    },
    renderer = {
      indent_markers = {
        enable = true,
      },
    },
  }
  vim.cmd(
    [[autocmd BufEnter * ++nested if winnr('$') == 1 && bufname() == 'NvimTree_' . tabpagenr() | quit | endif]]
  )
  require("nvim-tree").setup({
    disable_netrw = false,
    view = {
      width = 45,
    },
    update_focused_file = {
      enable = true,
    },
  })
end

return M
