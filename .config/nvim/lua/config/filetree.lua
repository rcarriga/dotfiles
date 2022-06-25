local M = {}

function M.post()
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
    renderer = {
      icons = {
        glyphs = {
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
        },
      },
      indent_markers = {
        enable = true,
      },
    },
  })
end

return M
