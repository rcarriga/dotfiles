local M = {}

function M.post()
  vim.cmd(
    [[autocmd BufEnter * ++nested if winnr('$') == 1 && bufname() == 'NvimTree_' . tabpagenr() | quit | endif]]
  )

  vim.api.nvim_set_keymap(
    "n",
    "<leader>x",
    "<CMD>NvimTreeToggle<CR>",
    { silent = true, noremap = true }
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
