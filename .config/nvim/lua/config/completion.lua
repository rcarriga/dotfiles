local M = {}
function M.pre()
  vim.cmd(
    [[ino <silent><expr> <CR>    pumvisible() ? (complete_info().selected == -1 ? "\<C-e><CR>" : "\<C-y>") : "\<CR>"]]
  )

  vim.g.coq_settings = {
    auto_start = "shut-up",
    clients = {
      lsp = {
        weight_adjust = 1,
      },
      snippets = {
        enabled = false,
      },
      tree_sitter = {
        weight_adjust = -1,
      },
      buffers = {
        weight_adjust = -0.5,
        enabled = false,
      },
    },
    keymap = {
      recommended = false,
      jump_to_mark = "<C-s>",
    },
    display = {
      icons = {
        mode = "short",
      },
      preview = {
        positions = { north = 2, south = 3, west = 4, east = 1 },
      },
    },
  }
end
function M.post()
  require("coq_3p")({
    -- { src = "nvimlua", short_name = "nvim" },
    { src = "bc", short_name = "MATH", precision = 6 },
    { src = "figlet", short_name = "BIG" },
  })
end
return M
