local M = {}
function M.post()
  require("indent_blankline").setup({
    char = "│",
    buftype_exclude = { "terminal" },
    show_first_indent_level = false,
    show_current_context = true,
    filetype_exclude = {
      "",
      "help",
      "dapui_scopes",
      "dapui_stacks",
      "dapui_watches",
      "dapui_breakpoints",
      "dapui_hover",
      "LuaTree",
      "dbui",
      "term",
      "fugitive",
      "fugitiveblame",
      "NvimTree",
      "UltestSummary",
      "packer",
      "UltestOutput",
    },
  })
end
return M
