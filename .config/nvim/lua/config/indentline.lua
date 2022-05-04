local M = {}
function M.post()
  require("indent_blankline").setup({
    char = "│",
    buftype_exclude = { "terminal" },
    show_first_indent_level = false,
    show_current_context = true,
    context_patterns = { "class", "function", "method", "if_statement", "block" },
    filetype_exclude = {
      "",
      "norg",
      "help",
      "markdown",
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
      "neotest-summary",
      "Outline",
      "lsp-installer",
    },
  })
end
return M
