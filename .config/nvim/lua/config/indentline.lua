local M = {}
function M.post()
  require("ibl").setup({
    indent = { char = "│" },
    exclude = {
      buftypes = {
        "terminal",
      },
      filetypes = {
        "",
        "norg",
        "help",
        "markdown",
        "dapui_scopes",
        "dapui_stacks",
        "dapui_watches",
        "dapui_breakpoints",
        "dapui_hover",
        "dap-repl",
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
        "mason",
      },
    },
  })
end

return M
