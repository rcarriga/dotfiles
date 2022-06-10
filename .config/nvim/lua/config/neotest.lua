local M = {}

function M.post()
  require("neotest").setup({
    icons = {
      running = "↻",
    },
    adapters = {
      require("neotest-python")({
        dap = { justMyCode = false, console = "integratedTerminal" },
      }),
      require("neotest-plenary"),
      require("neotest-vim-test")({
        ignore_file_types = { "python", "vim", "lua" },
      }),
    },
  })
end

return M
