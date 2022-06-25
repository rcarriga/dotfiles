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
      require("neotest-go"),
      require("neotest-vim-test")({
        allow_file_types = {"ruby", "typescript"},
      }),
    },
  })
end

return M
