local M = {}

function M.post()
  require("neotest").setup({
    adapters = {
      require("neotest-python")({
        dap = { justMyCode = false },
      }),
      require("neotest-plenary"),
      require("neotest-vim-test"),
    },
    icons = {
      running = "⟳",
      passed = "",
      failed = "",
      skipped = "",
    },
  })
end

return M
