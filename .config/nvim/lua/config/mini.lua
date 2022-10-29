local M = {}

function M.post()
  local map = require("mini.map")
  map.setup({
    integrations = {
      map.gen_integration.builtin_search(),
      map.gen_integration.gitsigns(),
      map.gen_integration.diagnostic(),
    },
    symbols = {
      encode = nil,
      scroll_line = "█",
      scroll_view = "┃",
    },

    window = {
      side = "right",
      show_integration_count = true,
      width = 10,
      winblend = 0,
    },
  })
  vim.api.nvim_set_keymap("n", "<leader>m", "", { noremap = true, callback = map.toggle })
end

return M
