local M = {}

function M.post()
  pcall(function()
    require("telescope").load_extension("notify")
  end)

  local stages_util = require("notify.stages.util")
  local notify = require("notify")
  notify.setup({
    background_colour = "#121212",
    fps = 60,
  })
  vim.api.nvim_set_keymap("n", "<leader>p", "", { callback = notify.dismiss })
end

return M
