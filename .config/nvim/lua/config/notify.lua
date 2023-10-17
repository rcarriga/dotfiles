local M = {}

function M.post()
  local base_stages = require("notify.stages.slide")("bottom_up")
  local notify = require("notify")

  notify.setup({
    render = "wrapped-compact",
    stages = {
      function(...)
        local opts = base_stages[1](...)
        if not opts then
          return
        end
        return opts
      end,
      unpack(base_stages, 2),
    },
    background_colour = "#121212",
    -- max_width = 120,
  })

  vim.api.nvim_set_keymap("n", "<leader>p", "", { callback = notify.dismiss })
end

return M
