local M = {}

function M.post()
  pcall(function() require("telescope").load_extension("notify") end)

  local notify_renderers = require("notify.render")

  require("notify").setup({
    background_colour = "#121212",
    -- stages = "slide",
    render = function(bufnr, notif, highlights)
      if notif.title[1] == "" then
        return notify_renderers.minimal(bufnr, notif, highlights)
      else
        return notify_renderers.default(bufnr, notif, highlights)
      end
    end,
  })
end

return M
