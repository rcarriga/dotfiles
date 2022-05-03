local M = {}

function M.post()
  pcall(function()
    require("telescope").load_extension("notify")
  end)

  require("notify").setup({
    background_colour = "#121212",
    fps = 60,
  })
end

return M
