local M = {}

function M.post()
  require("telescope").load_extension("notify")

  require("notify").setup({ background_colour = "#121212" })
end

return M
