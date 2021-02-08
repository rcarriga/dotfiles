local M = {}

function M.post()
  require("lspkind").init(
    {
      with_text = false,
    }
  )
end

return M
