local M = {}
function M.post()
  require "compe".setup {
    enabled = true,
    debug = false,
    min_length = 1,
    preselect = "enable",
    source = {
      path = true,
      vsnip = true,
      calc = true,
      nvim_lsp = true,
      nvim_lua = true,
      vim_dadbod_completion = true,
      treesitter = true,
    }
  }
end
return M
