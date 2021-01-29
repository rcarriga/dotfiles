
local M = {}
function M.post()
require'compe'.setup {
  enabled = true,
  debug = false,
  min_length = 1,
  preselect = 'enable',
  source = {
    path = true;
    buffer = true;
    vsnip = true;
    nvim_lsp = true;
    nvim_lua = true;
  };
}
end
return M
