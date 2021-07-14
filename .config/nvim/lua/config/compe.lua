local M = {}
function M.post()
  local t = function(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
  end

  -- Use (s-)tab to:
  --- move to prev/next item in completion menuone
  --- jump to prev/next snippet's placeholder
  _G.tab_complete = function()
    if vim.fn.pumvisible() == 1 then
      return t "<C-n>"
    elseif vim.fn.call("vsnip#available", {1}) == 1 then
      return t "<Plug>(vsnip-expand-or-jump)"
    else
      return t "<Tab>"
    end
  end
  _G.s_tab_complete = function()
    if vim.fn.pumvisible() == 1 then
      return t "<C-p>"
    elseif vim.fn.call("vsnip#jumpable", {-1}) == 1 then
      return t "<Plug>(vsnip-jump-prev)"
    else
      return t "<S-Tab>"
    end
  end

  vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
  vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
  vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
  vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
  require "compe".setup {
    enabled = true,
    debug = false,
    min_length = 1,
    preselect = "enable",
    documentation = {
      border = vim.g.border_chars
    },
    source = {
      spell = true,
      path = true,
      vsnip = true,
      calc = true,
      nvim_lsp = true,
      nvim_lua = true,
      ultisnips = true,
      vim_dadbod_completion = true,
      treesitter = true
    }
  }
end
return M
