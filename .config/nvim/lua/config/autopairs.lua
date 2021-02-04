local M = {}

function M.post()
  require("nvim-autopairs").setup()

  local remap = vim.api.nvim_set_keymap
  local npairs = require("nvim-autopairs")

  -- skip it, if you use another global object
  _G.MUtils = {}

  vim.g.completion_confirm_key = ""
  MUtils.completion_confirm = function()
    if vim.fn.pumvisible() ~= 0 then
      if vim.fn.complete_info()["selected"] ~= -1 then
        vim.fn["compe#confirm"]()
        return npairs.esc("<c-y>")
      else
        vim.fn.nvim_select_popupmenu_item(0, false, false, {})
        vim.fn["compe#confirm"]()
        return npairs.esc("<c-n><c-y>")
      end
    else
      return npairs.check_break_line_char()
    end
  end

  remap("i", "<CR>", "v:lua.MUtils.completion_confirm()", {expr = true, noremap = true})
end
return M
