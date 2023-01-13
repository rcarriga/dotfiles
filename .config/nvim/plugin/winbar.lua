function WinBar()
  local buf = vim.api.nvim_win_get_buf(vim.g.statusline_winid)
  local path = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(buf), ":p")
  local cwd = string.gsub(vim.loop.cwd(), "([^%w])", "%%%1") -- escape non-word characters
  path = path:gsub(cwd, ".")
  path = path:gsub(os.getenv("HOME"), "~")
  local elems = vim.split(path, "/", { trimempty = true })
  return "%#WinBarPath#" .. table.concat(elems, " %#WinBarSep# %#WinBarPath#") .. " %#WinBar#"
end

vim.opt.winbar = ""
local no_winbar_ft = { Trouble = true }

vim.api.nvim_create_autocmd("BufWinEnter", {
  callback = function()
    local buf = tonumber(vim.fn.expand("<abuf>"))
    vim.schedule(function()
      local winbar = ""
      if not vim.api.nvim_buf_is_valid(buf) then
        return
      end
      if vim.api.nvim_buf_get_option(buf, "buftype") == "" then
        winbar = "%!v:lua.WinBar()"
      end
      local win = vim.fn.bufwinid(buf)
      if win == -1 then
        return
      end
      if vim.api.nvim_win_get_option(win, "winbar") ~= "" then
        return
      end
      if no_winbar_ft[vim.api.nvim_buf_get_option(buf, "filetype")] then
        return
      end
      if vim.api.nvim_win_get_config(win).relative == "" then
        pcall(vim.api.nvim_win_set_option, win, "winbar", winbar)
      end
    end)
  end,
})
