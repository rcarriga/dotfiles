P = vim.schedule_wrap(function(...)
  print(vim.inspect(...))
end)

PP = vim.schedule_wrap(function(...)
  local is_string = select("#", ...) == 1 and type(select(1, ...)) == "string"
  local buf = vim.api.nvim_create_buf(false, true)
  local lines = vim.split(is_string and select(1, ...) or vim.inspect(...), "\n", { plain = true })
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.cmd("vsplit")
  vim.api.nvim_win_set_buf(0, buf)
end)

vim.cmd([[
augroup NvimAuCommands
  au!
  au TextYankPost * silent! lua vim.highlight.on_yank {on_visual = false, timeout = 50}
augroup END
]])

vim.cmd([[
augroup NicerTerminal
    au!
    au BufEnter term://* normal i
augroup END
]])

vim.api.nvim_create_autocmd("FileType", {
  pattern = "dap-repl",
  callback = function(args)
    vim.api.nvim_buf_set_option(args.buf, "buflisted", false)
  end,
})

-- Tell neovim which python to use
vim.g.python3_host_prog = "/usr/bin/python3"

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

require("colors")
vim.cmd([[filetype plugin indent on]])

for opt, val in pairs({
  backspace = "indent,eol,start",
  breakindent = true,
  completeopt = "menuone,noselect,menu",
  concealcursor = "n",
  conceallevel = 2,
  cursorlineopt = "number",
  diffopt = "internal,filler,closeoff,algorithm:patience",
  expandtab = true,
  fillchars = "fold: ,foldclose:,foldopen:,foldsep: ,diff: ,eob: ",
  fixendofline = false,
  foldexpr = "nvim_treesitter#foldexpr()",
  foldlevel = 99,
  foldmethod = "expr",
  formatoptions = "lnjqr",
  ignorecase = true,
  inccommand = "split",
  laststatus = 3,
  linebreak = true,
  mouse = "a",
  number = true,
  relativenumber = true,
  scrolloff = 10,
  shiftwidth = 0,
  showmode = false,
  signcolumn = "yes",
  smartcase = true,
  spelloptions = "noplainbuffer",
  splitbelow = true,
  splitright = true,
  switchbuf = "useopen",
  tabstop = 2,
  termguicolors = true,
  textwidth = 120,
  undodir = vim.fn.expand("~/.cache/nvim/undodir"),
  undofile = true,
  viewoptions = "cursor,folds,slash,unix",
  wrap = false,
}) do
  vim.opt[opt] = val
end

vim.g.mapleader = " "
vim.opt.shortmess:append("c")
vim.cmd([[
let FoldText = {-> substitute(getline(v:foldstart),"\s*{{{[0-9]\s*$","","")." ▶"}
set foldtext=FoldText()
]])
vim.g.border_chars = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" }

pcall(function()
  require("impatient")
end)

vim.notify = function(...)
  local exists, notify = pcall(require, "notify")
  if exists then
    notify(...)
  else
    print(...)
  end
end

local k = vim.api.nvim_set_keymap
local maps = {
  n = {
    [";"] = ":",
    [";;"] = ";",
    H = "^",
    L = "$",
    ["<leader>w"] = "<CMD>w<CR>",
    ["<leader>q"] = "<CMD>BufferClose<CR>",
    ["<Up>"] = "<C-y>",
    ["<Down>"] = "<C-e>",
    ["<Left>"] = "zh",
    ["<Right>"] = "zl",
    ["<C-h>"] = "<C-w><C-h>",
    ["<C-j>"] = "<C-w><C-j>",
    ["<C-k>"] = "<C-w><C-k>",
    ["<C-l>"] = "<C-w><C-l>",
  },
  t = {
    ["<C-h>"] = "<C-\\><C-n><C-w><C-h>",
    ["<C-j>"] = "<C-\\><C-n><C-w><C-j>",
    ["<C-k>"] = "<C-\\><C-n><C-w><C-k>",
    ["<C-l>"] = "<C-\\><C-n><C-w><C-l>",
  },
  o = {
    ie = "<CMD>normal! gg0vG$<cr>",
  },
}

for mode, mode_maps in pairs(maps) do
  for lhs, rhs in pairs(mode_maps) do
    k(mode, lhs, rhs, { silent = true, noremap = true })
  end
end

-- Find highlight group under cursor for changing colorschemes
vim.cmd([[
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
]])

vim.cmd([[command! Plugins lua require("plugins").update() ]])

-- From vim-unimpaired
local function putline(how)
  local body, type = vim.fn.getreg(vim.v.register), vim.fn.getregtype(vim.v.register)
  if type == "V" then
    vim.api.nvim_exec('normal! "' .. vim.v.register .. how, false)
  else
    vim.fn.setreg(vim.v.register, body, "l")
    vim.api.nvim_exec('normal! "' .. vim.v.register .. how, false)
    vim.fn.setreg(vim.v.register, body, type)
  end
end

k("n", "[p", "", {
  callback = function()
    putline("[p")
  end,
})
k("n", "[p", "", {
  callback = function()
    putline("]p")
  end,
})

k("n", "<C-[>", "<cmd>noh<cr>", { noremap = true })
