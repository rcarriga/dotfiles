vim.cmd([[
augroup NvimAuCommands
  au!
  au TextYankPost * silent! lua vim.highlight.on_yank {on_visual = false, timeout = 50}
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

require("colors").set()
vim.cmd([[filetype plugin indent on]])

local function set(opt, val)
  vim.opt[opt] = val
end

for opt, val in pairs({
  backspace = "indent,eol,start",
  breakindent = true,
  completeopt = "menuone,noselect,menu",
  concealcursor = "",
  conceallevel = 2,
  cursorline = true,
  cursorlineopt = "number",
  diffopt = "internal,filler,closeoff,algorithm:patience,linematch:60",
  expandtab = true,
  fillchars = "fold: ,foldclose:,foldopen:,foldsep: ,diff: ,eob: ",
  fixendofline = false,
  foldexpr = "nvim_treesitter#foldexpr()",
  foldlevel = 99,
  foldmethod = "expr",
  formatoptions = "lnjqr",
  guicursor = "",
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
  switchbuf = "useopen,uselast",
  tabstop = 2,
  termguicolors = true,
  textwidth = 80,
  undodir = vim.fn.expand("~/.cache/nvim/undodir"),
  undofile = true,
  viewoptions = "cursor,folds,slash,unix",
  wrap = false,
}) do
  local suc, err = pcall(set, opt, val)
  if not suc then
    print("Error setting option " .. opt .. ": " .. err)
  end
end

vim.g.mapleader = " "
vim.opt.shortmess:append("c")
vim.cmd([[
let FoldText = {-> substitute(getline(v:foldstart),"\s*{{{[0-9]\s*$","","")." ▶"}
set foldtext=FoldText()
]])
vim.g.border_chars = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" }

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
k("n", "]p", "", {
  callback = function()
    putline("]p")
  end,
})

require("plugins")
