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
  fillchars = "fold:─,foldclose:,foldopen:,foldsep: ,diff: ,eob: ",
  fixendofline = false,
  foldexpr = "nvim_treesitter#foldexpr()",
  foldtext = "",
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
local util = require("util")

local runners = {
  python = function(path)
    local python_path
    for _, client in pairs(vim.lsp.get_clients({ bufnr = vim.fn.bufnr(path) })) do
      if client.settings and client.settings.python then
        python_path = client.settings.python.pythonPath
      end
    end
    python_path = python_path or util.get_python_path(vim.loop.cwd())
    vim.cmd(string.format("FloatermNew %s %s", python_path, path))
  end,
  lua = function()
    vim.cmd(string.format("source %s", vim.fn.expand("%")))
  end,
}

vim.keymap.set("n", "<leader>r", function()
  local filetype = vim.bo.filetype
  runners[filetype](vim.fn.expand("%"))
end, { silent = true, noremap = true })

P = function(...)
  local obj = select("#", ...) == 1 and select(1, ...) or { ... }
  local s = type(obj) == "string" and obj or vim.inspect(obj)
  if vim.in_fast_event() then
    vim.schedule(function()
      print(s)
    end)
  else
    print(s)
  end
end

PP = vim.schedule_wrap(function(...)
  local is_string = select("#", ...) == 1 and type(select(1, ...)) == "string"
  local buf = vim.api.nvim_create_buf(false, true)
  local lines = vim.split(is_string and select(1, ...) or vim.inspect(...), "\n", { plain = true })
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.cmd("vsplit")
  vim.api.nvim_win_set_buf(0, buf)
end)

-- Find highlight group under cursor for changing colorschemes
vim.cmd([[
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
]])

if not vim.g.vscode then
  require("plugins")
end
