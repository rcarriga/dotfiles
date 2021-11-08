local util = require("util")
P = function(...)
  print(vim.inspect(...))
end

util.multilineCommand([[
augroup FileTypeInit
    au!
    au BufNew,VimEnter *[jJ]enkins* setlocal ft=Jenkinsfile
    au BufNew,VimEnter *\.nix setlocal ft=nix
    au BufNew,VimEnter *\.purs setlocal ft=purescript
    au BufNew,VimEnter *\.dhall setlocal ft=dhall
    au BufNew,VimEnter \.babelrc setlocal ft=json
    au BufNew,VimEnter \.conf setlocal ft=conf
    au Filetype dockerfile setlocal ft=Dockerfile
augroup END

augroup NvimAuCommands
  au!
  au TextYankPost * silent! lua vim.highlight.on_yank {on_visual=false}
augroup END
]])

util.multilineCommand([[
augroup NicerTerminal
    au!
    au BufEnter term://* normal i
augroup END
]])
-- Tell neovim which python to use
vim.g.python3_host_prog = "/usr/bin/python3"

-- Set completeopt to have a better completion experience
vim.opt.completeopt = "menuone,noselect,menu"

-- Disable modelines (Vim commands in files)
vim.opt.modeline = true

-- Always have a sign column
vim.opt.signcolumn = "yes"

vim.cmd([[color haslo]])

-- Indents word-wrapped lines as much as the 'parent' line
vim.opt.breakindent = true

-- Ensures word-wrap does not split words
vim.opt.formatoptions = "l"
vim.opt.linebreak = true

-- Allow filetype specific plugins and indenting
vim.cmd([[filetype plugin indent on]])

-- Always on statusline
vim.opt.laststatus = 2

-- Hides --insert-- under statusline
vim.opt.showmode = false

vim.opt.updatetime = 4000

-- Turn on 24 bit color. Delete this line if colors are weird
vim.opt.termguicolors = true

-- Delay to wait for next key in combo
vim.opt.timeoutlen = 1000

-- Show numbers relative to current line
vim.opt.relativenumber = true
-- vim.opt.cursorline = true
vim.opt.cursorlineopt = "number"
vim.opt.number = true

-- Make backspace work as expected
vim.opt.backspace = "indent,eol,start"

-- Setup tabs to be 4 spaces
vim.cmd([[set tabstop=2 softtabstop=0 expandtab shiftwidth=0 smarttab]])

-- Opens new panes below and to right of current
vim.opt.splitbelow = true
vim.opt.splitright = true

-- vim.opt.all code unfolded by default
vim.opt.foldlevel = 99
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"

-- Update files on change
vim.opt.autoread = true

-- Save edit history between sessions
vim.cmd([[ set undofile ]])
vim.opt.undodir = vim.fn.expand("~/.cache/nvim/undodir")

-- Don't unload buffers when left
vim.opt.hidden = true

-- Don't give ins-completion-menu messages
vim.opt.shortmess:append("c")

-- Ignore case in search unless contains capital
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Hide text vim.opt.as concealed
vim.opt.conceallevel = 3

-- Enable mouse so people don't get angry when using my editor...
vim.opt.mouse = "a"

-- Preview changes when using search and replace
vim.opt.inccommand = "nosplit"
-- vim.opt.characters for after foldtext, eof, foldcolumn
vim.opt.fillchars = "fold: ,foldclose:,foldopen:,foldsep: ,diff: ,eob: "

-- Jump to existing window when opening buffer already opened
vim.opt.switchbuf = "useopen"

-- Space as leader key
vim.g.mapleader = " "

-- Save state when using :mkview
vim.opt.viewoptions = "cursor,folds,slash,unix"

-- Show unwanted characters
vim.cmd("set listchars=tab:╍╍,nbsp:_,trail:·")
vim.opt.list = false

-- Keep a buffer of 10 lines/columns between cursor and edge when scrolling
vim.opt.scrolloff = 10

vim.opt.pyxversion = 3

-- Disable line wrapping
vim.opt.wrap = false

-- Text to appear on folded line
util.multilineCommand([[
let FoldText = {-> substitute(getline(v:foldstart),"\s*{{{[0-9]\s*$","","")." ▶"}
set foldtext=FoldText()
]])

-- Use patience algorithm for diffs
vim.opt.diffopt = "internal,filler,closeoff,algorithm:patience"

-- Don't add newline if missing on write
vim.opt.fixendofline = false

-- Explicitly auto select regex engine
vim.opt.regexpengine = 0

vim.opt.pumblend = 15

vim.g.border_chars = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" }

pcall(require, "impatient")

pcall(require, "my_packer")

local exists, notify = pcall(require, "notify")
if exists then
  vim.notify = notify
end

-- }}}1
-- ###################################################################################
-- Custom Mappings{{{1
util.multilineCommand([[
" Don't waste time holding shift for commands
map ; :
noremap ;; ;

" Jump to start and end of line easier
nnoremap H ^
nnoremap L $

nnoremap <BS> X

" Use arrow keys for scrolling
noremap <Up> <C-y>
noremap <Down> <C-e>
noremap <Left> zh
noremap <Right> zl

nnoremap <S-q> q:

" Switch windows with Ctrl + regular direction keys
nnoremap <silent> <C-h> <C-w><C-h>
nnoremap <silent> <C-j> <C-w><C-j>
nnoremap <silent> <C-k> <C-w><C-k>
nnoremap <silent> <C-l> <C-w><C-l>
tnoremap <silent> <C-h> <C-\><C-N><C-w><C-h>
tnoremap <silent> <C-j> <C-\><C-N><C-w><C-j>
tnoremap <silent> <C-k> <C-\><C-N><C-w><C-k>
tnoremap <silent> <C-l> <C-\><C-N><C-w><C-l>
inoremap <silent> <C-h> <C-\><C-N><C-w><C-h>
inoremap <silent> <C-j> <C-\><C-N><C-w><C-j>
inoremap <silent> <C-k> <C-\><C-N><C-w><C-k>
inoremap <silent> <C-l> <C-\><C-N><C-w><C-l>

" Select entire buffer
onoremap ie :exec "normal! gg0vG$"<cr>

" Enter normal mode with escape in terminal
" tnoremap <silent> <ESC> <C-\><C-N>

" Find highlight group under cursor for changing colorschemes
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
]])

vim.cmd([[command! Plugins lua require("plugins").update() ]])
