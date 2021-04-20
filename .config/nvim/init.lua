local util = require("util")
util.multilineCommand [[
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
]]

util.multilineCommand [[
augroup NicerTerminal
    au!
    au BufEnter term://* normal i
augroup END
]]
-- Tell neovim which python to use
vim.g.python3_host_prog = "/usr/bin/python3"

vim.g.vimsyn_embed = 'lP'

-- Set completeopt to have a better completion experience
vim.o.completeopt="menu,menuone,noselect"

-- Disable modelines (Vim commands in files)
vim.o.modeline = false

-- Always have a sign column
vim.wo.signcolumn = "yes"

vim.cmd [[color haslo]]

-- Indents word-wrapped lines as much as the 'parent' line
vim.wo.breakindent = true

-- Ensures word-wrap does not split words
vim.o.formatoptions = "l"
vim.wo.linebreak = true

-- Allow filetype specific plugins and indenting
vim.cmd [[filetype plugin indent on]]

-- Always on statusline
vim.o.laststatus = 2

-- Hides --insert-- under statusline
vim.o.showmode = false

vim.o.updatetime = 4000

-- Turn on 24 bit color. Delete this line if colors are weird
vim.o.termguicolors = true

-- Delay to wait for next key in combo
vim.o.timeoutlen = 1000

-- Show numbers relative to current line
vim.wo.relativenumber = true
vim.wo.number = true

-- Make backspace work as expected
vim.o.backspace = "indent,eol,start"

-- Setup tabs to be 4 spaces
vim.cmd [[set tabstop=2 softtabstop=0 expandtab shiftwidth=0 smarttab]]

-- Opens new panes below and to right of current
vim.o.splitbelow = true
vim.o.splitright = true

-- vim.o.all code unfolded by default
vim.wo.foldlevel = 99
vim.wo.foldmethod = "expr"
vim.wo.foldexpr = "nvim_treesitter#foldexpr()"

-- Update files on change
vim.o.autoread = true

-- Save edit history between sessions
vim.cmd [[ set undofile ]]
vim.o.undodir = vim.fn.expand "~/.cache/nvim/undodir"

-- Don't unload buffers when left
vim.o.hidden = true

-- Don't give ins-completion-menu messages
vim.o.shortmess = vim.o.shortmess .. "c"

-- Ignore case in search unless contains capital
vim.o.ignorecase = true
vim.o.smartcase = true

-- Hide text vim.o.as concealed
vim.o.conceallevel = 3

-- Enable mouse so people don't get angry when using my editor...
vim.o.mouse = "a"

-- Preview changes when using search and replace
vim.o.inccommand = "nosplit"
-- vim.o.characters for after foldtext, eof, foldcolumn
vim.o.fillchars = "fold: ,foldclose:,foldopen:,foldsep: ,eob: "


-- Jump to existing window when opening buffer already opened
vim.o.switchbuf = "useopen"

-- Space as leader key
vim.g.mapleader = " "

-- Save state when using :mkview
vim.o.viewoptions = "cursor,folds,slash,unix"

-- Show unwanted characters
vim.cmd "set listchars=tab:╍╍,nbsp:_,trail:·"
vim.o.list = false

-- Keep a buffer of 10 lines/columns between cursor and edge when scrolling
vim.o.scrolloff = 10

vim.o.pyxversion = 3

-- Disable line wrapping
vim.wo.wrap = false

-- Text to appear on folded line
util.multilineCommand [[
let FoldText = {-> substitute(getline(v:foldstart),"\s*{{{[0-9]\s*$","","")." ▶"}
set foldtext=FoldText()
]]

-- Use patience algorithm for diffs
vim.o.diffopt = "internal,filler,closeoff,algorithm:patience"

-- Don't add newline if missing on write
vim.o.fixendofline = false

-- Explicitly auto select regex engine
vim.o.regexpengine = 0
-- }}}1
-- ###################################################################################
-- Custom Mappings{{{1
util.multilineCommand [[
inoremap <TAB> <C-n>

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
onoremap ie :exec "normal! ggVG"<cr>

" Enter normal mode with escape in terminal
" tnoremap <silent> <ESC> <C-\><C-N>

" Find highlight group under cursor for changing colorschemes
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
]]

vim.cmd [[command! Plugins lua require("plugins").update() ]]
