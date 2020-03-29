augroup FileTypeInit
    au!
    au BufNew,VimEnter *[jJ]enkins* set ft=Jenkinsfile
    au BufNew,VimEnter *\.nix set ft=nix
    au BufNew,VimEnter *\.purs set ft=purescript
    au BufNew,VimEnter *\.dhall set ft=dhall
    au BufNew,VimEnter \.babelrc set ft=json
    au BufNew,VimEnter \.conf set ft=conf
augroup END

augroup NicerTerminal
    au!
    au BufEnter term://* normal i
augroup END
" }}}1
" ###################################################################################
" Native Vim Settings {{{1
" Tell neovim which python to use
let g:python3_host_prog = "/usr/bin/python3"

" Disable plaintex filetype
let g:tex_flavor = "latex"

" Disable modelines (Vim commands in files)
set nomodeline

" Always have a sign column
set signcolumn=yes

set rtp+=~/.vimdir/haslo-vim
color haslo

" Turn on syntax highlighting
syn on

" Indents word-wrapped lines as much as the 'parent' line
set breakindent

" Ensures word-wrap does not split words
set formatoptions=l
set linebreak

" Allow filetype specific plugins and indenting
filetype plugin indent on

" Always on statusline
set laststatus=2

" Hides --insert-- under lightline
set noshowmode

" Set file update time in milliseconds
set updatetime=100

" Turn on 24 bit color. Delete this line if colors are weird
set termguicolors

" Delay to wait for next key in combo
set timeoutlen=300

" Show numbers relative to current line
set relativenumber
set number

" Make backspace work as expected
set backspace=indent,eol,start

" Setup tabs to be 4 spaces
set tabstop=4 softtabstop=0 expandtab shiftwidth=0 smarttab

" Opens new panes below and to right of current
set splitbelow
set splitright

" Set all code unfolded by default
set foldlevel=99

" Update files on change
set autoread

" Save edit history between sessions
set undofile
set undodir=~/.config/nvim/undodir

" Don't unload buffers when left
set hidden

" Don't give ins-completion-menu messages
set shortmess+=c

" Ignore case in search unless contains capital
set ignorecase
set smartcase

" Hide text set as concealed
set conceallevel=3

" Enable mouse so people don't get angry when using my editor...
set mouse=a

if has("nvim")
    " Preview changes when using search and replace
    set inccommand=nosplit
    " Set characters for after foldtext, eof, foldcolumn
    set fillchars=fold:\ ,foldclose:,foldopen:,foldsep:\ ,eob:\ 

    set foldcolumn=1
else
    " Make vim command autocomplete better
    set wildmode=longest,list,full
    set wildmenu
    set fillchars=fold:\ 
endif

" Jump to existing window when opening buffer already opened
set switchbuf=useopen

" Space as leader key
let mapleader="\<Space>"

" Save state when using :mkview
set viewoptions=cursor,folds,slash,unix

" Show unwanted characters
exec "set listchars=tab:\u254D\u254D,nbsp:_,trail:\uB7"
set list

" Keep a buffer of 10 lines/columns between cursor and edge when scrolling
set scrolloff=10
set sidescrolloff=10

filetype plugin indent on

set pyxversion=3

" Disable line wrapping
set nowrap

" Text to appear on folded line
set foldtext=MyFoldText()

" Syntactic folding (Good for html, jsx, json etc)
set foldmethod=syntax

" Use patience algorithm for diffs
set diffopt+=internal,algorithm:patience

" }}}1
" ###################################################################################
" Custom Mappings{{{1
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
tnoremap <silent><C-h> <C-\><C-N><C-w><C-h>
tnoremap <silent><C-j> <C-\><C-N><C-w><C-j>
tnoremap <silent><C-k> <C-\><C-N><C-w><C-k>
tnoremap <silent><C-l> <C-\><C-N><C-w><C-l>
inoremap <silent><C-h> <C-\><C-N><C-w><C-h>
inoremap <silent><C-j> <C-\><C-N><C-w><C-j>
inoremap <silent><C-k> <C-\><C-N><C-w><C-k>
inoremap <silent><C-l> <C-\><C-N><C-w><C-l>

" Enter normal mode with escape in terminal
" tnoremap <silent> <ESC> <C-\><C-N>

" Use Tab for cycling through completions.
" Use Enter to expand a snippet.

" Find highlight group under cursor for changing colorschemes
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
"}}}1
" ###################################################################################
" Custom Commands {{{1
command! RunR exec "!Rscript "expand("%")
command! RunP exec '!python' shellescape(@%, 1)
"}}}1
" ###################################################################################
" Functions {{{1
function! MyFoldText() abort
    let line = getline(v:foldstart)
    return substitute(line,"\s*{{{[0-9]\s*$","","")." ▶"
endfunction
" }}}1
" ###################################################################################
" Autocommands {{{1
augroup VimInit
    au!
    au FileType vim setlocal foldmethod=marker
augroup END
" }}}1

" ###################################################################################
" Plugins Init {{{1
" Load plugins file after loading to reduce startup time.
augroup PluginInit
    au!
    au InsertEnter,CursorHold * ++once silent runtime autoload/plugins.vim
augroup END
if exists('g:started_by_firenvim')
    set laststatus=0
endif

