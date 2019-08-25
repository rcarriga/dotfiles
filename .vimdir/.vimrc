" ###################################################################################
" Load plugins file after loading to reduce startup time.
augroup PluginInit
    au!
    au CursorHold * ++once silent source ~/.vimdir/plugins.vim
augroup END

" ###################################################################################
" Native Vim Settings

set rtp+=~/.vimdir
set rtp+=~/.vimdir/haslo-vim
set rtp+=~/Repos/vim-test-status

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
set ttimeoutlen=50

" Show numbers relative to current line
set relativenumber
set number

" Make backspace work as expected
set backspace=indent,eol,start

" Make vim command autocomplete better
set wildmode=longest,list,full
set wildmenu

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
set undodir=~/.vim/undodir

" Don't unload buffers when left
set hidden

" Don't give ins-completion-menu messages
set shortmess+=c

" Disable netrw
let loaded_netrwPlugin = 1

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
else
    " Make vim command autocomplete better
    set wildmode=longest,list,full
    set wildmenu
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

color haslo

set scrolloff=10
set sidescrolloff=10

filetype plugin indent on

set wildcharm=<Tab>

set pyxversion=3

" ###################################################################################
" Custom Mappings

inoremap <TAB> <C-n>

" Don't waste time holding shift for commands
map ; :
noremap ;; ;

" Replace word with yanked text
nnoremap S "_dwP

" Jump to start and end of line easier
nnoremap H ^
nnoremap L $

"Save current buffer
nnoremap <leader>w :w<CR>
nnoremap <leader>q :q<CR>

"Cycle between last two open buffers
nnoremap <leader><leader> <c-^>

" Disable arrow keys (Just throw yourself into it trust me...)
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

" Switch windows with Ctrl + regular direction keys
nnoremap <silent> <C-h> <C-w><C-h>
nnoremap <silent> <C-j> <C-w><C-j>
nnoremap <silent> <C-k> <C-w><C-k>
nnoremap <silent> <C-l> <C-w><C-l>
tnoremap <silent><C-h> <C-\><C-N><C-w><C-h>
tnoremap <silent><C-h> <C-\><C-N><C-w><C-j>
tnoremap <silent><C-j> <C-\><C-N><C-w><C-k>
tnoremap <silent><C-k> <C-\><C-N><C-w><C-l>
inoremap <silent><C-l> <C-\><C-N><C-w><C-h>
inoremap <silent><C-j> <C-\><C-N><C-w><C-j>
inoremap <silent><C-k> <C-\><C-N><C-w><C-k>
inoremap <silent><C-l> <C-\><C-N><C-w><C-l>

" Enter normal mode with escape in terminal
tnoremap <silent> <ESC> <C-\><C-N>

"Replace the word under cursor
nmap <leader>os :%s/\<<c-r><c-w>\>//g<left><left>

" Distraction free writing
nmap <silent><leader>z :Goyo<CR>
" Use Tab for cycling through completions.
" Use Enter to expand a snippet.

" Find highlight group under cursor for changing colorschemes
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

