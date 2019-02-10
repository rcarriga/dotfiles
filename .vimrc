
" General configuration
" Show line numbers
set number
set relativenumber
" Fix backspace issue
set bs=2
" Make vim command autocomplete better
set wildmode=longest,list,full
set wildmenu
" Setup tabs to be 4 spaces
set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab
" Opens new panes below and to right of current
set splitbelow
set splitright
" Set all code unfolded by default
set foldlevel=99
" Switch panes with Ctrl + J/K/L/H
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
call plug#begin('~/.vim/plugged')
" Install fzf then fzf.vim
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'tpope/vim-surround'

" Async testing
Plug 'tpope/vim-dispatch'

" Json manipulation
Plug 'pope/vim-jdaddy'

Plug 'plytophogy/vim-virtualenv'

Plug 'tmhedberg/SimpylFold'
Plug 'chrisbra/csv.vim'
" Track the engine.
Plug 'SirVer/ultisnips'
" Snippets are separated from the engine. Add this if you want them:
Plug 'honza/vim-snippets'
" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-g>"

Plug 'scrooloose/nerdcommenter'
"Needed for NERD comment
filetype plugin on

Plug 'majutsushi/tagbar'
" Tagbar mapping
nmap <F8> :TagbarToggle<CR>


Plug 'heavenshell/vim-pydocstring'
" Mapping for auto docstring in python
nmap <silent> <Leader>ld <Plug>(pydocstring)

Plug 'hashivim/vim-terraform'
" Setup for terraform
let g:terraform_align=1
let g:terraform_fold_sections=1
let g:terraform_remap_spacebar=1
let g:terraform_commentstring='//%s'
let g:terraform_fmt_on_save=1

Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'ryanolsonx/vim-lsp-python'
if executable('solargraph')
   "" gem install solargraph
   "au User lsp_setup call lsp#register_server({
       "\ 'name': 'solargraph',
       "\ 'cmd': {server_info->[&shell, &shellcmdflag, 'solargraph stdio']},
       "\ 'initialization_options': {"diagnostics": "true"},
       "\ 'whitelist': ['ruby'],
       "\ })
endif
" Some lovely key bindings for vim-lsp
map <Leader>la :LspCodeAction<CR>
map <Leader>lk :LspHover<CR>
map <Leader>lg :LspDeclaration<CR>
map <Leader>lr :LspRename<CR>
map <Leader>lb :LspReferences<CR>
map <Leader>le :LspNextError<CR>
map <Leader>ls :LspCodeAction<CR>
map <Leader>lf :LspDocumentFormat<CR>
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
" Some not so lovely stuff for asyncomplete (Not sure what they do just
" copypastad everything
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<cr>"
imap <c-space> <Plug>(asyncomplete_force_refresh)
let g:asyncomplete_remove_duplicates = 1
let g:asyncomplete_smart_completion = 1
let g:asyncomplete_auto_popup = 1
set completeopt+=preview
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

Plug 'vim-syntastic/syntastic'
let g:syntastic_python_checkers = ['pylint']

Plug 'neovimhaskell/haskell-vim'
" Makes Haskell Vim work
syntax on
filetype plugin indent on

Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
" Auto opens NERDTree
autocmd vimenter * NERDTree
" Auto closes NERDTree if no other open pane
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" Toggle NERDTree with Ctrl+o
map <C-o> :NERDTreeToggle<CR>
" Hide 'Press ? for help'
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1

Plug 'itchyny/lightline.vim'
" Make lightline work 
set laststatus=2
" Hides --insert-- under lightline
set noshowmode

Plug 'airblade/vim-gitgutter'
" Set gitgutter update time 
set updatetime=100


Plug 'sonph/onehalf', {'rtp': 'vim/'}
" Initialize plugin system
call plug#end()

" Needs to be after plugend
colorscheme onehalfdark
let g:lightline = { 'colorscheme': 'onehalfdark' }
