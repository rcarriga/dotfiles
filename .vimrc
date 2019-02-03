" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'itchyny/lightline.vim'
Plug 'https://github.com/tpope/vim-surround'
Plug 'https://github.com/scrooloose/nerdtree.git'
Plug 'https://github.com/w0rp/ale'
Plug 'https://github.com/airblade/vim-gitgutter'
Plug 'flazz/vim-colorschemes'
Plug 'https://github.com/neovimhaskell/haskell-vim'
Plug 'mhartington/oceanic-next'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'sonph/onehalf', {'rtp': 'vim/'}
Plug 'rakr/vim-one'
Plug 'hashivim/vim-terraform'
Plug 'heavenshell/vim-pydocstring'
Plug 'tmhedberg/SimpylFold'
Plug 'JamshedVesuna/vim-markdown-preview'
Plug 'plytophogy/vim-virtualenv'
Plug 'majutsushi/tagbar'
Plug 'scrooloose/nerdcommenter'

"Needed for NERD comment
filetype plugin on

" Tagbar mapping
nmap <F8> :TagbarToggle<CR>

"python with virtualenv support
py3 << EOF
import os
import sys
if 'VIRTUAL_ENV' in os.environ:
  project_base_dir = os.environ['VIRTUAL_ENV']
  activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
  exec(open(activate_this).read(), dict(__file__=activate_this))
EOF

"Config for markdown preview
let vim_markdown_preview_github=1
" Config for folding python code
let g:SimpylFold_docstring_preview = 1

" Mapping for auto docstring in python
nmap <silent> <Leader>ld <Plug>(pydocstring)

" Setup for terraform
let g:terraform_align=1
let g:terraform_fold_sections=1
let g:terraform_remap_spacebar=1
let g:terraform_commentstring='//%s'
let g:terraform_fmt_on_save=1

" Configure vim-lsp to pyls
if executable('pyls')
    " pip install python-language-server
    au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pyls']},
        \ 'whitelist': ['python'],
        \ })
endif

" Some lovely key bindings for vim-lsp
map <Leader>la :LspCodeAction<CR>
map <Leader>lk :LspHover<CR>
map <Leader>lg :LspDeclaration<CR>
map <Leader>lr :LspRename<CR>
map <Leader>lb :LspReferences<CR>
map <Leader>le :LspNextError<CR>
map <Leader>ls :LspCodeAction<CR>

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

" Ale highlighting config
hi link ALEError Error
hi Warning term=underline cterm=underline ctermfg=Yellow gui=undercurl guisp=Gold
hi link ALEWarning Warning
hi link ALEInfo SpellCap
map <Leader>lf :ALEFix<CR>

" Mypy doesn't play nice with conda so don't use it
let g:ale_linters = {'python': ['pylint', 'flake8']}
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'python': ['yapf', 'isort'],
\}
" Switch panes with Ctrl + J/K/L/H
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Makes Haskell Vim work
syntax on
filetype plugin indent on

" Auto opens NERDTree
autocmd vimenter * NERDTree
" Auto closes NERDTree if no other open pane
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" Toggle NERDTree with Ctrl+o
map <C-o> :NERDTreeToggle<CR>
" Hide 'Press ? for help'
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1

" Make lightline work 
set laststatus=2
" Hides --insert-- under lightline
set noshowmode

" Opens new panes below and to right of current
set splitbelow
set splitright

" Show line numbers
set number
set relativenumber
" Fix backspace issue
set bs=2

" Set gitgutter update time 
set updatetime=100

" Make autocomplete better
set wildmode=longest,list,full
set wildmenu

" Setup tabs to be 4 spaces
set tabstop=8 softtabstop=0 expandtab shiftwidth=2 smarttab
" Initialize plugin system
call plug#end()

set t_8f=[38;2;%lu;%lu;%lum
set t_8b=[48;2;%lu;%lu;%lum
set t_Co=256
" Needs to be after plugend
colorscheme onehalfdark
let g:lightline = {
      \ 'colorscheme': 'onehalfdark',
      \ }
"if (has("termguicolors"))
"    set termguicolors
"endif
