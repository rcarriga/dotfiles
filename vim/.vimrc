" General configuration
" Enable search highlighting and set color
set hlsearch
hi Search guibg=LightBlue
" Reduce delay between switchin mode
set ttimeoutlen=50
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
" Autoupdate files on change
set autoread
au BufEnter * :checktime
" Switch panes with Ctrl + J/K/L/H
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Arrow keys are bad and you shouldn't use them
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

" Open up current file in chrome
nmap <silent> <leader>ch :exec 'silent !open -a "Google Chrome" % &'<CR>

" Don't waste time holding shift for commands
map ; :
noremap ;; ;

call plug#begin('~/.vim/plugged')

" Easy aligning
Plug 'godlygeek/tabular'

"Window focusing
Plug 'vim-scripts/ZoomWin'

" Auto indented pasting
Plug 'sickill/vim-pasta'

Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
nnoremap <Leader>pc :Pandoc pdf -f markdown+smart --pdf-engine=xelatex -V geometry:margin=2.5cm -V fontsize=12pt

" Install fzf then fzf.vim
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>ag :Ag .<CR>

" Fantastic git commands built in
Plug 'tpope/vim-fugitive'
Plug 'shumphrey/fugitive-gitlab.vim'
"Add private repo urls to this list to use Gbrowse(Opens file in browser)"
let g:fugitive_gitlab_domains = ['***REMOVED***']
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gp :Gpush<CR>
nnoremap <Leader>gb :Gbrowse<CR>
nnoremap <Leader>gl :Gblame<CR>
" Plugins for surrounding with quotes, brackets etc
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'

" Async testing
Plug 'tpope/vim-dispatch'

" Json manipulation
Plug 'tpope/vim-jdaddy'

Plug 'tomasiser/vim-code-dark'

Plug 'plytophogy/vim-virtualenv'

" Python friendly folding
Plug 'tmhedberg/SimpylFold'

"Needed for debugging plugin
Plug 'Shougo/vimproc.vim', {'do' : 'make'}
Plug 'idanarye/vim-vebugger'

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
nmap <Leader>tt :TagbarToggle<CR>

Plug 'hashivim/vim-terraform'
" Setup for terraform
let g:terraform_align=1
let g:terraform_fold_sections=1
let g:terraform_remap_spacebar=1
let g:terraform_commentstring='//%s'
let g:terraform_fmt_on_save=1
" Deoplete setup needed for language server
Plug 'Shougo/deoplete.nvim'
Plug 'roxma/nvim-yarp'
Plug 'roxma/vim-hug-neovim-rpc'
let g:deoplete#enable_at_startup = 1

Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \} 
let g:LanguageClient_serverCommands = {
    \ 'rust': ['solargraph','socket'],
    \ 'javascript': ['typescript-language-server', '--stdio'],
    \ 'python': ['pyls'],
    \ }
nnoremap <leader>ld :call LanguageClient#textDocument_definition()<CR>
nnoremap <leader>lr :call LanguageClient#textDocument_rename()<CR>
nnoremap <leader>lf :call LanguageClient#textDocument_formatting()<CR>
nnoremap <leader>lt :call LanguageClient#textDocument_typeDefinition()<CR>
nnoremap <leader>lx :call LanguageClient#textDocument_references()<CR>
nnoremap <leader>la :call LanguageClient_workspace_applyEdit()<CR>
nnoremap <leader>lc :call LanguageClient#textDocument_completion()<CR>
nnoremap <leader>lh :call LanguageClient#textDocument_hover()<CR>
nnoremap <leader>ls :call LanguageClient_textDocument_documentSymbol()<CR>
nnoremap <leader>lm :call LanguageClient_contextMenu()<CR>

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

Plug 'w0rp/ale'
let g:ale_linters = {
\   'python': ['pylint'],
\}
" Forces pylint to project base rather than running each file in it's own
" directory
let g:ale_python_pylint_change_directory = 0

Plug 'neovimhaskell/haskell-vim'
" Makes Haskell Vim work
syntax on
filetype plugin indent on

Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
" Auto opens NERDTree
"autocmd vimenter * NERDTree
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
colorscheme codedark
let g:lightline = { 'colorscheme': 'onehalfdark' }
