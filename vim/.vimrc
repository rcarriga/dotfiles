" General configuration
" Adjust quickfix size to contents: http://vim.wikia.com/wiki/Automatically_fitting_a_quickfix_window_height
au FileType qf call AdjustWindowHeight(3, 50)
   function! AdjustWindowHeight(minheight, maxheight)
       let l = 1
       let n_lines = 0
       let w_width = winwidth(0)
       while l <= line('$')
           " number to float for division
           let l_len = strlen(getline(l)) + 0.0
           let line_width = l_len/w_width
           let n_lines += float2nr(ceil(line_width))
           let l += 1
       endw
       exe max([min([n_lines, a:maxheight]), a:minheight]) . "wincmd _"
   endfunction
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

" Install fzf then fzf.vim
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>ag :Ag .<CR>

" Fantastic git commands built in
Plug 'tpope/vim-fugitive'
Plug 'shumphrey/fugitive-gitlab.vim'
"Add private repo urls to this list to use Gbrowse(Opens file in browser)"
let g:fugitive_gitlab_domains = ['https://gitlab-app.eng.qops.net']
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gp :Gpush<CR>
nnoremap <Leader>gb :Gbrowse<CR>
nnoremap <Leader>gl :Gblame<CR>
" Plugins for surrounding with quotes, brackets etc
Plug 'tpope/vim-surround'
Plug 'jiangmiao/auto-pairs'

Plug 'plytophogy/vim-virtualenv'

" Python friendly folding
Plug 'tmhedberg/SimpylFold'

Plug 'scrooloose/nerdcommenter'
"Needed for NERD comment
filetype plugin on

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
" These 3 lines are for speeding up startup time
let g:deoplete#enable_at_startup = 0
autocmd InsertEnter * call deoplete#enable()
let g:python3_host_prog = '/usr/bin/python3'
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

" Echodoc for function signatures
Plug 'Shougo/echodoc.vim'
set cmdheight=2
let g:echodoc#enable_at_startup = 1
let g:echodoc#type = 'signature'

Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \}
let g:LanguageClient_serverCommands = {
    \ 'javascript': ['typescript-language-server', '--stdio'],
    \ 'python': ['pyls'],
    \ 'java': ['/usr/local/bin/jdtls'],
    \ 'haskell': ['hie-wrapper']
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

Plug 'w0rp/ale'
let g:ale_linters = {
\   'python': ['pylint'],
\   'haskell': [],
\}
" Forces pylint to run at project base rather than running each file in it's own
" directory
let g:ale_python_pylint_change_directory = 0

"Plug 'neovimhaskell/haskell-vim'
"" Makes Haskell Vim work
"syntax on
"filetype plugin indent on

Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
" Auto closes NERDTree if no other open pane
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" Toggle NERDTree with Ctrl+o
map <C-o> :NERDTreeToggle<CR>
" Hide 'Press ? for help'
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1

Plug 'vim-airline/vim-airline'
let g:airline#extensions#tabline#enabled = 1
" Make lightline work
set laststatus=2
" Hides --insert-- under lightline
set noshowmode

Plug 'airblade/vim-gitgutter'
" Set gitgutter update time
set updatetime=100

Plug 'patstockwell/vim-monokai-tasty'

" Initialize plugin system
call plug#end()

" Needs to be after plugend
colorscheme vim-monokai-tasty
let g:airline_theme='monokai_tasty'
