
" ###################################################################################
" Install Plugins 
" See README for links (Or just paste each plugin to https://github.com/)

" Auto install vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'Ron89/thesaurus_query.vim', {'for': ['tex', 'markdown']}
Plug 'Shougo/deoplete.nvim'
Plug 'Shougo/echodoc.vim'
Plug 'airblade/vim-gitgutter'
Plug 'alvan/vim-closetag', {'for': 'html'}
Plug 'ap/vim-css-color'
Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }
Plug 'fszymanski/deoplete-emoji'
Plug 'itchyny/lightline.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align', {'for': 'markdown'}
Plug 'leafgarland/typescript-vim', {'for': 'typescript'}
Plug 'lervag/vimtex', {'for': 'tex'}
Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}
Plug 'patstockwell/vim-monokai-tasty'
Plug 'plytophogy/vim-virtualenv', {'for': 'python'}
Plug 'roxma/nvim-yarp'
Plug 'roxma/vim-hug-neovim-rpc'
Plug 'scrooloose/nerdcommenter'
Plug 'shumphrey/fugitive-gitlab.vim'
Plug 'tmhedberg/SimpylFold', {'for': 'python'}
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'ujihisa/neco-look'
Plug 'w0rp/ale'

" Initialize plugin system
call plug#end()

" ###################################################################################
" Native Vim Settings Section

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

au FileType tex set nowrap 

" Allow filetype specific plugins and indenting
filetype plugin indent on
" Honestly do not know but makes lightline work
set laststatus=2
" Hides --insert-- under lightline
set noshowmode
" Set file update time in milliseconds
set updatetime=100
" Turn on 24 bit color. Delete this line if colors are weird
set termguicolors
" Enable search highlighting and set color
set hlsearch
hi Search guibg=LightBlue
" Reduce delay between switchin mode
set ttimeoutlen=50
" Show line numbers
set number
" Show numbers relative to current line
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
let g:ale_linters = {
\   'python': ['pylint'],
\   'haskell': [],
\   'typescript': [],
\}
set foldlevel=99
" Update files on change
set autoread
au BufEnter * :checktime
" Save edit history between sessions
set undofile
set undodir=~/.vim/undodir
" Don't waste time holding shift for commands
map ; :
noremap ;; ;
" Who needs NERDTree? (Makes netrw look nicer)
let g:netrw_banner = 0
let g:netrw_liststyle = 3
" Allows commandline to usee 2 lines (Makes echodoc work) 
set cmdheight=2
" Only needed for Kitty so background isn't messed up
let &t_ut=''

" ###################################################################################
" Plugin Settings Section

"Add private repo urls to this list to use Gbrowse(Opens file in browser)"
let g:fugitive_gitlab_domains = ['https://gitlab-app.eng.qops.net', 'https://github.com', 'https://gitlab.engservices.qops.net']

" These 2 lines are for speeding up startup time
let g:deoplete#enable_at_startup = 0
autocmd InsertEnter * call deoplete#enable() | call deoplete#custom#source('emoji', 'converters', ['converter_emoji']) | call deoplete#custom#source('look', 'filetypes', ['markdown', 'tex'])

" Shows function signature above commandline instead of opening new window
let g:echodoc#enable_at_startup = 1
let g:echodoc#type = 'signature'

" Set commands to run for language server for filetypes. (Pass arguements in array)
let g:LanguageClient_serverCommands = {
    \ 'javascript': ['typescript-language-server', '--stdio'],
    \ 'typescript': ['typescript-language-server', '--stdio'],
    \ 'python': ['pyls'],
    \ 'java': ['/usr/local/bin/jdtls'],
    \ 'haskell': ['hie-wrapper']
    \ }

" Set linters for filetypes. I normally disable if running language server
" Python Language Server doesn't run pylint so enable it here.
let g:ale_linters = {
\   'python': ['pylint'], 
\   'haskell': [],
\   'typescript': [],
\}

" Forces pylint to run at project base rather than running each file in it's own directory
let g:ale_python_pylint_change_directory = 0

colorscheme vim-monokai-tasty
let g:lightline = {'colorscheme': 'monokai_tasty'}

" ###################################################################################
" Custom Mappings

" Netrw mappings
nnoremap <Leader>nv :Vex<CR>
nnoremap <Leader>ns :Sex<CR>

" Disable arrow keys (Just throw yourself into it trust me...)
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

" Switch windows with Ctrl + regular direction keys
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" FZF and Ag mappings
nnoremap <Leader>f :Files<CR>
nnoremap <Leader>ag :Ag .<CR>

" Git functions with vim-fugitive
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gp :Gpush<CR>
nnoremap <Leader>gb :Gbrowse<CR>
nnoremap <Leader>gl :Gblame<CR>

" Language server functions
nnoremap <leader>ld :call LanguageClient#textDocument_definition()<CR>
nnoremap <leader>lr :call LanguageClient#textDocument_rename()<CR>
nnoremap <leader>lf :call LanguageClient#textDocument_formatting()<CR>
nnoremap <leader>lt :call LanguageClient#textDocument_typeDefinition()<CR>
nnoremap <leader>lx :call LanguageClient#textDocument_references()<CR>
nnoremap <leader>la :call LanguageClient#textDocument_codeAction()<CR>
nnoremap <leader>lc :call LanguageClient#textDocument_completion()<CR>
nnoremap <leader>lk :call LanguageClient#textDocument_hover()<CR>
nnoremap <leader>ls :call LanguageClient_textDocument_documentSymbol()<CR>
nnoremap <leader>lm :call LanguageClient_contextMenu()<CR>
nnoremap <leader>lh :call LanguageClient#textDocument_documentHighlight()<CR>

" Use tab for cycling through autocomplete
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

" Built in thesaurus!
nnoremap <Leader>th :ThesaurusQueryReplaceCurrentWord<CR>

" Align GitHub-flavored Markdown tables
vmap <Leader>a :EasyAlign*<Bar><Enter>
