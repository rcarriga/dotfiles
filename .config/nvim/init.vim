
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
Plug 'Shougo/echodoc.vim'
Plug 'airblade/vim-gitgutter'
Plug 'alvan/vim-closetag', {'for': 'html'}
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
Plug 'scrooloose/nerdcommenter'
Plug 'shumphrey/fugitive-gitlab.vim'
Plug 'tmhedberg/SimpylFold', {'for': 'python'}
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'w0rp/ale'
Plug 'rhysd/vim-grammarous', {'for': ['markdown', 'tex']}
Plug 'neoclide/coc.nvim', {'tag': '*', 'do': { -> coc#util#install()}}
" Initialize plugin system
call plug#end()

" ###################################################################################
" Native Vim Settings Section

" Adjust quickfix size to contents: http://vim.wikia.com/wiki/Automatically_fitting_a_quickfix_window_height
au FileType qf call AdjustWindowHeight(3, 50)

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
set tabstop=4 softtabstop=0 expandtab shiftwidth=4 smarttab
" Opens new panes below and to right of current
set splitbelow
set splitright
" Set all code unfolded by default
let g:ale_linters = {
\   'python': ['mypy'],
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
" Don't unload buffers when left
set hidden
" Don't give ins-completion-menu messages
set shortmess+=c

" ###################################################################################
" Functions Section

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

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

" ###################################################################################
" Plugin Settings Section

"Add private repo urls to this list to use Gbrowse(Opens file in browser)"
let g:fugitive_gitlab_domains = ['https://gitlab-app.eng.qops.net', 'https://github.com', 'https://gitlab.engservices.qops.net']

" Shows function signature above commandline instead of opening new window
let g:echodoc#enable_at_startup = 1
let g:echodoc#type = 'signature'

" Set linters for filetypes. I normally disable if running language server
" Python Language Server doesn't run pylint so enable it here.
let g:ale_linters = {
\   'python': ['mypy'],
\   'haskell': [],
\   'typescript': [],
\}

colorscheme vim-monokai-tasty
let g:lightline = {
      \ 'colorscheme': 'monokai_tasty',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'cocstatus', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'cocstatus': 'coc#status'
      \ },
      \ }

let g:coc_global_extensions = [ 'coc-css', 'coc-pyls', 'coc-highlight', 'coc-html', 'coc-html', 'coc-tsserver', 'coc-yaml', 'coc-word', 'coc-emoji' ]

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
nnoremap <leader>ld :call CocAction('jumpDefinition')<CR>
nnoremap <leader>lr :call CocAction('rename')<CR>
nnoremap <leader>lf :call CocAction('format')<CR>
nnoremap <leader>lt :call CocAction('jumpTypeDefinition')<CR>
nnoremap <leader>lx :call CocAction('jumpReferences')<CR>
nnoremap <leader>la :call CocAction('codeAction')<CR>
nnoremap <leader>lk :call CocAction('doHover')<CR>
nnoremap <leader>ls :call CocAction('documentSymbols')<CR>
nnoremap <leader>lh :call CocAction('highlight')<CR>
nnoremap <leader>lq :call CocAction('quickfixes')<CR>

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
" Use <c-space> for trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()
" Use <cr> for confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" Who doesn't like a good thesauras
nnoremap <Leader>st :ThesaurusQueryReplaceCurrentWord<CR>
" Some lovely grammar checking
nnoremap <Leader>sg :GrammarousCheck<CR>

" Align GitHub-flavored Markdown tables
vmap <Leader>a :EasyAlign*<Bar><Enter>

