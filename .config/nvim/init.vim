" ###################################################################################
" Install Plugins
" See README for links (Or just paste each plugin to https://github.com/)

" Auto install dein
if empty(glob('~/.cache/dein'))
  silent !curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh > installer.sh
  silent !sh ./installer.sh ~/.cache/dein
  silent !rm ./installer.sh
endif

" Add the dein installation directory into runtimepath
set runtimepath+=~/.cache/dein/repos/github.com/Shougo/dein.vim
if dein#load_state('~/.cache/dein')
  call dein#begin('~/.cache/dein')
    call dein#add('Konfekt/FastFold', {'on_event': 'InsertEnter'})
    call dein#add('Ron89/thesaurus_query.vim', {'on_ft': ['tex', 'markdown']})
    call dein#add('Yggdroot/indentLine', {'on_event': 'InsertEnter'})
    call dein#add('alvan/vim-closetag', {'on_ft': 'html'})
    call dein#add('heavenshell/vim-pydocstring', {'on_event': 'InsertEnter'})
    call dein#add('honza/vim-snippets')
    call dein#add('iamcco/markdown-preview.nvim', {'on_ft': ['markdown', 'pandoc.markdown', 'rmd'], 'build': 'cd app & yarn install' })
    call dein#add('jamessan/vim-gnupg')
    call dein#add('janko/vim-test', {'on_event' : 'InsertEnter' })
    call dein#add('junegunn/goyo.vim', {'on_cmd': 'Goyo'})
    call dein#add('junegunn/vim-easy-align', {'on_ft': 'markdown'})
    call dein#add('leafgarland/typescript-vim', {'on_ft': 'typescript'})
    call dein#add('lervag/vimtex', {'on_ft': 'tex'})
    call dein#add('liuchengxu/vista.vim', {'on_cmd': 'Vista'})
    call dein#add('machakann/vim-sandwich', {'on_event': 'InsertEnter' })
    call dein#add('mbbill/undotree', {'on_event': 'InsertEnter','on_cmd' : 'UndotreeToggle' })
    call dein#add('metakirby5/codi.vim', {'on_cmd': 'Codi!!'})
    call dein#add('mhinz/vim-signify', {'on_event': 'InsertEnter'})
    call dein#add('neoclide/coc.nvim', {'on_func': 'CocActionAsync', 'on_cmd':['CocCommand', 'CocList'],'on_event': 'InsertEnter', 'merge':0, 'build': './install.sh nightly'})
    call dein#add('neovimhaskell/haskell-vim', {'on_ft': 'haskell'})
    call dein#add('numirias/semshi')
    call dein#add('rhysd/vim-grammarous', {'on_cmd': 'GrammarousCheck'})
    call dein#add('ryanoasis/vim-devicons')
    call dein#add('scrooloose/nerdcommenter', {'on_event': 'InsertEnter'})
    call dein#add('scrooloose/nerdtree', {'on_cmd' : 'NERDTreeToggle' })
    call dein#add('shumphrey/fugitive-gitlab.vim')
    call dein#add('tiagofumo/vim-nerdtree-syntax-highlight', {'on_cmd' : 'NERDTreeToggle' })
    call dein#add('tmhedberg/SimpylFold', {'on_ft': 'python'})
    call dein#add('tpope/vim-fugitive', {'on_event': 'InsertEnter' })
    call dein#add('vim-airline/vim-airline', {"lazy": 1, 'depends': ['vim-airline-themes'], 'on_event': "InsertEnter"})
    call dein#add('vim-airline/vim-airline-themes', {"lazy": 1, "on_event": "InsertEnter"})
    call dein#add('w0rp/ale', {'on_event': 'InsertEnter'})
    call dein#add('mhinz/vim-startify')
    call dein#add('whiteinge/diffconflicts', {'on_cmd' : 'DiffConflicts' })
    call dein#set_hook('indentLine', 'hook_post_source', 'IndentLinesEnable')
    call dein#remote_plugins()
  call dein#end()
  call dein#save_state()
endif

" ###################################################################################
" Native Vim Settings

" Indents word-wrapped lines as much as the 'parent' line
set breakindent

" Ensures word-wrap does not split words
set formatoptions=l
set lbr

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
set foldlevel=99

" Update files on change
set autoread

" Save edit history between sessions
set undofile
set undodir=~/.vim/undodir

" Don't waste time holding shift for commands
map ; :
noremap ;; ;

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
set mouse=nvi

" Preview changes when using search and replace
set inccommand=nosplit

" Dont wrap lines
set nowrap

" Jump to existing window when opening buffer already opened
set switchbuf=useopen

" Space as leader key
let mapleader="\<Space>"

" Save state when using :mkview
set viewoptions=cursor,folds,slash,unix

color hasklo

" ###################################################################################
" Functions

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

function! StatusDiagnostic() abort
  let info = get(b:, 'coc_diagnostic_info', {})
  if empty(info) | return '' | endif
  let msgs = []
  if get(info, 'error', 0)
    call add(msgs, 'E' . info['error'])
  endif
  if get(info, 'warning', 0)
    call add(msgs, 'W' . info['warning'])
  endif
  return join(msgs, ' ') . ' ' . get(g:, 'coc_status', '')
endfunction

function! CodiSplit()
    let l:ft = &filetype
    vnew
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
    exe "setlocal filetype=".l:ft
    :Codi
endfunction
" ###################################################################################
" Plugin Settings

let entry_format = "'   ['. index .']'. repeat(' ', (3 - strlen(index)))"

if exists('*WebDevIconsGetFileTypeSymbol')  " support for vim-devicons
    let entry_format .= ". WebDevIconsGetFileTypeSymbol(entry_path) .' '.  entry_path"
else
    let entry_format .= '. entry_path'
endif

" Don't open preview window after entering the markdown buffer
let g:mkdp_auto_start = 1
" Auto close current preview window when change
let g:mkdp_auto_close = 1
let g:vimtex_compiler_progname = 'nvr' 
"Add private repo urls to this list to use Gbrowse(Opens file in browser) 
let g:fugitive_gitlab_domains = ['***REMOVED***', 'https://github.com', '***REMOVED***']

" Shows function signature above commandline instead of opening new window
let g:echodoc#enable_at_startup = 1
let g:echodoc#type = 'signature'

let g:coc_global_extensions = [ "coc-yank","coc-lists","coc-git", "coc-solargraph", "coc-eslint", "coc-json", 'coc-post', 'coc-python', 'coc-snippets', 'coc-docker', 'coc-java', 'coc-pairs', 'coc-vimtex', 'coc-ccls', 'coc-css', 'coc-highlight', 'coc-html', 'coc-tsserver', 'coc-yaml', 'coc-word', 'coc-emoji', 'coc-vimlsp' ]

" Set GoYo width
let g:goyo_width = 100

let g:coc_snippet_next = '<tab>'

" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

let g:mkdp_browser = 'firefox'

" Make NERDTree nicer looking
let g:WebDevIconsUnicodeDecorateFolderNodes = v:true
let g:NERDTreeDirArrowExpandable = "\u00a0"
let g:NERDTreeDirArrowCollapsible = "\u00a0"
let NERDTreeIgnore=['__pycache__', '__main__.py']

" Enable devicons for other plugins
let g:webdevicons_enable_denite = 1
let g:webdevicons_enable_airline_statusline = 1
" Use terminal windows for running tests
let test#strategy = "neovim"

" Put UndoTree at left of window, width 50, and disable highlighting changes.
let g:undotree_WindowLayout = 3
let g:undotree_SplitWidth = 50
let g:undotree_HighlightChangedText = 0

let g:vista_ctags_cmd = {
      \ 'haskell': 'hasktags -x -o - -c',
      \ }
let g:vista_icon_indent = ["╰─▸ ", "├─▸ "]
let g:vista#renderer#enable_icon = 1
let g:vista_sidebar_width = 50

let g:indentLine_char = '▏'

" Pretty icons for airline
let g:airline_powerline_fonts = 1
" Use manual loading of extensions
let g:airline#extensions#disable_rtp_load = 1
let g:airline_extensions= ['branch', 'coc', 'vimtex', 'undotree', 'fugitiveline', 'hunks']
let g:airline#extensions#hunks#non_zero_only = 1
let g:airline_theme = 'molokai'
let airline#extensions#coc#error_symbol = ' '
let airline#extensions#coc#warning_symbol = ' '
let g:airline#extensions#tabline#enabled = 1

" Any file larger than 10mb has certain features disabled to speed up load times
let g:LargeFile = 1024 * 1024 * 10

let g:ale_virtualtext_cursor = 1
let g:ale_linters = {
    \ "python": [],
    \ "haskell": [],
    \ "javascript": [],
    \ "typescript": [],
    \ "ruby": ["rubocop"]
  \ }


let g:codi#interpreters = {
   \ 'haskell': {
        \ 'bin': ['stack','ghci']
        \ },
   \ }

" Disable thesauras default mappings
let g:tq_map_keys=0
" ###################################################################################
" Autocommands

" Quit if nerdtree is last open window
au BufEnter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
" Open windows in other buffer from nerdtree
au BufEnter * let t:ft = split(expand("%"), ":") | if (len(t:ft) != 0 && t:ft[0] == "term") | :startinsert | endif

" Show function signatures when calling function
au User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')

" Adjust quickfix size to contents: http://vim.wikia.com/wiki/Automatically_fitting_a_quickfix_window_height
au FileType qf call AdjustWindowHeight(3, 50)

" Define denite window mappings
autocmd FileType denite call s:denite_my_settings()

" Disable indent lines for certain files.
au FileType help IndentLinesDisable
au FileType markdown IndentLinesDisable
au FileType codi IndentLinesDisable

au InsertEnter call AirlineSettings()

augroup LargeFile 
  au!
  autocmd BufReadPre * let f=getfsize(expand("<afile>")) | if f > g:LargeFile || f == -2 | call LargeFile() | endif
augroup END

au User AirlineAfterInit let g:airline_section_x = airline#section#create(["readonly", "%{get(b:, 'coc_git_blame', ' ')}"])
" ###################################################################################
" Custom Mappings

" Replace word with yanked text
nnoremap S "_dwP

" Jump to start and end of line easier
nnoremap H ^
nnoremap L $

"Save current buffer
nnoremap <leader>w :w<CR>

"Cycle between last two open buffers
nnoremap <leader><leader> <c-^>

" Netrw mappings
nnoremap <leader>x :NERDTreeToggle<CR>

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
" Exit program without signal message
tnoremap <silent> <C-d> <C-d><C-\><C-N>:q<CR>

" Git functions with vim-fugitive and git messenger
nnoremap <silent><leader>gs :Gstatus<CR>
nnoremap <silent><leader>gd :Gdiff<CR>
nnoremap <silent><leader>gp :Gpush<CR>
nnoremap <silent><leader>gb :Gbrowse<CR>
nnoremap <silent><leader>gl :Gblame<CR>

" Auto docstring
nmap <leader>p <Plug>(pydocstring)

" Toggle UndoTree window
nnoremap <silent><leader>u :UndotreeToggle<CR>

" HTTP requests - coc-post
nnoremap <leader>hd :CocCommand post.do<CR>
nnoremap <leader>hn :CocCommand post.new<CR>
nnoremap <leader>hl :CocList post<CR>

" Coc List Mappings
nnoremap <silent><leader>df :CocList files<CR>
nnoremap <silent><leader>dm :CocList mru<CR>
nnoremap <silent><leader>dg :CocList grep<CR>
nnoremap <silent><leader>db :CocList buffers<CR>
nnoremap <silent><leader>do :CocList outline<CR>
nnoremap <silent><leader>dh :CocList helptags<CR>
nnoremap <silent><leader>dq :CocList quickfix<CR>
nnoremap <silent><leader>ds :CocList symbols<CR>
nnoremap <silent><leader>dc :CocList commits<CR>
nnoremap <silent><leader>dy :CocList yank<CR>
nnoremap <silent><leader>dw :CocList words<CR>

" Language server functions
nmap <silent><leader>ld <Plug>(coc-definition)
nmap <silent><leader>lr <Plug>(coc-rename)
nmap <silent><leader>lf <Plug>(coc-format)
nmap <silent><leader>lt <Plug>(coc-type-definition)
nmap <silent><leader>lx <Plug>(coc-references)
nmap <silent><leader>lg <Plug>(coc-diagnostic-info)
nmap <silent><leader>ln <Plug>(coc-diagnostic-next)
nmap <silent><leader>lp <Plug>(coc-diagnostic-prev)
nmap <silent><leader>la <Plug>(coc-codeaction)
nmap <silent><leader>ls <Plug>(coc-codelens-action)
nmap <silent><leader>lt <Plug>(coc-float-jump)
nmap <silent><leader>lk :call CocActionAsync('doHover')<CR>
nmap <silent><leader>ls :call CocActionAsync('documentSymbols')<CR>
nmap <silent><leader>lh :call CocActionAsync('highlight')<CR>
nmap <silent><leader>lq :call CocActionAsync('quickfixes')<CR>
nmap <silent><leader>li :CocList<CR>

nnoremap <silent><leader>sc :CocCommand session.save<CR>
nnoremap <silent><leader>so :CocCommand session.open<CR>
nnoremap <silent><leader>sr :CocCommand session.restart<CR>
nnoremap <silent><leader>sl :CocList sessions<CR>

" Testing functions
nnoremap <silent><leader>tn :TestNearest<CR>
nnoremap <silent><leader>tf :TestFile<CR>
nnoremap <silent><leader>ts :TestSuite<CR>
nnoremap <silent><leader>tl :TestLast<CR>
nnoremap <silent><leader>tv :TestVisit<CR>
nnoremap <silent><leader>tm :make test<CR>
nnoremap <silent><leader>to :!open coverage/index.html<CR>

" Ctags and LSP symbol finding
nnoremap <silent><leader>vv :Vista!!<CR>
nnoremap <silent><leader>vf :Vista finder<CR>

" Distraction free writing
nnoremap <silent><leader>z :Goyo<CR>
" Use Tab for cycling through completions.
" Use Enter to expand a snippet.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
imap <silent> <CR> <Plug>(coc-snippets-expand)

inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
" Use <c-space> for trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()
" Use <CR> for confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <silent><expr> <CR> pumvisible() ? coc#_select_confirm() :
                                           \"\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Who doesn't like a good thesauras
nnoremap <leader>ut :ThesaurusQueryReplaceCurrentWord<CR>
" Some lovely grammar checking
nnoremap <leader>ug :GrammarousCheck<CR>
"Replace the word under cursor
nnoremap <leader>us :%s/\<<c-r><c-w>\>//g<left><left>


" Align GitHub-flavored Markdown tables
vmap <leader>a :EasyAlign*<Bar><Enter>

" Find highlight group under cursor for changing colorschemes
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

