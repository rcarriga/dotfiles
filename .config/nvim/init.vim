
" ###################################################################################
" Install Plugins
" See README for links (Or just paste each plugin to https://github.com/)

" Auto install dein
if empty(glob('~/.cache/dein'))
  silent !curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh > installer.sh 
  silent !sh ./installer.sh ~/.cache/dein
  silent !rm ./installer.sh
endif

if &compatible
  set nocompatible
endif
" Add the dein installation directory into runtimepath
set runtimepath+=~/.cache/dein/repos/github.com/Shougo/dein.vim
if dein#load_state('~/.cache/dein')
  call dein#begin('~/.cache/dein')

    call dein#add('rhysd/git-messenger.vim', {
            \   'lazy' : 1,
            \   'on_cmd' : 'GitMessenger',
            \   'on_map' : '<Plug>(git-messenger)',
            \ })
    " Hopefully temporary to set nicer background for git messenger
    call dein#set_hook('git-messenger.vim', 'hook_source', 'hi gitmessengerPopupNormal term=None guifg=#eeeeee guibg=#222222 ctermfg=255 ctermbg=234')
    call dein#add('Ron89/thesaurus_query.vim', {'on_ft': ['tex', 'markdown']})
    call dein#add('Shougo/echodoc.vim', {'on_event': 'InsertEnter'})
    call dein#add('airblade/vim-gitgutter')
    call dein#add('honza/vim-snippets')
    call dein#add('alvan/vim-closetag', {'on_ft': 'html'})
    call dein#add('itchyny/lightline.vim')
    call dein#add('junegunn/fzf', { 'build': './install --all', 'merged': 0 }) 
    call dein#add('junegunn/fzf.vim', { 'depends': 'fzf' })
    call dein#add('junegunn/vim-easy-align', {'on_ft': 'markdown'})
    call dein#add('leafgarland/typescript-vim', {'on_ft': 'typescript'})
    call dein#add('lervag/vimtex', {'on_ft': 'tex'})
    call dein#add('neovimhaskell/haskell-vim', {'on_ft': 'haskell'})
    call dein#add('trusktr/seti.vim', {'style': 'colors'})
    call dein#add('patstockwell/vim-monokai-tasty', {'style': 'colors'})
    "call dein#add( 'plytophogy/vim-virtualenv', {'on_ft': 'python'})
    call dein#add('heavenshell/vim-pydocstring', {'on_ft': 'python', 'on_event': 'InsertEnter'})
    call dein#add('scrooloose/nerdcommenter', {'on_event': 'InsertEnter'})
    call dein#add('shumphrey/fugitive-gitlab.vim')
    call dein#add('junegunn/limelight.vim', {'on_event': 'InsertEnter'})
    call dein#add('tmhedberg/SimpylFold', {'on_ft': 'python'})
    call dein#add('tpope/vim-fugitive')
    call dein#add('machakann/vim-sandwich')
    call dein#add('w0rp/ale', {'on_event': 'InsertEnter'})
    call dein#add('rhysd/vim-grammarous', {'on_ft': ['markdown', 'tex']})
    call dein#add('neoclide/coc.nvim', {'merge':0, 'build': './install.sh nightly'})
    "call dein#add('numirias/semshi', {'on_ft': 'python'})
    call dein#add('junegunn/goyo.vim', {'on_event': 'InsertEnter'})
    call dein#add('iamcco/markdown-preview.nvim', {'on_ft': ['markdown', 'pandoc.markdown', 'rmd'],
					\ 'build': 'cd app & yarn install' })
    call dein#remote_plugins()
  call dein#end()
  call dein#save_state()
endif
" ###################################################################################
" Native Vim Settings

" Adjust quickfix size to contents: http://vim.wikia.com/wiki/Automatically_fitting_a_quickfix_window_height
au FileType qf call AdjustWindowHeight(3, 50)
" Transparent Background
au ColorScheme * hi Normal ctermbg=none guibg=none
" Slightly different background for popup menu. Easier to see
au ColorScheme * hi Pmenu guibg=#222222
" Default error text is too dark to read in floating windows
au ColorScheme * hi CocErrorFloat ctermfg=9 guifg=#FFFFFF guibg=#333333
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
set foldlevel=99
" Update files on change
set autoread
" Save edit history between sessions
set undofile
set undodir=~/.vim/undodir
" Don't waste time holding shift for commands
map ; :
noremap ;; ;
" Who needs NERDTree? (Makes netrw look nicer)
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_winsize = 12
let g:netrw_browse_split = 4
" Allows commandline to use 2 lines (Better for multiline lint messages etc)
"set cmdheight=2
" Don't unload buffers when left
set hidden
" Don't give ins-completion-menu messages
set shortmess+=c

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

 function! ToggleVExplorer()
  if exists("t:expl_buf_num")
      let expl_win_num = bufwinnr(t:expl_buf_num)
      if expl_win_num != -1
          let cur_win_nr = winnr()
          exec expl_win_num . 'wincmd w'
          close
          exec cur_win_nr . 'wincmd w'
          unlet t:expl_buf_num
      else
          unlet t:expl_buf_num
      endif
  else
      exec '1wincmd w'
      Vexplore
      let t:expl_buf_num = bufnr("%")
  endif
endfunction

" ###################################################################################
" Plugin Settings

" Open preview window after entering the markdown buffer
let g:mkdp_auto_start = 0
" Auto close current preview window when change
let g:mkdp_auto_close = 1

"Add private repo urls to this list to use Gbrowse(Opens file in browser)"
let g:fugitive_gitlab_domains = ['https://gitlab-app.eng.qops.net', 'https://github.com', 'https://gitlab.engservices.qops.net']

" Shows function signature above commandline instead of opening new window
let g:echodoc#enable_at_startup = 1
let g:echodoc#type = 'signature'

" Set linters for filetypes. I normally disable if running language server
let g:ale_linters = {
\   'python': [],
\   'haskell': [],
\   'typescript': [],
\   'javascript': []
\}

color vim-monokai-tasty
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

let g:coc_global_extensions = [ 'coc-python', 'coc-snippets', 'coc-docker', 'coc-java', 'coc-pairs', 'coc-vimtex', 'coc-ccls', 'coc-css', 'coc-highlight', 'coc-html', 'coc-tsserver', 'coc-yaml', 'coc-word', 'coc-emoji' ]
autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')

" Set GoYo width
let g:goyo_width = 100

" Enable limelight when using GoYo
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!
let g:limelight_conceal_guifg = 'DarkGray'


let g:coc_snippet_next = '<tab>'

" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

" ###################################################################################
" Custom Mappings

" Auto expand curly braces and place cursor in the middle
inoremap {<CR> {<CR>}<C-o>==<C-o>O

" Replace word with yanked text
nnoremap S "_diwP

" Netrw mappings
nnoremap <Leader>nv :call ToggleVExplorer()<CR>

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

" Git functions with vim-fugitive and git messenger
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gp :Gpush<CR>
nnoremap <Leader>gb :Gbrowse<CR>
nnoremap <Leader>gl :Gblame<CR>
nnoremap <Leader>m :GitMessenger<CR>

" Language server functions
nnoremap <leader>ld :call CocAction('jumpDefinition')<CR>
nnoremap <leader>lr :call CocAction('rename')<CR>
nnoremap <leader>lf :call CocAction('format')<CR>
nnoremap <leader>lt :call CocAction('jumpTypeDefinition')<CR>
nnoremap <leader>lx :call CocAction('jumpReferences')<CR>
nnoremap <leader>lg :call CocAction('diagnosticInfo')<CR>
nnoremap <leader>la :call CocAction('codeAction')<CR>
nnoremap <leader>lk :call CocAction('doHover')<CR>
nnoremap <leader>ls :call CocAction('documentSymbols')<CR>
nnoremap <leader>lh :call CocAction('highlight')<CR>
nnoremap <leader>lq :call CocAction('quickfixes')<CR>

" Distraction free writing
nnoremap <leader>d :Goyo<CR>

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
" Use <cr> for confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm() : 
                                           \"\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Who doesn't like a good thesauras
nnoremap <Leader>st :ThesaurusQueryReplaceCurrentWord<CR>
" Some lovely grammar checking
nnoremap <Leader>sg :GrammarousCheck<CR>

" Align GitHub-flavored Markdown tables
vmap <Leader>a :EasyAlign*<Bar><Enter>

" Disgusting mapping to find highlight group under cursor for changing
" colorschemes
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
