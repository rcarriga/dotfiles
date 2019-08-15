" ###################################################################################
" Install Plugins
"

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
    call dein#add('rhysd/reply.vim')
    call dein#add('Konfekt/FastFold')
    call dein#add('Ron89/thesaurus_query.vim', {'on_ft': ['tex', 'markdown']})
    call dein#add('Yggdroot/indentLine')
    call dein#add('alvan/vim-closetag', {'on_ft': 'html'})
    call dein#add('heavenshell/vim-pydocstring')
    call dein#add('tpope/vim-eunuch')
    call dein#add('honza/vim-snippets')
    call dein#add('iamcco/markdown-preview.nvim', {'on_ft': ['markdown', 'pandoc.markdown', 'rmd'], 'build': 'cd app & yarn install' })
    call dein#add('jamessan/vim-gnupg')
    call dein#add('janko/vim-test', {"lazy": 1})
    call dein#add('junegunn/goyo.vim', {'on_cmd': 'Goyo'})
    call dein#add('junegunn/vim-easy-align', {'on_ft': 'markdown'})
    call dein#add('leafgarland/typescript-vim', {'on_ft': 'typescript'})
    call dein#add('lervag/vimtex', {'on_ft': 'tex'})
    call dein#add('liuchengxu/vista.vim', {'on_cmd': 'Vista'})
    call dein#add('machakann/vim-sandwich')
    call dein#add('simnalamburt/vim-mundo',{'lazy':1})
    call dein#add('neoclide/coc.nvim', {'merge': 0, 'rev': 'release'})
    call dein#add('neovimhaskell/haskell-vim', {'on_ft': 'haskell'})
    call dein#add('numirias/semshi')
    call dein#add('rhysd/vim-grammarous', {'on_cmd': 'GrammarousCheck'})
    call dein#add('scrooloose/nerdcommenter', )
    call dein#add('Shougo/defx.nvim')
    call dein#add('kristijanhusak/defx-icons')
    call dein#add('kristijanhusak/defx-git')
    call dein#add('tmhedberg/SimpylFold', {'on_ft': 'python'})
    call dein#add('mhinz/vim-signify')
    call dein#add('tpope/vim-fugitive')
    call dein#add('vim-airline/vim-airline', {"lazy": 1, "depends": "vim-airline-themes"})
    call dein#add('vim-airline/vim-airline-themes')
    call dein#add('w0rp/ale', {"lazy": 1})
    call dein#add('whiteinge/diffconflicts', {'on_cmd' : 'DiffConflicts' })
    call dein#add('kkoomen/vim-doge',{"lazy":1})
    call dein#add('machakann/vim-swap')
    call dein#add('rhysd/clever-f.vim')
    call dein#add('justinmk/vim-sneak')
    call dein#add('udalov/kotlin-vim', {'on_ft': 'kotlin'})
    call dein#add('junegunn/vim-peekaboo')
    call dein#add('tpope/vim-unimpaired')
    " call dein#add('rcarriga/vim-test-status', {"lazy": 1, "depends": "vim-test"})
    if !has("nvim")
        call dein#add('roxma/nvim-yarp')
        call dein#add('roxma/vim-hug-neovim-rpc')
    endif
    call dein#set_hook('indentLine', 'hook_post_source', 'IndentLinesEnable')
    call dein#remote_plugins()
  call dein#end()
  call dein#save_state()
endif

filetype plugin on
syn on
echo ""

" ###################################################################################
" Functions

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

function! AdjustWindowHeight(minheight, maxheight) abort
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

function! AirlineSections() abort
    let g:airline_section_x = airline#section#create(["readonly","%{GetTestResults()}", "%{get(b:, 'coc_git_blame', ' ')}"])
    let g:airline_section_b =  airline#section#create(["%{get(g:, 'coc_git_status', ' ')}", "%{get(b:, 'coc_git_status', ' ')}"])
endfunction

function! OpenFileInPreviousWindow(file) abort
    normal p 
    exec "edit ".a:file
endfunction

function! GetTestResults() abort
    return get(b:, "test_status_total") ?
                \ get(b:, "test_status_passed")." Pass ".get(b:, "test_status_total")." Fail" : ""
endfunction

" ###################################################################################
" Plugin Settings

" Markdown preview default browser
let g:mkdp_browser = 'firefox'
" Don't open preview window after entering the markdown buffer
let g:mkdp_auto_start = 0
" Auto close current preview window when change
let g:mkdp_auto_close = 1
let g:vimtex_compiler_progname = 'nvr' 

let g:coc_global_extensions = [ "coc-tabnine", "coc-emmet", "coc-yank","coc-lists","coc-solargraph", "coc-eslint", "coc-json", 'coc-post', 'coc-python', 'coc-snippets', 'coc-docker', 'coc-java', 'coc-pairs', 'coc-vimtex', 'coc-ccls', 'coc-css', 'coc-highlight', 'coc-html', 'coc-tsserver', 'coc-yaml', 'coc-word', 'coc-emoji', 'coc-vimlsp' ]

" Set GoYo width
let g:goyo_width = 100

let g:coc_snippet_next = '<tab>'

" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

" Use terminal windows for running tests
let test#strategy = "neovim"

" Open undo tree on right
let g:mundo_right = 1

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
let g:airline_extensions= ['coc', 'vimtex']
let g:airline#extensions#hunks#non_zero_only = 1
let g:airline_theme = 'molokai'
let airline#extensions#coc#error_symbol = ' '
let airline#extensions#coc#warning_symbol = ' '
let g:airline#extensions#tabline#enabled = 1

let g:ale_virtualtext_cursor = 1
let g:ale_linters = {
    \ "python": [],
    \ "haskell": [],
    \ "javascript": [],
    \ "typescript": [],
    \ "ruby": ["rubocop"]
  \ }

" Disable thesauras default mappings
let g:tq_map_keys=0

let g:sneak#label = 1
let g:sneak#s_next = 1

let g:doge_mapping = "\<leader\>ii"
let g:doge_mapping_comment_jump_forward = "\<C-n\>"
let g:doge_mapping_comment_jump_backward = "\<C-p\>"

let g:test_status#icons = 1
let g:test_status#virtual_text = 1

let g:signify_sign_add               = "\u2503"
let g:signify_sign_delete            = "\u2503"
let g:signify_sign_delete_first_line = "\u2503"
let g:signify_sign_change            = "\u2503"

" ###################################################################################
" Autocommands

augroup AirlineInit
    au CursorMoved * call s:AirlineInit()
augroup END

function! s:AirlineInit()
    AirlineRefresh 
    augroup AirlineInit
        au!
    augroup END
endfunction

augroup SemshiInit
    au CursorMoved python call s:SemshiInit()
augroup END

function! s:SemshiInit()
    Semshi 
    augroup SemshiInit
        au!
    augroup END
endfunction

" Disable indent lines for certain files.
augroup IndentLinesDisabled
    au!
    au FileType help,markdown,tex,plaintex IndentLinesDisable
augroup END

augroup AirlineSetup
    au!
    au User AirlineAfterInit call AirlineSections()
augroup END

augroup CocSetup
    " Show function signatures when calling function
    au!
    au User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
    autocmd CursorHold * silent call CocActionAsync('highlight')
augroup END

augroup DefxSetup
    au!
    autocmd FileType defx call s:defx_my_settings()
augroup END

augroup SneakSetup
    au!
    au User SneakEnter IndentLinesDisable
    au User SneakLeave IndentLinesEnable
augroup END

augroup TestStatusRunner
    au!
    au CursorMoved * ++once TestStatus
    au BufWritePost * TestStatusNearest
augroup END

command! -nargs=1 OpenPrevious call OpenFileInPreviousWindow(<f-args>)

" ###################################################################################
" Plugin Mappings

" Defx file explorer commands
nnoremap <silent> <leader>x :Defx -toggle -split=vertical -direction=topleft -winwidth=30 -columns=indent:git:icons:mark:filename:type<CR>
function! s:defx_my_settings() abort
    nnoremap <silent><buffer><expr> <CR> defx#do_action('open', 'OpenPrevious')
    nnoremap <silent><buffer><expr> c defx#do_action('copy')
    nnoremap <silent><buffer><expr> m defx#do_action('move')
    nnoremap <silent><buffer><expr> p defx#do_action('paste')
    nnoremap <silent><buffer><expr> l defx#do_action('open_tree')
    nnoremap <silent><buffer><expr> h defx#do_action('close_tree')
    nnoremap <silent><buffer><expr> L defx#do_action('open_tree_recursive')
    nnoremap <silent><buffer><expr> v defx#do_action('open', 'vsplit')
    nnoremap <silent><buffer><expr> s defx#do_action('open', 'split')
    nnoremap <silent><buffer><expr> o defx#do_action('open_or_close_tree')
    nnoremap <silent><buffer><expr> M defx#do_action('new_directory')
    nnoremap <silent><buffer><expr> N defx#do_action('new_file')
    nnoremap <silent><buffer><expr> C defx#do_action('toggle_columns', 'indent:filename:type:size:time')
    nnoremap <silent><buffer><expr> S defx#do_action('toggle_sort', 'time')
    nnoremap <silent><buffer><expr> d defx#do_action('remove')
    nnoremap <silent><buffer><expr> r defx#do_action('rename')
    nnoremap <silent><buffer><expr> ! defx#do_action('execute_command')
    nnoremap <silent><buffer><expr> x defx#do_action('execute_system')
    nnoremap <silent><buffer><expr> yy defx#do_action('yank_path')
    nnoremap <silent><buffer><expr> . defx#do_action('toggle_ignored_files')
    nnoremap <silent><buffer><expr> ; defx#do_action('repeat')
    nnoremap <silent><buffer><expr> ~ defx#do_action('cd')
    nnoremap <silent><buffer><expr> q defx#do_action('quit')
    nnoremap <silent><buffer><expr> <Space> defx#do_action('toggle_select') . 'j'
    nnoremap <silent><buffer><expr> * defx#do_action('toggle_select_all')
    nnoremap <silent><buffer><expr> j line('.') == line('$') ? 'gg' : 'j'
    nnoremap <silent><buffer><expr> k line('.') == 1 ? 'G' : 'k'
    nnoremap <silent><buffer><expr> <C-g> defx#do_action('print')
    nnoremap <silent><buffer><expr> cd defx#do_action('change_vim_cwd')
endfunction

" Vim sneak commands
nmap x <Plug>Sneak_s
nmap X <Plug>Sneak_S
xmap x <Plug>Sneak_s
xmap X <Plug>Sneak_S
omap x <Plug>Sneak_s
omap X <Plug>Sneak_S

" Git functions with vim-fugitive and git messenger
nmap <silent><leader>gs :Gstatus<CR>
nmap <silent><leader>gd :Gdiff<CR>
nmap <silent><leader>gp :Gpush<CR>
nmap <silent><leader>gb :Gbrowse<CR>
nmap <silent><leader>gl :Gblame<CR>
nmap <silent><leader>gf :CocCommand git.foldUnchanged<CR>
nmap <silent><leader>gu :CocCommand git.chunkUndo<CR>
nmap <silent><leader>ga :CocCommand git.chunkStage<CR>
nmap <silent><leader>gp <Plug>(coc-git-prevchunk)
nmap <silent><leader>gn <Plug>(coc-git-nextchunk)
nmap <silent><leader>gi <Plug>(coc-git-chunkinfo)
nmap <silent><leader>gc <Plug>(coc-git-commit)

" Auto docstring
nmap <silent><leader>p <Plug>(pydocstring)

" Toggle UndoTree window
nmap <silent><leader>u :MundoToggle<CR>

" HTTP requests - coc-post
nmap <silent><leader>hd :CocCommand post.do<CR>
nmap <silent><leader>hn :CocCommand post.new<CR>
nmap <silent><leader>hl :CocList post<CR>

" Coc List Mappings
nmap <silent><leader>df :CocList files<CR>
nmap <silent><leader>dm :CocList mru<CR>
nmap <silent><leader>dg :CocList grep<CR>
nmap <silent><leader>db :CocList buffers<CR>
nmap <silent><leader>do :CocList outline<CR>
nmap <silent><leader>dh :CocList helptags<CR>
nmap <silent><leader>dq :CocList quickfix<CR>
nmap <silent><leader>ds :CocList symbols<CR>
nmap <silent><leader>dc :CocList commits<CR>
nmap <silent><leader>dy :CocList yank<CR>
nmap <silent><leader>dw :CocList words<CR>
nmap <silent><leader>dk :CocList marks<CR>

" Language server functions
nmap <silent><leader>ld <Plug>(coc-definition)
nmap <silent><leader>lr <Plug>(coc-rename)
nmap <silent><leader>lf <Plug>(coc-format)
vmap <silent><leader>lf <Plug>(coc-format-selected)
nmap <silent><leader>lt <Plug>(coc-type-definition)
nmap <silent><leader>lx <Plug>(coc-references)
nmap <silent><leader>lg <Plug>(coc-diagnostic-info)
nmap <silent><leader>ln <Plug>(coc-diagnostic-next-error)
nmap <silent><leader>lp <Plug>(coc-diagnostic-prev-error)
nmap <silent><leader>la <Plug>(coc-codeaction)
nmap <silent><leader>lj <Plug>(coc-float-jump)
nmap <silent><leader>ls :call CocActionAsync('codeLensAction')<CR>
nmap <silent><leader>lk :call CocActionAsync('doHover')<CR>
nmap <silent><leader>ls :call CocActionAsync('documentSymbols')<CR>
nmap <silent><leader>lq :call CocActionAsync('quickfixes')<CR>

" Session Management
nmap <silent><leader>sc :CocCommand session.save<CR>
nmap <silent><leader>so :CocCommand session.open<CR>
nmap <silent><leader>sr :CocCommand session.restart<CR>
nmap <silent><leader>sl :CocList sessions<CR>

" Repl Commands
nmap <silent><leader>ro :Repl<CR>
nmap <silent><leader>rc :ReplStop<CR>
nmap <silent><leader>rs :ReplSend<CR>
vmap <silent><leader>rs :ReplSend<CR>

" Testing functions
nmap <silent><leader>tn :TestNearest<CR>
nmap <silent><leader>tf :TestFile<CR>
nmap <silent><leader>tt :TestSuite<CR>
nmap <silent><leader>tl :TestLast<CR>
nmap <silent><leader>tv :TestVisit<CR>
nmap <silent><leader>tm :make test<CR>
nmap <silent><leader>to :!open coverage/index.html<CR>
nmap <silent><leader>ts <Plug>(test-status-run-all)
nmap <silent><leader>tj <Plug>(test-status-next-fail)
nmap <silent><leader>tk <Plug>(test-status-prev-fail)

" Ctags and LSP symbol finding
nmap <silent><leader>vv :Vista!!<CR>

" Who doesn't like a good thesauras
nmap <leader>ot :ThesaurusQueryReplaceCurrentWord<CR>
" Some lovely grammar checking
nmap <leader>og :GrammarousCheck<CR>

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
imap <silent> <CR> <Plug>(coc-snippets-expand)

inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
" Use <CR> for confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <silent><expr> <CR> pumvisible() ? coc#_select_confirm() :
                                           \"\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
