if exists("g:plugins_loaded")
    finish
endif

let plugins = {
      \ "lervag/vimtex": {"lazy": 1},
      \ "KabbAmine/vCoolor.vim": {},
      \ "Konfekt/FastFold": {},
      \ "Ron89/thesaurus_query.vim": {"on_ft": ["tex", "markdown"]},
      \ "Vigemus/iron.nvim": {},
      \ "Yggdroot/LeaderF": {"build": "./install.sh"},
      \ "alvan/vim-closetag": {},
      \ "godlygeek/tabular": {},
      \ "honza/vim-snippets": {},
      \ "iamcco/markdown-preview.nvim": {"on_ft": ["markdown", "pandoc.markdown", "rmd"], "build": "cd app & yarn install" },
      \ "jalvesaq/Nvim-R": {},
      \ "jamessan/vim-gnupg": {},
      \ "janko/vim-test": {"lazy": 1},
      \ "junegunn/goyo.vim": {"on_cmd": "Goyo"},
      \ "justinmk/vim-sneak": {},
      \ "kkoomen/vim-doge": {"lazy":1, "hook_post_source": "DogeGenerate"},
      \ "liuchengxu/vista.vim": {"on_cmd": "Vista"},
      \ "machakann/vim-sandwich": {},
      \ "machakann/vim-swap": {},
      \ "mhinz/vim-signify": {},
      \ "neoclide/coc.nvim": {"merge": 0, "rev": "release"},
      \ "numirias/semshi": {},
      \ "rhysd/clever-f.vim": {},
      \ "rhysd/vim-grammarous": {"on_cmd": "GrammarousCheck"},
      \ "scrooloose/nerdcommenter": {},
      \ "sheerun/vim-polyglot": {"depends": "tabular"},
      \ "simnalamburt/vim-mundo": {"lazy":1},
      \ "takac/vim-hardtime": {},
      \ "tpope/vim-abolish": {},
      \ "tpope/vim-eunuch": {},
      \ "tpope/vim-fugitive": {},
      \ "tpope/vim-sleuth": {"hook_post_source": "Sleuth"},
      \ "tpope/vim-unimpaired": {},
      \ "vim-airline/vim-airline": {"lazy": 1, "depends": "vim-airline-themes"},
      \ "vim-airline/vim-airline-themes": {"lazy": 1},
      \ "vim-pandoc/vim-pandoc": {},
      \ "vim-pandoc/vim-pandoc-syntax": {},
      \ "vim-scripts/ReplaceWithRegister": {},
      \ "w0rp/ale": {"lazy": 1},
      \ "wellle/targets.vim": {},
      \ "whiteinge/diffconflicts": {"on_cmd" : "DiffConflicts" },
      \ "junegunn/gv.vim": {}
\ }

if !has("nvim")
  let plugins = extend(plugins,
  \ {"roxma/nvim-yarp": {},
  \ "roxma/vim-hug-neovim-rpc": {}})
endif

let g:plugins_loaded = 1
" ###################################################################################
" Plugin Settings {{{1
"
let g:vimtex_quickfix_enabled = 0
let g:vimtex_compiler_progname = "nvr"
let g:vimtex_view_method = "zathura"

" Store compiled latex files in "build" dir
let g:vimtex_compiler_latexmk = {"build_dir": "build"}

" Ignore files for  fuzzy searching
let g:Lf_WildIgnore = {
        \ 'dir': [".env", "__pycache__", ".mypy_cache", ".stack"],
        \ 'file': []
        \}

" Vim hardtime keys
let g:list_of_normal_keys = ["h", "j", "k", "l", "-", "+"]

let g:pandoc#folding#fdc = 0

let g:polyglot_disabled = ['jsx', "latex"]

" Markdown preview default browser
let g:mkdp_browser = "firefox"
" Don"t open preview window after entering the markdown buffer
let g:mkdp_auto_start = 0
" Auto close current preview window when change
let g:mkdp_auto_close = 0

let g:coc_global_extensions = [ "coc-sh", "coc-gitignore", "coc-yank", "coc-lists", "coc-eslint", "coc-json", "coc-post", "coc-python", "coc-snippets", "coc-docker", "coc-css", "coc-highlight", "coc-html", "coc-tsserver", "coc-yaml", "coc-word", "coc-vimlsp" ]

" Set GoYo width
let g:goyo_width = 100

let g:coc_snippet_next = "<tab>"

" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = "left"
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

if has("nvim")
    " Use terminal windows for running tests
    let test#strategy = "neovim"
    let test#python#pytest#options = "--disable-warnings"
endif

" Open undo tree on right
let g:mundo_right = 1

let g:vista_ctags_cmd = {
      \ "haskell": "hasktags -x -o - -c",
      \ }
let g:vista_icon_indent = ["╰─▸", "├─▸"]
let g:vista#renderer#enable_icon = 1
let g:vista_sidebar_width = 50

" Pretty icons for airline
let g:airline_powerline_fonts = 1
" Use manual loading of extensions
let g:airline#extensions#tabline#show_splits = 1
let g:airline#extensions#hunks#non_zero_only = 1
let g:airline_theme = "night_owl"
let g:airline#extensions#tmuxline#enabled = 1
let g:airline#extensions#coc#error_symbol = " "
let g:airline#extensions#coc#warning_symbol = " "
let g:airline#extensions#tabline#enabled = 1

let g:ale_virtualtext_cursor = 1
let g:ale_linters = {
    \ "python": [],
    \ "haskell": [],
    \ "javascript": [],
    \ "typescript": [],
    \ "ruby": ["rubocop"],
    \ "r": []
  \ }

" Disable thesauras default mappings
let g:tq_map_keys=0

let g:sneak#label = 1
let g:sneak#s_next = 1

let g:doge_mapping = "\<leader\>i"
let g:doge_mapping_comment_jump_forward = "\<C-\]>"
let g:doge_mapping_comment_jump_backward = "\<C-[\>"
let g:doge_doc_standard_python = "sphinx"

let g:vitest#icons = 1
let g:vitest#virtual_text = 1

let g:signify_sign_add               = "\u258B"
let g:signify_sign_delete            = "\u258B"
let g:signify_sign_delete_first_line = "\u258B"
let g:signify_sign_change            = "\u258B"

let g:vim_markdown_math = 1
let g:vim_markdown_new_list_item_indent = 0

let g:sleuth_automatic = 1

let g:closetag_xhtml_filenames = '*.xhtml,*.jsx,*.tsx'
let g:closetag_xhtml_filetypes = 'xhtml,jsx,tsx,typescriptreact'
let g:closetag_emptyTags_caseSensitive = 1
let g:closetag_regions = {
    \ 'typescriptreact': 'jsxRegion,tsxRegion',
    \ 'javascriptreact': 'jsxRegion',
    \ }

let g:Lf_WindowPosition = 'popup'
let g:Lf_PreviewInPopup = 1
let g:Lf_StlSeparator = { 'left': '', 'right': '' }
let g:Lf_PopupWidth = &columns * 1/2
let g:Lf_PopupShowStatusline = 0

let g:pandoc#modules#disabled = ["formatting", "command", "menu", "keyboard", "bibliographies", "completion", "toc", "spell", "hypertext"]

" }}}1
" ###################################################################################
" Install Plugins {{{1
"
" Auto install dein
if empty(glob("~/.cache/dein"))
  let g:needs_reload = 1
  silent !curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh > installer.sh
  silent !sh ./installer.sh ~/.cache/dein
  silent !rm ./installer.sh
endif

" Add the dein installation directory into runtimepath
set runtimepath+=~/.cache/dein/repos/github.com/Shougo/dein.vim
let g:dein#auto_recache = 1
if dein#load_state("~/.cache/dein")
  call dein#begin("~/.cache/dein")

    for [plugin, options] in items(plugins)
      call dein#add(plugin, options)
    endfor


    call dein#remote_plugins()
  call dein#end()
  call dein#save_state()
endif
if exists("g:needs_reload")
  echom "Restart needed"
  call dein#update()
  finish
endif

filetype plugin indent on
syn on

" }}}1
" ###################################################################################
" Functions {{{1
"
function! s:isOverWhitespace() abort
  let col = col(".") - 1
  " return !col || getline(".")[col - 1]  =~# "\s"
  return 1
endfunction

function! AirlineSections() abort
    " let g:airline_section_x = airline#section#create(["readonly"])
    " let g:airline_section_b =  airline#section#create(["%{get(g:, 'coc_git_status', ' ')}", "%{get(b:, 'coc_git_status', ' ')}"])
endfunction

function! OpenFileInPreviousWindow(file) abort
    normal p
    exec "edit ".a:file
endfunction

function! GetTestResults() abort
    return " " + get(b:, "vitest_total") ?
                \ get(b:, "vitest_passed")." Pass ".get(b:, "vitest_failed")." Fail" : ""
endfunction

function! OpenInFloating(params) abort
    let [_, curs, filename] = split(a:params)
    let out_buffer = bufadd(filename)
    call bufload(out_buffer)
    let height = nvim_buf_line_count(out_buffer)
    let window_params = {"relative": "cursor", "width": 100, "height": height < 20 ? height : 20, "row": 1, "col": 1, "anchor": "SW", "style": "minimal"}
    let created_window = nvim_open_win(out_buffer, v:true, window_params)
    exec "call ".curs
    exec "au WinLeave * ++once call nvim_win_close(".created_window.", v:true)"
endfunction

" }}}1
" ###################################################################################
" NeoVim Specific {{{1

lua << EOF
local iron = require("iron")

iron.core.set_config{
  repl_open_cmd = "vsplit"
}

EOF

" }}}1
" ###################################################################################
" Autocommands {{{1

augroup VimtexInit
    au!
    au BufReadPre *.tex let b:vimtex_main = 'main.tex'
augroup END

augroup AirlineInit
    au!
    au CursorMoved * ++once AirlineRefresh
augroup END

augroup SemshiInit
    au!
    au CursorMoved python ++once Semshi
augroup END

augroup AirlineSetup
    au!
    au User AirlineAfterInit call AirlineSections()
augroup END

augroup CocSetup
    " Show function signatures when calling function
    au!
    au User CocJumpPlaceholder call CocActionAsync("showSignatureHelp")
    autocmd CursorHold * silent call CocActionAsync("highlight")
augroup END

" augroup ViTestStatusRunner
"     au!
"     au BufWritePost * ViTest
" augroup END
" }}}1
" ###################################################################################
" Custom Commands {{{1

command! -nargs=1 OpenPrevious call OpenFileInPreviousWindow(<f-args>)
command! -nargs=1 OpenPeek call OpenInFloating(<f-args>)
command CC CocCommand

" }}}1
" ###################################################################################
" Plugin Mappings {{{1

" Open config directory
nnoremap <silent>` :CocCommand explorer ~/.vim<CR>

" Vim sneak commands
nmap x <Plug>Sneak_s
nmap X <Plug>Sneak_S
xmap x <Plug>Sneak_s
xmap X <Plug>Sneak_S
omap x <Plug>Sneak_s
omap X <Plug>Sneak_S

" Git functions and text objects with vim-fugitive and signify
nmap <silent><leader>gs :Gstatus<CR>
nmap <silent><leader>gp :Gpush<CR>
nmap <silent><leader>gb :Gbrowse<CR>
nmap <silent><leader>gl :Gblame<CR>
nmap <silent><leader>gf :SignifyFold!<CR>
nmap <silent><leader>gu :SignifyHunkUndo<CR>
nmap <silent><leader>gi :SignifyHunkDiff<CR>
omap ic <plug>(signify-motion-inner-pending)
xmap ic <plug>(signify-motion-inner-visual)
omap ac <plug>(signify-motion-outer-pending)
xmap ac <plug>(signify-motion-outer-visual)

" Merge conflict helpers
nmap <silent><leader>ms :Gdiffsplit!<CR>
nmap <silent><leader>ml :diffget //2<CR>
nmap <silent><leader>mr :diffget //3<CR>
vmap <silent><leader>mg :diffget<CR>
vmap <silent><leader>mp :diffput<CR>

" Toggle UndoTree window
nmap <silent><leader>u :MundoToggle<CR>
" Directory tree
nmap <silent><leader>x :CocCommand explorer<CR>
" Ctags and LSP symbol finding
nmap <silent><leader>v :Vista!!<CR>

" HTTP requests - coc-post
nmap <silent><leader>hd :CocCommand post.do<CR>
nmap <silent><leader>hn :CocCommand post.new<CR>
nmap <silent><leader>hl :CocList post<CR>

" Coc List Mappings
nmap <silent><leader>df :Leaderf file<CR>
nmap <silent><leader>dg :Leaderf rg<CR>
nmap <silent><leader>db :Leaderf buffer<CR>
nmap <silent><leader>dt :Leaderf bufTag<CR>
nmap <silent><leader>dh :Leaderf help<CR>
nmap <silent><leader>dw :Leaderf line<CR>

" Language server functions
nmap <silent><leader>ld <Plug>(coc-definition)
nnoremap <silent><leader>lp :call CocAction('jumpDefinition', "OpenPeek")<CR>
nmap <silent><leader>lr <Plug>(coc-rename)
nmap <silent><leader>lf <Plug>(coc-format)
vmap <silent><leader>lf <Plug>(coc-format-selected)
nmap <silent><leader>lt <Plug>(coc-type-definition)
nmap <silent><leader>lx <Plug>(coc-references)
nmap <silent><leader>lg <Plug>(coc-diagnostic-info)
nmap <silent><leader>la <Plug>(coc-codeaction)
nmap <silent><leader>lj <Plug>(coc-float-jump)
nmap <silent><leader>ls :call CocActionAsync("codeLensAction")<CR>
nmap <silent><leader>lk :call CocActionAsync("doHover")<CR>
nmap <silent><leader>lq :call CocActionAsync("quickfixes")<CR>

" Session Management
nmap <silent><leader>sc :CocCommand session.save<CR>
nmap <silent><leader>so :CocCommand session.open<CR>
nmap <silent><leader>sr :CocCommand session.restart<CR>
nmap <silent><leader>sl :CocList sessions<CR>

" Repl Commands
nmap <silent><leader>ro :IronFocus<CR>
nmap <silent><leader>rw :IronWatchCurrentFile
nmap <silent><leader>ru :IronUnwatchCurrentFile<CR>


" Testing functions
nmap <silent><leader>tn :ViTestNearest<CR>
nmap <silent><leader>tf :TestFile<CR>
nmap <silent><leader>tt :TestSuite<CR>
nmap <silent><leader>tl :TestLast<CR>
nmap <silent><leader>tv :TestVisit<CR>
nmap <silent><leader>tm :make test<CR>
nmap <silent><leader>to :!open coverage/index.html<CR>
nmap <silent><leader>ts <Plug>(vitest-run-all)
nmap <silent><leader>tj <Plug>(vitest-next-fail)
nmap <silent><leader>tk <Plug>(vitest-prev-fail)

" LaTex Bindings
nnoremap <silent><leader>tb :CocCommand latex.Build<CR>
nnoremap <silent><leader>tv :CocCommand latex.ForwardSearch<CR>

inoremap <silent><expr> <TAB> pumvisible() ? "\<C-n>" : <SID>isOverWhitespace() ? "\<TAB>" : coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
inoremap <silent><expr> <C-n> pumvisible() ? "\<C-n>" : coc#refresh()

" Use <CR> for confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <silent><expr> <CR> pumvisible() ? coc#_select_confirm() : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
" }}}1
