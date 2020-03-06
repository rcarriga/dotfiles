if exists("g:plugins_loaded")
    finish
endif


let plugins = {
      \ "segeljakt/vim-isotope": {},
      \ "tmhedberg/SimpylFold": {"lazy": 1},
      \ "tomtom/tcomment_vim": {},
      \ "moll/vim-bbye": {},
      \ "dstein64/vim-win": {},
      \ "tpope/vim-rhubarb": {},
      \ "Konfekt/FastFold": {},
      \ "kkoomen/vim-doge": { "hook_post_source": "call doge#activate()" },
      \ "Yggdroot/LeaderF": {"build": "./install.sh"},
      \ "Yggdroot/hiPairs": {},
      \ "alvan/vim-closetag": {},
      \ "godlygeek/tabular": {},
      \ "honza/vim-snippets": {},
      \ "iamcco/markdown-preview.nvim": {"on_ft": ["markdown", "pandoc.markdown", "rmd"], "build": "cd app & yarn install" },
      \ "janko/vim-test": {"lazy": 1},
      \ "junegunn/goyo.vim": {"on_cmd": "Goyo"},
      \ "junegunn/gv.vim": {},
      \ "justinmk/vim-sneak": {},
      \ "lervag/vimtex": {"lazy": 1},
      \ "liuchengxu/vim-which-key": {"lazy": 1, "hook_post_source": "call which_key#register('<Space>', 'g:which_key_map')"},
      \ "liuchengxu/vista.vim": {"on_cmd": "Vista"},
      \ "machakann/vim-sandwich": {},
      \ "machakann/vim-swap": {},
      \ "mhinz/vim-signify": {},
      \ "neoclide/coc.nvim": {"merge": 0, "rev": "release"},
      \ "rhysd/clever-f.vim": {},
      \ "rhysd/vim-grammarous": {"on_cmd": "GrammarousCheck"},
      \ "sheerun/vim-polyglot": {"depends": "tabular"},
      \ "simnalamburt/vim-mundo": {"lazy":1},
      \ "takac/vim-hardtime": {},
      \ "tpope/vim-abolish": {},
      \ "tpope/vim-eunuch": {},
      \ "tpope/vim-fugitive": {},
      \ "tpope/vim-sleuth": {"hook_post_source": "Sleuth"},
      \ "vim-airline/vim-airline": {"lazy": 1, "depends": "vim-airline-themes"},
      \ "vim-airline/vim-airline-themes": {"lazy": 1},
      \ "tpope/vim-unimpaired": {},
      \ "vim-pandoc/vim-pandoc": {},
      \ "vim-pandoc/vim-pandoc-syntax": {},
      \ "vim-scripts/ReplaceWithRegister": {},
      \ "w0rp/ale": {"lazy": 1},
      \ "wellle/targets.vim": {},
      \ "whiteinge/diffconflicts": {"on_cmd" : "DiffConflicts" },
\ }

if !has("nvim")
  let plugins = extend(plugins,
      \ {"roxma/nvim-yarp": {},
      \ "roxma/vim-hug-neovim-rpc": {}})
else
  let plugins = extend(plugins,
      \ {"Vigemus/iron.nvim": {},
      \ "numirias/semshi": {}})
endif

let g:plugins_loaded = 1

" ###################################################################################
" Plugin Settings {{{1
"
let g:hiPairs_enable_matchParen = 1

let g:tcomment_maps = 0

let g:vue_pre_processors = ["typescript"]

let g:caw_operator_keymappings = 1

" Disable default windowswap mappings
let g:windowswap_map_keys = 0

let g:coc_config_home = trim(system("echo $HOME"))."/.vim"

let g:vimtex_quickfix_enabled = 0
let g:vimtex_compiler_progname = "nvr"
let g:vimtex_view_method = "zathura"

" Store compiled latex files in "build" dir
let g:vimtex_compiler_latexmk = {"build_dir": "build"}

" Vim hardtime keys
let g:list_of_normal_keys = ["h", "j", "k", "l", "-", "+"]

let g:pandoc#folding#fdc = 0

let g:polyglot_disabled = ["latex"]

" Markdown preview default browser
let g:mkdp_browser = "firefox"
" Don"t open preview window after entering the markdown buffer
let g:mkdp_auto_start = 0
" Auto close current preview window when change
let g:mkdp_auto_close = 0

let g:coc_global_extensions = [ "coc-sh", "coc-yank", "coc-lists", "coc-eslint", "coc-json", "coc-python", "coc-snippets", "coc-docker", "coc-css", "coc-highlight", "coc-html", "coc-tsserver", "coc-yaml", "coc-word", "coc-vimlsp" ]

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
let g:vista_echo_cursor_strategy = "floating_win"
let g:vista_executive_for = {
  \ "javascript": "coc",
  \ "typescript": "coc",
  \ "javascriptreact": "coc",
  \ "typescriptreact": "coc"
  \ }

" Pretty icons for airline
let g:airline_powerline_fonts = 1
" Use manual loading of extensions
let g:airline#extensions#tabline#show_splits = 1
let g:airline#extensions#hunks#non_zero_only = 1
let g:airline_theme = "molokai"
let g:airline#extensions#tmuxline#enabled = 1
let g:airline#extensions#coc#error_symbol = " "
let g:airline#extensions#coc#warning_symbol = " "
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#enabled = 1

let g:ale_virtualtext_cursor = 1
let g:ale_linters = {
    \ "vue": [],
    \ "python": [],
    \ "haskell": [],
    \ "javascript": [],
    \ "typescript": [],
    \ "typescriptreact": [],
    \ "javascriptreact": [],
    \ "ruby": ["rubocop"],
    \ "r": []
  \ }

" Disable thesauras default mappings
let g:tq_map_keys = 0

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

let g:closetag_xhtml_filenames = '*.xhtml,*.jsx,*.tsx'
let g:closetag_xhtml_filetypes = 'xhtml,jsx,tsx,typescriptreact'
let g:closetag_filetypes = 'jsx,tsx,typescriptreact'
let g:closetag_emptyTags_caseSensitive = 1
let g:closetag_regions = {
    \ 'typescriptreact': 'jsxRegion,tsxRegion',
    \ 'javascriptreact': 'jsxRegion',
    \ }

" Ignore files for  fuzzy searching
let g:Lf_WildIgnore = {
        \ 'dir': [".env", "__pycache__", ".mypy_cache", ".stack"],
        \ 'file': []
        \}
let g:Lf_WindowPosition = 'popup'
let g:Lf_PreviewInPopup = 1
let g:Lf_StlSeparator = { 'left': '', 'right': '' }
let g:Lf_PopupWidth = &columns * 1/2
let g:Lf_ShortcutB = ""
let g:Lf_ShortcutF = ""
let g:Lf_AutoResize = 1

let g:pandoc#modules#disabled = ["formatting", "command", "menu", "keyboard", "bibliographies", "completion", "toc", "spell", "hypertext"]

let g:which_key_position = 'topleft'
let g:which_key_max_size = 20
let g:which_key_floating_opts = { "col": "+30"}

let g:spaceline_seperate_style= "curve"
let g:spaceline_colorscheme = "space"
" }}}1
" ###################################################################################
" Functions {{{1
"
function! s:isOverWhitespace() abort
  let col = col(".") - 1
  return !col || getline(".")[col - 1]  =~# "\s"
endfunction

function! ReactSetup() abort
    " Due to lazy loading some plugins in polyglot need to force started.
    let ft = &filetype
    set filetype=html
    exec "set filetype=".ft
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

function! RipgrepFzf(query, fullscreen)
  let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case %s || true'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q}')
  let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
endfunction

" }}}1
" ###################################################################################
" Autocommands {{{1

augroup WhichKeyInit
  au!
  au  FileType which_key set laststatus=0 noshowmode noruler
    \| au BufLeave <buffer> set laststatus=2 showmode ruler
augroup END
augroup SleuthInit
  au!
  au FileType * Sleuth
augroup END

augroup ReactInit
    au!
    au Filetype javascriptreact,typescriptreact ++once call ReactSetup()
augroup END

augroup SemshiInit
    au!
    au CursorMoved python ++once Semshi
augroup END

augroup AirlineSetup
    au!
    au CursorMoved * ++once AirlineRefresh
augroup END

augroup CocSetup
    " Show function signatures when calling function
    au!
    au User CocJumpPlaceholder call CocActionAsync("showSignatureHelp")
    au CursorHold * try | silent call CocActionAsync("highlight") | catch /.*/ | endtry
augroup END

if exists(":ViTest")
  augroup ViTestStatusRunner
      au!
      au BufWritePost * ViTest
  augroup END
endif
" }}}1
" ###################################################################################
" Custom Commands {{{1
command! -bang -nargs=? -complete=dir Files call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)
command! -bang -nargs=? Rg call fzf#vim#grep('rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1, fzf#vim#with_preview(), <bang>0)
command! -bang -nargs=? RG call RipgrepFzf(<q-args>, <bang>0)

command! -nargs=1 OpenPrevious call OpenFileInPreviousWindow(<f-args>)
command! -nargs=1 OpenPeek call OpenInFloating(<f-args>)
command CC CocCommand

" }}}1
" ###################################################################################
" Plugin Mappings {{{1
let g:which_key_map = {}

" Doge Mapping
let g:which_key_map.i = "Generate Documentation"

let g:which_key_map.c = {"name": "Comments",
      \ "c": "Comment Line",
      \ "b": "Comment as Block"
      \ }
nmap <silent><leader>cc :TComment<CR>
vmap <silent><leader>cc :TComment<CR>
nmap <silent><leader>cb :TCommentBlock<CR>
vmap <silent><leader>cb :TCommentBlock<CR>

nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>
vnoremap <silent> <leader> :<c-u>WhichKeyVisual '<Space>'<CR>
nnoremap <localleader> :<c-u>WhichKey  '\'<CR>
vnoremap <localleader> :<c-u>WhichKeyVisual  '\'<CR>

let g:which_key_map.e = "Manage Windows"
nmap <silent><leader>e <plug>WinWin

"Replace the word under cursor
let g:which_key_map.o = "Replace Current Word"
nmap <leader>o :%s/\<<c-r><c-w>\>//g<left><left>

" Distraction free writing
let g:which_key_map.z = "Zen Mode"
nmap <silent><leader>z :Goyo<CR>

"Save current buffer
let g:which_key_map.w = "Write File"
nnoremap <leader>w :w<CR>
let g:which_key_map.q = "Quit Buffer"
nnoremap <silent><leader>q :Bdelete<CR>

"Cycle between last two open buffers
let g:which_key_map["<Space>"] = "Switch to Previous Buffer"
nnoremap <leader><leader> <c-^>
let g:which_key_map.n = "Compile to PDF"
nnoremap <silent><leader>n :exec "silent !pandoc"expand("%")" -o /tmp/pandoc.pdf && (pkill zathura;  zathura /tmp/pandoc.pdf) &"<CR>


" Open config directory
nnoremap <silent>` :CocCommand explorer ~/.vim<CR>

" Vim sneak commands
nmap x <Plug>Sneak_s
nmap X <Plug>Sneak_S
xmap x <Plug>Sneak_s
xmap X <Plug>Sneak_S
omap x <Plug>Sneak_s
omap X <Plug>Sneak_S


let g:which_key_map.g = {
      \ "name": "Git Control",
      \ "s": "Status",
      \ "p": "Push",
      \ "b": "Browse",
      \ "l": "Blame",
      \ "f": "Fold Around Changes",
      \ "u": "Revert Hunk",
      \ "i": "Show Hunk Changes",
      \ }
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

let g:which_key_map.m = {
  \ "name": "Merge Conflicts",
  \ "s": "Start Diff Merge",
  \ "l": "Get Left Version",
  \ "r": "Get Right Version"
  \ }
" Merge conflict helpers
nmap <silent><leader>ms :Gdiffsplit!<CR>
nmap <silent><leader>ml :diffget //2<CR>
nmap <silent><leader>mr :diffget //3<CR>
vmap <silent><leader>mg :diffget<CR>
vmap <silent><leader>mp :diffput<CR>

" Toggle UndoTree window
let g:which_key_map.u = "Undo Tree"
nmap <silent><leader>u :MundoToggle<CR>
" Directory tree
let g:which_key_map.x = "File Explorer"
nmap <silent><leader>x :CocCommand explorer<CR>
" Ctags and LSP symbol finding
let g:which_key_map.v = "Tags"
nmap <silent><leader>v :Vista!!<CR>

" HTTP requests - coc-post
let g:which_key_map.h = {
      \ "name": "HTTP Requests",
      \ "d": "Send Current Request",
      \ "n": "Open New Request",
      \ "l": "View Saved Requests",
      \ }
nmap <silent><leader>hd :CocCommand post.do<CR>
nmap <silent><leader>hn :CocCommand post.new<CR>
nmap <silent><leader>hl :CocList post<CR>

" Fuzzy finding mappings
let g:which_key_map.d = {
  \ "name": "Fuzzy Finder",
  \ "f": "File Names",
  \ "g": "Grep",
  \ "b": "Buffers",
  \ "t": "Buffer Tags",
  \ "h": "Help Tags",
  \ "w": "Current File",
  \ "u": "Functions",
  \ "c": "Colorschemes",
  \ }
nmap <silent><leader>df :Leaderf file<CR>
nmap <silent><leader>dg :Leaderf rg<CR>
nmap <silent><leader>db :Leaderf buffer<CR>
nmap <silent><leader>dt :Leaderf bufTag<CR>
nmap <silent><leader>dh :Leaderf help<CR>
nmap <silent><leader>dw :Leaderf line<CR>
nmap <silent><leader>du :Leaderf function<CR>
nmap <silent><leader>dc :Leaderf colorscheme<CR>

" Language server functions
let g:which_key_map.l = {
  \ "name": "LSP",
  \ "d": "Go to Definition",
  \ "p": "Peek Definition",
  \ "r": "Rename Symbol",
  \ "f": "Format Text",
  \ "t": "Type Definition",
  \ "x": "References",
  \ "g": "Diagnostics Info",
  \ "a": "Show Code Actions",
  \ "j": "Jump to Floating Window",
  \ "s": "Code Lens Action",
  \ "k": "Hover Information",
  \ "q": "Quickfixes",
  \ }
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

let g:which_key_map.r = {
  \ "name": "REPL Control",
  \ "o": "Open REPL",
  \ "w": "Run Command on Write",
  \ "u": "Stop Watching File"
  \ }
" Repl Commands
nmap <silent><leader>ro :IronFocus<CR>
nmap <silent><leader>rw :IronWatchCurrentFile
nmap <silent><leader>ru :IronUnwatchCurrentFile<CR>


let g:which_key_map.b = {
      \ "name": "Debugging Controls",
      \ "g": "Start Debugger",
      \ "c": "Continue",
      \ "s": "Stop",
      \ "r": "Restart",
      \ "p": "Pause",
      \ "b": "ToggleBreakpoint",
      \ "f": "AddFunctionBreakpoint",
      \ "o": "StepOver",
      \ "i": "StepInto",
      \ "x": "StepOut"
      \ }
nmap <silent><leader>bg :call vimspector#Launch()<CR>
nmap <silent><leader>bc <Plug>VimspectorContinue
nmap <silent><leader>bs <Plug>VimspectorStop
nmap <silent><leader>br <Plug>VimspectorRestart
nmap <silent><leader>bp <Plug>VimspectorPause
nmap <silent><leader>bb <Plug>VimspectorToggleBreakpoint
nmap <silent><leader>bf <Plug>VimspectorAddFunctionBreakpoint
nmap <silent><leader>bo <Plug>VimspectorStepOver
nmap <silent><leader>bi <Plug>VimspectorStepInto
nmap <silent><leader>bx <Plug>VimspectorStepOut



let g:which_key_map.t = {
  \ "name": "Tests",
  \ "n": "Run Nearest",
  \ "f": "Run File",
  \ "t": "Run All Tests",
  \ "l": "Repeat Most Recent",
  \ "v": "Open Last Test",
  \ "m": "Run Make 'test 'Rule",
  \ "o": "Open Test Coverage Report",
  \ "s": "Update Status of All in File",
  \ "j": "Jump to Next Failure",
  \ "k": "Jump to Previous Failure"
  \ }
" Testing functions
nmap <silent><leader>tn :ViTestNearest<CR>
nmap <silent><leader>tf :TestFile<CR>
nmap <silent><leader>tt :TestSuite<CR>
nmap <silent><leader>tl :TestLast<CR>
nmap <silent><leader>tv :TestVisit<CR>
nmap <silent><leader>tm :make test<CR>
nmap <silent><leader>to :!firefox coverage/index.html<CR>
nmap <silent><leader>ts <Plug>(vitest-run-all)
nmap <silent><leader>tj <Plug>(vitest-next-fail)
nmap <silent><leader>tk <Plug>(vitest-prev-fail)

" inoremap <silent><expr> <TAB> pumvisible() ? "\<C-n>" : <SID>isOverWhitespace() ? "\<TAB>" : coc#refresh()
inoremap <silent><expr> <TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
inoremap <silent><expr> <C-n> pumvisible() ? "\<C-n>" : coc#refresh()

" Use <CR> for confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <silent><expr> <CR> pumvisible() ? coc#_select_confirm() : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
" }}}1
" ###################################################################################
" Install Plugins {{{1

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
" NeoVim Specific {{{1
if has("nvim")

lua << EOF
local iron = require("iron")

iron.core.set_config{
  repl_open_cmd = "vsplit"
}

EOF
nnoremap cl cl

endif

" }}}1
if get(g:, 'started_by_firenvim')
  redir! > ~/testecho
  mes
  redir END
endif
