if exists("g:plugins_loaded")

    finish
endif

function! s:AddPlugins(args) abort
  let s:plugins = extend(get(s:, "plugins", {}), a:args)
endfunction

call s:AddPlugins({
    \ "glepnir/galaxyline.nvim": {},
    \ "romgrk/barbar.nvim": {},
    \ "AndrewRadev/splitjoin.vim": {},
    \ "nvim-telescope/telescope-dap.nvim": {},
    \ "nvim-lua/popup.nvim": {},
    \ "nvim-lua/plenary.nvim": {},
    \ "mfussenegger/nvim-dap-python": {},
    \ "nvim-telescope/telescope.nvim": {},
    \ "nvim-telescope/telescope-fzy-native.nvim": {},
    \ "rrethy/vim-hexokinase": { "build": "make hexokinase" },
    \ "mfussenegger/nvim-dap": {},
    \ "theHamsta/nvim-dap-virtual-text": {},
    \ "kyazdani42/nvim-web-devicons": {},
    \ "sodapopcan/vim-twiggy": {},
    \ "tpope/vim-dadbod": {},
    \ "kristijanhusak/vim-dadbod-ui": {},
    \ "ajorgensen/vim-markdown-toc": {},
    \ "rcarriga/vim-ultest": {},
    \ "svermeulen/vim-subversive": {},
    \ "voldikss/vim-floaterm": {"lazy": 1},
    \ "rhysd/conflict-marker.vim": {},
    \ "Konfekt/FastFold": {},
    \ "Yggdroot/hiPairs": {},
    \ "dstein64/vim-win": {},
    \ "godlygeek/tabular": {},
    \ "honza/vim-snippets": {},
    \ "janko/vim-test": {"lazy": 1},
    \ "junegunn/fzf": {},
    \ "junegunn/fzf.vim": {},
    \ "junegunn/goyo.vim": {"on_cmd": "Goyo"},
    \ "kkoomen/vim-doge": { "rev": "v3.1.1" },
    \ "liuchengxu/vim-which-key": {"lazy": 1, "hook_post_source": "call which_key#register('<Space>', 'g:which_key_map')"},
    \ "machakann/vim-sandwich": {},
    \ "mhinz/vim-signify": {},
    \ "neoclide/coc.nvim": {"merge": 0, "rev": "master", "build": "yarn install --frozen-lockfile"},
    \ "nvim-treesitter/playground": {"merge": 0},
    \ "nvim-treesitter/nvim-treesitter-textobjects": {},
    \ "nvim-treesitter/nvim-treesitter-refactor": {},
    \ "rhysd/clever-f.vim": {},
    \ "rhysd/git-messenger.vim": {},
    \ "simnalamburt/vim-mundo": {"lazy":1},
    \ "tomtom/tcomment_vim": {},
    \ "tpope/vim-abolish": {},
    \ "tpope/vim-eunuch": {},
    \ "tpope/vim-fugitive": {},
    \ "tpope/vim-rhubarb": {},
    \ "tpope/vim-unimpaired": {},
    \ "wellle/targets.vim": {},
\ })

" Language plugins
call s:AddPlugins({
      \ "MTDL9/vim-log-highlighting": {},
      \ "ekalinin/Dockerfile.vim": {},
      \ "iamcco/markdown-preview.nvim": {"build": "cd app && yarn install" },
      \ "neovimhaskell/haskell-vim": {},
      \ "posva/vim-vue": {},
\})

let g:plugins_loaded = 1

" ###################################################################################
" Plugin Settings {{{1

set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()

let bufferline = {}
let bufferline.closable = v:false

let g:Hexokinase_refreshEvents = ["BufRead", "TextChanged", "InsertLeave"]

let g:dap_virtual_text = "all_frames"

let g:twiggy_num_columns = 50

let g:db_ui_auto_execute_table_helpers = 1
let g:db_ui_show_database_icon = 1
let g:db_ui_use_nerd_fonts = 1

let g:conflict_marker_highlight_group = ''

let g:hiPairs_enable_matchParen = 0


let g:vue_pre_processors = ["typescript", "scss"]

" Disable default windowswap mappings
let g:windowswap_map_keys = 0

" Markdown preview default browser
let g:mkdp_browser = "firefox"
" Don"t open preview window after entering the markdown buffer
let g:mkdp_auto_start = 0
" Auto close current preview window when change
let g:mkdp_auto_close = 0

let g:coc_global_extensions = [ "coc-sh", "coc-yank", "coc-lists", "coc-json", "coc-snippets", "coc-docker", "coc-css", "coc-tsserver", "coc-yaml", "coc-word", "coc-vimlsp" ]

" Set GoYo width
let g:goyo_width = 100
let g:goyo_linenr = 1

let g:coc_snippet_next = "<tab>"

let test#strategy = "floaterm"
let test#python#pytest#options = "--disable-warnings --color=yes"
let test#javascript#jest#options = "--color=always"

" Open undo tree on right
let g:mundo_right = 1


let g:doge_mapping = "\<leader\>i"
let g:doge_mapping_comment_jump_forward = "\<C-\]>"
let g:doge_mapping_comment_jump_backward = "\<C-[\>"
let g:doge_doc_standard_python = "sphinx"

let g:ultest_virtual_text = 1

let g:signify_sign_add               = "\u258B"
let g:signify_sign_delete            = "\u258B"
let g:signify_sign_delete_first_line = "\u258B"
let g:signify_sign_change            = "\u258B"

let g:which_key_position = 'topleft'
let g:which_key_max_size = 20
let g:which_key_floating_opts = { "col": "+30"}

let g:fzf_layout = { 'window': { 'width': 0.7, 'height': 0.5, 'yoffset': 0.1, 'highlight': 'Operator' } }
let g:fzf_preview_window = 'right:60%'

" }}}1
" ###################################################################################
" Functions {{{1

function! GetTestResults() abort
    return get(b:, "ultest_total") ?
                \ get(b:,"ultest_passed")." Pass ".get(b:, "ultest_failed")." Fail" : ""
endfunction


function! s:isOverWhitespace() abort
  let col = col(".") - 1
  return !col || getline(".")[col - 1]  =~# "\s"
endfunction

function! RipgrepFzf(query, fullscreen)
  let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case -- %s || true'
  let initial_command = printf(command_fmt, shellescape(a:query))
  let reload_command = printf(command_fmt, '{q}')
  let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
  call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
endfunction

function! CleanNoNameEmptyBuffers()
    let buffers = filter(range(1, bufnr('$')), 'buflisted(v:val) && empty(bufname(v:val)) && bufwinnr(v:val) < 0 && (getbufline(v:val, 1, "$") == [""])')
    if !empty(buffers)
        exe 'BufferClose '.join(buffers, ' ')
    endif
endfunction

" }}}1
" ###################################################################################
" Autocommands {{{1

augroup CleanEmpty
  au!
  au BufEnter * call CleanNoNameEmptyBuffers()
augroup END

augroup NvimAuCommands
  au!
  au TextYankPost * silent! lua vim.highlight.on_yank {on_visual=false}
augroup END

augroup WhichKeyInit
  au!
  au  FileType which_key setlocal laststatus=0 noshowmode noruler
    \| au BufLeave <buffer> set laststatus=2 showmode ruler
augroup END

augroup CocSetup
    " Show function signatures when calling function
    au!
    au User CocJumpPlaceholder call CocActionAsync("showSignatureHelp")
    " au CursorHold * try | silent call CocActionAsync("highlight") | catch /.*/ | endtry
augroup END
" }}}1
" ###################################################################################
" Custom Commands {{{1

command! -nargs=* -bang RG call RipgrepFzf(<q-args>, <bang>0)

command! CC CocCommand

" }}}1
" ###################################################################################
" Plugin Mappings {{{1
let g:which_key_map = {}

autocmd FileType dbui nmap <buffer> l <Plug>(DBUI_SelectLine)

let g:which_key_map.a = "Switch Buffer"
nnoremap <silent> <leader>a :BufferPick<CR>

" Doge Mapping
let g:which_key_map.i = "Generate Documentation"

let g:which_key_map.c = {"name": "Comments",
      \ "c": "Comment Line",
      \ "s": "Comment as Block"
      \ }

let g:which_key_map.x = {"name": "Substitute Text",
      \ "s": "Substitute within range",
      \ "ss": "Substitute word within range"
      \ }
nmap <leader>s <plug>(SubversiveSubstituteRange)
xmap <leader>s <plug>(SubversiveSubstituteRange)

nmap <leader>ss <plug>(SubversiveSubstituteWordRange)

nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>
vnoremap <silent> <leader> :<c-u>WhichKeyVisual '<Space>'<CR>
nnoremap <localleader> :<c-u>WhichKey  '\'<CR>
vnoremap <localleader> :<c-u>WhichKeyVisual  '\'<CR>

nmap x <plug>(SubversiveSubstitute)
nmap xx <plug>(SubversiveSubstituteLine)
nmap X <plug>(SubversiveSubstituteToEndOfLine)

let g:which_key_map.e = "Manage Windows"
nmap <silent><leader>e <plug>WinWin

" Distraction free writing
let g:which_key_map.z = "Zen Mode"
nmap <silent><leader>z :Goyo<CR>

"Save current buffer
let g:which_key_map.w = "Write File"
nnoremap <leader>w :w<CR>
let g:which_key_map.q = "Quit Buffer"
nnoremap <silent><leader>q :BufferClose<CR>

"Cycle between last two open buffers
" let g:which_key_map["<Space>"] = "Switch to Previous Buffer"
" nnoremap <leader><leader> <c-^>
let g:which_key_map.n = "Compile to PDF"
nnoremap <silent><leader>n :exec "silent !pandoc"expand("%")" -o /tmp/pandoc.pdf && (pkill zathura;  zathura /tmp/pandoc.pdf) &"<CR>

" Open config directory

let g:which_key_map.g = {
      \ "name": "Git Control",
      \ "s": "Status",
      \ "p": "Push",
      \ "b": "Branch Management",
      \ "l": "Blame",
      \ "f": "Fold Around Changes",
      \ "u": "Revert Hunk",
      \ "i": "Show Hunk Changes",
      \ "m": "Line History",
      \ "o": "Keep our changes from conflict",
      \ "t": "Keep their changes from conflict",
      \ "c": "Keep both changes from conflict"
      \ }
" Git functions and text objects with vim-fugitive and signify
nmap <silent><leader>gs :vertical Git \| vertical resize 50 <CR>
nmap <silent><leader>gp :Git push<CR>
nmap <silent><leader>gb :Twiggy<CR>
nmap <silent><leader>gl :Git blame<CR>
nmap <silent><leader>gf :SignifyFold!<CR>
nmap <silent><leader>gu :SignifyHunkUndo<CR>
nmap <silent><leader>gi :SignifyHunkDiff<CR>
nmap <silent><leader>go :ConflictMarkerOurselves<CR>
nmap <silent><leader>gt :ConflictMarkerThemselves<CR>
nmap <silent><leader>gc :ConflictMarkerBoth<CR>
omap ic <plug>(signify-motion-inner-pending)
xmap ic <plug>(signify-motion-inner-visual)
omap ac <plug>(signify-motion-outer-pending)
xmap ac <plug>(signify-motion-outer-visual)

" Toggle UndoTree window
let g:which_key_map.u = "Undo Tree"
nmap <silent><leader>u :MundoToggle<CR>
" Directory tree
let g:which_key_map.x = "File Explorer"
nmap <silent><leader>x :CocCommand explorer<CR>

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
  \ "c": "Colorschemes",
  \ }
nmap <silent><leader>df :Files<CR>
nmap <silent><leader>dg :Rg<CR>
nmap <silent><leader>db :Buffers<CR>
nmap <silent><leader>dt :Telescope treesitter<CR>
nmap <silent><leader>dh :Telescope help_tags<CR>

let g:which_key_map.b = {
  \ "name": "Debugger",
  \ "c": "Start/Continue",
  \ "s": "Step Over",
  \ "i": "Step Into",
  \ "o": "Step Out",
  \ "b": "Toggle Breakpoint",
  \ "B": "Conditional Breakpoint",
  \ "l": "Log point",
  \ "r": "Open REPL",
  \ "g": "Repeat last run",
  \ "t": "Run test method (python only)"
  \ }
nnoremap <silent> <leader>bc :lua require'dap'.continue()<CR>
nnoremap <silent> <leader>bs :lua require'dap'.step_over()<CR>
nnoremap <silent> <leader>bi :lua require'dap'.step_into()<CR>
nnoremap <silent> <leader>bo :lua require'dap'.step_out()<CR>
nnoremap <silent> <leader>bb :lua require'dap'.toggle_breakpoint()<CR>
nnoremap <silent> <leader>bB :lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>
nnoremap <silent> <leader>bl :lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>
nnoremap <silent> <leader>br :vertical lua require'dap'.repl.open()<CR>
nnoremap <silent> <leader>bg :lua require'dap'.repl.run_last()<CR>
nnoremap <silent> <leader>bt :lua require('dap-python').test_method()<CR>
vnoremap <silent> <leader>b <ESC>:lua require('dap-python').debug_selection()<CR>

inoremap <expr> <c-x><c-f> fzf#vim#complete#path('rg --files --hidden')
inoremap <expr> <c-x><c-k> fzf#vim#complete#word({'window': { 'width': 0.2, 'height': 0.9, 'xoffset': 1 }})
inoremap <expr> <c-x><c-l> fzf#vim#complete#line()

nnoremap <silent><leader>f :FloatermToggle<CR>
tnoremap <silent> <C-a> <C-\><C-n>:FloatermNew<CR>
tnoremap <silent> <C-x> <C-\><C-n>:FloatermToggle<CR>
tnoremap <silent> <C-n> <C-\><C-n>:FloatermNext<CR>
tnoremap <silent> <C-p> <C-\><C-n>:FloatermPrev<CR>

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
  \ "n": "Jump to next diagnostic",
  \ "b": "Jump to previous diagnostic"
  \ }
nmap <silent><leader>ld <Plug>(coc-definition)
nmap <silent><leader>lr <Plug>(coc-rename)
nmap <silent><leader>lf <Plug>(coc-format)
xmap <silent><leader>lf :call CocActionAsync("formatSelected", "line")<CR>
nmap <silent><leader>lt <Plug>(coc-type-definition)
nmap <silent><leader>lx <Plug>(coc-references)
nmap <silent><leader>lg <Plug>(coc-diagnostic-info)
nmap <silent><leader>la <Plug>(coc-codeaction)
nmap <silent><leader>lj <Plug>(coc-float-jump)
nmap <silent><leader>ln <Plug>(coc-diagnostic-next)
nmap <silent><leader>lb <Plug>(coc-diagnostic-prev)
nmap <silent><leader>ls :call CocActionAsync("codeLensAction")<CR>
nmap <silent><leader>lk :call CocActionAsync("doHover")<CR>
nmap <silent><leader>lq :call CocActionAsync("quickfixes")<CR>

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
nmap <silent><leader>tn :TestNearest<CR>
nmap <silent><leader>tf :TestFile<CR>
nmap <silent><leader>tt :TestSuite<CR>
nmap <silent><leader>tl :TestLast<CR>
nmap <silent><leader>tv :TestVisit<CR>
nmap <silent><leader>tm :make test<CR>
nmap <silent><leader>to :!firefox coverage/index.html<CR>
nmap <leader>vf <Plug>(ultest-run-file)
nmap <leader>vn <Plug>(ultest-run-nearest)
nmap <leader>vj <Plug>(ultest-next-fail)
nmap <leader>vk <Plug>(ultest-prev-fail)
nmap <leader>vg <Plug>(ultest-output-jump)
nmap <leader>vo <Plug>(ultest-output-show)

" inoremap <silent><expr> <TAB> pumvisible() ? "\<C-n>" : <SID>isOverWhitespace() ? "\<TAB>" : coc#refresh()
inoremap <silent><expr> <TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
inoremap <silent><expr> <C-n> pumvisible() ? "\<C-n>" : coc#refresh()

" Use <CR> for confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <silent><expr> <CR> pumvisible() ? coc#_select_confirm() : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
" inoremap <expr><silent> <CR> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\:call pear_tree#insert_mode#PrepareExpansion()<CR>"



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

    for [plugin, options] in items(s:plugins)
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
"
lua require("plugins")
lua require("statusline")
call sy#start_all()
