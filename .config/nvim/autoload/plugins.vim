if exists("g:plugins_loaded")

    finish
endif

function! s:AddPlugins(args) abort
  let s:plugins = extend(get(s:, "plugins", {}), a:args)
endfunction


call s:AddPlugins({
    \ "svermeulen/vim-subversive": {},
    \ "norcalli/nvim-colorizer.lua": {},
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
    \ "kkoomen/vim-doge": { "hook_post_source": "call doge#activate()" },
    \ "liuchengxu/vim-which-key": {"lazy": 1, "hook_post_source": "call which_key#register('<Space>', 'g:which_key_map')"},
    \ "machakann/vim-sandwich": {},
    \ "machakann/vim-swap": {},
    \ "mhinz/vim-signify": {},
    \ "moll/vim-bbye": {},
    \ "neoclide/coc.nvim": {"merge": 0, "rev": "release"},
    \ "nvim-treesitter/playground": {"merge": 0},
    \ "rhysd/clever-f.vim": {},
    \ "rhysd/git-messenger.vim": {},
    \ "simnalamburt/vim-mundo": {"lazy":1},
    \ "tomtom/tcomment_vim": {},
    \ "tpope/vim-abolish": {},
    \ "tpope/vim-eunuch": {},
    \ "tpope/vim-fugitive": {},
    \ "tpope/vim-rhubarb": {},
    \ "tpope/vim-unimpaired": {},
    \ "vim-airline/vim-airline": {"lazy": 1, "depends": "vim-airline-themes"},
    \ "vim-airline/vim-airline-themes": {"lazy": 1},
    \ "wellle/targets.vim": {},
    \ "wfxr/minimap.vim": {},
\ })

" Language plugins
call s:AddPlugins({
      \ "MTDL9/vim-log-highlighting": {},
      \ "ekalinin/Dockerfile.vim": {},
      \ "iamcco/markdown-preview.nvim": {"build": "cd app && yarn install" },
      \ "maxmellon/vim-jsx-pretty": {},
      \ "neovimhaskell/haskell-vim": {},
      \ "othree/html5.vim": {},
      \ "posva/vim-vue": {},
      \ "tmhedberg/SimpylFold": {"lazy": 1},
      \ "yuezk/vim-js": {},
\})

let g:plugins_loaded = 1

" ###################################################################################
" Plugin Settings {{{1


let g:conflict_marker_highlight_group = ''

let g:minimap_highlight="CursorLineNr"

let g:hiPairs_enable_matchParen = 0

let g:tcomment_maps = 0

let g:vue_pre_processors = ["typescript", "scss"]

" Disable default windowswap mappings
let g:windowswap_map_keys = 0

" Markdown preview default browser
let g:mkdp_browser = "firefox"
" Don"t open preview window after entering the markdown buffer
let g:mkdp_auto_start = 0
" Auto close current preview window when change
let g:mkdp_auto_close = 0

let g:coc_global_extensions = [ "coc-sh", "coc-yank", "coc-lists", "coc-eslint", "coc-json", "coc-python", "coc-snippets", "coc-docker", "coc-css", "coc-html", "coc-tsserver", "coc-yaml", "coc-word", "coc-vimlsp" ]

" Set GoYo width
let g:goyo_width = 100
let g:goyo_linenr = 1

let g:coc_snippet_next = "<tab>"

if has("nvim")
    " Use terminal windows for running tests
    let test#strategy = "neovim"
    let test#python#pytest#options = "--disable-warnings"
endif

" Open undo tree on right
let g:mundo_right = 1

" Pretty icons for airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#show_splits = 1
let g:airline#extensions#hunks#non_zero_only = 1
let g:airline_theme = "molokai"
let g:airline#extensions#coc#error_symbol = " "
let g:airline#extensions#coc#warning_symbol = " "
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#vista#enabled = 0

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

let g:which_key_position = 'topleft'
let g:which_key_max_size = 20
let g:which_key_floating_opts = { "col": "+30"}

let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.9 } }
let g:fzf_preview_window = 'right:60%'

" }}}1
" ###################################################################################
" Functions {{{1
"
function AirlineInit()
  let g:airline_section_y = airline#section#create_right(['filetype'])
  let g:airline_section_z = airline#section#create([
              \ '%#__accent_bold#%3l%#__restore__#/%L', ' ',
              \ '%#__accent_bold#%3v%#__restore__#/%3{virtcol("$") - 1}',
              \ ])
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

" }}}1
" ###################################################################################
" Autocommands {{{1

augroup NvimAuCommands
  au!
  au TextYankPost * silent! lua vim.highlight.on_yank {on_visual=false}
augroup END

augroup AirlineInit
  au!
  autocmd User AirlineAfterInit call AirlineInit()
augroup EN

augroup WhichKeyInit
  au!
  au  FileType which_key setlocal laststatus=0 noshowmode noruler
    \| au BufLeave <buffer> set laststatus=2 showmode ruler
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
    " au CursorHold * try | silent call CocActionAsync("highlight") | catch /.*/ | endtry
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

command! -nargs=* -bang RG call RipgrepFzf(<q-args>, <bang>0)

command! CC CocCommand

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
nnoremap <silent><leader>q :Bdelete<CR>

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
      \ "b": "Browse",
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
nmap <silent><leader>gs :Gstatus<CR>
nmap <silent><leader>gp :Gpush<CR>
nmap <silent><leader>gb :Gbrowse<CR>
nmap <silent><leader>gl :Gblame<CR>
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

let g:which_key_map.m = "Code Minimap"
nmap <silent><leader>m :MinimapToggle<CR>

" Toggle UndoTree window
let g:which_key_map.u = "Undo Tree"
nmap <silent><leader>u :MundoToggle<CR>
" Directory tree
let g:which_key_map.x = "File Explorer"
nmap <silent><leader>x :CocCommand explorer<CR>
nnoremap <silent>` :CocCommand explorer ~/.config/nvim<CR>
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
  \ "c": "Colorschemes",
  \ }
nmap <silent><leader>df :Files<CR>
nmap <silent><leader>dg :Rg<CR>
nmap <silent><leader>db :Buffer<CR>
nmap <silent><leader>dt :BTags<CR>
nmap <silent><leader>dh :Helptags<CR>
nmap <silent><leader>dw :Lines<CR>
nmap <silent><leader>dc :Colors<CR>

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
vmap <silent><leader>lf <Plug>(coc-format-selected)
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
