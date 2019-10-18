if exists("g:plugins_loaded")
    finish
endif
let g:plugins_loaded = 1
" ###################################################################################
" Install Plugins
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
let g:dein#install_progress_type = "none"
if dein#load_state("~/.cache/dein")
  call dein#begin("~/.cache/dein")
    call dein#add("KabbAmine/vCoolor.vim")
    call dein#add("Konfekt/FastFold")
    call dein#add("Ron89/thesaurus_query.vim", {"on_ft": ["tex", "markdown"]})
    call dein#add("alvan/vim-closetag", {"on_ft": "html"})
    call dein#add("godlygeek/tabular")
    call dein#add("honza/vim-snippets")
    call dein#add("iamcco/markdown-preview.nvim", {"on_ft": ["markdown", "pandoc.markdown", "rmd"], "build": "cd app & yarn install" })
    call dein#add("jamessan/vim-gnupg")
    call dein#add("janko/vim-test", {"lazy": 1})
    call dein#add("junegunn/goyo.vim", {"on_cmd": "Goyo"})
    call dein#add("junegunn/vim-easy-align", {"on_ft": "markdown"})
    call dein#add("junegunn/vim-peekaboo")
    call dein#add("justinmk/vim-sneak")
    call dein#add("kkoomen/vim-doge",{"lazy":1})
    call dein#add("kshenoy/vim-signature")
    call dein#add("lervag/vimtex", {"on_ft": "tex"})
    call dein#add("liuchengxu/vista.vim", {"on_cmd": "Vista"})
    call dein#add("machakann/vim-sandwich")
    call dein#add("machakann/vim-swap")
    call dein#add("mhinz/vim-signify")
    call dein#add("neoclide/coc.nvim", {"merge": 0, "rev": "release"})
    call dein#add("numirias/semshi")
    call dein#add("rhysd/clever-f.vim")
    call dein#add("rhysd/reply.vim")
    call dein#add("rhysd/vim-grammarous", {"on_cmd": "GrammarousCheck"})
    call dein#add("scrooloose/nerdcommenter", )
    call dein#add("sheerun/vim-polyglot", {"depends": "tabular"})
    call dein#add("simnalamburt/vim-mundo",{"lazy":1})
    call dein#add("tpope/vim-abolish")
    call dein#add("tpope/vim-endwise")
    call dein#add("tpope/vim-eunuch")
    call dein#add("tpope/vim-fugitive")
    call dein#add("tpope/vim-sleuth", {"hook_post_source": "Sleuth"})
    call dein#add("tpope/vim-unimpaired")
    call dein#add("vim-airline/vim-airline", {"lazy": 1, "depends": "vim-airline-themes"})
    call dein#add("vim-airline/vim-airline-themes")
    call dein#add("vim-scripts/ReplaceWithRegister")
    call dein#add("w0rp/ale", {"lazy": 1})
    call dein#add("wellle/targets.vim")
    call dein#add("whiteinge/diffconflicts", {"on_cmd" : "DiffConflicts" })
    if !has("nvim")
        call dein#add("roxma/nvim-yarp")
        call dein#add("roxma/vim-hug-neovim-rpc")
    endif
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

" ###################################################################################
" Functions

function! s:isOverWhitespace() abort
  let col = col(".") - 1
  return !col || getline(".")[col - 1]  =~# "\s"
endfunction

function! AirlineSections() abort
    let g:airline_section_x = airline#section#create(["readonly","%{dein#get_progress()}", "readonly","%{GetTestResults()}"])
    let g:airline_section_b =  airline#section#create(["%{get(g:, 'coc_git_status', ' ')}", "%{get(b:, 'coc_git_status', ' ')}"])
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
    let out_buffer = bufnr(filename) > -1 ? bufnr(filename) : bufadd(filename)
    call bufload(out_buffer)
    let height = nvim_buf_line_count(out_buffer)
    if height > 1
        let window_params = {"relative": "cursor", "width": 100, "height": height < 20 ? height : 20, "row": 1, "col": 1, "anchor": "SW", "style": "minimal"}
        let created_window = nvim_open_win(out_buffer, v:true, window_params)
        exec "call ".curs
        exec "au WinLeave * ++once call nvim_win_close(".created_window.", v:true)"
    endif
endfunction

" ###################################################################################
" Plugin Settings

" Markdown preview default browser
let g:mkdp_browser = "firefox"
" Don"t open preview window after entering the markdown buffer
let g:mkdp_auto_start = 0
" Auto close current preview window when change
let g:mkdp_auto_close = 1
let g:vimtex_compiler_progname = "nvr" 

let g:coc_global_extensions = [ "coc-explorer", "coc-sh", "coc-gitignore", "coc-yank", "coc-lists", "coc-eslint", "coc-json", "coc-post", "coc-python", "coc-snippets", "coc-docker", "coc-css", "coc-highlight", "coc-html", "coc-tsserver", "coc-yaml", "coc-word", "coc-vimlsp" ]

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
let g:vista_icon_indent = ["╰─▸ ", "├─▸ "]
let g:vista#renderer#enable_icon = 1
let g:vista_sidebar_width = 50

" Pretty icons for airline
let g:airline_powerline_fonts = 1
" Use manual loading of extensions
let g:airline#extensions#disable_rtp_load = 1
let g:airline_extensions= ["coc", "vimtex"]
let g:airline#extensions#hunks#non_zero_only = 1
let g:airline_theme = "molokai"
let airline#extensions#coc#error_symbol = " "
let airline#extensions#coc#warning_symbol = " "
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
" ###################################################################################
" Autocommands


augroup AirlineInit
    au!
    au CursorMoved * ++once AirlineRefresh
augroup END

augroup SemshiInit
    au!
    au CursorMoved python ++ once Semshi
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

" augroup HardModeConfig
"     au!
"     autocmd VimEnter,BufNewFile,BufReadPost * silent! call HardMode()
" augroup END
" augroup ViTestStatusRunner
"     au!
"     au BufWritePost * ViTest
" augroup END

" ###################################################################################
" Custom Commands

command! -nargs=1 OpenPrevious call OpenFileInPreviousWindow(<f-args>)
command! -nargs=1 OpenPeek call OpenInFloating(<f-args>)

" ###################################################################################
" Plugin Mappings

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
nmap <silent><leader>mg :diffget<CR>
nmap <silent><leader>mp :diffput<CR>
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
nmap <silent><leader>ro :Repl<CR>
nmap <silent><leader>rc :ReplStop<CR>
nmap <silent><leader>rs :ReplSend<CR>
vmap <silent><leader>rs :ReplSend<CR>

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

" Who doesn"t like a good thesauras
nmap <leader>ot :ThesaurusQueryReplaceCurrentWord<CR>
" Some lovely grammar checking
nmap <leader>og :GrammarousCheck<CR>

inoremap <silent><expr> <C-n> pumvisible() ? "\<C-n>" : coc#refresh()
inoremap <silent><expr> <TAB> pumvisible() ? "\<C-n>" : <SID>isOverWhitespace() ? "\<TAB>" : coc#refresh()

inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
" Use <CR> for confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <silent><expr> <CR> pumvisible() ? coc#_select_confirm() : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
