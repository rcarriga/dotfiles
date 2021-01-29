" ###################################################################################
" Plugin Settings {{{1
let g:nvim_tree_git_hl = 1
let g:nvim_tree_indent_markers = 1
let g:nvim_tree_follow = 1
let g:nvim_tree_auto_close = 1

map *  <Plug>(asterisk-z*)<Plug>(is-nohl-1)
map g* <Plug>(asterisk-gz*)<Plug>(is-nohl-1)
map #  <Plug>(asterisk-z#)<Plug>(is-nohl-1)
map g# <Plug>(asterisk-gz#)<Plug>(is-nohl-1)

let g:dap_virtual_text = "all_frames"

let g:bufferline = { "closable" : 0  }

let g:Hexokinase_refreshEvents = ["BufRead", "TextChanged", "InsertLeave"]
let g:Hexokinase_optOutPatterns = [ "colour_names" ]

nnoremap <silent> <leader>a :BufferPick<CR>

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


" Set GoYo width
let g:goyo_width = 100
let g:goyo_linenr = 1


let test#strategy = "floaterm"
let test#python#pytest#options = "--disable-warnings --color=yes"
let test#javascript#jest#options = "--color=always"

" Open undo tree on right
let g:mundo_right = 1


let g:doge_mapping = "\<leader\>i"
let g:doge_mapping_comment_jump_forward = "\<C-\]>"
let g:doge_mapping_comment_jump_backward = "\<C-[\>"
let g:doge_doc_standard_python = "sphinx"

let g:ultest_virtual_text = 0

let g:signify_sign_add               = "\u258B"
let g:signify_sign_delete            = "\u258B"
let g:signify_sign_delete_first_line = "\u258B"
let g:signify_sign_change            = "\u258B"

let g:fzf_layout = { 'window': { 'width': 0.7, 'height': 0.5, 'yoffset': 0.1, 'highlight': 'FloatermBorder' } }
let g:fzf_preview_window = 'right:60%'
command! Conf Files ~/.config/nvim
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

" }}}1
" ###################################################################################
" Custom Commands {{{1

command! -nargs=* -bang RG call RipgrepFzf(<q-args>, <bang>0)


" }}}1
" ###################################################################################
" Plugin Mappings {{{1

autocmd FileType dbui nmap <buffer> l <Plug>(DBUI_SelectLine)

nmap <silent><leader>lf :Format<CR>

nnoremap <silent><leader>x :NvimTreeToggle<CR>

inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm('<CR>')
inoremap <silent><expr> <C-e>     compe#close('<C-e>')


nmap <leader>s <plug>(SubversiveSubstituteRange)
xmap <leader>s <plug>(SubversiveSubstituteRange)

nmap <leader>ss <plug>(SubversiveSubstituteWordRange)

nmap x <plug>(SubversiveSubstitute)
nmap xx <plug>(SubversiveSubstituteLine)
nmap X <plug>(SubversiveSubstituteToEndOfLine)

nmap <silent><leader>e <plug>WinWin

" Distraction free writing
nmap <silent><leader>z :Goyo<CR>

"Save current buffer
nnoremap <leader>w :w<CR>
nnoremap <silent><leader>q :BufferClose<CR>

"Cycle between last two open buffers
nnoremap <silent><leader>n :exec "silent !pandoc"expand("%")" -o /tmp/pandoc.pdf && (pkill zathura;  zathura /tmp/pandoc.pdf) &"<CR>

" Open config directory

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
nmap <silent><leader>u :MundoToggle<CR>

" Fuzzy finding mappings
nmap <silent><leader>df :Files<CR>
nmap <silent><leader>dg :Rg<CR>
nmap <silent><leader>db :Buffers<CR>
nmap <silent><leader>dt :Telescope treesitter<CR>
nmap <silent><leader>dh :Telescope help_tags<CR>
nmap <silent><leader>dc :Conf<CR>

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
nmap <leader>vs <Plug>(ultest-open-summary)
