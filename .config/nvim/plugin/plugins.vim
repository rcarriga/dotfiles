" ###################################################################################
" Plugin Settings {{{1
let g:copilot_no_tab_map = 1
let g:copilot_assume_mapped = 1
let g:copilot_filetypes = {
      \ 'dap-repl': v:false,
      \ }

let g:cursorhold_updatetime = 100

let g:git_messenger_floating_win_opts = {
   \ 'border': get(g:, "border_chars", "rounded")
   \ }

let g:bufferline = { "closable" : 0  }

let g:Hexokinase_refreshEvents = ["BufRead", "TextChanged", "InsertLeave"]
let g:Hexokinase_optOutPatterns = [ "colour_names" ]

nnoremap <silent> <leader>a :BufferPick<CR>

let g:twiggy_num_columns = 50

let g:db_ui_auto_execute_table_helpers = 1
let g:db_ui_show_database_icon = 1
let g:db_ui_use_nerd_fonts = 1

let g:conflict_marker_highlight_group = ''

let g:hiPairs_enable_matchParen = 0

" Markdown preview default browser
let g:mkdp_browser = "firefox"
" Don"t open preview window after entering the markdown buffer
let g:mkdp_auto_start = 0
" Auto close current preview window when change
let g:mkdp_auto_close = 0

let test#strategy = "floaterm"
let test#python#runner = "pytest"
let test#javascript#runner = "jest"
let test#go#runner = "gotest"

let g:floaterm_autoclose = 0
" Open undo tree on right
let g:mundo_right = 1

let g:doge_mapping = "\<leader\>i"
let g:doge_mapping_comment_jump_forward = "\<C-\]>"
let g:doge_mapping_comment_jump_backward = "\<C-[\>"
let g:doge_doc_standard_python = "sphinx"

let g:ultest_attach_width = 180
let g:ultest_virtual_text = 0
let g:ultest_output_cols = 120
let g:ultest_max_threads = 4
let g:ultest_use_pty = 1
let g:ultest_pass_sign = " "
let g:ultest_fail_sign = " "
let g:ultest_running_sign = " "
let g:ultest_output_on_run = 0
let g:ultest_output_on_line = 1

" }}}1
" ###################################################################################
" Plugin Mappings {{{1

autocmd FileType dbui nmap <buffer> l <Plug>(DBUI_SelectLine)

nnoremap <silent><leader>x :NvimTreeToggle<CR>

nmap <leader>s <plug>(SubversiveSubstituteRange)
xmap <leader>s <plug>(SubversiveSubstituteRange)

nmap <leader>ss <plug>(SubversiveSubstituteWordRange)

nmap x <plug>(SubversiveSubstitute)
nmap xx <plug>(SubversiveSubstituteLine)
nmap X <plug>(SubversiveSubstituteToEndOfLine)

nmap <silent><leader>e <plug>WinWin

" Distraction free writing
nmap <silent><leader>z :ZenMode<CR>

"Save current buffer
nnoremap <leader>w :w<CR>
nnoremap <silent><leader>q :BufferClose<CR>

nmap <silent><leader>gs :vertical Git \| vertical resize 50 <CR>
nmap <silent><leader>gp :Git push<CR>
nmap <silent><leader>gl :Git blame<CR>
nmap <silent><leader>go :ConflictMarkerOurselves<CR>
nmap <silent><leader>gt :ConflictMarkerThemselves<CR>
nmap <silent><leader>gc :ConflictMarkerBoth<CR>

" Toggle UndoTree window
nmap <silent><leader>u :MundoToggle<CR>

" Fuzzy finding mappings

nnoremap <silent> <M-c> :lua require'dap'.continue()<CR>
nnoremap <silent> <M-right> :lua require'dap'.step_over()<CR>
nnoremap <silent> <M-down> :lua require'dap'.step_into()<CR>
nnoremap <silent> <M-up> :lua require'dap'.step_out()<CR>
nnoremap <silent> <M-x> :lua require'dap'.toggle_breakpoint()<CR>
nnoremap <silent> <M-t> :lua require('dapui').toggle()<CR>
vnoremap <silent> <M-c> <ESC>:lua require('dap-python').debug_selection()<CR>
nnoremap <silent> <M-l> :lua require"osv".launch()<CR>
nnoremap <silent> <M-k> :lua require'dapui'.eval()<cr>
vnoremap <M-k> <Cmd>lua require'dapui'.eval()<cr>
nnoremap <silent> <M-m> :lua require'dapui'.float_element()<cr>
nnoremap <silent> <M-v> :lua require'dapui'.float_element("scopes")<cr>
nnoremap <silent> <M-r> :lua require'dapui'.float_element("repl")<cr>
nnoremap <silent> <M-q> :lua require'dap'.terminate()<cr>

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
nmap <leader>vl <Plug>(ultest-run-last)
nmap <leader>vj <Plug>(ultest-next-fail)
nmap <leader>vk <Plug>(ultest-prev-fail)
nmap <leader>vg <Plug>(ultest-output-jump)
nmap <leader>vo <Plug>(ultest-output-show)
nnoremap <silent><leader>vs :UltestSummary<CR>
nmap <leader>vS <Plug>(ultest-summary-jump)
nmap <leader>va <Plug>(ultest-attach)
nmap <leader>vc <Plug>(ultest-stop-nearest)
nmap <leader>vx <Plug>(ultest-stop-file)
nmap <leader>vd <Plug>(ultest-debug-nearest)

nnoremap <silent><leader>nr <cmd>lua require("neotest").run.run(vim.fn.expand("%"))<CR>
nnoremap <silent><leader>ns <cmd>lua require("neotest").run.run(vim.fn.getcwd())<CR>
nnoremap <silent><leader>nn <cmd>lua require("neotest").run.run()<CR>
nnoremap <silent><leader>nd <cmd>lua require("neotest").run.run({strategy = "dap"})<CR>
nnoremap <silent><leader>na <cmd>lua require("neotest").run.attach()<CR>
nnoremap <silent><leader>no <cmd>lua require("neotest").output.open({ enter = true })<CR>
nnoremap <silent><leader>nO <cmd>lua require("neotest").output.open({enter = true, short = true})<CR>
nnoremap <silent><leader>np <cmd>lua require("neotest").summary.toggle()<CR>

nnoremap <silent><leader>p <cmd>lua require("notify").dismiss()<CR>
