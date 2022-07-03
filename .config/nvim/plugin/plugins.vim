let g:cursorhold_updatetime = 100

let g:git_messenger_floating_win_opts = {
   \ 'border': get(g:, "border_chars", "rounded")
   \ }

let g:bufferline = { "closable" : 0  }

let g:Hexokinase_refreshEvents = ["BufRead", "TextChanged", "InsertLeave"]
let g:Hexokinase_optOutPatterns = [ "colour_names" ]

nnoremap <silent> <leader>a :BufferPick<CR>

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
let g:ultest_deprecation_notice = 0
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

let g:floaterm_autoclose = 0
" Open undo tree on right
let g:mundo_right = 1

nmap <leader>s <plug>(SubversiveSubstituteRange)
xmap <leader>s <plug>(SubversiveSubstituteRange)

nmap <leader>ss <plug>(SubversiveSubstituteWordRange)

nmap x <plug>(SubversiveSubstitute)
nmap xx <plug>(SubversiveSubstituteLine)
nmap X <plug>(SubversiveSubstituteToEndOfLine)
nmap <silent><leader>gs :vertical Git \| vertical resize 50 <CR>
nmap <silent><leader>gl :Git blame<CR>

nmap <silent><leader>u :MundoToggle<CR>
