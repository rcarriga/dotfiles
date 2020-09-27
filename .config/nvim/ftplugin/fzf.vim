" setlocal laststatus=0 noshowmode noruler

if !get(s:, "fzf_init_run")
  tunmap <C-h>
  tunmap <C-j>
  tunmap <C-k>
  tunmap <C-l>
  let s:fzf_init_run = 1
endif
