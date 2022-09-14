local M = {}

function M.post()
  -- Need to install following python requirements
  -- pynvim jupyter_client ueberzug Pillow cairosvg pnglatex plotly

  vim.cmd([[
    nnoremap <silent><expr> <LocalLeader>r  :MagmaEvaluateOperator<CR>
    nnoremap <silent>       <LocalLeader>rr :MagmaEvaluateLine<CR>
    xnoremap <silent>       <LocalLeader>r  :<C-u>MagmaEvaluateVisual<CR>
    nnoremap <silent>       <LocalLeader>rc :MagmaReevaluateCell<CR>
    nnoremap <silent>       <LocalLeader>rd :MagmaDelete<CR>
    nnoremap <silent>       <LocalLeader>ro :MagmaShowOutput<CR>
  ]])
  vim.g.magma_automatically_open_output = true
  vim.g.magma_image_provider = "ueberzug"
end

return M
