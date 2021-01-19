
vim.g.completion_sorting = "none"
vim.g.completion_enable_auto_paren = 1
vim.g.completion_enable_snippet = "vim-vsnip"
vim.g.completion_chain_complete_list = {
  default = {
    { complete_items = { 'lsp', "snippet", "buffers", "tags", "path" } },
    { mode = { '<c-p>' } },
    { mode = { '<c-n>' } }
  },
}
