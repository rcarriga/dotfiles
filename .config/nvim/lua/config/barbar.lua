local M = {}

function M.pre()
    vim.g.bufferline = {
        closable = false
    }
    vim.cmd "nnoremap <silent> <leader>a :BufferPick<CR>"
end

return M
