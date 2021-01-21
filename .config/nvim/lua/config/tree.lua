local M = {}

function M.pre()
    vim.g.nvim_tree_git_hl = 1
    vim.g.nvim_tree_indent_markers = 1
    vim.g.nvim_tree_follow = 1
    vim.g.nvim_tree_auto_close = 1
end

return M
