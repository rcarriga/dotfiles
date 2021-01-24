local M = {}

function M.multilineCommand(command)
    for line in vim.gsplit(command, "\n", true) do
        vim.cmd(vim.trim(line))
    end
end

return M
