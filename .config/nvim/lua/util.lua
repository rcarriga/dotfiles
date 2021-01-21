local M = {}

function M.multilineCommand(command)
    for line in vim.gsplit(command, "\n", true) do
        vim.api.nvim_command(line)
    end
end

return M
