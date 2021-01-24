local M = {}

function M.pre()
    require("util").multilineCommand [[
    ]]
    vim.g["asterisk#keeppos"] = 1
end

return M
