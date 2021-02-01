local M = {}

function M.post()
    local dap = require("dap")

    local dap_python = require("dap-python")

    dap_python.setup(
        "$HOME/.cache/virtualenvs/debugpy/bin/python",
        {
            include_configs = true
        }
    )
end

return M
