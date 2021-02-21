local M = {}

function M.post()
  local dap = require("dap")

  local dap_python = require("dap-python")

  dap_python.setup("$HOME/.cache/virtualenvs/debugpy/bin/python", {
    console = 'internalConsole'
  })
  dap_python.test_runner = "pytest"
end

return M
