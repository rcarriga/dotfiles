local M = {}

function M.post()
  local dap = require("dap")

  local dap_python = require("dap-python")

  dap_python.setup(
    "python",
    {
      console = "internalConsole"
    }
  )
  dap_python.test_runner = "pytest"

  dap.adapters.go = {
    type = "executable",
    command = "node",
    args = {os.getenv("HOME") .. "/Dev/repos/vscode-go/dist/debugAdapter.js"}
  }
  -- https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_dap.md
  dap.configurations.go = {
    {
      type = "go",
      name = "Debug",
      request = "launch",
      program = "${file}",
      dlvToolPath = vim.fn.exepath("dlv")
    }
  }

  dap.adapters.node2 = {
    type = "executable",
    command = "node",
    args = {os.getenv("HOME") .. "/Dev/repos/vscode-node-debug2/out/src/nodeDebug.js"}
  }
  dap.configurations.javascript = {
    {
      type = "node2",
      request = "launch",
      cwd = vim.fn.getcwd(),
      protocol = "inspector",
      sourceMaps = false,
      skipFiles= {"<node_internals>/**/*.js"},
      runTimeArgs = {
        "node_modules/.bin/jest",
        "--color=always",
        "--no-coverage",
        "-t",
        "'^Schema with no data doesnt  return any conditional results$'",
        "--",
        "test/index.test.js"
      },
      console = "integratedTerminal"
    }
  }
  dap.configurations.typescript = dap.configurations.javascript
end

return M
