local M = {}

function M.post()
  local dap = require("dap")
  local dapui = require("dapui")
  dapui.setup({
    layouts = {
      {
        elements = {
          "scopes",
          "breakpoints",
          "stacks",
          "watches",
        },
        size = 80,
        position = "left",
      },
      {
        elements = { "repl" },
        size = 10,
        position = "bottom",
      },
      {
        elements = {
          "console",
        },
        size = 60,
        position = "right",
      },
    },
    floating = { max_width = 0.9, max_height = 0.5, border = vim.g.border_chars },
  })

  local mappings = {
    ["<M-c>"] = dap.continue,
    ["<M-right>"] = dap.step_over,
    ["<M-down>"] = dap.step_into,
    ["<M-up>"] = dap.step_out,
    ["<M-x>"] = dap.toggle_breakpoint,
    ["<M-t>"] = dapui.toggle,
    ["<M-k>"] = dapui.eval,
    ["<M-m>"] = dapui.float_element,
    ["<M-v>"] = function() dapui.float_element("scopes") end,
    ["<M-r>"] = function() dapui.float_element("repl") end,
    ["<M-q>"] = dap.terminate,
  }
  for keys, mapping in pairs(mappings) do
    vim.api.nvim_set_keymap("n", keys, "", { callback = mapping, noremap = true })
  end

  vim.api.nvim_set_keymap("v", "<M-k>", "", { callback = dapui.eval })
  vim.fn.sign_define("DapBreakpoint", { text = "→", texthl = "Error", linehl = "", numhl = "" })
  vim.fn.sign_define("DapStopped", { text = "→", texthl = "Success", linehl = "", numhl = "" })

  dap.set_log_level("DEBUG")
  local dap_python = require("dap-python")

  dap_python.setup("~/.cache/virtualenvs/debugpy/bin/python", { include_configs = false })
  dap.configurations.python = {
    {
      type = "python",
      request = "launch",
      name = "Launch file",
      justMyCode = false,
      program = "${file}",
      console = "integratedTerminal",
      pythonPath = require("util").get_python_path(),
    },
    {
      type = "python",
      request = "attach",
      name = "Attach remote",
      justMyCode = false,
      pythonPath = require("util").get_python_path(),
      host = function()
        local value = vim.fn.input("Host [127.0.0.1]: ")
        if value ~= "" then
          return value
        end
        return "127.0.0.1"
      end,
      port = function()
        return tonumber(vim.fn.input("Port [5678]: ")) or 5678
      end,
    },
  }

  dap_python.test_runner = "pytest"

  dap.configurations.lua = {
    {
      type = "nlua",
      request = "attach",
      name = "Attach to running Neovim instance",
      host = function()
        local value = vim.fn.input("Host [127.0.0.1]: ")
        if value ~= "" then
          return value
        end
        return "127.0.0.1"
      end,
      port = function()
        local val = tonumber(vim.fn.input("Port: "))
        assert(val, "Please provide a port number")
        return val
      end,
    },
  }

  dap.adapters.nlua = function(callback, config)
    callback({ type = "server", host = config.host, port = config.port })
  end

  dap.adapters.go = {
    type = "executable",
    command = "node",
    args = { os.getenv("HOME") .. "/Dev/repos/vscode-go/dist/debugAdapter.js" },
  }
  -- https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_dap.md
  dap.configurations.go = {
    {
      type = "go",
      name = "Debug",
      request = "launch",
      program = "${file}",
      dlvToolPath = vim.fn.exepath("dlv"),
    },
  }

  dap.adapters.node2 = {
    type = "executable",
    command = "node",
    args = {
      os.getenv("HOME") .. "/Dev/repos/vscode-node-debug2/out/src/nodeDebug.js",
    },
  }
  dap.configurations.javascript = {
    {
      type = "node2",
      request = "launch",
      program = "${workspaceFolder}/${file}",
      cwd = vim.fn.getcwd(),
      sourceMaps = true,
      protocol = "inspector",
      console = "integratedTerminal",
    },
  }
  dap.configurations.typescript = dap.configurations.javascript

  dap.adapters.lldb = {
    type = "executable",
    attach = { pidProperty = "pid", pidSelect = "ask" },
    command = "lldb-vscode",
    env = { LLDB_LAUNCH_FLAG_LAUNCH_IN_TTY = "YES" },
  }

  dap.adapters.rust = dap.adapters.lldb
  dap.adapters.cpp = dap.adapters.lldb

  dap.configurations.cpp = {
    {
      name = "Launch",
      type = "lldb",
      request = "launch",
      program = function()
        return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
      end,
      cwd = "${workspaceFolder}",
      stopOnEntry = false,
      args = {},
    },
  }

  dap.configurations.rust = {
    {
      type = "rust",
      request = "launch",
      name = "lldb",
      program = function()
        local metadata_json = vim.fn.system("cargo metadata --format-version 1 --no-deps")
        local metadata = vim.fn.json_decode(metadata_json)
        local target_name = metadata.packages[1].targets[1].name
        local target_dir = metadata.target_directory
        return target_dir .. "/debug/" .. target_name
      end,
    },
  }

  dap.configurations.c = {
    {
      -- If you get an "Operation not permitted" error using this, try disabling YAMA:
      --  echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope
      name = "Attach to process",
      type = "lldb", -- Adjust this to match your adapter name (`dap.adapters.<name>`)
      request = "attach",
      pid = require("dap.utils").pick_process,
      args = {},
    },
  }
end

return M
