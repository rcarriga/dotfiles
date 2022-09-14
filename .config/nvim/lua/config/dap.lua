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
        elements = { "repl", "console" },
        size = 0.25,
        position = "bottom",
      },
    },
    render = {
      max_value_lines = 3,
    },
    floating = { max_width = 0.9, max_height = 0.5, border = vim.g.border_chars },
  })
  dap.set_log_level("DEBUG")

  local mappings = {
    ["<M-c>"] = dap.continue,
    ["<M-right>"] = dap.step_over,
    ["<M-down>"] = dap.step_into,
    ["<M-up>"] = dap.step_out,
    ["<M-x>"] = dap.toggle_breakpoint,
    ["<M-t>"] = function()
      dapui.toggle({ reset = true })
    end,
    ["<M-k>"] = dapui.eval,
    ["<M-m>"] = dapui.float_element,
    ["<M-v>"] = function()
      dapui.float_element("scopes")
    end,
    ["<M-r>"] = function()
      dapui.float_element("repl")
    end,
    ["<M-q>"] = dap.terminate,
  }
  for keys, mapping in pairs(mappings) do
    vim.api.nvim_set_keymap("n", keys, "", { callback = mapping, noremap = true })
  end

  vim.api.nvim_set_keymap("v", "<M-k>", "", { callback = dapui.eval })
  vim.fn.sign_define("DapBreakpoint", { text = "→", texthl = "Error", linehl = "", numhl = "" })
  vim.fn.sign_define("DapStopped", { text = "→", texthl = "Success", linehl = "", numhl = "" })

  local dap_python = require("dap-python")

  local mason_registry = require("mason-registry")

  dap_python.setup(
    mason_registry.get_package("debugpy"):get_install_path() .. "/venv/bin/python",
    { include_configs = false }
  )

  dap.adapters.nlua = function(callback, config)
    callback({ type = "server", host = config.host, port = config.port })
  end

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
      request = "launch",
      name = "Launch Module",
      justMyCode = false,
      module = function()
        return string.gsub(vim.fn.expand("%:.:r"), "/", ".")
      end,
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

  dap.adapters.lldb = {
    type = "executable",
    attach = { pidProperty = "pid", pidSelect = "ask" },
    command = "lldb-vscode",
    env = { LLDB_LAUNCH_FLAG_LAUNCH_IN_TTY = "YES" },
  }

  dap.adapters.rust = dap.adapters.lldb

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
end

return M
