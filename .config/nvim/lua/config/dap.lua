local M = {}

function M.post()
  local dap = require("dap")

  local dapui = require("dapui")
  dapui.setup({
    force_buffers = false,
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
      sort_variables = function(a, b)
        return a.name < b.name
      end,
    },
    floating = { max_width = 0.9, max_height = 0.5, border = vim.g.border_chars },
  })

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
    ["<M-w>"] = dapui.elements.watches.add,
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

  dap_python.setup(
    vim.fn.stdpath("data") .. "/mason/packages/debugpy/venv/bin/python",
    { include_configs = false }
  )

  dap.configurations.python = {
    {
      type = "python",
      request = "launch",
      name = "Launch File",
      justMyCode = false,
      cwd = vim.fn.getcwd(),
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
      name = "Attach Remote",
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
end

return M
