local M = {}

function M.post()
  local neotest = require("neotest")
  local get_env = function()
    local env = {}
    for _, file in ipairs({ ".env" }) do
      if vim.fn.empty(vim.fn.glob(file)) ~= 0 then
        break
      end

      for _, line in ipairs(vim.fn.readfile(file)) do
        for name, value in string.gmatch(line, "(.+)=['\"]?(.*)['\"]?") do
          local str_end = string.sub(value, -1, -1)
          if str_end == "'" or str_end == '"' then
            value = string.sub(value, 1, -2)
          end

          env[name] = value
        end
      end
    end
    return env
  end
  neotest.setup({
    running = {
      concurrent = false,
    },
    status = {
      virtual_text = true,
      signs = false,
    },
    strategies = {
      integrated = {
        width = 180,
      },
    },
    adapters = {
      require("neotest-python")({
        dap = { justMyCode = false, console = "integratedTerminal" },
      }),
      require("neotest-plenary"),
      require("neotest-go"),
      require("neotest-vim-test")({
        allow_file_types = { "ruby", "typescript" },
      }),
    },
    projects = {
      ["/home/ronan/Dev/repos/pydantic"] = {
        discovery = { enabled = false },
      },
    },
  })

  local mappings = {
    ["<leader>nr"] = function()
      neotest.run.run({ vim.fn.expand("%"), env = get_env() })
    end,
    ["<leader>ns"] = function()
      for _, adapter_id in ipairs(neotest.run.adapters()) do
        neotest.run.run({ suite = true, adapter = adapter_id })
      end
    end,
    ["<leader>nw"] = function()
      neotest.watch.watch()
    end,
    ["<leader>nx"] = function()
      neotest.run.stop()
    end,
    ["<leader>nn"] = function()
      neotest.run.run({ env = get_env() })
    end,
    ["<leader>nd"] = function()
      neotest.run.run({ strategy = "dap" })
    end,
    ["<leader>nl"] = neotest.run.run_last,
    ["<leader>nD"] = function()
      neotest.run.run_last({ strategy = "dap" })
    end,
    ["<leader>na"] = neotest.run.attach,
    ["<leader>no"] = function()
      neotest.output.open({ enter = true })
    end,
    ["<leader>nO"] = function()
      neotest.output.open({ enter = true, short = true })
    end,
    ["<leader>np"] = neotest.summary.toggle,
    ["<leader>nm"] = neotest.summary.run_marked,
    ["[n"] = function()
      neotest.jump.prev({ status = "failed" })
    end,
    ["]n"] = function()
      neotest.jump.next({ status = "failed" })
    end,
  }

  for keys, mapping in pairs(mappings) do
    vim.api.nvim_set_keymap("n", keys, "", { callback = mapping, noremap = true })
  end
end

return M
