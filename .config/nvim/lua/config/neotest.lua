local M = {}

function M.post()
  _G.NEOTEST_LOADED = true
  local neotest = require("neotest")
  local lib = require("neotest.lib")
  local get_env = function()
    local env = {}
    local file = ".env"
    if not lib.files.exists(file) then
      return {}
    end

    for _, line in ipairs(vim.fn.readfile(file)) do
      for name, value in string.gmatch(line, "(%S+)=['\"]?(.*)['\"]?") do
        local str_end = string.sub(value, -1, -1)
        if str_end == "'" or str_end == '"' then
          value = string.sub(value, 1, -2)
        end

        env[name] = value
      end
    end
    return env
  end
  neotest.setup({
    log_level = vim.log.levels.DEBUG,
    quickfix = {
      open = false,
    },
    status = {
      virtual_text = true,
      signs = true,
    },
    output = {
      open_on_run = false,

    },
    icons = {
      running_animated = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" },
    },
    strategies = {
      integrated = {
        width = 180,
      },
    },
    adapters = {
      require("neotest-python")({
        dap = { justMyCode = false, console = "integratedTerminal", subProcess = false },
        pytest_discovery = true,
      }),
      require("neotest-plenary"),
    },
  })

  local group = vim.api.nvim_create_augroup("NeotestConfig", {})
  for _, ft in ipairs({ "output", "attach", "summary" }) do
    vim.api.nvim_create_autocmd("FileType", {
      pattern = "neotest-" .. ft,
      group = group,
      callback = function(opts)
        vim.keymap.set("n", "q", function()
          pcall(vim.api.nvim_win_close, 0, true)
        end, {
          buffer = opts.buf,
        })
      end,
    })
  end

  vim.api.nvim_create_autocmd("FileType", {
    pattern = "neotest-output-panel",
    group = group,
    callback = function()
      vim.cmd("norm G")
    end,
  })

  local mappings = {
    ["<leader>nr"] = function()
      neotest.run.run({ vim.fn.expand("%:p"), env = get_env() })
    end,
    ["<leader>ns"] = function()
      for _, adapter_id in ipairs(neotest.state.adapter_ids()) do
        neotest.run.run({ suite = true, adapter = adapter_id, env = get_env() })
      end
    end,
    ["<leader>nx"] = function()
      neotest.run.stop()
    end,
    ["<leader>nn"] = function()
      neotest.run.run({ env = get_env() })
    end,
    ["<leader>nd"] = function()
      neotest.run.run({ strategy = "dap", env = get_env() })
    end,
    ["<leader>nl"] = neotest.run.run_last,
    ["<leader>nD"] = function()
      neotest.run.run_last({ strategy = "dap" })
    end,
    ["<leader>na"] = neotest.run.attach,
    ["<leader>no"] = function()
      neotest.output.open({ enter = true, last_run = true })
    end,
    ["<leader>ni"] = function()
      neotest.output.open({ enter = true })
    end,
    ["<leader>nO"] = function()
      neotest.output.open({ enter = true, short = true })
    end,
    ["<leader>np"] = neotest.summary.toggle,
    ["<leader>nm"] = neotest.summary.run_marked,
    ["<leader>ne"] = neotest.output_panel.toggle,
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
