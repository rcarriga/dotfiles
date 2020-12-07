
local telescope = require("telescope")
local actions = require('telescope.actions')
telescope.setup {
  defaults = {
    vim_buffers_everywhere = true, -- Will most likely be renamed to something more useful
    prompt_position = "top",
    prompt_prefix = " ❯",
    vimgrep_arguments = {
      'rg',
      '--color=never',
      '--no-heading',
      '--with-filename',
      '--line-number',
      '--column',
      '--smart-case'
    },
    selection_strategy = "reset",
    sorting_strategy = "ascending",
    layout_strategy = "horizontal",
    mappings = {
      i = {
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
      }
    }
  },
}

telescope.load_extension('fzy_native')


local dap = require('dap')

dap.adapters.python = {
  type = 'executable';
  command = 'python';
  args = { '-m', 'debugpy.adapter' };
}

dap.configurations.python = {
  {
    type = 'python';
    request = 'launch';
    name = "Launch file";
    program = "${file}";
    --console= "internalConsole"; --one out of "internalConsole", "integratedTerminal", "externalConsole"
    pythonPath = function()
      return 'python'
      --local cwd = vim.fn.getcwd()
      --if vim.fn.executable(cwd .. '/venv/bin/python') then
        --return cwd .. '/venv/bin/python'
      --elseif vim.fn.executable(cwd .. '/.venv/bin/python') then
        --return cwd .. '/.venv/bin/python'
      --else
        --return '/usr/bin/python'
      --end
    end;
  },
}

local dap_python = require('dap-python')

dap_python.setup('python')
dap_python.test_method()
