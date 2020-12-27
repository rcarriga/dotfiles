
local telescope = require("telescope")
local actions = require('telescope.actions')
local previewers = require('telescope.previewers')
telescope.setup {
  defaults = {
    file_previewer = previewers.vim_buffer_cat.new,
    grep_previewer = previewers.vim_buffer_vimgrep.new,
    qflist_previewer = previewers.vim_buffer_qflist.new,
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
    layout_config = {
      preview_width = 0.65,
    },
    mappings = {
      i = {
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
        ["<esc>"] = actions.close
      }
    }
  },
}

telescope.load_extension('fzy_native')


local dap = require('dap')

local dap_python = require('dap-python')

dap_python.setup('$HOME/.cache/virtualenv/debugpy/bin/python', {
  include_configs = true
})
