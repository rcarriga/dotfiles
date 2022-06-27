local M = {}

function M.post()
  -- Something is causing telescope to enter insert mode when picking entry
  vim.api.nvim_create_autocmd("BufEnter", {
    callback = function()
      if vim.api.nvim_buf_get_option(0, "filetype") ~= "" then
        vim.cmd("stopinsert")
      end
    end,
  })
  local telescope = require("telescope")
  local actions = require("telescope.actions")
  local previewers = require("telescope.previewers")
  telescope.setup({
    defaults = {
      file_previewer = previewers.vim_buffer_cat.new,
      grep_previewer = previewers.vim_buffer_vimgrep.new,
      qflist_previewer = previewers.vim_buffer_qflist.new,
      prompt_prefix = " ❯ ",
      find_command = {
        "fd",
        "--type",
        "f",
        "--no-ignore",
        "--color=never",
        "--hidden",
        "--follow",
      },
      vimgrep_arguments = {
        "rg",
        "--color=never",
        "--no-heading",
        "--with-filename",
        "--line-number",
        "--column",
        "--smart-case",
        "--hidden",
      },
      file_ignore_patterns = {
        "workbench/.*",
        ".git/.*",
        ".venv/.*",
        "*.png",
        "*.jpg",
        "node_modules",
      },
      selection_strategy = "reset",
      sorting_strategy = "ascending",
      layout_strategy = "horizontal",
      layout_config = {
        prompt_position = "top",
      },
      mappings = {
        i = {
          ["<C-j>"] = actions.move_selection_next,
          ["<C-k>"] = actions.move_selection_previous,
          ["<esc>"] = actions.close,
        },
        n = {
          ["q"] = actions.close,
        },
      },
    },
    extensions = {
      fzf = {
        fuzzy = true,
        override_generic_sorter = true,
        override_file_sorter = true,
        case_mode = "smart_case",
      },
      ["ui-select"] = {
        require("telescope.themes").get_dropdown({}),
      },
    },
  })

  telescope.load_extension("fzf")
  telescope.load_extension("ui-select")
  telescope.load_extension("yaml_schema")

  local builtin = require("telescope.builtin")
  local finders = require("telescope.finders")
  local make_entry = require("telescope.make_entry")
  local pickers = require("telescope.pickers")
  local conf = require("telescope.config").values

  local yadm_files = function(opts)
    opts = opts or {}
    opts.cwd = vim.env.HOME
    opts.entry_maker = make_entry.gen_from_file(opts)

    pickers.new(opts, {
      prompt_title = "Yadm Files",
      finder = finders.new_oneshot_job(
        { "yadm", "ls-files", "--exclude-standard", "--cached" },
        opts
      ),
      previewer = conf.file_previewer(opts),
      sorter = conf.file_sorter(opts),
    }):find()
  end

  local keys = {
    ["<leader>df"] = { builtin.find_files },
    ["<leader>dg"] = { builtin.grep_string, { search = "", debounce = 30 } },
    ["<leader>dG"] = { builtin.live_grep },
    ["<leader>db"] = { builtin.buffers },
    ["<leader>dt"] = { builtin.treesitter },
    ["<leader>dh"] = { builtin.help_tags },
    ["<leader>dc"] = { yadm_files },
  }

  for key, map in pairs(keys) do
    local callback, args = unpack(map)
    vim.api.nvim_set_keymap("n", key, "", {
      callback = function()
        callback(args)
      end,
      noremap = true,
      silent = true,
    })
  end
end

return M
