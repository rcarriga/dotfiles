local M = {}

function M.post()
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
    },
  })

  telescope.load_extension("fzf")

  local builtin = require("telescope.builtin")
  local finders = require("telescope.finders")
  local make_entry = require("telescope.make_entry")
  local pickers = require("telescope.pickers")
  local utils = require("telescope.utils")
  local conf = require("telescope.config").values

  local yadm_files = function(opts)
    opts = opts or {}
    if opts.is_bare then
      utils.notify("builtin.git_files", {
        msg = "This operation must be run in a work tree",
        level = "ERROR",
      })
      return
    end

    local show_untracked = utils.get_default(opts.show_untracked, false)
    local recurse_submodules = utils.get_default(opts.recurse_submodules, false)
    if show_untracked and recurse_submodules then
      utils.notify("builtin.git_files", {
        msg = "Git does not support both --others and --recurse-submodules",
        level = "ERROR",
      })
      return
    end

    -- By creating the entry maker after the cwd options,
    -- we ensure the maker uses the cwd options when being created.
    opts.entry_maker = vim.F.if_nil(opts.entry_maker, make_entry.gen_from_file(opts))
    local git_command = vim.F.if_nil(
      opts.git_command,
      { "yadm", "ls-files", "--exclude-standard", "--cached" }
    )

    pickers.new(opts, {
      prompt_title = "Git Files",
      finder = finders.new_oneshot_job(
        vim.tbl_flatten({
          git_command,
          show_untracked and "--others" or nil,
          recurse_submodules and "--recurse-submodules" or nil,
        }),
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
