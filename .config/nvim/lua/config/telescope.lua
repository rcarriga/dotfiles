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
        "--no-ignore-vcs",
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
      ["zf-native"] = {
        -- options for sorting file-like items
        file = {
          -- override default telescope file sorter
          enable = true,

          -- highlight matching text in results
          highlight_results = true,

          -- enable zf filename match priority
          match_filename = true,
        },

        -- options for sorting all other items
        generic = {
          -- override default telescope generic item sorter
          enable = true,

          -- highlight matching text in results
          highlight_results = true,

          -- disable zf filename match priority
          match_filename = false,
        },
      },
      fzf = {
        fuzzy = true,
        override_generic_sorter = true,
        override_file_sorter = false,
        case_mode = "smart_case",
      },
    },
  })

  telescope.load_extension("fzf")
  telescope.load_extension("zf-native")

  vim.ui.select = function(items, opts, on_choice)
    local themes = require("telescope.themes")
    local actions = require("telescope.actions")
    local state = require("telescope.actions.state")
    local pickers = require("telescope.pickers")
    local finders = require("telescope.finders")
    local conf = require("telescope.config").values

    local entry_maker = function(item)
      local formatted = opts.format_item and opts.format_item(item) or item
      return {
        display = formatted,
        ordinal = formatted,
        value = item,
      }
    end

    local picker_opts = themes.get_dropdown({
      previewer = false,
    })
    pickers.new(picker_opts, {
      prompt_title = opts.prompt,
      finder = finders.new_table({
        results = items,
        entry_maker = entry_maker,
      }),
      sorter = conf.generic_sorter(opts),
      attach_mappings = function(prompt_bufnr)
        actions.select_default:replace(function()
          local selection = state.get_selected_entry()
          actions._close(prompt_bufnr, false)
          if not selection then
            -- User did not select anything.
            on_choice(nil, nil)
            return
          end
          local idx = nil
          for i, item in ipairs(items) do
            if item == selection.value then
              idx = i
              break
            end
          end
          on_choice(selection.value, idx)
        end)

        actions.close:replace(function()
          actions._close(prompt_bufnr, false)
          on_choice(nil, nil)
        end)

        return true
      end,
    }):find()
  end
end
return M
