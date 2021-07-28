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
			file_ignore_patterns = { "workbench/.*" },
			selection_strategy = "reset",
			layout_strategy = "horizontal",
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
				override_generic_sorter = true,
				override_file_sorter = true,
				case_mode = "smart_case",
			},
		},
	})

	telescope.load_extension("fzf")
end
return M
