local M = {}
---@class OpenWin
---@field id number
---@field opts table

---@type table<number, OpenWin>
local open_wins = {}

local notifications = {}

vim.notify = function(message)
	notifications[#notifications + 1] = { text = message, index = #notifications }

	local buf = vim.api.nvim_create_buf(false, true)
	vim.api.nvim_buf_set_lines(buf, 0, -1, false, { message })

	local opts = {
		relative = "editor",
		anchor = "NE",
		width = math.min(#message, 50),
		height = math.ceil(#message / 50),
		col = vim.opt.columns:get() - 3,
		row = 3,
		border = vim.g.border_chars,
		style = "minimal",
	}
	local win = vim.api.nvim_open_win(buf, false, opts)

	local index = #open_wins + 1
	open_wins[index] = { id = win, opts = opts }

	vim.api.nvim_win_set_option(win, "wrap", true)

	vim.defer_fn(function()
		vim.api.nvim_win_close(win, true)
		table.remove(open_wins, index)
	end, 5000)
end

function M.telescope(opts)
	local previewers = require("telescope.previewers")
	local entry_display = require("telescope.pickers.entry_display")
	local conf = require("telescope.config").values

	local displayer = entry_display.create({
		separator = "▏",
		items = {
			{ width = 8 },
			{ remaining = true },
		},
	})

	local make_display = function(entry)
		local text = entry.value
		local level = entry.level or "info"

		return displayer({
			level,
			text,
		})
	end
	return require("telescope.pickers").new(opts, {
		finder = require("telescope.finders").new_table({
			results = notifications,
			entry_maker = function(entry)
				return {
					valid = true,
					value = entry.text,
					ordinal = tostring(entry.index),
					display = make_display,
				}
			end,
		}),
		previewer = previewers.new_termopen_previewer({
			title = "Notificiations",
			get_command = function(entry)
				return { "echo", entry.value }
			end,
		}),
		sorter = conf.generic_sorter(opts),
	}):find()
end

return M
