local M = {}

function M.multilineCommand(command)
	for line in vim.gsplit(command, "\n", true) do
		vim.cmd(vim.trim(line))
	end
end

function M.lua_map(args)
	local opts = { noremap = true, silent = true }
	if args.bufnr then
		vim.api.nvim_buf_set_keymap(
			args.bufnr,
			args.mode or "n",
			args.keys,
			"<cmd>lua " .. args.mapping .. "<CR>",
			opts
		)
	else
		vim.api.nvim_set_keymap(args.mode or "n", args.keys, "<cmd>lua " .. args.mapping .. "<CR>", opts)
	end
end

return M
