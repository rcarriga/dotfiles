local M = {}

function M.lua_map(args)
  local opts = {noremap = true, silent = true}
  vim.api.nvim_buf_set_keymap(args.bufnr, args.mode or "n", args.keys, "<cmd>lua " .. args.mapping .. "<CR>", opts)
end

function M.line_diagnostics(client_id)
  local win =
    vim.lsp.diagnostic.show_line_diagnostics({border = "single"}, vim.fn.bufnr(), vim.fn.line(".") - 1, client_id)
end

local function make_prompt(opts)
  local prompt_buf = vim.api.nvim_create_buf(false, true)

  vim.api.nvim_buf_set_option(prompt_buf, "buftype", "prompt")

  local prompt_window =
    vim.api.nvim_open_win(
    prompt_buf,
    true,
    {relative = "cursor", row = 1, col = 1, width = 20, height = 1, border = "single", style = "minimal"}
  )
  vim.fn.prompt_setprompt(prompt_buf, opts.prompt)

  vim.fn.prompt_setcallback(
    prompt_buf,
    function(text)
      if opts.callback(text) then
        vim.api.nvim_win_close(prompt_window, true)
        vim.api.nvim_buf_delete(prompt_buf, {force = true})
      end
    end
  )

  if opts.initial then
    vim.cmd("norm i" .. opts.initial)
  end
  vim.cmd("startinsert")

  return prompt_buf, prompt_window
end

function M.rename()
  local bufnr = vim.api.nvim_get_current_buf()
  local params = vim.lsp.util.make_position_params()
  make_prompt(
    {
      prompt = " → ",
      callback = function(new_name)
        if not (new_name and #new_name > 0) then
          return true
        end
        params.newName = new_name
        vim.lsp.buf_request(bufnr, "textDocument/rename", params)
        return true
      end
    }
  )
end

return M
