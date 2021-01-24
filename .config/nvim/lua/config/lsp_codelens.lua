-- Temp file until lsp handles codelens https://github.com/neovim/neovim/pull/13165
local util = require("vim.lsp.util")
local api = vim.api
--- bufnr -> client_id -> lenses
local lens_cache_by_buf =
  setmetatable(
  {},
  {
    __index = function(t, b)
      local key = b > 0 and b or api.nvim_get_current_buf()
      return rawget(t, key)
    end
  }
)
local active_requests = {}
local M = {}
local namespaces =
  setmetatable(
  {},
  {
    __index = function(t, key)
      local value = api.nvim_create_namespace("vim_lsp_codelens:" .. key)
      rawset(t, key, value)
      return value
    end
  }
)

--- Return all lenses for the given buffer
-- TODO: add client_id filter?
function M.get(bufnr)
  local lenses_by_client = lens_cache_by_buf[bufnr]
  if not lenses_by_client then
    return {}
  end
  local lenses = {}
  for _, client_lenses in pairs(lenses_by_client) do
    vim.list_extend(lenses, client_lenses)
  end
  return lenses
end

-- todo: expose get_all() ?

--- Run the code lens in the current line
function M.run()
  local line = api.nvim_win_get_cursor(0)[1]
  local bufnr = api.nvim_get_current_buf()
  local options = {}
  local lenses_by_client = lens_cache_by_buf[bufnr] or {}
  for client, lenses in pairs(lenses_by_client) do
    for _, lens in pairs(lenses) do
      if lens.range.start.line == (line - 1) then
        table.insert(options, {client = client, lens = lens})
      end
    end
  end
  if #options == 0 then
    print("No executable codelens found at current line")
  elseif #options == 1 then
    local option = options[1]
    api.nvim_buf_clear_namespace(bufnr, namespaces[option.client], line - 1, line)
    vim.lsp.buf.execute_command(option.lens.command)
  else
    local options_strings = {"Code lenses:"}
    for i, option in ipairs(options) do
      table.insert(options_strings, string.format("%d. %s", i, option.lens.command.title))
    end
    local choice = vim.fn.inputlist(options_strings)
    if choice < 1 or choice > #options then
      return
    end
    local option = options[choice]
    api.nvim_buf_clear_namespace(bufnr, namespaces[option.client], line - 1, line)
    vim.lsp.buf.execute_command(option.lens.command)
  end
end

local function resolve_lens(cache, idx, lens)
  -- If the chandler should be overridable in vim.lsp.handlers the structure of the cache would need to change
  -- So that the closure is not necessary
  -- Maybe {bufnr -> client_id -> linenr -> lenses}?
  local _, cancel =
    vim.lsp.buf_request(
    0,
    "codeLens/resolve",
    lens,
    function(err, _, result, client_id, bufnr)
      assert(not err, vim.inspect(err))
      if not result then
        return
      end
      if result.command then
        local line = result.range.start.line
        local chunks = {{result.command.title, "LspCodeLens"}}
        local ns = namespaces[client_id]
        api.nvim_buf_set_virtual_text(bufnr, ns, line, chunks, {})
      end
      cache[idx] = result
    end
  )
  table.insert(active_requests, cancel)
end

function M.on_codelens(err, _, result, client_id, bufnr)
  if err then
    return
  end
  -- todo, window could have changed, retrieve window using bufnr?
  local wininfo = vim.fn.getwininfo(api.nvim_get_current_win())[1]
  local lenses = {}
  local ns = namespaces[client_id]
  -- Can we avoid flickering? should we track which lens disappeared somehow?
  api.nvim_buf_clear_namespace(bufnr, ns, 1, -1)
  for idx, lens in pairs(result or {}) do
    lenses[idx] = lens
    if lens.command then
      local line = lens.range.start.line
      local chunks = {{lens.command.title, "LspCodeLens"}}
      api.nvim_buf_set_virtual_text(bufnr, ns, line, chunks, {})
    else
      -- todo: Should there be a different logic to decide whether to resolve the codelens?
      local within_viewport = (wininfo.topline < lens.range.start.line and wininfo.botline > lens.range.start.line)
      if within_viewport then
        resolve_lens(lenses, idx, lens)
      end
    end
  end
  local lenses_by_client = lens_cache_by_buf[bufnr]
  if not lenses_by_client then
    lenses_by_client = {}
    lens_cache_by_buf[bufnr] = lenses_by_client
    api.nvim_buf_attach(
      bufnr,
      false,
      {
        on_detach = function(b)
          lens_cache_by_buf[b] = nil
        end
      }
    )
  end
  lenses_by_client[client_id] = lenses
end

--- Refresh the codelens for the current buffer and display them using virtual text
function M.refresh()
  -- TODO: debounce?
  -- Could also track bufchanged and avoid re-retrieving the codelens if the buffer is still the same.
  -- But would still need to resolve existing lenses that fall into a potentially changed viewport?
  local params = {
    textDocument = util.make_text_document_params()
  }
  for _, cancel_req in pairs(active_requests) do
    cancel_req()
  end
  active_requests = {}
  local _, cancel = vim.lsp.buf_request(0, "textDocument/codeLens", params, M.on_codelens)
  table.insert(active_requests, cancel)
end

return M
