local M = {}
local finders = require("telescope.finders")
local make_entry = require("telescope.make_entry")
local pickers = require("telescope.pickers")
local conf = require("telescope.config").values

local lsp_definitions = function(opts)
  opts = opts or {}
  opts.tail_path = true

  local params = vim.lsp.util.make_position_params()
  local action =  "textDocument/definition"
  local result, err = vim.lsp.buf_request_sync(0, action, params, opts.timeout or 10000)
  if err then
    vim.api.nvim_err_writeln("Error when executing " .. action .. " : " .. err)
    return
  end
  local flattened_results = {}
  for _, server_results in pairs(result) do
    if server_results.result then
      vim.list_extend(flattened_results, server_results.result)
    end
  end

  if #flattened_results == 0 then
    return
  else
    local success, locations = pcall(vim.lsp.util.locations_to_items, flattened_results)
    if not success then
      print("Error opening locations " .. locations)
      return
    end
    pickers.new(
      opts,
      {
        prompt_title = "LSP Definitions",
        finder = finders.new_table {
          results = locations,
          entry_maker = opts.entry_maker or make_entry.gen_from_quickfix(opts)
        },
        previewer = conf.qflist_previewer(opts),
        sorter = conf.generic_sorter(opts)
      }
    ):find()
  end
end

local function wrap_options(custom, handler)
  return function(opts)
    opts = opts and vim.tbl_extend(opts, custom) or custom
    handler(opts)
  end
end

function M.setup()
  vim.lsp.handlers["textDocument/publishDiagnostics"] =
    vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics,
    {
      signs = {
        priority = 5
      },
      underline = false,
      virtual_text = {
        prefix = "⚫"
      }
    }
  )
  vim.lsp.handlers["textDocument/codeAction"] =
    wrap_options({layout_strategy = "vertical", width = 50}, require("telescope.builtin").lsp_code_actions)
  vim.lsp.handlers["textDocument/references"] =
    wrap_options({layout_strategy = "vertical"}, require("telescope.builtin").lsp_references)
  vim.lsp.handlers["textDocument/definition"] =
    wrap_options({layout_strategy = "vertical"}, lsp_definitions)
  vim.lsp.handlers["textDocument/documentSymbol"] = (require("telescope.builtin").lsp_document_symbols)
  vim.lsp.handlers["textDocument/hover"] =
    vim.lsp.with(
    vim.lsp.handlers.hover,
    {
      border = "single"
    }
  )
end
return M
