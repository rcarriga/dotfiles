local M = {}

local lsp_definitions = function(opts)
  opts = {}

  local params = vim.lsp.util.make_position_params()
  params.context = {includeDeclaration = true}

  local results_lsp = vim.lsp.buf_request_sync(0, "textDocument/definition", params, opts.timeout or 10000)
  local locations = {}
  for _, server_results in pairs(results_lsp) do
    if server_results.result then
      vim.list_extend(locations, vim.lsp.util.locations_to_items(server_results.result) or {})
    end
  end

  if vim.tbl_isempty(locations) then
    return
  end
  local conf = require("telescope.config").values

  require("telescope.pickers").new(
    opts,
    {
      prompt_title = "LSP Definitions",
      layout_strategy = "vertical",
      finder = require("telescope.finders").new_table {
        results = locations,
        entry_maker = opts.entry_maker or require("telescope.make_entry").gen_from_quickfix(opts)
      },
      previewer = conf.qflist_previewer(opts),
      sorter = conf.generic_sorter(opts)
    }
  ):find()
end

local wrap_options = function(custom, handler)
  return function(opts)
    opts = opts and vim.tbl_extend(opts, custom) or custom
    handler(opts)
  end
end

function M.post()
  local lsp_status = require("lsp-status")
  lsp_status.register_progress()
  vim.fn.sign_define("LightBulbSign", { text = "", texthl = "Info", linehl="", numhl="" })
  vim.cmd [[autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()]]
  vim.fn.sign_define(
    "LspDiagnosticsSignError",
    {text = "", texthl = "LspDiagnosticsDefaultError", numhl = "LspDiagnosticsDefaultError"}
  )
  vim.fn.sign_define(
    "LspDiagnosticsSignWarning",
    {text = "", texthl = "LspDiagnosticsDefaultWarning", numhl = "LspDiagnosticsDefaultWarning"}
  )
  vim.fn.sign_define(
    "LspDiagnosticsSignInformation",
    {text = "", texthl = "LspDiagnosticsDefaultInformation", numhl = "LspDiagnosticsDefaultInformation"}
  )
  vim.fn.sign_define(
    "LspDiagnosticsSignHint",
    {text = "", texthl = "LspDiagnosticsDefaultHint", numhl = "LspDiagnosticsDefaultHint"}
  )
  local opts = {
    border_style = 1,
    max_diag_msg_width = 80
  }

  require("lspsaga").init_lsp_saga(opts)
  local nvim_lsp = require("lspconfig")
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities.textDocument.completion.completionItem.snippetSupport = true
  capabilities = vim.tbl_extend("keep", capabilities or {}, lsp_status.capabilities)
  vim.lsp.handlers["textDocument/publishDiagnostics"] =
    vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics,
    {
      signs = true,
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
  vim.lsp.handlers["textDocument/definition"] = lsp_definitions
  vim.lsp.handlers["textDocument/documentSymbol"] = (require("telescope.builtin").lsp_document_symbols)
  vim.lsp.handlers["textDocument/codeLens"] = require("config.lsp_codelens").on_codelens
  local on_attach = function(client, bufnr)
    lsp_status.on_attach(client)
    local function buf_set_keymap(...)
      vim.api.nvim_buf_set_keymap(bufnr, ...)
    end

    -- Mappings.
    local opts = {noremap = true, silent = true}
    buf_set_keymap("n", "gD", "<Cmd>lua vim.lsp.buf.definition()<CR>", opts)
    buf_set_keymap("n", "gd", "<cmd>lua require'lspsaga.provider'.preview_definition()<CR>", opts)
    buf_set_keymap("n", "ge", "<cmd>lua require'lspsaga.diagnostic'.show_line_diagnostics()<CR>", opts)
    buf_set_keymap("n", "K", "<cmd>lua require('lspsaga.hover').render_hover_doc()<CR>", opts)
    buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
    buf_set_keymap("n", "gr", "<cmd>lua require('lspsaga.rename').rename()<CR>", opts)
    buf_set_keymap("n", "[d", "<cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_prev()<CR>", opts)
    buf_set_keymap("n", "]d", "<cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_next()<CR>", opts)
    buf_set_keymap("n", "<C-k>", "<cmd>lua require('lspsaga.signaturehelp').signature_help()<CR>", opts)
    buf_set_keymap("n", "<space>la", "<cmd>lua require('lspsaga.codeaction').code_action()<CR>", opts)
    buf_set_keymap("v", "<space>la", "<cmd>lua require('lspsaga.codeaction').range_code_action()<CR>", opts)
    buf_set_keymap("n", "<space>lt", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
    buf_set_keymap("n", "<space>ls", "<cmd>lua vim.lsp.buf.document_symbol()<CR>", opts)
    buf_set_keymap("n", "<space>ln", "<cmd>lua require('config.lsp_codelens').run()<CR>", opts)
    buf_set_keymap("n", "gq", "<cmd>lua require'lspsaga.provider'.lsp_finder()<CR>", opts)
    require("util").multilineCommand [[
      augroup CodeLensRegfresh
        au!
        au BufWrite,BufEnter * lua require("config.lsp_codelens").refresh()
      augroup END

      hi link LspCodeLens Hidden
      hi link LspReferenceRead Error
      hi link LspReferenceText Error
      hi link LspReferenceWrite Error
    ]]
  end

  local servers = {"tsserver", "hls", "gopls", "dockerls", "vimls", "bashls"}
  for _, lsp in ipairs(servers) do
    nvim_lsp[lsp].setup {on_attach = on_attach, capabilities = capabilities}
  end
  nvim_lsp.pyright.setup {
    handlers = lsp_status.extensions.pyls_ms.setup(),
    on_attach = on_attach,
    capabilities = capabilities
  }

  nvim_lsp.jsonls.setup {
    cmd = {"vscode-json-language-server", "--stdio"},
    on_attach = on_attach,
    capabilities = capabilities
  }
  nvim_lsp.html.setup {
    cmd = {"vscode-html-language-server", "--stdio"},
    on_attach = on_attach,
    capabilities = capabilities
  }
  nvim_lsp.cssls.setup {
    cmd = {"vscode-css-language-server", "--stdio"},
    on_attach = on_attach,
    capabilities = capabilities
  }
  -- Vue setup
  nvim_lsp.vuels.setup {
    on_attach = on_attach,
    init_options = {
      config = {
        css = {},
        emmet = {},
        html = {
          suggest = {}
        },
        javascript = {
          format = {}
        },
        stylusSupremacy = {},
        typescript = {
          format = {}
        },
        vetur = {
          experimental = {
            templateInterpolationService = true
          },
          completion = {
            autoImport = true,
            tagCasing = "kebab",
            useScaffoldSnippets = false
          },
          format = {
            defaultFormatter = {
              js = "none",
              ts = "none"
            },
            defaultFormatterOptions = {},
            scriptInitialIndent = false,
            styleInitialIndent = false
          },
          useWorkspaceDependencies = false,
          validation = {
            script = true,
            style = true,
            template = true
          }
        }
      }
    }
  }
  -- Yaml Setup
  nvim_lsp.yamlls.setup {
    on_attach = on_attach,
    init_options = {
      config = {
        yaml = {
          schemas = {
            kubernetes = "helm/**.yaml"
          }
        }
      }
    }
  }
  vim.split(package.path .. ";" .. vim.fn.expand("$HOME/.cache/nvim/site/packer/?.lua"), ";")

  -- Lua setup
  require "lspconfig".sumneko_lua.setup {
    cmd = {"lua-language-server"},
    on_attach = on_attach,
    capabilities = capabilities,
    settings = {
      Lua = {
        runtime = {
          version = "LuaJIT",
          path = vim.split(package.path, ";")
        },
        diagnostics = {
          globals = {"vim"}
        },
        workspace = {
          library = {
            [vim.fn.expand("$VIMRUNTIME/lua")] = true,
            [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true
          }
        }
      }
    }
  }

end

return M
