local M = {}

function M.setup(on_attach, capabilities)
  local has_status, lsp_status = pcall(require, "lsp-status")

  local null_ls = require("null-ls")
  local blt = null_ls.builtins

  null_ls.setup({
    on_attach = on_attach,
    capabilities = capabilities,
    autostart = true,
    sources = {
      blt.formatting.stylua,
      blt.formatting.black,
      blt.formatting.goimports,
      blt.formatting.gofumpt,
      blt.formatting.isort,
      blt.formatting.prettier,
      blt.formatting.shfmt,
    },
  })

  local server_configs = {
    sumneko_lua = require("lua-dev").setup({
      library = { plugins = { "nvim-cmp", "plenary.nvim", "neotest" }, types = true },
      lspconfig = {
        on_attach = on_attach,
        capabilities = capabilities,
        settings = {
          Lua = {
            IntelliSense = {
              traceLocalSet = true,
            },
            diagnostics = {
              globals = { "describe", "it", "before_each", "after_each", "vim" },
            },
          },
        },
      },
    }),
    pyright = {
      handlers = has_status and lsp_status.extensions.pyls_ms.setup() or nil,
      on_attach = on_attach,
      before_init = function(_, config)
        config.settings.python.pythonPath = require("util").get_python_path(config.root_dir)
      end,
      settings = {
        python = {
          pythonPath = "python",
        },
      },
      capabilities = capabilities,
    },
    vuels = {
      on_attach = on_attach,
      init_options = {
        config = {
          vetur = {
            experimental = { templateInterpolationService = true },
            completion = {
              autoImport = true,
              tagCasing = "kebab",
              useScaffoldSnippets = true,
            },
            useWorkspaceDependencies = false,
            validation = { script = true, style = true, template = true },
          },
          flags = { debounce_text_changes = 150 },
        },
      },
    },
    yamlls = require("yaml-companion").setup({
      on_attach = on_attach,
      init_options = {
        config = { yaml = { schemas = { kubernetes = "helm/**.yaml" } } },
      },
    }),
  }

  local lspconfig = require("lspconfig")
  local mason_handlers = {
    function(server_name)
      lspconfig[server_name].setup({ on_attach = on_attach })
    end,
  }
  for server, settings in pairs(server_configs) do
    mason_handlers[server] = function()
      lspconfig[server].setup(settings)
    end
  end
  require("mason").setup({
    ui = {
      border = vim.g.border_chars,
    },
  })
  require("mason-lspconfig").setup({
    ensure_installed = vim.list_extend(vim.tbl_keys(server_configs), {
      "bashls",
      "clangd",
      "gopls",
      "hls",
      "jsonls",
      "rust_analyzer",
      "vimls",
      "tsserver",
    }),
  })
  require("mason-lspconfig").setup_handlers(mason_handlers)
end

return M
