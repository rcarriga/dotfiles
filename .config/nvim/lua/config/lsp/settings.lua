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
    bashls = { on_attach = on_attach },
    clangd = { on_attach = on_attach },
    gopls = { on_attach = on_attach },
    hls = { on_attach = on_attach },
    jsonls = { on_attach = on_attach },
    rust_analyzer = { on_attach = on_attach },
    vimls = { on_attach = on_attach },
    tsserver = { on_attach = on_attach },
  }

  require("nvim-lsp-installer").setup({
    automatic_installation = true,
  })

  for server, settings in pairs(server_configs) do
    require("lspconfig")[server].setup(settings)
  end
end
return M
