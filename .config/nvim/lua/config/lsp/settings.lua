local M = {}

function M.setup(on_attach, capabilities)
  local lsp_status = require("lsp-status")

  local null_ls = require("null-ls")
  local blt = null_ls.builtins

  null_ls.setup({
    on_attach = on_attach,
    capabilities = capabilities,
    sources = {
      blt.formatting.stylua.with({
        extra_args = { "--config-path", vim.fn.expand("~/.config/stylua.toml") },
      }),
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
      library = { plugins = { "plenary.nvim", "neotest" }, types = true },
      lspconfig = {
        on_attach = on_attach,
        capabilities = capabilities,
        settings = {
          Lua = {
            IntelliSense = {
              traceLocalSet = true
            },
            diagnostics = {
              globals = { "describe", "it", "before_each", "after_each", "vim" },
            },
          },
        },
      },
    }),
    pyright = {
      handlers = lsp_status.extensions.pyls_ms.setup(),
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
    volar = {
      on_attach = on_attach,
      init_options = {
        documentFeatures = {
          documentColor = false,
          documentFormatting = {
            defaultPrintWidth = 100,
          },
          documentSymbol = true,
          foldingRange = true,
          linkedEditingRange = true,
          selectionRange = true,
        },
        languageFeatures = {
          callHierarchy = true,
          codeAction = true,
          codeLens = true,
          completion = {
            defaultAttrNameCase = "kebabCase",
            defaultTagNameCase = "both",
          },
          definition = true,
          diagnostics = true,
          documentHighlight = true,
          documentLink = true,
          hover = true,
          references = true,
          rename = true,
          renameFileRefactoring = true,
          schemaRequestService = true,
          semanticTokens = false,
          signatureHelp = true,
          typeDefinition = true,
        },
        typescript = {
          serverPath = "",
        },
      },
    },
    yamlls = {
      on_attach = on_attach,
      init_options = {
        config = { yaml = { schemas = { kubernetes = "helm/**.yaml" } } },
      },
    },
  }

  local lsp_installer = require("nvim-lsp-installer")

  lsp_installer.on_server_ready(function(server)
    server:setup(server_configs[server.name] or {
      on_attach = on_attach,
      capabilities = capabilities,
    })
    vim.cmd([[ do User LspAttachBuffers ]])
  end)
end
return M
