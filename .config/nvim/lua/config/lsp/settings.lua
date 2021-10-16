local M = {}

local function setup_null_ls()
  local null_ls = require("null-ls")
  local blt = null_ls.builtins

  null_ls.config({
    sources = {
      require("null-ls").builtins.formatting.stylua,
      blt.formatting.black,
      blt.formatting.goimports,
      blt.formatting.gofumpt,
      blt.formatting.isort,
      blt.formatting.prettier,
      null_ls.builtins.formatting.shfmt,
    },
  })
end

function M.setup(on_attach, capabilities)
  local lsp_status = require("lsp-status")

  setup_null_ls()
  require("lspconfig")["null-ls"].setup({
    on_attach = on_attach,
    capabilities = capabilities,
  })

  local configs = require("lspconfig/configs")
  local util = require("lspconfig/util")

  local path = util.path

  local function get_python_path(workspace)
    -- Use activated virtualenv.
    if vim.env.VIRTUAL_ENV then
      return path.join(vim.env.VIRTUAL_ENV, "bin", "python")
    end

    -- Find and use virtualenv in workspace directory.
    for _, pattern in ipairs({ "*", ".*" }) do
      local match = vim.fn.glob(path.join(workspace, pattern, "pyvenv.cfg"))
      if match ~= "" then
        return path.join(path.dirname(match), "bin", "python")
      end
    end

    -- Fallback to system Python.
    return vim.fn.exepath("python3") or vim.fn.exepath("python") or "python"
  end

  local server_configs = {
    sumneko_lua = require("lua-dev").setup({
      library = { plugins = { "plenary.nvim" }, types = false },
      lspconfig = {
        on_attach = on_attach,
        capabilities = capabilities,
        settings = {
          Lua = {
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
        config.settings.python.pythonPath = get_python_path(config.root_dir)
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
