local M = {}

function M.setup(on_attach, capabilities)
  local lsp_status = require("lsp-status")
  -- local servers = {"tsserver", "hls", "gopls", "dockerls", "vimls", "bashls", "clangd"}
  require "lspinstall".setup()

  -- local servers = {
  --   "python",
  --   "json",
  --   "html",
  --   "css",
  --   "vue",
  --   "yaml",
  --   "efm",
  --   "go",
  --   "dockerfile",
  --   "bash",
  --   "lua",
  --   "typescript",
  --   "vim"
  -- }

  local server_configs = {
    efm = {
      filetypes = {"python", "typescript", "html", "json", "css", "vue", "javascript", "zsh", "sh", "bash", "go", "lua"},
      on_attach = on_attach,
      capabilities = capabilities,
      root_dir = vim.loop.cwd,
      settings = require("config.lsp.efm").settings(),
      init_options = {documentFormatting = true}
    },
    lua = require("lua-dev").setup(
      {
        lspconfig = {
          on_attach = on_attach,
          capabilities = capabilities
        }
      }
    ),
    python = {
      handlers = lsp_status.extensions.pyls_ms.setup(),
      on_attach = on_attach,
      capabilities = capabilities
    },
    vue = {
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
    },
    yaml = {
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
  }

  local nvim_lsp = require("lspconfig")

  local function setup_servers()
    require "lspinstall".setup()
    local servers = require "lspinstall".installed_servers()
    for _, server in pairs(servers) do
      nvim_lsp[server].setup(server_configs[server] or {on_attach = on_attach, capabilities = capabilities})
    end
  end

  setup_servers()

  -- Automatically reload after `:LspInstall <server>` so we don't have to restart neovim
  require "lspinstall".post_install_hook = function()
    setup_servers() -- reload installed servers
    vim.cmd("bufdo e") -- this triggers the FileType autocmd that starts the server
  end
end
return M
