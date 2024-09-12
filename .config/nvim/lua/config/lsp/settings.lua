local M = {}

function M.setup(on_attach, capabilities)
  local null_ls = require("null-ls")
  local blt = null_ls.builtins
  local lspconfig = require("lspconfig")

  null_ls.setup({
    on_attach = on_attach,
    capabilities = capabilities,
    autostart = true,
    sources = {
      blt.formatting.stylua.with({
        extra_args = { "--config-path", vim.fs.normalize("~/.config/stylua.toml") },
      }),
      blt.formatting.goimports,
      blt.formatting.gofumpt,
      blt.formatting.prettier,
      blt.formatting.shfmt,
    },
  })

  require("neodev").setup({
    library = {
      enabled = true,
      runtime = true,
      types = true,
      plugins = true,
    },
    setup_jsonls = true,
    override = function(_, options)
      options.enabled = true
      options.plugins = {
        "nvim-cmp",
        "plenary.nvim",
        "neotest",
        "nvim-dap",
        "nvim-dap-ui",
        "nvim-lspconfig",
        "nvim-notify",
        "nui.nvim",
        "nvim-nio",
      }
    end,
  })
  local server_configs = {
    ruff = {
      on_attach = on_attach,
      init_options = {
        settings = {
          configurationPreference = "filesystemFirst",
        },
      },
    },

    basedpyright = {
      on_attach = on_attach,
      before_init = function(_, config)
        config.settings.python.pythonPath = require("util").get_python_path(config.root_dir)
      end,
      settings = {
        basedpyright = {
          typeCheckingMode = "standard",
        },
        python = {
          pythonPath = "python",
          analysis = {
            autoSearchPaths = true,
            useLibraryCodeForTypes = true,
            diagnosticMode = "workspace",
          },
        },
      },
    },
    lua_ls = {
      on_attach = on_attach,
      capabilities = capabilities,
      settings = {
        Lua = {
          hint = {
            enable = true,
            paramName = "Disable",
            paramType = false,
            setType = true,
            arrayIndex = false,
          },
          IntelliSense = {
            traceLocalSet = true,
          },
          diagnostics = {
            globals = { "describe", "it", "before_each", "after_each", "vim" },
          },
          runtime = {
            path = {
              "lua/?.lua",
              "lua/?/init.lua",
            },
            -- plugin = "scripts/lspdoc.old.lua",
          },
          workspace = {
            library = {
              vim.fn.expand("$VIMRUNTIME"),
              require("neodev.config").types(),
              "${3rd}/busted/library",
              "${3rd}/luassert/library",
              "${3rd}/luv/library",
            },
          },
        },
      },
    },

    volar = {
      on_attach = on_attach,
      filetypes = {
        "typescript",
        "javascript",
        "javascriptreact",
        "typescriptreact",
        "vue",
        "json",
      },
      capabilities = capabilities,
    },
    yamlls = require("yaml-companion").setup({
      on_attach = on_attach,
      lspconfig = {
        on_attach = on_attach,
        settings = { yaml = { schemas = { kubernetes = "helm/**.yaml" } } },
        flags = { debounce_text_changes = 150 },
        capabilities = capabilities,
      },
    }),
    ocamllsp = {},
  }

  local mason_handlers = {
    function(server_name)
      if server_name == "tsserver" then
        return
      end
      lspconfig[server_name].setup({ on_attach = on_attach })
    end,
  }

  lspconfig.postgres_lsp.setup({})
  lspconfig.nixd.setup({ on_attach = on_attach })

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
      "gopls",
      "jsonls",
      "vimls",
    }),
  })
  require("mason-lspconfig").setup_handlers(mason_handlers)
end

return M
