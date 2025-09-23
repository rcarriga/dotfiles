local M = {}

function M.setup(on_attach, capabilities)
  local null_ls = require("null-ls")
  local blt = null_ls.builtins
  local lspconfig = require("lspconfig")
  local configs = require("lspconfig.configs")

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
          analysis = {
            inlayHints = { callArgumentNames = false },
            diagnosticSeverityOverrides = {
              reportUnhashable = false,
              reportPrivateImportUsage = false,
            },
          },
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

    vue_ls = {
      on_attach = on_attach,
      init_options = {
        typescript = {},
      },
      filetypes = {
        -- "typescript",
        "javascript",
        "javascriptreact",
        "typescriptreact",
        "vue",
        "json",
      },
      capabilities = capabilities,
    },
    -- yamlls = require("yaml-companion").setup({
    --   on_attach = on_attach,
    --   lspconfig = {
    --     on_attach = on_attach,
    --     settings = { yaml = { schemas = { kubernetes = "helm/**.yaml" } } },
    --     flags = { debounce_text_changes = 150 },
    --     capabilities = capabilities,
    --   },
    -- }),
    ocamllsp = {},
    -- ty = {
    --   default_config = {
    --     cmd = { "ty", "server" },
    --     root_dir = function(fname)
    --       return lspconfig.util.root_pattern(
    --         "pyproject.toml",
    --         "requirements.txt",
    --         "Pipfile",
    --         "pyrightconfig.json",
    --         ".git"
    --       )(fname)
    --     end,
    --     single_file_support = true,
    --     filetypes = { "python" },
    --     settings = {},
    --   },
    -- },
  }

  -- lspconfig.postgres_lsp.setup({})
  -- lspconfig.nixd.setup({ on_attach = on_attach })

  vim.api.nvim_create_autocmd("LspAttach", {
    group = vim.api.nvim_create_augroup("lsp_attach_disable_ruff_hover", { clear = true }),
    callback = function(args)
      local client = vim.lsp.get_client_by_id(args.data.client_id)
      if client == nil then
        return
      end
      if client.name == "ruff" then
        -- Disable hover in favor of Pyright
        client.server_capabilities.hoverProvider = false
      end
    end,
    desc = "LSP: Disable hover capability from Ruff",
  })

  for server, settings in pairs(server_configs) do
    vim.lsp.config(server, settings)
    vim.lsp.enable(server)
  end
  require("mason").setup({
    ui = {
      border = vim.g.border_chars,
    },
  })
  -- require("mason-lspconfig").setup({
    -- ensure_installed = vim.list_extend(vim.tbl_keys(server_configs), {
    --   "bashls",
    --   "gopls",
    --   "jsonls",
    --   "vimls",
    -- }),
  -- })
  -- require("mason-lspconfig").setup_handlers(mason_handlers)
end

return M
