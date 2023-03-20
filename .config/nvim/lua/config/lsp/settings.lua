local M = {}

function M.setup(on_attach, capabilities)
  local null_ls = require("null-ls")
  local blt = null_ls.builtins
  local lspconfig = require("lspconfig")
  local lsputil = require("lspconfig.util")

  require("lspconfig.configs").pyright = {
    default_config = {
      name = "pyright",
      autostart = true,
      single_file_support = true,
      cmd = {
        "node",
        vim.fn.expand("$HOME/.local/share/pyright/server.bundle.js"),
        "--stdio",
      },
      filetypes = { "python" },
      root_dir = function(fname)
        local markers = {
          "Pipfile",
          "pyproject.toml",
          "pyrightconfig.json",
          "setup.py",
          "setup.cfg",
          "requirements.txt",
        }
        return lsputil.root_pattern(unpack(markers))(fname)
            or lsputil.find_git_ancestor(fname)
            or lsputil.path.dirname(fname)
      end,
      settings = {
        python = {
          analysis = vim.empty_dict(),
          telemetry = {
            enable = false,
          },
        },
        telemetry = {
          telemetryLevel = "off",
        },
      },
      docs = {
        description = [[
         https://github.com/microsoft/pyright
         `pyright`, a static type checker and language server for python
         ]],
      },
    },
  }

  null_ls.setup({
    on_attach = on_attach,
    capabilities = capabilities,
    autostart = true,
    sources = {
      blt.formatting.stylua.with({
        extra_args = { "--config-path", vim.fs.normalize("~/.config/stylua.toml") },
      }),
      blt.formatting.black.with({ args = { "--quiet", "-" } }),
      blt.formatting.goimports,
      blt.formatting.gofumpt,
      blt.formatting.isort,
      blt.formatting.prettier,
      blt.formatting.shfmt,
    },
  })

  local opts = {
    tools = {
      executor = require("rust-tools/executors").termopen,
      on_initialized = nil,
      reload_workspace_from_cargo_toml = true,
      inlay_hints = {
        auto = true,
        only_current_line = false,
        show_parameter_hints = true,
        parameter_hints_prefix = "<- ",
        other_hints_prefix = "=> ",
        max_len_align = false,
        max_len_align_padding = 1,
        right_align = false,
        right_align_padding = 7,
        highlight = "Comment",
      },
      crate_graph = {
        backend = "x11",
        output = nil,
        full = true,
      },
    },
    server = {
      standalone = false,
    },
    dap = {
      adapter = {
        type = "executable",
        command = "lldb-vscode",
        name = "rt_lldb",
      },
    },
  }

  require("rust-tools").setup(opts)

  require("neodev").setup({
    library = {
      enabled = true,
      runtime = false,
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
      }
    end,
  })
  local server_configs = {
    lua_ls = {
      on_attach = on_attach,
      capabilities = capabilities,
      settings = {
        Lua = {
          hint = {
            enable = true,
            setType = true,
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
    -- pyright = {
    --   handlers = has_status and lsp_status.extensions.pyls_ms.setup() or nil,
    --   on_attach = on_attach,
    --   before_init = function(_, config)
    --     config.settings.python.pythonPath = require("util").get_python_path(config.root_dir)
    --   end,
    --   settings = {
    --     python = {
    --       pythonPath = "python",
    --     },
    --   },
    --   capabilities = capabilities,
    -- },
    volar = {
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
      lspconfig = {
        on_attach = on_attach,
        settings = { yaml = { schemas = { kubernetes = "helm/**.yaml" } } },
        flags = { debounce_text_changes = 150 },
        capabilities = capabilities,
      },
    }),
  }

  local mason_handlers = {
    function(server_name)
      lspconfig[server_name].setup({ on_attach = on_attach })
    end,
  }

  lspconfig.pyright.setup({
    on_attach = on_attach,
    settings = {
      python = {
        -- analysis = {
        --   indexing = true,
        --   typeCheckingMode = "basic",
        --   diagnosticMode = "workspace",
        --   inlayHints = {
        --     variableTypes = true,
        --     functionReturnTypes = true,
        --   },
        --   diagnosticSeverityOverrides = {
        --     reportMissingTypeStubs = "information",
        --   },
        -- },
      },
    },
  })
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
