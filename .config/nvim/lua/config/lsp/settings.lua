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

  require("lua-dev").setup({
    library = {
      enabled = true,
      runtime = true,
      types = true,
      plugins = true,
    },
    setup_jsonls = true,
    override = function(_, options)
      options.enabled = true
      options.plugins = { "nvim-cmp", "plenary.nvim", "neotest" }
    end,
  })
  local server_configs = {
    sumneko_lua = {
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
        },
      },
    },
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
