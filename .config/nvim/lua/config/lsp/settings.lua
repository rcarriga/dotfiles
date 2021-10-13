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
  require("lspinstall").setup()

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
    lua = require("lua-dev").setup({
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
    python = {
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
    vue = {
      on_attach = on_attach,
      init_options = {
        config = {
          vetur = {
            -- experimental = { templateInterpolationService = true },
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
    yaml = {
      on_attach = on_attach,
      init_options = {
        config = { yaml = { schemas = { kubernetes = "helm/**.yaml" } } },
      },
    },
  }

  local nvim_lsp = require("lspconfig")

  local function setup_servers()
    require("lspinstall").setup()
    local servers = require("lspinstall").installed_servers()
    for _, server in pairs(vim.list_extend({ "hls" }, servers)) do
      nvim_lsp[server].setup(server_configs[server] or {
        on_attach = on_attach,
        capabilities = capabilities,
      })
    end
  end

  setup_servers()

  -- Automatically reload after `:LspInstall <server>` so we don't have to restart neovim
  require("lspinstall").post_install_hook = function()
    setup_servers() -- reload installed servers
    vim.cmd("bufdo e") -- this triggers the FileType autocmd that starts the server
  end
end
return M
