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
						experimental = { templateInterpolationService = true },
						completion = {
							autoImport = true,
							tagCasing = "kebab",
							useScaffoldSnippets = true,
						},
						format = {
							scriptInitialIndent = true,
							styleInitialIndent = true,
						},
						useWorkspaceDependencies = false,
						validation = { script = true, style = true, template = true },
					},
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
		for _, server in pairs(vim.list_extend({ "hie" }, servers)) do
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
