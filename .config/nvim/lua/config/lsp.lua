local nvim_lsp = require("lspconfig")
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
vim.lsp.handlers["textDocument/publishDiagnostics"] =
    vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics,
    {
        signs = true,
        underline = false,
        virtual_text = {
            prefix = "← "
        }
    }
)
vim.lsp.handlers["textDocument/codeAction"] = require "lsputil.codeAction".code_action_handler
vim.lsp.handlers["textDocument/references"] = require "lsputil.locations".references_handler
vim.lsp.handlers["textDocument/definition"] = require "lsputil.locations".definition_handler
vim.lsp.handlers["textDocument/declaration"] = require "lsputil.locations".declaration_handler
vim.lsp.handlers["textDocument/typeDefinition"] = require "lsputil.locations".typeDefinition_handler
vim.lsp.handlers["textDocument/implementation"] = require "lsputil.locations".implementation_handler
vim.lsp.handlers["textDocument/documentSymbol"] = require "lsputil.symbols".document_handler
vim.lsp.handlers["workspace/symbol"] = require "lsputil.symbols".workspace_handler

local on_attach = function(client, bufnr)
    local function buf_set_keymap(...)
        vim.api.nvim_buf_set_keymap(bufnr, ...)
    end

    -- Mappings.
    local opts = {noremap = true, silent = true}
    buf_set_keymap("n", "gD", "<Cmd>lua vim.lsp.buf.declaration()<CR>", opts)
    buf_set_keymap("n", "gd", "<Cmd>lua vim.lsp.buf.definition()<CR>", opts)
    buf_set_keymap("n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>", opts)
    buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
    buf_set_keymap("n", "<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
    buf_set_keymap("n", "<space>la", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
    buf_set_keymap("n", "<space>lA", "<cmd>lua vim.lsp.buf.range_code_action()<CR>", opts)
    buf_set_keymap("n", "<space>lw", "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", opts)
    buf_set_keymap("n", "<space>ld", "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", opts)
    buf_set_keymap("n", "<space>ll", "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", opts)
    buf_set_keymap("n", "<space>lt", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
    buf_set_keymap("n", "<space>lr", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
    buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
    buf_set_keymap("n", "[d", '<cmd>lua vim.lsp.diagnostic.goto_prev({severity_limit = "Warning"})<CR>', opts)
    buf_set_keymap("n", "]d", '<cmd>lua vim.lsp.diagnostic.goto_next({severity_limit = "Warning"})<CR>', opts)

    -- Set autocommands conditional on server_capabilities
    if client.resolved_capabilities.document_highlight then
        require("lspconfig").util.nvim_multiline_command [[
      :hi link LspReferenceRead Error
      :hi link LspReferenceText Error
      :hi link LspReferenceWrite Error
    ]]
    end
end

-- Use a loop to conveniently both setup defined servers
-- and map buffer local keybindings when the language server attaches
local servers = {"pyright", "tsserver", "hls", "gopls", "jsonls", "yamlls", "dockerls"}
for _, lsp in ipairs(servers) do
    nvim_lsp[lsp].setup {on_attach = on_attach, capabilities = capabilities}
end

-- Lua Specific setup

require "lspconfig".sumneko_lua.setup {
    cmd = {"lua-language-server"},
    on_attach = on_attach,
    capabilities = capabilities,
    settings = {
        Lua = {
            runtime = {
                version = "LuaJIT",
                path = vim.split(package.path, ";")
            },
            diagnostics = {
                globals = {"vim", "use"}
            },
            workspace = {
                library = {
                    [vim.fn.expand("$VIMRUNTIME/lua")] = true,
                    [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true
                }
            }
        }
    }
}

-- Emmet setup
local configs = require "lspconfig/configs"

configs.emmet_ls = {
    default_config = {
        cmd = {"emmet-ls", "--stdio"},
        filetypes = {"html", "css", "vue", "typescriptreact", "javascriptreact"},
        root_dir = function()
            return vim.loop.cwd()
        end,
        settings = {}
    }
}

nvim_lsp.emmet_ls.setup {
    on_attach = on_attach,
    capabilities = capabilities,
}
