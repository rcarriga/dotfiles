local M = {}
local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
  execute("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
end

function M.update()
  vim.cmd [[ PackerCompile ]]
  vim.cmd [[ PackerSync ]]
end

require("packer").startup(
  function(use)
    use {"folke/twilight.nvim", cmd = "Twilight", opt = true, requires = {"folke/zen-mode.nvim", config = "require('config.zen').post()", opt = true}}
    use {"sindrets/diffview.nvim", cmd = "DiffviewOpen", config = "require('config.git').post()"}
    use {"steelsojka/pears.nvim", config = "require('config.autopairs').post()"}
    use {"iamcco/markdown-preview.nvim", run = "cd app && yarn install"}
    use {"MTDL9/vim-log-highlighting"}
    use {"glepnir/galaxyline.nvim", config = "require('config.galaxyline').post()"}
    use {"godlygeek/tabular", cmd = "Tabularize"}
    use {"kkoomen/vim-doge", cmd = "DogeGenerate", run = ":call doge#install()"}
    use {
      "kristijanhusak/vim-dadbod-ui",
      cmd = {"DBUI"},
      requires = {"tpope/vim-dadbod", opt = true},
      {"kristijanhusak/vim-dadbod-completion", opt = true}
    }
    use {"kyazdani42/nvim-tree.lua", cmd = "NvimTreeToggle"}
    use {"machakann/vim-sandwich"}
    use {"mhinz/vim-signify"}
    use {
      "neovim/nvim-lspconfig",
      config = "require('config.lsp').post()",
      requires = {
        "kabouzeid/nvim-lspinstall",
        "nvim-lua/lsp-status.nvim",
        "folke/lua-dev.nvim",
        {"folke/trouble.nvim", cmd = "Trouble", config = "require('trouble').setup({})"},
        "ray-x/lsp_signature.nvim",
        
      }
    }
    use {"neovimhaskell/haskell-vim"}
    use {
      "/home/ronan/Dev/repos/vim-ultest",
      config = "require('config.ultest').post()",
      keys = {"<Plug>(ultest-run-nearest)", "<Plug>(ultest-run-file)", "<Plug>(ultest-summary-toggle)"},
      requires = {"janko/vim-test", cmd = {"TestNearest", "TestFile"}}
    }
    use {"rhysd/git-messenger.vim", keys = "<leader>gm"}
    use {"romgrk/barbar.nvim"}
    use {"rrethy/vim-hexokinase", run = "make hexokinase", cmd = "HexokinaseTurnOn"}
    use {"simnalamburt/vim-mundo", cmd = "MundoToggle"}
    use {"svermeulen/vim-subversive"}
    use {"JoosepAlviste/nvim-ts-context-commentstring", requires = {"tpope/vim-commentary"}}
    use {"tpope/vim-abolish", cmd = "S"}
    use {"tpope/vim-eunuch", cmd = {"Rename", "Delete", "Remove", "Chmod"}}
    use {"tpope/vim-fugitive", requires = {{"tpope/vim-rhubarb"}, {"shumphrey/fugitive-gitlab.vim"}}}
    use {"tpope/vim-unimpaired"}
    use {"voldikss/vim-floaterm", cmd = "FloatermNew"}
    use {"wbthomason/packer.nvim"}
    use {"wellle/targets.vim", requires = {"wellle/line-targets.vim"}}
    use {
      "nvim-telescope/telescope.nvim",
      cmd = "Telescope",
      branch = "async_v2",
      config = "require('config.telescope').post()",
      requires = {
        {"nvim-telescope/telescope-fzf-native.nvim", run = "make"},
        {"nvim-lua/popup.nvim"},
        {"nvim-lua/plenary.nvim", branch = "async_jobs_v2"},
        {"kyazdani42/nvim-web-devicons"}
      }
    }
    use {
      "mfussenegger/nvim-dap",
      config = "require('config.dap').post()",
      requires = {
        {"mfussenegger/nvim-dap-python"},
        {"/home/ronan/Dev/repos/nvim-dap-ui"}
      }
    }
    use {
      "nvim-treesitter/nvim-treesitter",
      config = "require('config.treesitter').post()",
      requires = {
        {"nvim-treesitter/playground"},
        {"nvim-treesitter/nvim-treesitter-textobjects"},
        {"nvim-treesitter/nvim-treesitter-refactor"},
        {"mfussenegger/nvim-ts-hint-textobject"}
      }
    }
    use {
      "hrsh7th/nvim-compe",
      event = {"InsertEnter"},
      config = "require('config.compe').post()",
      requires = {{"hrsh7th/vim-vsnip", opt = true}, {"onsails/lspkind-nvim"}}
    }
  end
)
return M
