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
    use {"steelsojka/pears.nvim", config = "require('config.autopairs').post()"}
    use {"lukas-reineke/indent-blankline.nvim", branch = "lua"}
    use {"eddiebergman/nvim-treesitter-pyfold"}
    use {"iamcco/markdown-preview.nvim", run = "cd app && yarn install"}
    use {"rafcamlet/nvim-luapad", cmd = "Luapad"}
    use {"mattn/emmet-vim"}
    use {"Konfekt/FastFold"}
    use {"MTDL9/vim-log-highlighting"}
    use {"Yggdroot/hiPairs"}
    use {"ekalinin/Dockerfile.vim", ft = {"dockerfile", "Dockerfile"}}
    use {"glepnir/galaxyline.nvim", config = "require('config.galaxyline').post()"}
    use {"godlygeek/tabular", cmd = "Tabularize"}
    use {"haya14busa/vim-asterisk", requires = {"haya14busa/is.vim"}}
    use {"AndrewRadev/splitjoin.vim", keys = {"gS", "gJ"}}
    use {"junegunn/fzf", requires = {{"junegunn/fzf.vim", opt = true}}, cmd = {"Files", "Rg"}}
    use {"junegunn/goyo.vim", cmd = "Goyo"}
    use {"kkoomen/vim-doge", cmd = "DogeGenerate", run = ":call doge#install()"}
    use {
      "kristijanhusak/vim-dadbod-ui",
      cmd = {"DBUI"},
      requires = {"tpope/vim-dadbod", opt = true},
      {"kristijanhusak/vim-dadbod-completion", opt = true}
    }
    use {"kyazdani42/nvim-tree.lua"}
    use {"lukas-reineke/format.nvim", cmd = "Format", config = "require('config.format').pre()"}
    use {"machakann/vim-sandwich"}
    use {"mhinz/vim-signify"}
    use {
      "neovim/nvim-lspconfig",
      config = "require('config.lsp').post()",
      requires = {
        "nvim-lua/lsp-status.nvim",
        {"rcarriga/lspsaga.nvim", branch = "feat/avoidPmenu"},
        {"onsails/lspkind-nvim", config = "require('config.lspkind').post()"},
      }
    }
    use {"neovimhaskell/haskell-vim"}
    use {
      "rcarriga/vim-ultest",
      config = "require('config.ultest').post()",
      keys = {"<Plug>(ultest-run-nearest)", "<Plug>(ultest-run-file)", "<Plug>(ultest-summary-toggle)"},
      requires = {"janko/vim-test", cmd = {"TestNearest", "TestFile"}}
    }
    use {"rhysd/clever-f.vim", keys = {"f", "t"}}
    use {"rhysd/conflict-marker.vim"}
    use {"rhysd/git-messenger.vim", keys = "<leader>gm"}
    use {"romgrk/barbar.nvim"}
    use {"rrethy/vim-hexokinase", run = "make hexokinase"}
    use {"simnalamburt/vim-mundo", cmd = "MundoToggle"}
    use {"sodapopcan/vim-twiggy", cmd = "Twiggy"}
    use {"svermeulen/vim-subversive"}
    use {"JoosepAlviste/nvim-ts-context-commentstring", requires = {"tpope/vim-commentary"}}
    use {"tpope/vim-abolish", cmd = "S"}
    use {"tpope/vim-eunuch", cmd = {"Rename", "Delete", "Remove", "Chmod"}}
    use {"tpope/vim-fugitive", requires = {{"tpope/vim-rhubarb"}}}
    use {"tpope/vim-unimpaired"}
    use {"voldikss/vim-floaterm", cmd = "FloatermNew"}
    use {"wbthomason/packer.nvim"}
    use {"wellle/targets.vim", requires = {"wellle/line-targets.vim"}}
    use {
      "nvim-telescope/telescope.nvim",
      config = "require('config.telescope').post()",
      requires = {
        {"nvim-telescope/telescope-fzf-native.nvim", run = "make"},
        {"nvim-telescope/telescope-dap.nvim"},
        {"nvim-lua/popup.nvim"},
        {"nvim-lua/plenary.nvim"},
        {"kyazdani42/nvim-web-devicons"},
      }
    }
    use {
      "mfussenegger/nvim-dap",
      config = "require('config.dap').post()",
      requires = {
        {"mfussenegger/nvim-dap-python"},
        {"rcarriga/nvim-dap-ui"}
      }
    }
    use {
      "nvim-treesitter/nvim-treesitter",
      config = "require('config.treesitter').post()",
      requires = {
        {"nvim-treesitter/playground"},
        {"nvim-treesitter/nvim-treesitter-textobjects"},
        {"nvim-treesitter/nvim-treesitter-refactor"},
        {"romgrk/nvim-treesitter-context"}
      }
    }
    use {
      "rcarriga/nvim-compe",
      branch = "feat/doc-border",
      event = {"InsertEnter"},
      config = "require('config.compe').post()",
      requires = {{"hrsh7th/vim-vsnip"}}
    }
  end
)
return M
