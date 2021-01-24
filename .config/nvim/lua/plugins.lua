local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath("data") .. "/site/pack/packer/opt/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
  execute("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
  execute "packadd packer.nvim"
end

vim.cmd [[packadd packer.nvim]]

require("packer").startup(
  function(use)
    use {"Konfekt/FastFold"}
    use {"MTDL9/vim-log-highlighting"}
    use {"Yggdroot/hiPairs"}
    use {"ekalinin/Dockerfile.vim", ft = {"dockerfile", "Dockerfile"}}
    use {"glepnir/galaxyline.nvim", config = "require('config.galaxyline').post()"}
    use {"godlygeek/tabular", cmd = "Tabularize"}
    use {"haya14busa/vim-asterisk", requires = {"haya14busa/is.vim"}}
    use {"indrewRadev/splitjoin.vim", keys = {"gS", "gJ"}}
    use {"janko/vim-test", cmd = {"TestNearest", "TestFile"}, opt = true}
    use {"junegunn/fzf", requires = {{"junegunn/fzf.vim"}}}
    use {"junegunn/goyo.vim", cmd = "Goyo"}
    use {"kkoomen/vim-doge", cmd = "DogeGenerate", run = ":call doge#install()"}
    use {"kristijanhusak/vim-dadbod-ui", cmd = {"DBUI"}, requires = {"tpope/vim-dadbod", opt = true}}
    use {"kyazdani42/nvim-tree.lua", cmd = "NvimTreeToggle"}
    use {"lukas-reineke/format.nvim", cmd = "Format", config = "require('config.format').pre()"}
    use {"machakann/vim-sandwich"}
    use {"mhinz/vim-signify"}
    use {"neovim/nvim-lspconfig", config = "require('config.lsp').post()"}
    use {"neovimhaskell/haskell-vim"}
    use {"posva/vim-vue", ft = "vue"}
    use {"rcarriga/vim-ultest", cmd = {"Ultest", "UltestNearest"}}
    use {"rhysd/clever-f.vim", keys = {"f", "t"}}
    use {"rhysd/conflict-marker.vim"}
    use {"rhysd/git-messenger.vim", keys = "<leader>gm"}
    use {"romgrk/barbar.nvim"}
    use {"rrethy/vim-hexokinase", run = "make hexokinase"}
    use {"simnalamburt/vim-mundo", cmd = "MundoToggle"}
    use {"sodapopcan/vim-twiggy", cmd = "Twiggy"}
    use {"svermeulen/vim-subversive", keys = {"<leader>s"}}
    use {"tomtom/tcomment_vim", keys = {"gcc"}}
    use {"tpope/vim-abolish", cmd = "S"}
    use {"tpope/vim-eunuch", cmd = {"Rename", "Delete", "Remove", "Chmod"}}
    use {"tpope/vim-fugitive", requires = {{"tpope/vim-rhubarb"}}}
    use {"tpope/vim-unimpaired"}
    use {"voldikss/vim-floaterm", cmd = "FloatermNew"}
    use {"wbthomason/packer.nvim", opt = true}
    use {"wellle/targets.vim", requires = {"wellle/line-targets.vim"}}
    use {"windwp/nvim-autopairs", config = "require('config.autopairs').post()"}
    use {
      "nvim-telescope/telescope.nvim",
      config = "require('config.telescope').post()",
      requires = {
        {"nvim-telescope/telescope-dap.nvim"},
        {"nvim-lua/popup.nvim"},
        {"nvim-lua/plenary.nvim"},
        {"kyazdani42/nvim-web-devicons"},
        {"nvim-telescope/telescope-fzy-native.nvim"}
      }
    }
    use {
      "mfussenegger/nvim-dap",
      config = "require('config.dap').post()",
      requires = {
        {"theHamsta/nvim-dap-virtual-text"},
        {"mfussenegger/nvim-dap-python"}
      }
    }
    use {
      "nvim-treesitter/nvim-treesitter",
      config = "require('config.treesitter').post()",
      requires = {
        {"nvim-treesitter/playground"},
        {"nvim-treesitter/nvim-treesitter-textobjects"},
        {"nvim-treesitter/nvim-treesitter-refactor"}
      }
    }
    use {
      "nvim-lua/completion-nvim",
      requires = {
        {"hrsh7th/vim-vsnip"},
        {"hrsh7th/vim-vsnip-integ"},
        {"steelsojka/completion-buffers"},
        {"kristijanhusak/completion-tags"}
      }
    }
  end
)
