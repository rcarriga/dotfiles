local M = {}
local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
  execute("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
end

local packer = require("packer")

function M.update()
  vim.cmd([[ PackerCompile ]])
  vim.cmd([[ PackerSync ]])
end

local function exists(path)
  local ok, err, code = os.rename(path, path)
  if not ok then
    if code == 13 then
      -- Permission denied, but it exists
      return true
    end
  end
  return ok, err
end

local function maybe_local(path)
  if exists(path) then
    return path
  end
  local elems = vim.split(path, "/", { plain = true })
  return "git@github.com:rcarriga/" .. elems[#elems]
end

packer.startup({
  config = {
    compile_path = fn.stdpath("config") .. "/lua/my_packer.lua",
    display = {
      open_fn = function()
        return require("packer.util").float({ border = vim.g.border_chars })
      end,
    },
  },
  function(use)
    use({"mzlogin/vim-markdown-toc"})
    use {"MunifTanjim/nui.nvim"}
    use({ "wbthomason/packer.nvim" })
    use({ "nvim-lua/plenary.nvim" })
    use({ "antoinemadec/FixCursorHold.nvim" })
    use({ "lukas-reineke/indent-blankline.nvim", config = "require('config.indentline').post()" })
    use({
      "kristijanhusak/orgmode.nvim",
      config = "require('config.org').post()",
      requires = { "akinsho/org-bullets.nvim", "lukas-reineke/headlines.nvim" },
    })
    use({
      "luukvbaal/stabilize.nvim",
      config = "require('config.stabilize').post()",
    })
    use({ "Vimjas/vim-python-pep8-indent" })
    use({ "lewis6991/impatient.nvim" })
    use({ "jose-elias-alvarez/null-ls.nvim" })
    use({
      maybe_local("/home/ronan/Dev/repos/neotest"),
      config = "require('config.neotest').post()",
      requires = {
        maybe_local("/home/ronan/Dev/repos/neotest-python"),
        maybe_local("/home/ronan/Dev/repos/neotest-plenary"),
        maybe_local("/home/ronan/Dev/repos/neotest-vim-test"),
      },
    })
    use({
      maybe_local("/home/ronan/Dev/repos/nvim-notify"),
      config = "require('config.notify').post()",
    })
    use({
      "sindrets/diffview.nvim",
      requires = { "lewis6991/gitsigns.nvim" },
      config = "require('config.git').post()",
    })
    use({ "iamcco/markdown-preview.nvim", run = "cd app && yarn install" })
    use({ "MTDL9/vim-log-highlighting" })
    use({ "NTBBloodbath/galaxyline.nvim", config = "require('config.galaxyline').post()" })
    use({ "kyazdani42/nvim-web-devicons", requires = { "yamatsum/nvim-nonicons" } })
    use({ "godlygeek/tabular", cmd = "Tabularize" })
    use({ "danymat/neogen", config = "require('config.docs').post()" })
    use({ "kyazdani42/nvim-tree.lua", config = "require('config.filetree').post()" })
    use({ "machakann/vim-sandwich" })
    use({
      "neovim/nvim-lspconfig",
      config = "require('config.lsp').post()",
      requires = {
        "williamboman/nvim-lsp-installer",
        "nvim-lua/lsp-status.nvim",
        "folke/lua-dev.nvim",
        "ray-x/lsp_signature.nvim",
        { "simrat39/symbols-outline.nvim", setup = "require('config.lsp').pre()" },
        "onsails/diaglist.nvim",
      },
    })
    use({ "neovimhaskell/haskell-vim" })
    use({
      maybe_local("/home/ronan/Dev/repos/vim-ultest"),
      config = "require('config.ultest').post()",
      keys = {
        "<Plug>(ultest-run-nearest)",
        "<Plug>(ultest-run-file)",
        "<Plug>(ultest-summary-toggle)",
      },
      cmd = { "UltestSummary" },
      requires = { "janko/vim-test", cmd = { "TestNearest", "TestFile" } },
    })
     use({ "rhysd/git-messenger.vim", keys = "<leader>gm" })
     use({ "romgrk/barbar.nvim" })
     use({ "rrethy/vim-hexokinase", run = "make hexokinase", cmd = "HexokinaseTurnOn" })
     use({ "simnalamburt/vim-mundo", cmd = "MundoToggle" })
     use({ "svermeulen/vim-subversive" })
     use({ "JoosepAlviste/nvim-ts-context-commentstring", requires = { "tpope/vim-commentary" } })
     use({ "tpope/vim-abolish", cmd = "S" })
     use({ "tpope/vim-eunuch", cmd = { "Rename", "Delete", "Remove", "Chmod" } })
     use({
       "tpope/vim-fugitive",
       requires = { { "tpope/vim-rhubarb" }, { "shumphrey/fugitive-gitlab.vim" } },
     })
     use({ "tpope/vim-unimpaired" })
     use({ "voldikss/vim-floaterm", cmd = "FloatermNew" })
     use({ "wellle/targets.vim", requires = { "wellle/line-targets.vim" } })
     use({
       "nvim-telescope/telescope.nvim",
       config = "require('config.telescope').post()",
       requires = {
         { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
         { "nvim-lua/popup.nvim" },
       },
     })
     use({
       "mfussenegger/nvim-dap",
       config = "require('config.dap').post()",
       requires = {
         { "mfussenegger/nvim-dap-python" },
         { maybe_local("/home/ronan/Dev/repos/nvim-dap-ui") },
         { maybe_local("/home/ronan/Dev/repos/cmp-dap") },
         { "jbyuki/one-small-step-for-vimkind" },
       },
     })
     use({
       "nvim-treesitter/nvim-treesitter",
       config = "require('config.treesitter').post()",
       requires = {
         { "lewis6991/spellsitter.nvim" },
         { "nvim-treesitter/playground" },
         { "nvim-treesitter/nvim-treesitter-textobjects" },
         { "nvim-treesitter/nvim-treesitter-refactor" },
         { "mfussenegger/nvim-ts-hint-textobject" },
       },
     })
     use({
       "hrsh7th/nvim-cmp",
       config = "require('config.completion').post()",
       requires = {
         "rafamadriz/friendly-snippets",
         "L3MON4D3/LuaSnip",
         "saadparwaiz1/cmp_luasnip",
         "github/copilot.vim",
         "hrsh7th/cmp-copilot",
         "petertriho/cmp-git",
         "onsails/lspkind-nvim",
         "hrsh7th/cmp-nvim-lsp",
         "hrsh7th/cmp-buffer",
         "hrsh7th/cmp-path",
         "hrsh7th/cmp-vsnip",
         "hrsh7th/vim-vsnip",
         "lukas-reineke/cmp-under-comparator",
       },
     })
  end,
})
return M
