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
    display = {
      open_fn = function()
        return require("packer.util").float({ border = vim.g.border_chars })
      end,
    },
  },
  function(use)
    use { "williamboman/mason.nvim", branch = "alpha" }
    use({ "echasnovski/mini.nvim" })
    use({ "mzlogin/vim-markdown-toc" })
    use({
      "nvim-neorg/neorg",
      config = "require('config.org').post()",
      cmd = "Neorg",
      ft = "norg",
      after = { "nvim-treesitter", "telescope.nvim" },
      requires = { "nvim-neorg/neorg-telescope", "folke/zen-mode.nvim" },
    })
    use({ "wbthomason/packer.nvim" })
    use({ "nvim-lua/plenary.nvim" })
    use({ "antoinemadec/FixCursorHold.nvim" })
    use({ "lukas-reineke/indent-blankline.nvim", config = "require('config.indentline').post()" })
    use({
      "kristijanhusak/orgmode.nvim",
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
        { "akinsho/neotest-go", module = "neotest-go" },
        { maybe_local("/home/ronan/Dev/repos/neotest-python"), module = "neotest-python" },
        { maybe_local("/home/ronan/Dev/repos/neotest-plenary"), module = "neotest-plenary" },
        { maybe_local("/home/ronan/Dev/repos/neotest-vim-test"), module = "neotest-vim-test" },
      },
    })
    use({
      maybe_local("/home/ronan/Dev/repos/nvim-notify"),
      module = "notify",
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
    use({ "danymat/neogen", cmd = "Neogen", config = "require('config.docs').post()" })
    use({
      "kyazdani42/nvim-tree.lua",
      keys = "<leader>x",
      config = "require('config.filetree').post()",
    })
    use({ "machakann/vim-sandwich", keys = { "sa", "sr", "sd" } })
    use({
      "neovim/nvim-lspconfig",
      config = "require('config.lsp').post()",
      requires = {
        -- "williamboman/nvim-lsp-installer",
        "nvim-lua/lsp-status.nvim",
        "folke/lua-dev.nvim",
        "ray-x/lsp_signature.nvim",
        { "simrat39/symbols-outline.nvim", setup = "require('config.lsp').pre()" },
        "onsails/diaglist.nvim",
      },
    })
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
    use({
      "svermeulen/vim-subversive",
      keys = {
        "<plug>(SubversiveSubstituteRange)",
        "<plug>(SubversiveSubstituteRange)",
        "<plug>(SubversiveSubstituteWordRange)",
        "<plug>(SubversiveSubstitute)",
        "<plug>(SubversiveSubstituteLine)",
        "<plug>(SubversiveSubstituteToEndOfLine)",
      },
    })
    use({
      "JoosepAlviste/nvim-ts-context-commentstring",
      requires = { "tpope/vim-commentary" },
    })
    use({ "tpope/vim-abolish", cmd = "S" })
    use({ "tpope/vim-eunuch", cmd = { "Rename", "Delete", "Remove", "Chmod" } })
    use({
      "tpope/vim-fugitive",
      requires = { { "tpope/vim-rhubarb" }, { "shumphrey/fugitive-gitlab.vim" } },
    })
    use({ "voldikss/vim-floaterm", cmd = "FloatermNew" })
    use({ "wellle/targets.vim", requires = { "wellle/line-targets.vim" } })
    use({
      "nvim-telescope/telescope.nvim",
      config = "require('config.telescope').post()",
      keys = { "<leader>d" },
      requires = {
        { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
        { "nvim-lua/popup.nvim" },
        { "nvim-telescope/telescope-ui-select.nvim" },
        { "someone-stole-my-name/yaml-companion.nvim" },
      },
    })
    use({
      "mfussenegger/nvim-dap",
      config = "require('config.dap').post()",
      keys = { "<M-t>", "<M-c>", "<M-x" },
      module = {"dap", "dapui"},
      opt = true,
      requires = {
        { "mfussenegger/nvim-dap-python", module = "dap-python" },
        { maybe_local("/home/ronan/Dev/repos/nvim-dap-ui"), module = "dapui" },
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
      { "rafamadriz/friendly-snippets", event = "InsertEnter" },
      { "L3MON4D3/LuaSnip", event = "InsertEnter" },
      { "saadparwaiz1/cmp_luasnip", event = "InsertEnter" },
      { "github/copilot.vim", event = "InsertEnter" },
      { "hrsh7th/cmp-copilot", event = "InsertEnter" },
      { "petertriho/cmp-git" },
      { "onsails/lspkind-nvim", event = "InsertEnter" },
      { "hrsh7th/cmp-nvim-lsp", event = "InsertEnter" },
      { "hrsh7th/cmp-buffer", event = "InsertEnter" },
      { "hrsh7th/cmp-cmdline", event = "InsertEnter" },
      { "hrsh7th/cmp-path", event = "InsertEnter" },
      { "lukas-reineke/cmp-under-comparator" },
      { maybe_local("/home/ronan/Dev/repos/cmp-dap"), event = "InsertEnter" },
      {
        "hrsh7th/nvim-cmp",
        event = "InsertEnter",
        config = "require('config.completion').post()",
      },
    })
  end,
})
return M
