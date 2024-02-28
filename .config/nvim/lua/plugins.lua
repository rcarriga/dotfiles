local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

local home = vim.loop.os_homedir()

local function maybe_local(name)
  local path = string.format("%s/Dev/%s", home, name)
  if vim.loop.fs_stat(path) then
    return path
  end
end

local plugins = {
  {
    "nomnivore/ollama.nvim",
    config = function()
      require("config.ollama").post()
    end,
  },
  {
    "gorbit99/codewindow.nvim",
    config = function()
      local codewindow = require("codewindow")
      codewindow.setup()
      codewindow.apply_default_keybinds()
    end,
  },
  -- { "kevinhwang91/nvim-ufo", dependencies = "kevinhwang91/promise-async" },
  {
    "williamboman/mason.nvim",
    branch = "main",
    dependencies = { "williamboman/mason-lspconfig.nvim" },
  },
  {
    "ThePrimeagen/refactoring.nvim",
    config = function()
      require("config.refactoring").post()
    end,
  },
  { "echasnovski/mini.nvim" },
  {
    "nvim-neorg/neorg",
    config = function()
      require("config.org").post()
    end,
    cmd = "Neorg",
    ft = "norg",
    after = { "nvim-treesitter" },
    dependencies = { "folke/zen-mode.nvim" },
  },
  { "wbthomason/packer.nvim" },
  { "nvim-lua/plenary.nvim" },
  { "antoinemadec/FixCursorHold.nvim" },
  {
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require("config.indentline").post()
    end,
  },
  -- { "Vimjas/vim-python-pep8-indent" },
  { "nvimtools/none-ls.nvim" },
  { "folke/noice.nvim",               dependencies = { "MunifTanjim/nui.nvim" } },
  {
    "nvim-neotest/neotest",
    dir = maybe_local("neotest"),
    keys = { "<leader>n" },
    config = function()
      require("config.neotest").post()
    end,
    dependencies = {
      { "andythigpen/nvim-coverage" },
      { "nvim-neotest/nvim-nio",        dir = maybe_local("nvim-nio") },
      { "nvim-neotest/neotest-python",  dir = maybe_local("neotest-python") },
      { "nvim-neotest/neotest-plenary", dir = maybe_local("neotest-plenary") },
      { "marilari88/neotest-vitest" },
      { "rouge8/neotest-rust" },
      {
        "mfussenegger/nvim-dap",
        lazy = true,
        config = function()
          require("config.dap").post()
        end,
      },
      { "mfussenegger/nvim-dap-python" },
      { "rcarriga/nvim-dap-ui",        dir = maybe_local("nvim-dap-ui") },
    },
  },
  {
    "rcarriga/nvim-notify",
    dir = maybe_local("nvim-notify"),
    config = function()
      require("config.notify").post()
    end,
  },
  {
    "lewis6991/gitsigns.nvim",
    dependencies = {
      "sindrets/diffview.nvim",
      "ruifm/gitlinker.nvim",
      "TimUntersberger/neogit",
    },
    config = function()
      require("config.git").post()
    end,
  },
  {
    "iamcco/markdown-preview.nvim",
    build = "cd app && npm install",
    init = function()
      vim.g.mkdp_filetypes = { "markdown" }
    end,
  },
  { "MTDL9/vim-log-highlighting" },
  {
    "glepnir/galaxyline.nvim",
    config = function()
      require("config.galaxyline").post()
    end,
  },
  { "nvim-tree/nvim-web-devicons" },
  { "godlygeek/tabular",          cmd = "Tabularize" },
  {
    "danymat/neogen",
    cmd = "Neogen",
    config = function()
      require("config.docs").post()
    end,
  },
  {
    "kyazdani42/nvim-tree.lua",
    keys = "<leader>x",
    config = function()
      require("config.filetree").post()
    end,
  },
  { "machakann/vim-sandwich",  keys = { "sa", "sr", "sd" } },
  {
    "neovim/nvim-lspconfig",
    config = function()
      require("config.lsp").post()
    end,
    dependencies = {
      "folke/trouble.nvim",
      "nvim-lua/lsp-status.nvim",
      "glepnir/lspsaga.nvim",
      "folke/lua-dev.nvim",
      "simrat39/rust-tools.nvim",
      "stevearc/aerial.nvim",
    },
  },
  { "rhysd/git-messenger.vim", keys = "<leader>gm" },
  {
    "uga-rosa/ccc.nvim",
    config = function()
      require("config.colourpicker").post()
    end,
  },
  { "simnalamburt/vim-mundo",   cmd = "MundoToggle" },
  { "svermeulen/vim-subversive" },
  {
    "JoosepAlviste/nvim-ts-context-commentstring",
    dependencies = { "tpope/vim-commentary" },
  },
  { "tpope/vim-abolish" },
  { "tpope/vim-eunuch",      cmd = { "Rename", "Delete", "Remove", "Chmod" } },
  { "voldikss/vim-floaterm", cmd = "FloatermNew" },
  { "wellle/targets.vim",    dependencies = { "wellle/line-targets.vim" } },
  {
    "ibhagwan/fzf-lua",
    config = function()
      require("config.fuzzy").post()
    end,
  },
  { "hiberabyss/nvim-dbg" },
  { "someone-stole-my-name/yaml-companion.nvim" },

  {
    "nvim-treesitter/nvim-treesitter",
    config = function()
      require("config.treesitter").post()
    end,
    dependencies = {
      { "nvim-treesitter/nvim-treesitter-textobjects" },
      { "nvim-treesitter/nvim-treesitter-refactor" },
      { "mfussenegger/nvim-ts-hint-textobject" },
      { "mizlan/iswap.nvim" },
    },
  },
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      { "rafamadriz/friendly-snippets" },
      { "petertriho/cmp-git" },
      { "zbirenbaum/copilot.lua" },
      { "zbirenbaum/copilot-cmp" },
      { "onsails/lspkind-nvim" },
      { "hrsh7th/cmp-nvim-lsp" },
      { "hrsh7th/cmp-buffer" },
      { "hrsh7th/cmp-cmdline" },
      { "hrsh7th/cmp-path" },
      { "rcarriga/cmp-dap",            dir = maybe_local("cmp-dap") },
      { "L3MON4D3/LuaSnip" },
      {
        "saadparwaiz1/cmp_luasnip",
      },
    },
    config = function()
      require("config.completion").post()
    end,
  },
}
require("lazy").setup(plugins, {
  ui = {
    border = vim.g.border_chars,
  },
  install = {
    colorscheme = {},
  },
})
