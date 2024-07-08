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
    "gorbit99/codewindow.nvim",
    config = function()
      local codewindow = require("codewindow")
      codewindow.setup()
      codewindow.apply_default_keybinds()
    end,
  },
  {
    "williamboman/mason.nvim",
    branch = "main",
    dependencies = { "williamboman/mason-lspconfig.nvim" },
  },
  { "echasnovski/mini.nvim" },
  { "wbthomason/packer.nvim" },
  { "nvim-lua/plenary.nvim" },
  {
    "antoinemadec/FixCursorHold.nvim",
    init = function()
      vim.g.cursorhold_updatetime = 50
    end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require("ibl").setup({
        indent = { char = "│" },
        exclude = {
          buftypes = {
            "terminal",
          },
          filetypes = {
            "",
            "norg",
            "help",
            "markdown",
            "dapui_scopes",
            "dapui_stacks",
            "dapui_watches",
            "dapui_breakpoints",
            "dapui_hover",
            "dap-repl",
            "LuaTree",
            "dbui",
            "term",
            "fugitive",
            "fugitiveblame",
            "NvimTree",
            "packer",
            "neotest-summary",
            "Outline",
            "lsp-installer",
            "mason",
          },
        },
      })
    end,
  },
  -- { "Vimjas/vim-python-pep8-indent" },
  { "nvimtools/none-ls.nvim" },
  { "folke/noice.nvim",      dependencies = { "MunifTanjim/nui.nvim" } },
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
      "seanbreckenridge/gitsigns-yadm.nvim",
      { "TimUntersberger/neogit" },
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
      vim.g.mkdp_browser = "firefox"
      vim.g.mkdp_auto_start = 0
      vim.g.mkdp_auto_close = 0
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
    "kyazdani42/nvim-tree.lua",
    keys = {{ "<leader>x", "<CMD>NvimTreeToggle<CR>" }},
    config = function()
      require("nvim-tree").setup({
        view = {
          width = 45,
        },
        update_focused_file = {
          enable = true,
        },
        renderer = {
          indent_markers = {
            enable = true,
          },
        },
      })
    end,
  },
  { "machakann/vim-sandwich", keys = { "sa", "sr", "sd" } },
  {
    "neovim/nvim-lspconfig",
    config = function()
      require("config.lsp").post()
    end,
    keys = {
      {
        "<leader>td",
        "<cmd>Trouble diagnostics toggle<cr>",
        desc = "Diagnostics (Trouble)",
      },
      {
        "<leader>tD",
        "<cmd>Trouble diagnostics toggle filter.buf=0<cr>",
        desc = "Buffer Diagnostics (Trouble)",
      },
      {
        "<leader>ts",
        "<cmd>Trouble symbols toggle<cr>",
        desc = "Symbols (Trouble)",
      },
      {
        "<leader>tp",
        "<cmd>Trouble lsp toggle win.size=0.3 win.position=right<cr>",
        desc = "LSP Definitions / references / ... (Trouble)",
      },
      {
        "<leader>tl",
        "<cmd>Trouble loclist toggle<cr>",
        desc = "Location List (Trouble)",
      },
      {
        "<leader>tq",
        "<cmd>Trouble qflist toggle<cr>",
        desc = "Quickfix List (Trouble)",
      },
    },
    dependencies = {
      {
        "folke/trouble.nvim",
        "nvim-lua/lsp-status.nvim",
        "glepnir/lspsaga.nvim",
        "folke/lua-dev.nvim",
        "simrat39/rust-tools.nvim",
        "stevearc/aerial.nvim",
      },
    },
  },
  {
    "rhysd/git-messenger.vim",
    keys = "<leader>gm",
    init = function()
      vim.g.git_messenger_floating_win_opts = {
        border = vim.g.border_chars,
      }
    end,
  },
  {
    "uga-rosa/ccc.nvim",
    config = function()
      require("ccc").setup({
        highlighter = {
          auto_enable = true,
        },
      })
    end,
  },
  {
    "simnalamburt/vim-mundo",
    cmd = "MundoToggle",
    init = function()
      vim.g.mundo_right = 1
    end,
    keys = {
      { "<leader>u", "<cmd>MundoToggle<cr>" },
    },
  },
  {
    "svermeulen/vim-subversive",
    keys = {
      { "<leader>s",  "<plug>(SubversiveSubstituteRange)" },
      { "<leader>s",  "<plug>(SubversiveSubstituteRange)" },
      { "<leader>ss", "<plug>(SubversiveSubstituteWordRange)" },
      { "x",          "<plug>(SubversiveSubstitute)" },
      { "xx",         "<plug>(SubversiveSubstituteLine)" },
      { "X",          "<plug>(SubversiveSubstituteToEndOfLine)" },
    },
  },
  {
    "JoosepAlviste/nvim-ts-context-commentstring",
    dependencies = { "tpope/vim-commentary" },
  },
  { "tpope/vim-abolish" },
  { "tpope/vim-eunuch",       cmd = { "Rename", "Delete", "Remove", "Chmod" } },
  {
    "voldikss/vim-floaterm",
    cmd = "FloatermNew",
    init = function()
      vim.g.floaterm_autoclose = 0
    end,
  },
  { "wellle/targets.vim",                       dependencies = { "wellle/line-targets.vim" } },
  {
    "ibhagwan/fzf-lua",
    config = function()
      vim.cmd([[
        inoremap <c-x><c-f> <cmd>lua require("fzf-lua").complete_path()<cr>
        tunmap <C-h>
        tunmap <C-j>
        tunmap <C-k>
        tunmap <C-l>
      ]])
      local fzf = require("fzf-lua")
      fzf.setup({
        fzf_colors = true,
        winopts = {
          border = vim.g.border_chars,
        },
      })
      fzf.register_ui_select({ winopts = { height = 0.4, width = 0.4 } })
    end,
    keys = {
      { "<leader>df", "<cmd>FzfLua files<cr>" },
      { "<leader>dg", "<cmd>FzfLua grep<cr>" },
      { "<leader>db", "<cmd>FzfLua buffers<cr>" },
      { "<leader>dh", "<cmd>FzfLua help_tags<cr>" },
      { "<leader>dc", "<cmd>FzfLua files cmd=yadm\\ ls-files cwd=~/<cr>" },
    },
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
