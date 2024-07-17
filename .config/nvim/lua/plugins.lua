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
  { "nvimtools/none-ls.nvim" },
  {
    "folke/noice.nvim",
    config = function()
      vim.defer_fn(function()
        require("noice").setup({
          cmdline = {
            enabled = false,
          },
          messages = {
            enabled = false,
          },
          popupmenu = {
            enabled = false,
          },
          commands = {},
          notify = {
            enabled = false,
          },
          lsp = {
            progress = {
              enabled = false,
            },
            override = {
              ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
              ["vim.lsp.util.stylize_markdown"] = true,
              ["cmp.entry.get_documentation"] = true,
            },
          },
          presets = {
            lsp_doc_border = true,
          },
        })
      end, 1000)
    end,
    dependencies = { "MunifTanjim/nui.nvim" },
  },
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
      local base_stages = require("notify.stages.slide")("bottom_up")
      local notify = require("notify")

      notify.setup({
        render = "wrapped-compact",
        stages = {
          function(...)
            local opts = base_stages[1](...)
            if not opts then
              return
            end
            return opts
          end,
          unpack(base_stages, 2),
        },
        background_colour = "#121212",
        max_width = 80,
        on_open = function(win)
          vim.api.nvim_set_option_value("wrap", true, { win = win })
        end,
      })

      vim.api.nvim_set_keymap("n", "<leader>p", "", { callback = notify.dismiss })
    end,
  },
  {
    {
      "lewis6991/gitsigns.nvim",
      event = "VeryLazy",
      config = function()
        require("gitsigns").setup({
          signs = {
            add = { text = "┃" },
            change = {
              text = "┃",
            },
            delete = {
              text = "┃",
            },
            topdelete = {
              text = "┳",
            },
            changedelete = {
              text = "~",
            },
          },
          _on_attach_pre = function(_, callback)
            require("gitsigns-yadm").yadm_signs(callback)
          end,
        })
      end,
      keys = {
        {
          "]h",
          "&diff ? ']h' : '<cmd>lua require\"gitsigns.actions\".next_hunk()<CR>'",
          expr = true,
        },
        {
          "[h",
          "&diff ? '[h' : '<cmd>lua require\"gitsigns.actions\".prev_hunk()<CR>'",
          expr = true,
        },
        { "<leader>hs", '<cmd>lua require"gitsigns".stage_hunk()<CR>' },
        { "<leader>hu", '<cmd>lua require"gitsigns".undo_stage_hunk()<CR>' },
        { "<leader>hr", '<cmd>lua require"gitsigns".reset_hunk()<CR>' },
        { "<leader>hR", '<cmd>lua require"gitsigns".reset_buffer()<CR>' },
        { "<leader>hp", '<cmd>lua require"gitsigns".preview_hunk()<CR>' },
        { "<leader>hb", '<cmd>lua require"gitsigns".blame_line({full = true})<CR>' },
        { "<leader>hU", '<cmd>lua require"gitsigns".reset_buffer_index()<CR>' },
        {
          "<leader>hs",
          '<cmd>lua require"gitsigns".stage_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>',
          mode = "v",
        },
        {
          "<leader>hr",
          '<cmd>lua require"gitsigns".reset_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>',
          mode = "v",
        },
        { "ih", ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>', mode = "o" },
        { "ih", ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>', mode = "x" },
      },
    },

    {
      "sindrets/diffview.nvim",
      cmd = { "DiffviewOpen" },
      config = function()
        require("diffview").setup({})
      end,
    },
    {
      "ruifm/gitlinker.nvim",
      keys = "<leader>gy",
      config = function()
        require("gitlinker").setup()
      end,
    },
    "seanbreckenridge/gitsigns-yadm.nvim",
    {
      "aaronhallaert/advanced-git-search.nvim",
      cmd = { "AdvancedGitSearch" },
      config = function()
        require("advanced_git_search.fzf").setup({
          diff_plugin = "diffview",
        })
      end,
    },
    {
      "TimUntersberger/neogit",
      config = function()
        require("neogit").setup({})
      end,
    },
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
    keys = { { "<leader>x", "<CMD>NvimTreeToggle<CR>" } },
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
      { "petertriho/cmp-git" },
      { "hrsh7th/cmp-nvim-lsp" },
      { "rcarriga/cmp-dap",    dir = maybe_local("cmp-dap") },
    },
    config = function()
      local cmp = require("cmp")
      require("cmp_git").setup({})

      cmp.setup({
        performance = {
          debounce = 30,
          throttle = 10,
          fetching_timeout = 100,
        },
        window = {
          completion = {
            border = vim.g.border_chars,
          },
          documentation = {
            border = vim.g.border_chars,
          },
        },
        sorting = {
          comparators = {
            cmp.config.compare.offset,
            cmp.config.compare.exact,
            function(a, b)
              local a_under = select(2, a.completion_item.label:find("^_+")) or 0
              local b_under = select(2, b.completion_item.label:find("^_+")) or 0
              if a_under == b_under then
                return nil
              end
              return a_under < b_under
            end,
            cmp.config.compare.score,
            cmp.config.compare.kind,
            cmp.config.compare.sort_text,
            cmp.config.compare.length,
            cmp.config.compare.order,
          },
        },
        experimental = {
          ghost_text = {
            hl_group = "Comment",
          },
        },
        mapping = {
          ["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
          ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
          ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
          ["<C-e>"] = cmp.mapping({
            i = cmp.mapping.abort(),
            c = cmp.mapping.close(),
          }),
          ["<CR>"] = cmp.mapping.confirm({}),
          ["<C-n>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "c" }),
          ["<C-p>"] = cmp.mapping(cmp.mapping.select_prev_item(), { "i", "c" }),
        },
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "git" },
        }),
        view = {
          entries = "custom",
        },
        enabled = function()
          return vim.api.nvim_get_option_value("buftype", {}) ~= "prompt"
              or require("cmp_dap").is_dap_buffer()
        end,
      })
      cmp.setup.filetype({ "dap-repl", "dapui_watches" }, {
        sources = {
          { name = "dap" },
        },
      })
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
