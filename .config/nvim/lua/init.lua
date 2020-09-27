  require'nvim-treesitter.configs'.setup {
    ensure_installed = "typescript",
    disable = { "python" },
    highlight = {
      enable = true,
      custom_captures = {
        ["keyword"] = "BuiltIn",
        ["punctuation.bracket"] = "Decoration",
        ["type"] = "TypeName",
        ["type.builtIn"] = "BuiltIn",
        ["function"] = "FuncName",
        ["property"] = "Key",
        ["string"] = "String",
      },
    },
    incremental_selection = {
      enable = true,
      keymaps = {
        init_selection = "gl",
        node_incremental = "<Down>",
        node_decremental = "<Up>",
      },
    },
    refactor = {
      highlight_definitions = { enable = true },
    },
    textobjects = {
      select = {
        enable = true,
        keymaps = {
          ["af"] = "@function.outer",
          ["if"] = "@function.inner",
          ["ac"] = "@class.outer",
          ["ic"] = "@class.inner",
        },
      },
      swap = {
        enable = true,
        swap_next = {
          ["<leader>fn"] = "@function.outer",
        },
        swap_previous = {
          ["<leader>fp"] = "@function.outer",
        },
      },
      move = {
        enable = true,
        goto_next_start = {
          ["]f"] = "@function.outer",
          ["]]"] = "@class.outer",
        },
        goto_next_end = {
          ["]F"] = "@function.outer",
          ["]["] = "@class.outer",
        },
        goto_previous_start = {
          ["[f"] = "@function.outer",
          ["[["] = "@class.outer",
        },
        goto_previous_end = {
          ["[F"] = "@function.outer",
          ["[]"] = "@class.outer",
        },
      },
    },
  }
