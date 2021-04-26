local M = {}

function M.post()
  require "nvim-treesitter.configs".setup {
    highlight = {
      enable = true
    },
    indent = {
      enable = false
    },
    incremental_selection = {
      enable = true,
      keymaps = {
        init_selection = "gl",
        node_incremental = "<Down>",
        node_decremental = "<Up>"
      }
    },
    refactor = {
      highlight_definitions = {enable = true}
    },
    context_commentstring = {
      enable = true
    },
    textobjects = {
      select = {
        enable = true,
        keymaps = {
          ["af"] = "@function.outer",
          ["if"] = "@function.inner",
          ["ac"] = "@class.outer",
          ["ic"] = "@class.inner",
          ["ab"] = "@block.outer",
          ["ib"] = "@block.inner"
        }
      },
      swap = {
        enable = true,
        swap_next = {
          ["sp"] = "@parameter.inner",
          ["sf"] = "@function.outer",
          ["sc"] = "@class.outer",
          ["ss"] = "@statement.outer",
          ["sb"] = "@block.outer"
        },
        swap_previous = {
          ["sP"] = "@parameter.inner",
          ["sF"] = "@function.outer",
          ["sC"] = "@class.outer",
          ["sS"] = "@statement.outer",
          ["sB"] = "@block.outer"
        }
      },
      move = {
        enable = true,
        goto_next_start = {
          ["]f"] = "@function.outer",
          ["]]"] = "@class.outer"
        },
        goto_next_end = {
          ["]F"] = "@function.outer",
          ["]["] = "@class.outer"
        },
        goto_previous_start = {
          ["[f"] = "@function.outer",
          ["[["] = "@class.outer"
        },
        goto_previous_end = {
          ["[F"] = "@function.outer",
          ["[]"] = "@class.outer"
        }
      }
    }
  }
end

return M
