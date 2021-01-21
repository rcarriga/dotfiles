local M = {}

function M.post()
    require "nvim-treesitter.configs".setup {
        highlight = {
            enable = true
        },
        indent = {
            enable = true
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
            highlight_definitions = {enable = true},
            highlight_current_scope = {enable = true}
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
                    ["g>"] = "@parameter.inner"
                },
                swap_previous = {
                    ["g<"] = "@parameter.inner"
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
