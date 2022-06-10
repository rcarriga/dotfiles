local M = {}

function M.post()
    vim.api.nvim_set_keymap("n", "<leader>o", "", {
      noremap = true,
      silent = true,
      callback = function()
        vim.cmd("NeorgStart")
        vim.api.nvim_del_keymap("n", "<leader>o")
        vim.api.nvim_set_keymap(
          "n",
          "<leader>oj",
          "<cmd>Neorg journal today<CR>",
          { silent = true, noremap = true }
        )
      end,
    })
  pcall(function()
    require("neorg").setup({
      load = {
        ["core.defaults"] = {},
        ["core.keybinds"] = {
          config = {
            hook = function(keybinds)
              keybinds.map_event_to_mode("norg", {
                n = {
                  { "<M-k>", "core.norg.manoeuvre.item_up" },
                  { "<M-j>", "core.norg.manoeuvre.item_down" },
                  { "ah", "core.norg.manoeuvre.textobject.around-heading" },
                  { "ih", "core.norg.manoeuvre.textobject.inner-heading" },
                  { "at", "core.norg.manoeuvre.textobject.around-tag" },
                  { "it", "core.norg.manoeuvre.textobject.inner-tag" },
                  { "al", "core.norg.manoeuvre.textobject.around-whole-list" },
                  { "al", "core.norg.manoeuvre.textobject.around-whole-list" },
                },
              }, {
                silent = true,
                noremap = true,
              })
              keybinds.map_to_mode("norg", {
                n = {
                  { "<localleader>c", "<cmd>Neorg toc split<CR>" },
                },
              }, {
                silent = true,
                noremap = true,
              })
            end,
          },
        },
        ["core.norg.dirman"] = {
          config = {
            workspaces = {
              notes = "~/org/notes",
              journal = "~/org/journal",
              gtd = "~/org/gtd",
            },
          },
        },
        ["core.norg.manoeuvre"] = {},
        ["core.norg.completion"] = {
          config = {
            engine = "nvim-cmp",
          },
        },
        ["core.norg.concealer"] = {
          config = {
            completion_level = {
              enabled = true,

              queries = vim.tbl_deep_extend(
                "keep",
                {},
                (function()
                  local result = {}

                  for i = 1, 6 do
                    result["heading" .. i] = {
                      text = {
                        "(",
                        { "<done>", "Info" },
                        " / ",
                        { "<total>", "Info" },
                        ") [",
                        { "<percentage>%", "Success" },
                        "]",
                      },

                      highlight = "DiagnosticVirtualTextHint",
                    }
                  end

                  return result
                end)()
                --[[ (function()
                local result = {}

                for i = 1, 6 do
                    result["todo_item" .. i] = {
                        text = "[<done>/<total>]",
                        highlight = "DiagnosticVirtualTextHint",
                    }
                end

                return result
            end)() ]]
              ),
            },
          },
        },
        -- ["core.integrations.telescope"] = {},
        ["core.export"] = {
          config = {
            export_dir = "~/notes/export",
          },
        },
        ["core.presenter"] = { config = {
          zen_mode = "zen-mode",
        } },
        ["core.export.markdown"] = {},
        ["core.norg.qol.toc"] = {
          config = { keep_buf = true },
        },
        ["core.norg.journal"] = {
          config = {
            workspace = "journal",
          },
        },
        ["core.gtd.base"] = {
          config = {
            workspace = "gtd",
          },
        },
      },
    })
  end)
end

return M
