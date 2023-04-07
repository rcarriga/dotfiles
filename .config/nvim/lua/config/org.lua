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
    require("zen-mode").setup({
      window = {
        backdrop = 1,
        width = 140,
        height = 1,
        options = {
          wrap = true,
          signcolumn = "no",
          number = false,
          relativenumber = false,
          cursorline = false,
          cursorcolumn = false,
          foldcolumn = "0",
          list = false,
        },
      },
      plugins = {
        options = {
          enabled = true,
          ruler = false,
          showcmd = false,
        },
        twilight = { enabled = true },
        gitsigns = { enabled = false },
        kitty = {
          enabled = true,
          font = "+4",
        },
      },
    })
    require("neorg").setup({
      load = {
        ["core.defaults"] = {},
        ["core.keybinds"] = {},
        ["core.norg.dirman"] = {
          config = {
            workspaces = {
              notes = "~/org/notes",
              journal = "~/org/journal",
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
              ),
            },
          },
        },
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
      },
    })

    require("colors").set()
  end)
end

return M
