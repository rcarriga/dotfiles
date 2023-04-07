local M = {}

function M.post()
  local fzf = require("fzf-lua")
  local actions = require("fzf-lua.actions")
  fzf.setup({
    winopts = {
      border = vim.g.border_chars,
    },
  })
  fzf.register_ui_select({ winopts = { height = 0.2, width = 0.2 } })

  local keys = {
    ["<leader>df"] = fzf.files,
    ["<leader>dg"] = fzf.grep,
    ["<leader>db"] = fzf.buffers,
    ["<leader>dh"] = fzf.help_tags,
    ["<leader>dc"] = function()
      fzf.files({ cmd = "yadm ls-files", cwd = vim.env.HOME })
    end,
    ["<leader>dn"] = function()
      fzf.files({
        cwd = "~/org/notes",
        actions = {
          ["default"] = function (selected, opts)
            actions.file_edit(selected, opts)
          end,
        },
      })
    end,
  }

  for key, map in pairs(keys) do
    vim.api.nvim_set_keymap("n", key, "", {
      callback = map,
      noremap = true,
      silent = true,
    })
  end
end

return M
