local M = {}

function M.post()
  require("neorg").setup({
    load = {
      ["core.defaults"] = {},
      ["core.highlights"] = {},
      ["core.integrations.treesitter"] = {},
      ["core.norg.concealer"] = {},
      ["core.norg.dirman"] = {
        config = {
          workspaces = {
            my_workspace = "~/.local/share/neorg",
          },
        },
      },
    },
  })
end

return M
