local M = {}

function M.post()
  pcall(function()
    require("neorg").setup({
      load = {
        ["core.defaults"] = {},
        ["core.norg.dirman"] = {
          config = {
            workspaces = {
              work = "~/notes/work",
              home = "~/notes/home",
              example_gtd = "~/Dev/repos/example_workspaces/gtd",
            },
          },
        },
        ["core.norg.completion"] = {
          config = {
            engine = "nvim-cmp",
          },
        },
        ["core.norg.concealer"] = {},
        ["core.integrations.telescope"] = {},
        ["core.export"] = {
          config = {
            export_dir = "~/notes/export",
          },
        },
        ["core.presenter"] = { config = {
          zen_mode = "truezen",
        } },
        ["core.export.markdown"] = {},
        ["core.norg.qol.toc"] = {},
        ["core.norg.journal"] = {},
        ["core.gtd.base"] = {
          config = {
            workspace = "example_gtd",
          },
        },
      },
    })
  end)
end

return M
