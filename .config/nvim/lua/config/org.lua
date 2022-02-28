local M = {}

function M.post()
  pcall(function()
  vim.cmd([[highlight Headline1 guibg=#1e2718]])
  vim.cmd([[highlight Headline2 guibg=#21262d]])
  vim.cmd([[highlight CodeBlock guibg=#1c1c1c]])
  vim.cmd([[highlight Dash guibg=#D19A66 gui=bold]])
  vim.fn.sign_define("Headline1", { linehl = "Headline1" })
  vim.fn.sign_define("Headline2", { linehl = "Headline2" })
  pcall(require('orgmode').setup_ts_grammar)

  require("headlines").setup({
    org = {
      headline_signs = { "Headline1", "Headline2" },
    },
    markdown = false,
  })
  require("org-bullets").setup({
    symbols = { "◉", "○", "✸", "✿" },
  })
  require("orgmode").setup({
    org_agenda_files = { "~/Notes/*" },
    org_default_notes_file = "~/Notes/notes.org",
  })
end)
end

return M
