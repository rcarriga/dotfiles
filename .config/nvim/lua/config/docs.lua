local M = {}

function M.post()
  local i = require("neogen.types.template").item

  local untyped_reST = {
    { nil, '""" $1 """', { no_results = true, type = { "class", "func" } } },
    { nil, '"""$1', { no_results = true, type = { "file" } } },
    { nil, "", { no_results = true, type = { "file" } } },
    { nil, "$1", { no_results = true, type = { "file" } } },
    { nil, '"""', { no_results = true, type = { "file" } } },
    { nil, "", { no_results = true, type = { "file" } } },

    { nil, "# $1", { no_results = true, type = { "type" } } },

    { nil, '"""$1' },
    { nil, "" },
    {
      i.Parameter,
      ":param %s: $1",
      { type = { "func" } },
    },
    {
      { i.Parameter, i.Type },
      ":param %s: $1",
      {
        required = "typed_parameters",
        type = { "func" },
      },
    },
    { i.ClassAttribute, ":param %s: $1" },
    { i.HasReturn, ":return: $1", { type = { "func" } } },
    { nil, '"""' },
  }

  require("neogen").setup({
    enabled = true,
    languages = {
      python = {
        template = {
          annotation_convention = "untyped_reST",
          untyped_reST = untyped_reST,
        },
      },
    },
  })
end

return M
