local M = {}

function M.settings()
  local shfmt = {formatCommand = "shfmt -ci -s -bn", formatStdin = true}
  local prettier = {
    formatStdin = true,
    formatCommand = ([[
        prettier
        ${--config-precedence:configPrecedence}
        ${--tab-width:tabWidth}
        ${--single-quote:singleQuote}
        ${--trailing-comma:trailingComma}
    ]]):gsub(
      "\n",
      ""
    )
  }
  local cai_lint = {
    lintCommand = [[cai-lint]],
    lintStdin = false,
    lintFormats = {"\\w* %l:%c\\s*%m"}
  }
  return {
    rootMarkers = {".git/"},
    languages = {
      javascript = {prettier},
      html = {prettier},
      css = {prettier},
      vue = {prettier},
      typescript = {prettier},
      zsh = {shfmt},
      bash = {shfmt},
      sh = {shfmt},
      yaml = {prettier},
      -- json = {prettier},
      go = {
        {formatCommand = "goimports", formatStdin = true},
        {formatCommand = "gofumpt", formatStdin = true}
      },
      lua = {
        {
          formatCommand = "stylua -",
          formatStdin = true
        }
      },
      python = {
        -- {
        --   lintCommand = "pydocstyle ${INPUT}",
        --   lintStdin = false,
        --   lintIgnoreExitCode = true,
        --   lintFormats = {"%I%f:%l %.%#:", "%Z%*\\sD%n: %m"}
        -- },
        {
          formatCommand = "black --quiet -",
          formatStdin = true
        },
        {
          formatCommand = "isort --quiet -",
          formatStdin = true
        }
      }
    }
  }
end

return M
