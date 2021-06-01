local M = {}

function M.settings()
  local shfmt = {formatCommand = "shfmt -ci -s -bn", formatStdin = true}
  local prettier = {
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
      json = {prettier},
      go = {
        {formatCommand = "goimports", formatStdin = true},
        {formatCommand = "gofumpt", formatStdin = true}
      },
      lua = {
        {
          formatCommand = "luafmt -i 2 -l 120 --stdin",
          formatStdin = true
        }
      },
      python = {
        {
          lintCommand = "pydocstyle ${INPUT}",
          lintStdin = false,
          lintIgnoreExitCode = true,
          lintFormats = {"%I%f:%l %.%#:", "%Z%*\\sD%n: %m"}
        },
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
