require "format".setup {
    ["*"] = {
        {cmd = {"sed -i 's/[ \t]*$//'"}} -- remove trailing whitespace
    },
    lua = {
        {
            cmd = {
                function(file)
                    return string.format("luafmt -l %s -w replace %s", 120, file)
                end
            }
        }
    },
    go = {
        {
            cmd = {"gofumpt -w", "goimports -w"},
            tempfile_postfix = ".tmp"
        }
    },
    javascript = {
        {cmd = {"prettier -w", "./node_modules/.bin/eslint --fix"}}
    },
    json = {
        {cmd = {"prettier -w"}}
    },
    yaml = {
        {cmd = {"prettier -w"}}
    },
    typescript = {
        {cmd = {"prettier -w", "./node_modules/.bin/eslint --fix"}}
    },
    markdown = {
        {cmd = {"prettier -w"}},
        {
            cmd = {"black"},
            start_pattern = "^```python$",
            end_pattern = "^```$",
            target = "current"
        }
    },
    python = {
        {cmd = {"./scripts/style -w", "./scripts/code-style -w", "black"}}
    }
}
