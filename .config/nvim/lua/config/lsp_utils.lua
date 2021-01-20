local border_chars = {
    TOP_LEFT = "┌",
    TOP_RIGHT = "┐",
    MID_HORIZONTAL = "─",
    MID_VERTICAL = "│",
    BOTTOM_LEFT = "└",
    BOTTOM_RIGHT = "┘"
}
vim.g.lsp_utils_location_opts = {
    height = 24,
    mode = "editor",
    preview = {
        title = "Location Preview",
        border = true,
        border_chars = border_chars
    },
    list = {
        title = "Location List",
        border = true,
        border_chars = border_chars
    },
    keymaps = {
        n = {
            ["<C-n>"] = "j",
            ["<C-p>"] = "k"
        }
    }
}
vim.g.lsp_utils_symbols_opts = {
    height = 24,
    mode = "editor",
    preview = {
        title = "Symbols Preview",
        border = true,
        border_chars = border_chars
    },
    prompt = {}
}
