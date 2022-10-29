local M = {}

function M.post()
  local parser_configs = require("nvim-treesitter.parsers").get_parser_configs()

  parser_configs.norg = {
    install_info = {
      url = "https://github.com/nvim-neorg/tree-sitter-norg",
      files = { "src/parser.c", "src/scanner.cc" },
      branch = "main",
    },
  }

  vim.cmd([[
    omap     <silent> m <cmd><C-U>lua require('tsht').nodes()<CR>
    vnoremap <silent> m <cmd>lua require('tsht').nodes()<CR>
  ]])


  parser_configs.gotmpl = {
    install_info = {
      url = "https://github.com/ngalaiko/tree-sitter-go-template",
      files = { "src/parser.c" },
    },
    filetype = "gotmpl",
    used_by = { "gohtmltmpl", "gotexttmpl", "gotmpl", "yaml" },

  }


  require("nvim-treesitter.configs").setup({
    highlight = {
      enable = true,
    },
    ensure_installed = {},
    indent = { enable = false },
    incremental_selection = {
      enable = true,
      keymaps = {
        init_selection = "gl",
        node_incremental = "<Down>",
        node_decremental = "<Up>",
      },
    },
    refactor = { highlight_definitions = { enable = false } },
    context_commentstring = { enable = true },
    query_linter = {
      enable = true,
      use_virtual_text = true,
      lint_events = { "BufWrite", "CursorHold" },
    },
    textobjects = {
      select = {
        enable = true,
        keymaps = {
          ["af"] = "@function.outer",
          ["if"] = "@function.inner",
          ["ac"] = "@class.outer",
          ["ic"] = "@class.inner",
          ["ab"] = "@block.outer",
          ["ib"] = "@block.inner",
        },
      },
      swap = {
        enable = true,
        swap_next = {
          ["sp"] = "@parameter.inner",
          ["sf"] = "@function.outer",
          ["sc"] = "@class.outer",
          ["ss"] = "@statement.outer",
          ["sb"] = "@block.outer",
        },
        swap_previous = {
          ["sP"] = "@parameter.inner",
          ["sF"] = "@function.outer",
          ["sC"] = "@class.outer",
          ["sS"] = "@statement.outer",
          ["sB"] = "@block.outer",
        },
      },
      move = {
        enable = true,
        goto_next_start = {
          ["]f"] = "@function.outer",
          ["]]"] = "@class.outer",
        },
        goto_next_end = {
          ["]F"] = "@function.outer",
          ["]["] = "@class.outer",
        },
        goto_previous_start = {
          ["[f"] = "@function.outer",
          ["[["] = "@class.outer",
        },
        goto_previous_end = {
          ["[F"] = "@function.outer",
          ["[]"] = "@class.outer",
        },
      },
    },
  })

  vim.cmd([[omap     <silent> m :<C-U>lua require('tsht').nodes()<CR>]])
  vim.cmd([[vnoremap <silent> m :lua require('tsht').nodes()<CR>]])
end

return M
