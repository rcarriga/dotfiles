local M = {}

function M.post()
  local parser_configs = require("nvim-treesitter.parsers").get_parser_configs()

  require("nvim-biscuits").setup({
    -- show_on_start = true, -- defaults to false
    toggle_keybind = "<leader>cb",
    default_config = {
      max_length = 50,
      min_distance = 5,
      prefix_string = " ↳ ",
    },
  })
  vim.cmd("nnoremap <silent> <leader>cb <CMD>lua require('nvim-biscuits').toggle_biscuits()<CR>")

  parser_configs.norg = {
    install_info = {
      url = "https://github.com/nvim-neorg/tree-sitter-norg",
      files = { "src/parser.c", "src/scanner.cc" },
      branch = "main",
    },
  }

  parser_configs.org = {
    install_info = {
      url = "https://github.com/milisims/tree-sitter-org",
      revision = "main",
      files = { "src/parser.c", "src/scanner.cc" },
    },
    filetype = "org",
  }


  vim.cmd([[
    omap     <silent> m <cmd><C-U>lua require('tsht').nodes()<CR>
    vnoremap <silent> m <cmd>lua require('tsht').nodes()<CR>
  ]])

  require("spellsitter").setup({
    -- Whether enabled, can be a list of filetypes, e.g. {'python', 'lua'}
    enable = true,
  })


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
      disable = { "org" }, -- Remove this to use TS highlighter for some of the highlights (Experimental)
      additional_vim_regex_highlighting = { "org" }, -- Required since TS highlighter doesn't support all syntax features (conceal)
    },
    ensure_installed = { "norg", "python", "lua", "javascript", "vue", "html", "css" },
    indent = { enable = false },
    incremental_selection = {
      enable = true,
      keymaps = {
        init_selection = "gl",
        node_incremental = "<Down>",
        node_decremental = "<Up>",
      },
    },
    refactor = { highlight_definitions = { enable = true } },
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
