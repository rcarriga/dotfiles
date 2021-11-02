local M = {}
function M.pre()
  vim.cmd(
    [[ino <silent><expr> <CR>    pumvisible() ? (complete_info().selected == -1 ? "\<C-e><CR>" : "\<C-y>") : "\<CR>"]]
  )

  vim.g.coq_settings = {
    auto_start = "shut-up",
    clients = {
      lsp = {
        weight_adjust = 1,
      },
      snippets = {
        enabled = false,
      },
      tree_sitter = {
        weight_adjust = -1,
      },
      buffers = {
        weight_adjust = -0.5,
        enabled = false,
      },
    },
    keymap = {
      recommended = false,
      jump_to_mark = "<C-s>",
    },
    display = {
      icons = {
        mode = "short",
      },
      preview = {
        positions = { north = 2, south = 3, west = 4, east = 1 },
      },
    },
  }
end
function M.post()
  -- require("coq_3p")({
  --   -- { src = "nvimlua", short_name = "nvim" },
  --   { src = "bc", short_name = "MATH", precision = 6 },
  --   { src = "figlet", short_name = "BIG" },
  -- })

  -- Setup nvim-cmp.
  local cmp = require("cmp")

  cmp.setup({
    snippet = {
      -- REQUIRED - you must specify a snippet engine
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
        -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
        -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
        -- require'snippy'.expand_snippet(args.body) -- For `snippy` users.
      end,
    },
    documentation = {
      border = vim.g.border_chars,
      winhighlight = "NormalFloat:NormalFloat,FloatBorder:FloatBorder"
    },
    mapping = {
      ["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
      ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
      ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
      ["<C-y>"] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
      ["<C-e>"] = cmp.mapping({
        i = cmp.mapping.abort(),
        c = cmp.mapping.close(),
      }),
      ["<CR>"] = cmp.mapping.confirm({ select = true }),
    },
    sources = cmp.config.sources({
      { name = "nvim_lsp" },
      { name = "vsnip" }, -- For vsnip users.
      -- { name = 'luasnip' }, -- For luasnip users.
      -- { name = 'ultisnips' }, -- For ultisnips users.
      -- { name = 'snippy' }, -- For snippy users.
    }, {
      { name = "buffer" },
    }),
  })

  -- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline("/", {
    sources = {
      { name = "buffer" },
    },
  })

  -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline(":", {
    sources = cmp.config.sources({
      { name = "path" },
    }, {
      { name = "cmdline" },
    }),
  })
end
return M
