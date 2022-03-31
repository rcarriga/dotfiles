local M = {}

function M.post()
  require("luasnip.loaders.from_vscode").lazy_load()

  local luasnip = require("luasnip")

  local t = function(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
  end

  local tab_complete = function()
    if luasnip.expand_or_jumpable() then
      return t("<Plug>luasnip-expand-or-jump")
    end
    return t("<Tab>")
  end
  local s_tab_complete = function()
    if luasnip.jumpable(-1) then
      return t("<Plug>luasnip-jump-prev")
    end
    return t("<S-Tab>")
  end

  vim.api.nvim_set_keymap("i", "<Tab>", "", { callback = tab_complete, expr = true })
  vim.api.nvim_set_keymap("s", "<Tab>", "", { callback = tab_complete, expr = true })
  vim.api.nvim_set_keymap("i", "<S-Tab>", "", { callback = s_tab_complete, expr = true})
  vim.api.nvim_set_keymap("s", "<S-Tab>", "", { callback = s_tab_complete, expr = true })
  vim.api.nvim_set_keymap("i", "<C-E>", "<Plug>luasnip-next-choice", {})
  vim.api.nvim_set_keymap("s", "<C-E>", "<Plug>luasnip-next-choice", {})
  -- Setup nvim-cmp.
  local cmp = require("cmp")
  local types = require("cmp.types")

  local cmp_kinds = {
    Text = "  ",
    Method = "  ",
    Function = "  ",
    Constructor = "  ",
    Field = "  ",
    Variable = "  ",
    Class = "  ",
    Interface = "  ",
    Module = "  ",
    Property = "  ",
    Unit = "  ",
    Value = "  ",
    Enum = "  ",
    Keyword = "  ",
    Snippet = "  ",
    Color = "  ",
    File = "  ",
    Reference = "  ",
    Folder = "  ",
    EnumMember = "  ",
    Constant = "  ",
    Struct = "  ",
    Event = "  ",
    Operator = "  ",
    TypeParameter = "  ",
  }

  require("cmp_git").setup({})

  cmp.setup({
    window = {
      completion = {
        border = vim.g.border_chars,
      },
      documentation = {
        border = vim.g.border_chars,
      },
    },
    snippet = {
      expand = function(args)
        require("luasnip").lsp_expand(args.body)
      end,
    },
    sorting = {
      comparators = {
        cmp.config.compare.offset,
        cmp.config.compare.exact,
        cmp.config.compare.score,
        require("cmp-under-comparator").under,
        cmp.config.compare.kind,
        cmp.config.compare.sort_text,
        cmp.config.compare.length,
        cmp.config.compare.order,
      },
    },
    formatting = {
      fields = { "kind", "abbr" },
      format = function(_, vim_item)
        vim_item.kind = cmp_kinds[vim_item.kind] or ""
        return vim_item
      end,
    },
    experimental = {
      ghost_text = {
        hl_group = "Comment",
      },
    },
    mapping = {
      ["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
      ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
      ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
      ["<C-e>"] = cmp.mapping({
        i = cmp.mapping.abort(),
        c = cmp.mapping.close(),
      }),
      ["<CR>"] = cmp.mapping.confirm({ select = true }),
    },
    sources = cmp.config.sources({
      { name = "copilot" },
      { name = "nvim_lsp" },
      { name = "cmp_git" },
      { name = "luasnip" },
      { name = "orgmode" },
    }, {
      { name = "buffer" },
    }),
    view = {
      entries = "custom",
    },
  })

  -- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
  -- cmp.setup.cmdline("/", {
  --   sources = {
  --     { name = "buffer" },
  --   },
  -- })

  -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
  -- cmp.setup.cmdline(":", {
  --   sources = cmp.config.sources({
  --     { name = "path" },
  --   }, {
  --     { name = "cmdline" },
  --   }),
  -- })
end
return M
