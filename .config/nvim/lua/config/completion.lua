local M = {}

function M.post()
  local luasnip = require("luasnip")

  luasnip.config.set_config({
    history = true,
    ext_base_prio = 200,
    ext_prio_increase = 2,
    enable_autosnippets = true,
  })
  -- require("luasnip.loaders.from_lua").load({ paths = vim.fn.stdpath("config") .. "/snippets" })
  -- require("luasnip.loaders.from_vscode").lazy_load()

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
  vim.api.nvim_set_keymap("i", "<S-Tab>", "", { callback = s_tab_complete, expr = true })
  vim.api.nvim_set_keymap("s", "<S-Tab>", "", { callback = s_tab_complete, expr = true })
  vim.api.nvim_set_keymap("i", "<C-E>", "<Plug>luasnip-next-choice", {})
  vim.api.nvim_set_keymap("s", "<C-E>", "<Plug>luasnip-next-choice", {})

  local cmp = require("cmp")

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

  local function is_auto_import(item)
    return item.data and item.data.autoImportText
  end

  ---@type cmp.ConfigSchema
  local args = {
    performance = {
      debounce = 30,
      throttle = 10,
      fetching_timeout = 100,
    },
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
        function(a, b)
          local a_under = select(2, a.completion_item.label:find("^_+")) or 0
          local b_under = select(2, b.completion_item.label:find("^_+")) or 0
          if a_under == b_under then
            return nil
          end
          return a_under < b_under
        end,
        cmp.config.compare.score,
        cmp.config.compare.kind,
        cmp.config.compare.sort_text,
        cmp.config.compare.length,
        cmp.config.compare.order,
      },
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
      ["<CR>"] = cmp.mapping.confirm({}),
      ["<C-n>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "c" }),
      ["<C-p>"] = cmp.mapping(cmp.mapping.select_prev_item(), { "i", "c" }),
    },
    sources = cmp.config.sources({
      { name = "nvim_lsp" },
      { name = "git" },
      { name = "luasnip" },
      { name = "path" },
    }),
    view = {
      entries = "custom",
    },
    enabled = function()
      return vim.api.nvim_buf_get_option(0, "buftype") ~= "prompt"
        or require("cmp_dap").is_dap_buffer()
    end,
  }

  cmp.setup(args)
  cmp.setup.filetype({ "dap-repl", "dapui_watches" }, {
    sources = {
      { name = "dap" },
    },
  })

  -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
  -- cmp.setup.cmdline(":", {
  --   sources = cmp.config.sources({
  --     { name = "cmdline" },
  --     { name = "path" },
  --   }, {}),
  -- })

end

return M
