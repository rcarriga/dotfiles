local M = {}
function M.pre()
  vim.cmd(
    [[ino <silent><expr> <CR>    pumvisible() ? (complete_info().selected == -1 ? "\<C-e><CR>" : "\<C-y>") : "\<CR>"]]
  )

  vim.g.coq_settings = {
    auto_start = "shut-up",
    clients = {
      lsp = {
        weight_adjust = 2,
      },
      tree_sitter = {
        weight_adjust = -1,
      },
      buffers = {
        weight_adjust = -0.5,
      }
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
  require("lspkind").init({ with_text = false })
  require("cmp_nvim_lsp").setup({})
  local cmp = require("cmp")
  cmp.setup({
    snippet = {
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body)
      end,
    },

    documentation = {
      border = vim.g.border_chars,
      winhighlight = "NormalFloat:CmpDocumentation,FloatBorder:CmpDocumentationBorder",
    },

    mapping = {
      ["<C-p>"] = cmp.mapping.prev_item(),
      ["<C-n>"] = cmp.mapping.next_item(),
      ["<C-d>"] = cmp.mapping.scroll(-4),
      ["<C-f>"] = cmp.mapping.scroll(4),
      ["<C-Space>"] = cmp.mapping.complete(),
      ["<C-e>"] = cmp.mapping.close(),
      ["<CR>"] = cmp.mapping.confirm({
        behavior = cmp.ConfirmBehavior.Replace,
        select = true,
      }),
    },
    sources = {
      { name = "nvim_lsp" },
      { name = "vsnip" },
      { name = "buffer" },
      { name = "calc" },
    },
  })
end
return M
