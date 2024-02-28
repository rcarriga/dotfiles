local M = {}

function M.post()
  require("gitlinker").setup()

  require("gitsigns").setup({
    signs = {
      add = { hl = "GitSignsAdd", text = "┃", numhl = "GitSignsAddNr", linehl = "GitSignsAddLn" },
      change = {
        hl = "GitSignsChange",
        text = "┃",
        numhl = "GitSignsChangeNr",
        linehl = "GitSignsChangeLn",
      },
      delete = {
        hl = "GitSignsDelete",
        text = "┃",
        numhl = "GitSignsDeleteNr",
        linehl = "GitSignsDeleteLn",
      },
      topdelete = {
        hl = "GitSignsDelete",
        text = "┳",
        numhl = "GitSignsDeleteNr",
        linehl = "GitSignsDeleteLn",
      },
      changedelete = {
        hl = "GitSignsChange",
        text = "~",
        numhl = "GitSignsChangeNr",
        linehl = "GitSignsChangeLn",
      },
    },
    signcolumn = false, -- Toggle with `:Gitsigns toggle_signs`
    numhl = true,       -- Toggle with `:Gitsigns toggle_numhl`
    linehl = false,     -- Toggle with `:Gitsigns toggle_linehl`
    word_diff = false,  -- Toggle with `:Gitsigns toggle_word_diff`
    watch_gitdir = {
      interval = 1000,
      follow_files = true,
    },
    attach_to_untracked = false,
    current_line_blame = false,
    current_line_blame_opts = {
      virt_text = true,
      virt_text_pos = "eol", -- 'eol' | 'overlay' | 'right_align'
      delay = 300,
    },
    sign_priority = 3,
    update_debounce = 100,
    preview_config = {
      border = vim.g.border_chars,
      style = "minimal",
      relative = "cursor",
      row = 0,
      col = 1,
    },
    yadm = { enable = true },
  })
  local keymaps = {
    -- Default keymap options
    n = {
      ["]h"] = {
        "&diff ? ']h' : '<cmd>lua require\"gitsigns.actions\".next_hunk()<CR>'",
        expr = true,
      },
      ["[h"] = {
        "&diff ? '[h' : '<cmd>lua require\"gitsigns.actions\".prev_hunk()<CR>'",
        expr = true,
      },
      ["<leader>hs"] = '<cmd>lua require"gitsigns".stage_hunk()<CR>',
      ["<leader>hu"] = '<cmd>lua require"gitsigns".undo_stage_hunk()<CR>',
      ["<leader>hr"] = '<cmd>lua require"gitsigns".reset_hunk()<CR>',
      ["<leader>hR"] = '<cmd>lua require"gitsigns".reset_buffer()<CR>',
      ["<leader>hp"] = '<cmd>lua require"gitsigns".preview_hunk()<CR>',
      ["<leader>hb"] = '<cmd>lua require"gitsigns".blame_line({full = true})<CR>',
      ["<leader>hU"] = '<cmd>lua require"gitsigns".reset_buffer_index()<CR>',
    },
    v = {
      ["<leader>hs"] = '<cmd>lua require"gitsigns".stage_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>',
      ["<leader>hr"] = '<cmd>lua require"gitsigns".reset_hunk({vim.fn.line("."), vim.fn.line("v")})<CR>',
    },
    o = { -- Text objects
      ["ih"] = ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>',
    },
    x = { ["ih"] = ':<C-U>lua require"gitsigns.actions".select_hunk()<CR>' },
  }
  for mode, mappings in pairs(keymaps) do
    for k, v in pairs(mappings) do
      if type(v) == "string" then
        v = { v }
      end
      vim.api.nvim_set_keymap(mode, k, v[1], { noremap = true, silent = true, expr = v.expr })
    end
  end

  local cb = require("diffview.config").diffview_callback
  require("diffview").setup({
    diff_binaries = false, -- Show diffs for binaries
    file_panel = {
      win_config = {
        width = 50,
      },
    },
    key_bindings = {
      disable_defaults = false, -- Disable the default key bindings
      -- The `view` bindings are active in the diff buffers, only when the current
      -- tabpage is a Diffview.
      view = {
        ["<tab>"] = cb("select_next_entry"),   -- Open the diff for the next file
        ["<s-tab>"] = cb("select_prev_entry"), -- Open the diff for the previous file
        ["<leader>e"] = cb("focus_files"),     -- Bring focus to the files panel
        ["<leader>b"] = cb("toggle_files"),    -- Toggle the files panel.
      },
      file_panel = {
        ["j"] = cb("next_entry"),      -- Bring the cursor to the next file entry
        ["<down>"] = cb("next_entry"),
        ["k"] = cb("prev_entry"),      -- Bring the cursor to the previous file entry.
        ["<up>"] = cb("prev_entry"),
        ["<cr>"] = cb("select_entry"), -- Open the diff for the selected entry.
        ["o"] = cb("select_entry"),
        ["<2-LeftMouse>"] = cb("select_entry"),
        ["-"] = cb("toggle_stage_entry"), -- Stage / unstage the selected entry.
        ["S"] = cb("stage_all"),          -- Stage all entries.
        ["U"] = cb("unstage_all"),        -- Unstage all entries.
        ["R"] = cb("refresh_files"),      -- Update stats and entries in the file list.
        ["<tab>"] = cb("select_next_entry"),
        ["<s-tab>"] = cb("select_prev_entry"),
        ["<leader>e"] = cb("focus_files"),
        ["<leader>b"] = cb("toggle_files"),
      },
    },
  })
  --
  -- Config value: `section.unmerged.hidden` had error -> Expected `section.unmerged.hidden` to be of type 'boolean', got 'nil'
  -- Config value: `mappings.status['<tab>']` had error -> Expected a valid status command, got ''. Valid status commands: { false, "Depth3", "FetchPopup", "SplitOpen", "RevertPopup", "Depth4", "CherryPickPopup", "Unstage", "Toggle", "GoToNextHunkHeader", "GoToPreviousHu nkHeader", "StashPopup", "CommitPopup", "PushPopup", "Depth1", "TabOpen", "Discard", "Console", "Stage", "GoToFile", "Close", "StageAll", "PullPopup", "InitRepo", "UnstageStaged", "DiffAtFile", "LogPopup", "RebasePopup", "HelpPopup", "RemotePopup", "Depth2", "VSplit Open", "DiffPopup", "MergePopup", "StageUnstaged", "ResetPopup", "BranchPopup", "RefreshBuffer", "CommandHistory" }
  -- Config value: `section.unpulled.hidden` had error -> Expected `section.unpulled.hidden` to be of type 'boolean', got 'nil'

  require("neogit").setup({
    disable_signs = false,
    disable_hint = false,
    disable_context_highlighting = false,
    disable_commit_confirmation = false,
    auto_refresh = true,
    disable_builtin_notifications = false,
    use_magit_keybindings = false,
    kind = "vsplit",
    console_timeout = 2000,
    auto_show_console = true,
    commit_popup = {
      kind = "split",
    },
    popup = {
      kind = "split",
    },
    signs = {
      section = { "", "" },
      item = { "", "" },
      hunk = { "", "" },
    },
    integrations = {
      diffview = true,
    },
    -- Setting any section to `false` will make the section not render at all
    sections = {
      untracked = {
        folded = true,
      },
      unstaged = {
        folded = false,
      },
      staged = {
        folded = false,
      },
      stashes = {
        folded = true,
      },
      unpulled = {
        hidden = true,
        folded = true,
      },
      unmerged = {
        hidden = true,
        folded = false,
      },
      recent = {
        folded = true,
      },
    },
    mappings = {
      status = {
        -- ["<CR>"] = "Toggle",
        -- ["<tab>"] = false,
        -- ["o"] = "GoToFile",
      },
    },
  })
end

return M
