local M = {}

local function wrap_options(custom, handler)
  return function(opts)
    opts = opts and vim.tbl_extend(opts, custom) or custom
    if type(handler) == "string" then
      require("telescope.builtin")[handler](opts)
    else
      handler(opts)
    end
  end
end

function M.setup()
  require("noice").setup({
    cmdline = {
      enabled = false, -- enables the Noice cmdline UI
      view = "cmdline_popup", -- view for rendering the cmdline. Change to `cmdline` to get a classic cmdline at the bottom
      opts = {}, -- global options for the cmdline. See section on views
      format = {
        -- conceal: (default=true) This will hide the text in the cmdline that matches the pattern.
        -- view: (default is cmdline view)
        -- opts: any options passed to the view
        -- icon_hl_group: optional hl_group for the icon
        -- title: set to anything or empty string to hide
        cmdline = { pattern = "^:", icon = "", lang = "vim" },
        search_down = { kind = "search", pattern = "^/", icon = " ", lang = "regex" },
        search_up = { kind = "search", pattern = "^%?", icon = " ", lang = "regex" },
        filter = { pattern = "^:%s*!", icon = "$", lang = "bash" },
        lua = { pattern = "^:%s*lua%s+", icon = "", lang = "lua" },
        help = { pattern = "^:%s*he?l?p?%s+", icon = "" },
        calculator = { pattern = "^=", icon = "", lang = "vimnormal" },
        input = {}, -- Used by input()
        -- lua = false, -- to disable a format, set to `false`
      },
    },
    messages = {
      -- NOTE: If you enable messages, then the cmdline is enabled automatically.
      -- This is a current Neovim limitation.
      enabled = false, -- enables the Noice messages UI
      view = "notify", -- default view for messages
      view_error = "notify", -- view for errors
      view_warn = "notify", -- view for warnings
      view_history = "messages", -- view for :messages
      view_search = "virtualtext", -- view for search count messages. Set to `false` to disable
    },
    popupmenu = {
      enabled = false, -- enables the Noice popupmenu UI
      ---@type 'nui'|'cmp'
      backend = "nui", -- backend to use to show regular cmdline completions
      -- Icons for completion item kinds (see defaults at noice.config.icons.kinds)
      kind_icons = {}, -- set to `false` to disable icons
    },
    -- default options for require('noice').redirect
    -- see the section on Command Redirection
    redirect = {
      view = "popup",
      filter = { event = "msg_show" },
    },
    -- You can add any custom commands below that will be available with `:Noice command`
    commands = {
      history = {
        -- options for the message history that you get with `:Noice`
        view = "split",
        opts = { enter = true, format = "details" },
        filter = {
          any = {
            { event = "notify" },
            { error = true },
            { warning = true },
            { event = "msg_show", kind = { "" } },
            { event = "lsp", kind = "message" },
          },
        },
      },
      -- :Noice last
      last = {
        view = "popup",
        opts = { enter = true, format = "details" },
        filter = {
          any = {
            { event = "notify" },
            { error = true },
            { warning = true },
            { event = "msg_show", kind = { "" } },
            { event = "lsp", kind = "message" },
          },
        },
        filter_opts = { count = 1 },
      },
      -- :Noice errors
      errors = {
        -- options for the message history that you get with `:Noice`
        view = "popup",
        opts = { enter = true, format = "details" },
        filter = { error = true },
        filter_opts = { reverse = true },
      },
    },
    notify = {
      -- Noice can be used as `vim.notify` so you can route any notification like other messages
      -- Notification messages have their level and other properties set.
      -- event is always "notify" and kind can be any log level as a string
      -- The default routes will forward notifications to nvim-notify
      -- Benefit of using Noice for this is the routing and consistent history view
      enabled = false,
      view = "notify",
    },
    lsp = {
      progress = {
        enabled = false,
        -- Lsp Progress is formatted using the builtins for lsp_progress. See config.format.builtin
        -- See the section on formatting for more details on how to customize.
        format = "lsp_progress",
        format_done = "lsp_progress_done",
        throttle = 1000 / 30, -- frequency to update lsp progress message
        view = "mini",
      },
      override = {
        -- override the default lsp markdown formatter with Noice
        ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
        -- override the lsp markdown formatter with Noice
        ["vim.lsp.util.stylize_markdown"] = true,
        -- override cmp documentation with Noice (needs the other options to work)
        ["cmp.entry.get_documentation"] = true,
      },
      hover = {
        enabled = true,
        view = nil, -- when nil, use defaults from documentation
        opts = {}, -- merged with defaults from documentation
      },
      signature = {
        enabled = true,
        auto_open = {
          enabled = true,
          trigger = true, -- Automatically show signature help when typing a trigger character from the LSP
          luasnip = true, -- Will open signature help when jumping to Luasnip insert nodes
          throttle = 50, -- Debounce lsp signature help request by 50ms
        },
        view = nil, -- when nil, use defaults from documentation
        opts = {}, -- merged with defaults from documentation
      },
      message = {
        -- Messages shown by lsp servers
        enabled = true,
        view = "notify",
        opts = {},
      },
      -- defaults for hover and signature help
      documentation = {
        view = "hover",
        opts = {
          replace = true,
          render = "plain",
          format = { "{message}" },
          win_options = { concealcursor = "n", conceallevel = 3 },
        },
      },
    },
    markdown = {
      hover = {
        ["|(%S-)|"] = vim.cmd.help, -- vim help links
        ["%[.-%]%((%S-)%)"] = require("noice.util").open, -- markdown links
      },
      highlights = {
        ["|%S-|"] = "@text.reference",
        ["@%S+"] = "@parameter",
        ["^%s*(Parameters:)"] = "@text.title",
        ["^%s*(Return:)"] = "@text.title",
        ["^%s*(See also:)"] = "@text.title",
        ["{%S-}"] = "@parameter",
      },
    },
    health = {
      checker = true, -- Disable if you don't want health checks to run
    },
    smart_move = {
      -- noice tries to move out of the way of existing floating windows.
      enabled = false, -- you can disable this behaviour here
      -- add any filetypes here, that shouldn't trigger smart move.
      excluded_filetypes = { "cmp_menu", "cmp_docs", "notify" },
    },
    presets = {
      -- you can enable a preset by setting it to true, or a table that will override the preset config
      -- you can also add custom presets that you can enable/disable with enabled=true
      bottom_search = false, -- use a classic bottom cmdline for search
      command_palette = false, -- position the cmdline and popupmenu together
      long_message_to_split = false, -- long messages will be sent to a split
      inc_rename = false, -- enables an input dialog for inc-rename.nvim
      lsp_doc_border = true, -- add a border to hover docs and signature help
      cmdline_output_to_split = false, -- send the output of a command you executed in the cmdline to a split
    },
    throttle = 1000 / 30, -- how frequently does Noice need to check for ui updates? This has no effect when in blocking mode.
    views = {
      popup = {
        border = {
          style = "rounded",
        },
      },
    }, ---@see section on views
    routes = {}, --- @see section on routes
    status = {}, --- @see section on statusline components
    format = {}, --- @see section on formatting
    debug = false,
    log_max_size = 1024 * 1024 * 2, -- 10MB
  })
  if pcall(require, "telescope") then
    vim.lsp.handlers["textDocument/documentSymbol"] =
    require("telescope.builtin").lsp_document_symbols
    -- vim.lsp.handlers["textDocument/hover"] = require("noice.lsp.hover")
  end
  vim.lsp.handlers["textDocument/codeLens"] = vim.lsp.codelens.on_codelens
  local severity = {
    "error",
    "warn",
    "info",
    "info", -- map both hint and info to info?
  }
  vim.lsp.handlers["window/showMessage"] = function(_, method, params, client_id)
    local client = vim.lsp.get_client_by_id(client_id)
    vim.notify(method.message, severity[params.type], { title = client and client.name })
  end

  local base_logger = vim.lsp.handlers["window/logMessage"]
  local protocol = vim.lsp.protocol
  local msg_types = {
    [protocol.MessageType.Error] = "ERROR",
    [protocol.MessageType.Warning] = "WARN",
    [protocol.MessageType.Info] = "INFO",
  }
  local client_logs = {}
  vim.lsp.handlers["window/logMessage"] = function(...)
    local _, result, ctx, _ = ...
    local client_id = ctx.client_id
    local msg_type = msg_types[result.type or protocol.MessageType.Info]
    client_logs[client_id] = client_logs[client_id] or {}
    client_logs[client_id][#client_logs[client_id] + 1] = {
      type = msg_type,
      message = result.message,
    }

    return base_logger(...)
  end

  vim.api.nvim_create_user_command("LspLogs", function()
    local buf = vim.api.nvim_create_buf(false, true)
    local lines = {}
    for client_id, logs in pairs(client_logs) do
      local client = vim.lsp.get_client_by_id(client_id)
      local client_name = client and client.name or string.format("id=%d", client_id)
      lines[#lines + 1] = string.format("%s logs:", client_name)
      for _, log in ipairs(logs) do
        lines[#lines + 1] = string.format("%s | %s", log.type, log.message)
      end
    end
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
    vim.api.nvim_buf_set_option(buf, "filetype", "log")
    vim.api.nvim_win_set_buf(0, buf)
  end, {})

  local handle_locations = function(err, result, ctx, config)
    local client_encoding = vim.lsp.get_client_by_id(ctx.client_id).offset_encoding
    if err then
      vim.notify(err.message)
      return
    end
    if result == nil then
      vim.notify("Location not found", "LSP")
      return
    end
    if vim.tbl_islist(result) and result[1] then
      vim.lsp.util.jump_to_location(result[1], client_encoding)

      if #result > 1 then
        vim.fn.setqflist(vim.lsp.util.locations_to_items(result, client_encoding))
        vim.cmd("TroubleToggle quickfix")
      end
    else
      vim.lsp.util.jump_to_location(result, client_encoding)
    end
  end
  vim.lsp.handlers["textDocument/definition"] = handle_locations
  vim.lsp.handlers["textDocument/typeDefinition"] = handle_locations
  vim.lsp.handlers["textDocument/references"] = handle_locations
end

return M
