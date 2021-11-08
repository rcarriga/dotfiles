local M = {}

function M.post()
  local gl = require("galaxyline")
  local gls = gl.section
  local vcs = require("galaxyline.providers.vcs")

  gl.short_line_list = {
    "dapui_scopes",
    "dapui_stacks",
    "dapui_watches",
    "dapui_breakpoints",
    "LuaTree",
    "dbui",
    "term",
    "fugitive",
    "fugitiveblame",
    "NvimTree",
    "UltestSummary",
  }

  local colors = {
    bg = "none",
    normal = "#F8F8F8",
    grey = "#132434",
    grey1 = "#262626",
    grey2 = "#8B8B8B",
    grey3 = "#bdbdbd",
    grey4 = "#F8F8F8",
    violet = "#D484FF",
    blue = "#2f628e",
    cyan = "#00f1f5",
    green = "#A9FF68",
    green2 = "#2f7366",
    yellow = "#FFF59D",
    orange = "#F79000",
    red = "#F70067",
  }

  local buffer_not_empty = function()
    if vim.fn.empty(vim.fn.expand("%:t")) ~= 1 then
      return true
    end
    return false
  end

  local mode_color = {
    n = colors.green,
    i = colors.cyan,
    v = colors.violet,
    [""] = colors.cyan,
    V = colors.cyan,
    c = colors.red,
    no = colors.violet,
    s = colors.orange,
    S = colors.orange,
    [""] = colors.orange,
    ic = colors.yellow,
    cv = colors.red,
    ce = colors.red,
    ["!"] = colors.green,
    t = colors.green,
    ["r?"] = colors.red,
    ["r"] = colors.red,
    rm = colors.red,
    R = colors.yellow,
    Rv = colors.violet,
  }

  local mode_alias = {
    n = " ",
    i = " ",
    c = " ",
    V = " ",
    [""] = " ",
    v = " ",
    ["r?"] = ":CONFIRM",
    rm = "--MORE",
    R = " ",
    Rv = "VIRTUAL",
    s = " ",
    S = " ",
    ["r"] = "HIT-ENTER",
    [""] = " ",
    t = " ",
    ["!"] = "SHELL",
  }

  local function long_filename()
    local file = vim.fn.expand("%")
    if file == "" then
      return ""
    end
    return file .. " "
  end

  local checkwidth = function()
    local squeeze_width = vim.fn.winwidth(0) / 2
    if squeeze_width > 60 then
      return true
    end
    return false
  end

  local spinner_frames = { "⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷" }
  local function lsp_status(status)
    local buf_messages = require("lsp-status").messages()
    if vim.tbl_isempty(buf_messages) then
      return ""
    end
    local msgs = {}
    for _, msg in ipairs(buf_messages) do
      local name = msg.name
      local client_name = name
      local contents = ""
      if msg.progress then
        contents = msg.title
        if msg.message then
          contents = contents .. " " .. msg.message
        end

        if msg.percentage then
          contents = contents .. "(" .. msg.percentage .. ")"
        end

        if msg.spinner then
          contents = contents .. " " .. spinner_frames[(msg.spinner % #spinner_frames) + 1]
        end
      elseif msg.status then
        contents = msg.content
        if msg.uri then
          local filename = vim.uri_to_fname(msg.uri)
          filename = vim.fn.fnamemodify(filename, ":~:.")
          local space = math.min(60, math.floor(0.6 * vim.fn.winwidth(0)))
          if #filename > space then
            filename = vim.fn.pathshorten(filename)
          end

          contents = "(" .. filename .. ") " .. contents
        end
      else
        contents = msg.content
      end

      table.insert(msgs, client_name .. ":" .. contents)
    end
    status = ""
    for index, msg in ipairs(msgs) do
      status = status .. (index > 1 and " | " or "") .. msg
    end
    return status .. " "
  end

  gls.left = {
    {
      ViMode = {
        provider = function()
          local vim_mode = vim.fn.mode()
          vim.cmd("hi GalaxyViMode guifg=" .. mode_color[vim_mode])
          return "  " .. mode_alias[vim_mode]
        end,
        highlight = "GalaxyViMode",
        separator = " ",
      },
    },

    {
      LongFileName = {
        provider = long_filename,
        condition = buffer_not_empty,
        highlight = { colors.normal, colors.bg, "bold" },
      },
    },

    {
      FileIcon = {
        provider = "FileIcon",
        condition = buffer_not_empty,
        separator = " ",
        highlight = { require("galaxyline.providers.fileinfo").get_file_icon_color, colors.bg },
      },
    },

    {
      FileStatus = {
        provider = function()
          if vim.bo.filetype ~= "help" and vim.bo.readonly then
            return " "
          end
        end,
        highlight = { colors.cyan, colors.bg },
      },
    },

    {
      CustomGitBranch = {
        provider = function()
          local branch = vcs.get_git_branch()
          if branch == nil then
            return ""
          end
          return " " .. branch .. " "
        end,
        condition = checkwidth,
        highlight = { colors.grey2, colors.bg, "bold" },
      },
    },

    {
      DiffAdd = {
        provider = "DiffAdd",
        condition = checkwidth,
        icon = " ",
        highlight = { colors.green, "none", "bold" },
      },
    },
    {
      DiffModified = {
        provider = "DiffModified",
        condition = checkwidth,
        icon = " ",
        highlight = { colors.yellow, "none", "bold" },
      },
    },
    {
      DiffRemove = {
        provider = "DiffRemove",
        condition = checkwidth,
        icon = " ",
        highlight = { colors.red, "none", "bold" },
      },
    },
  }

  gls.right = {
    {
      LspStatus = {
        provider = lsp_status,
        highlight = { colors.grey2, colors.bg },
      },
    },
    {
      DiagnosticError = {
        provider = "DiagnosticError",
        icon = " ",
        highlight = { colors.red, colors.bg },
      },
    },
    {
      DiagnosticWarn = {
        provider = "DiagnosticWarn",
        icon = " ",
        highlight = { colors.yellow, colors.bg },
      },
    },
    {
      DiagnosticInfo = {
        provider = "DiagnosticInfo",
        icon = " ",
        highlight = { colors.green, colors.bg },
      },
    },
    {
      DiagnosticHint = {
        provider = "DiagnosticHint",
        icon = " ",
        highlight = { colors.cyan, colors.bg },
      },
    },
    {
      LineInfo = {
        provider = "LineColumn",
        highlight = { colors.fg, colors.bg },
      },
    },
    {
      PerCent = {
        provider = "LinePercent",
        highlight = { colors.cyan, colors.bg, "bold" },
      },
    },
    {
      FileSize = {
        provider = "FileSize",
        condition = buffer_not_empty,
        highlight = { colors.normal, colors.bg },
      },
    },
  }

  gls.mid = {
    {
      WinBar = {
        provider = function()
          local colour = colors.grey2
          if vim.bo.modified then
            colour = colors.cyan
          end
          vim.cmd("hi GalaxyFileStatus guifg=" .. colour)
          local all_providers = {}
          for _, providers in pairs(gls.left) do
            for name, provider in pairs(providers) do
              all_providers[name] = provider
            end
          end
          for _, providers in pairs(gls.right) do
            for name, provider in pairs(providers) do
              all_providers[name] = provider
            end
          end
          local sum = ""
          for name, provider in pairs(all_providers) do
            if not provider.condition or provider.condition() then
              sum = sum .. gl.component_decorator(name) .. (provider.separator or "")
            end
          end
          local width = vim.fn.winwidth(0) - vim.str_utfindex(sum)
          return "├" .. string.rep("─", width - 2) .. "┤"
        end,
        highlight = "GalaxyFileStatus",
      },
    },
  }

  gls.short_line_left = {
    {
      LongFileName = {
        provider = long_filename,
        highight = { colors.grey2, colors.bg },
      },
    },

    {
      FileIcon = {
        provider = "FileIcon",
        condition = buffer_not_empty,
        separator = " ",
        highlight = { require("galaxyline.providers.fileinfo").get_file_icon_color, colors.bg },
      },
    },
    {
      Bar = {
        provider = function()
          local width = vim.fn.winwidth(0)
          return string.rep("─", width - 2)
        end,
        highlight = {colors.grey2}
      },
    },
  }
end

return M
