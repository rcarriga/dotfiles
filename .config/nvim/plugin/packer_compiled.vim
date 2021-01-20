" Automatically generated packer.nvim plugin loader code

if !has('nvim-0.5')
  echohl WarningMsg
  echom "Invalid Neovim version for packer.nvim!"
  echohl None
  finish
endif
try

lua << END
local plugins = {
  ["Dockerfile.vim"] = {
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/Dockerfile.vim"
  },
  ["clever-f.vim"] = {
    keys = { { "", "f" }, { "", "t" } },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/clever-f.vim"
  },
  ["completion-nvim"] = {
    loaded = false,
    only_sequence = true,
    only_setup = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/completion-nvim"
  },
  ["git-messenger.vim"] = {
    keys = { { "", "<leader>gm" } },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/git-messenger.vim"
  },
  ["goyo.vim"] = {
    commands = { "Goyo" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/goyo.vim"
  },
  ["packer.nvim"] = {
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/packer.nvim"
  },
  ["splitjoin.vim"] = {
    keys = { { "", "gS" }, { "", "gJ" } },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/splitjoin.vim"
  },
  tabular = {
    commands = { "Tabularize" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/tabular"
  },
  tcomment_vim = {
    keys = { { "", "gcc" } },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/tcomment_vim"
  },
  ["telescope.nvim"] = {
    commands = { "Telescope" },
    config = { "require('config.telescope')" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/telescope.nvim"
  },
  ["vim-abolish"] = {
    commands = { "S" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-abolish"
  },
  ["vim-dadbod-ui"] = {
    commands = { "DBUI" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-dadbod-ui"
  },
  ["vim-doge"] = {
    commands = { "DogeGenertate" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-doge"
  },
  ["vim-eunuch"] = {
    commands = { "Rename", "Delete" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-eunuch"
  },
  ["vim-floaterm"] = {
    commands = { "Floaterm" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-floaterm"
  },
  ["vim-mundo"] = {
    commands = { "MundoToggle" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-mundo"
  },
  ["vim-subversive"] = {
    keys = { { "", "<leader>s" } },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-subversive"
  },
  ["vim-test"] = {
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-test"
  },
  ["vim-twiggy"] = {
    commands = { "Twiggy" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-twiggy"
  },
  ["vim-ultest"] = {
    commands = { "Ultest", "UltestNearest" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-ultest"
  },
  ["vim-vue"] = {
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-vue"
  }
}

local function handle_bufread(names)
  for _, name in ipairs(names) do
    local path = plugins[name].path
    for _, dir in ipairs({ 'ftdetect', 'ftplugin', 'after/ftdetect', 'after/ftplugin' }) do
      if #vim.fn.finddir(dir, path) > 0 then
        vim.cmd('doautocmd BufRead')
        return
      end
    end
  end
end

local packer_load = nil
local function handle_after(name, before)
  local plugin = plugins[name]
  plugin.load_after[before] = nil
  if next(plugin.load_after) == nil then
    packer_load({name}, {})
  end
end

packer_load = function(names, cause)
  local some_unloaded = false
  for _, name in ipairs(names) do
    if not plugins[name].loaded then
      some_unloaded = true
      break
    end
  end

  if not some_unloaded then return end

  local fmt = string.format
  local del_cmds = {}
  local del_maps = {}
  for _, name in ipairs(names) do
    if plugins[name].commands then
      for _, cmd in ipairs(plugins[name].commands) do
        del_cmds[cmd] = true
      end
    end

    if plugins[name].keys then
      for _, key in ipairs(plugins[name].keys) do
        del_maps[key] = true
      end
    end
  end

  for cmd, _ in pairs(del_cmds) do
    vim.cmd('silent! delcommand ' .. cmd)
  end

  for key, _ in pairs(del_maps) do
    vim.cmd(fmt('silent! %sunmap %s', key[1], key[2]))
  end

  for _, name in ipairs(names) do
    if not plugins[name].loaded then
      vim.cmd('packadd ' .. name)
      if plugins[name].config then
        for _i, config_line in ipairs(plugins[name].config) do
          loadstring(config_line)()
        end
      end

      if plugins[name].after then
        for _, after_name in ipairs(plugins[name].after) do
          handle_after(after_name, name)
          vim.cmd('redraw')
        end
      end

      plugins[name].loaded = true
    end
  end

  handle_bufread(names)

  if cause.cmd then
    local lines = cause.l1 == cause.l2 and '' or (cause.l1 .. ',' .. cause.l2)
    vim.cmd(fmt('%s%s%s %s', lines, cause.cmd, cause.bang, cause.args))
  elseif cause.keys then
    local keys = cause.keys
    local extra = ''
    while true do
      local c = vim.fn.getchar(0)
      if c == 0 then break end
      extra = extra .. vim.fn.nr2char(c)
    end

    if cause.prefix then
      local prefix = vim.v.count ~= 0 and vim.v.count or ''
      prefix = prefix .. '"' .. vim.v.register .. cause.prefix
      if vim.fn.mode('full') == 'no' then
        if vim.v.operator == 'c' then
          prefix = '' .. prefix
        end

        prefix = prefix .. vim.v.operator
      end

      vim.fn.feedkeys(prefix, 'n')
    end

    local escaped_keys = vim.api.nvim_replace_termcodes(cause.keys .. extra, true, true, true)
    vim.api.nvim_feedkeys(escaped_keys, 'm', true)
  elseif cause.event then
    vim.cmd(fmt('doautocmd <nomodeline> %s', cause.event))
  elseif cause.ft then
    vim.cmd(fmt('doautocmd <nomodeline> %s FileType %s', 'filetypeplugin', cause.ft))
    vim.cmd(fmt('doautocmd <nomodeline> %s FileType %s', 'filetypeindent', cause.ft))
  end
end

_packer_load_wrapper = function(names, cause)
  success, err_msg = pcall(packer_load, names, cause)
  if not success then
    vim.cmd('echohl ErrorMsg')
    vim.cmd('echomsg "Error in packer_compiled: ' .. vim.fn.escape(err_msg, '"') .. '"')
    vim.cmd('echomsg "Please check your config for correctness"')
    vim.cmd('echohl None')
  end
end

-- Runtimepath customization

-- Pre-load configuration
-- Setup for: completion-nvim
require('config.completion')
vim.cmd("packadd completion-nvim")
-- Post-load configuration
-- Config for: nvim-treesitter
require('config.treesitter')
-- Config for: nvim-dap
require('config.dap')
-- Config for: galaxyline.nvim
require('config.galaxyline')
-- Config for: nvim-lspconfig
require('config.lsp')
-- Config for: format.nvim
require('config.format')
-- Conditional loads
-- Load plugins in order defined by `after`
END

function! s:load(names, cause) abort
  call luaeval('_packer_load_wrapper(_A[1], _A[2])', [a:names, a:cause])
endfunction


" Command lazy-loads
command! -nargs=* -range -bang -complete=file MundoToggle call s:load(['vim-mundo'], { "cmd": "MundoToggle", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Ultest call s:load(['vim-ultest'], { "cmd": "Ultest", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Rename call s:load(['vim-eunuch'], { "cmd": "Rename", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file DBUI call s:load(['vim-dadbod-ui'], { "cmd": "DBUI", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Delete call s:load(['vim-eunuch'], { "cmd": "Delete", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Telescope call s:load(['telescope.nvim'], { "cmd": "Telescope", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Twiggy call s:load(['vim-twiggy'], { "cmd": "Twiggy", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file S call s:load(['vim-abolish'], { "cmd": "S", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Goyo call s:load(['goyo.vim'], { "cmd": "Goyo", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Tabularize call s:load(['tabular'], { "cmd": "Tabularize", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file UltestNearest call s:load(['vim-ultest'], { "cmd": "UltestNearest", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Floaterm call s:load(['vim-floaterm'], { "cmd": "Floaterm", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file DogeGenertate call s:load(['vim-doge'], { "cmd": "DogeGenertate", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })

" Keymap lazy-loads
noremap <silent> gJ <cmd>call <SID>load(['splitjoin.vim'], { "keys": "gJ", "prefix": "" })<cr>
noremap <silent> f <cmd>call <SID>load(['clever-f.vim'], { "keys": "f", "prefix": "" })<cr>
noremap <silent> gcc <cmd>call <SID>load(['tcomment_vim'], { "keys": "gcc", "prefix": "" })<cr>
noremap <silent> gS <cmd>call <SID>load(['splitjoin.vim'], { "keys": "gS", "prefix": "" })<cr>
noremap <silent> t <cmd>call <SID>load(['clever-f.vim'], { "keys": "t", "prefix": "" })<cr>
noremap <silent> <leader>gm <cmd>call <SID>load(['git-messenger.vim'], { "keys": "\<leader\>gm", "prefix": "" })<cr>
noremap <silent> <leader>s <cmd>call <SID>load(['vim-subversive'], { "keys": "\<leader\>s", "prefix": "" })<cr>

augroup packer_load_aucmds
  au!
  " Filetype lazy-loads
  au FileType Dockerfile ++once call s:load(['Dockerfile.vim'], { "ft": "Dockerfile" })
  au FileType vue ++once call s:load(['vim-vue'], { "ft": "vue" })
  au FileType dockerfile ++once call s:load(['Dockerfile.vim'], { "ft": "dockerfile" })
  " Event lazy-loads
  " Function lazy-loads
augroup END

catch
  echohl ErrorMsg
  echom "Error in packer_compiled: " .. v:exception
  echom "Please check your config for correctness"
  echohl None
endtry
