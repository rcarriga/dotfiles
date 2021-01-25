" Automatically generated packer.nvim plugin loader code

if !has('nvim-0.5')
  echohl WarningMsg
  echom "Invalid Neovim version for packer.nvim!"
  echohl None
  finish
endif
try

lua << END
  local package_path_str = "/home/ronan/.cache/nvim/packer_hererocks/2.0.5/share/lua/5.1/?.lua;/home/ronan/.cache/nvim/packer_hererocks/2.0.5/share/lua/5.1/?/init.lua;/home/ronan/.cache/nvim/packer_hererocks/2.0.5/lib/luarocks/rocks-5.1/?.lua;/home/ronan/.cache/nvim/packer_hererocks/2.0.5/lib/luarocks/rocks-5.1/?/init.lua"
  local install_cpath_pattern = "/home/ronan/.cache/nvim/packer_hererocks/2.0.5/lib/lua/5.1/?.so"
  if not string.find(package.path, package_path_str, 1, true) then
    package.path = package.path .. ';' .. package_path_str
  end

  if not string.find(package.cpath, install_cpath_pattern, 1, true) then
    package.cpath = package.cpath .. ';' .. install_cpath_pattern
  end

_G.packer_plugins = {
  ["Dockerfile.vim"] = {
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/Dockerfile.vim"
  },
  FastFold = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/FastFold"
  },
  ["barbar.nvim"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/barbar.nvim"
  },
  ["clever-f.vim"] = {
    keys = { { "", "f" }, { "", "t" } },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/clever-f.vim"
  },
  ["completion-buffers"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/completion-buffers"
  },
  ["completion-nvim"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/completion-nvim"
  },
  ["completion-tags"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/completion-tags"
  },
  ["conflict-marker.vim"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/conflict-marker.vim"
  },
  ["format.nvim"] = {
    commands = { "Format" },
    config = { "require('config.format').pre()" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/format.nvim"
  },
  fzf = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/fzf"
  },
  ["fzf.vim"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/fzf.vim"
  },
  ["galaxyline.nvim"] = {
    config = { "require('config.galaxyline').post()" },
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/galaxyline.nvim"
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
  ["haskell-vim"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/haskell-vim"
  },
  hiPairs = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/hiPairs"
  },
  ["is.vim"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/is.vim"
  },
  ["line-targets.vim"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/line-targets.vim"
  },
  ["lsp-status.nvim"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/lsp-status.nvim"
  },
  ["lspsaga.nvim"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/lspsaga.nvim"
  },
  ["nvim-autopairs"] = {
    config = { "require('config.autopairs').post()" },
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-autopairs"
  },
  ["nvim-dap"] = {
    config = { "require('config.dap').post()" },
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-dap"
  },
  ["nvim-dap-python"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-dap-python"
  },
  ["nvim-dap-virtual-text"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-dap-virtual-text"
  },
  ["nvim-lspconfig"] = {
    config = { "require('config.lsp').post()" },
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-lspconfig"
  },
  ["nvim-tree.lua"] = {
    commands = { "NvimTreeToggle" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/nvim-tree.lua"
  },
  ["nvim-treesitter"] = {
    config = { "require('config.treesitter').post()" },
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["nvim-treesitter-refactor"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-treesitter-refactor"
  },
  ["nvim-treesitter-textobjects"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-treesitter-textobjects"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/packer.nvim"
  },
  playground = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/playground"
  },
  ["plenary.nvim"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["popup.nvim"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/popup.nvim"
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
  ["targets.vim"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/targets.vim"
  },
  tcomment_vim = {
    keys = { { "", "gcc" } },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/tcomment_vim"
  },
  ["telescope-dap.nvim"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/telescope-dap.nvim"
  },
  ["telescope-fzy-native.nvim"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/telescope-fzy-native.nvim"
  },
  ["telescope.nvim"] = {
    config = { "require('config.telescope').post()" },
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/telescope.nvim"
  },
  ["vim-abolish"] = {
    commands = { "S" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-abolish"
  },
  ["vim-asterisk"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-asterisk"
  },
  ["vim-dadbod"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-dadbod"
  },
  ["vim-dadbod-ui"] = {
    commands = { "DBUI" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-dadbod-ui"
  },
  ["vim-doge"] = {
    commands = { "DogeGenerate" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-doge"
  },
  ["vim-eunuch"] = {
    commands = { "Rename", "Delete", "Remove", "Chmod" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-eunuch"
  },
  ["vim-floaterm"] = {
    commands = { "FloatermNew" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-floaterm"
  },
  ["vim-fugitive"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-fugitive"
  },
  ["vim-hexokinase"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-hexokinase"
  },
  ["vim-log-highlighting"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-log-highlighting"
  },
  ["vim-mundo"] = {
    commands = { "MundoToggle" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-mundo"
  },
  ["vim-rhubarb"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-rhubarb"
  },
  ["vim-sandwich"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-sandwich"
  },
  ["vim-signify"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-signify"
  },
  ["vim-subversive"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-subversive"
  },
  ["vim-test"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-test"
  },
  ["vim-twiggy"] = {
    commands = { "Twiggy" },
    loaded = false,
    only_sequence = false,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-twiggy"
  },
  ["vim-ultest"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-ultest"
  },
  ["vim-unimpaired"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-unimpaired"
  },
  ["vim-vsnip"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-vsnip"
  },
  ["vim-vsnip-integ"] = {
    loaded = true,
    only_sequence = true,
    only_setup = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-vsnip-integ"
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
    local path = packer_plugins[name].path
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
  local plugin = packer_plugins[name]
  plugin.load_after[before] = nil
  if next(plugin.load_after) == nil then
    packer_load({name}, {})
  end
end

packer_load = function(names, cause)
  local some_unloaded = false
  for _, name in ipairs(names) do
    if not packer_plugins[name].loaded then
      some_unloaded = true
      break
    end
  end

  if not some_unloaded then return end

  local fmt = string.format
  local del_cmds = {}
  local del_maps = {}
  for _, name in ipairs(names) do
    if packer_plugins[name].commands then
      for _, cmd in ipairs(packer_plugins[name].commands) do
        del_cmds[cmd] = true
      end
    end

    if packer_plugins[name].keys then
      for _, key in ipairs(packer_plugins[name].keys) do
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
    if not packer_plugins[name].loaded then
      vim.cmd('packadd ' .. name)
      if packer_plugins[name].config then
        for _i, config_line in ipairs(packer_plugins[name].config) do
          loadstring(config_line)()
        end
      end

      if packer_plugins[name].after then
        for _, after_name in ipairs(packer_plugins[name].after) do
          handle_after(after_name, name)
          vim.cmd('redraw')
        end
      end

      packer_plugins[name].loaded = true
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
-- Post-load configuration
-- Config for: nvim-treesitter
require('config.treesitter').post()
-- Config for: nvim-dap
require('config.dap').post()
-- Config for: galaxyline.nvim
require('config.galaxyline').post()
-- Config for: telescope.nvim
require('config.telescope').post()
-- Config for: nvim-autopairs
require('config.autopairs').post()
-- Config for: nvim-lspconfig
require('config.lsp').post()
-- Conditional loads
-- Load plugins in order defined by `after`
END

function! s:load(names, cause) abort
  call luaeval('_packer_load_wrapper(_A[1], _A[2])', [a:names, a:cause])
endfunction


" Command lazy-loads
command! -nargs=* -range -bang -complete=file DogeGenerate call s:load(['vim-doge'], { "cmd": "DogeGenerate", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Format call s:load(['format.nvim'], { "cmd": "Format", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file MundoToggle call s:load(['vim-mundo'], { "cmd": "MundoToggle", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file NvimTreeToggle call s:load(['nvim-tree.lua'], { "cmd": "NvimTreeToggle", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Rename call s:load(['vim-eunuch'], { "cmd": "Rename", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Delete call s:load(['vim-eunuch'], { "cmd": "Delete", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file DBUI call s:load(['vim-dadbod-ui'], { "cmd": "DBUI", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Twiggy call s:load(['vim-twiggy'], { "cmd": "Twiggy", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Remove call s:load(['vim-eunuch'], { "cmd": "Remove", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Tabularize call s:load(['tabular'], { "cmd": "Tabularize", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file S call s:load(['vim-abolish'], { "cmd": "S", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Chmod call s:load(['vim-eunuch'], { "cmd": "Chmod", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file Goyo call s:load(['goyo.vim'], { "cmd": "Goyo", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })
command! -nargs=* -range -bang -complete=file FloatermNew call s:load(['vim-floaterm'], { "cmd": "FloatermNew", "l1": <line1>, "l2": <line2>, "bang": <q-bang>, "args": <q-args> })

" Keymap lazy-loads
noremap <silent> t <cmd>call <SID>load(['clever-f.vim'], { "keys": "t", "prefix": "" })<cr>
noremap <silent> f <cmd>call <SID>load(['clever-f.vim'], { "keys": "f", "prefix": "" })<cr>
noremap <silent> gcc <cmd>call <SID>load(['tcomment_vim'], { "keys": "gcc", "prefix": "" })<cr>
noremap <silent> gJ <cmd>call <SID>load(['splitjoin.vim'], { "keys": "gJ", "prefix": "" })<cr>
noremap <silent> <leader>gm <cmd>call <SID>load(['git-messenger.vim'], { "keys": "\<leader\>gm", "prefix": "" })<cr>
noremap <silent> gS <cmd>call <SID>load(['splitjoin.vim'], { "keys": "gS", "prefix": "" })<cr>

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
