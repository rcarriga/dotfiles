-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

  local time
  local profile_info
  local should_profile = false
  if should_profile then
    local hrtime = vim.loop.hrtime
    profile_info = {}
    time = function(chunk, start)
      if start then
        profile_info[chunk] = hrtime()
      else
        profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
      end
    end
  else
    time = function(chunk, start) end
  end
  
local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end

  _G._packer = _G._packer or {}
  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/home/ronan/.cache/nvim/packer_hererocks/2.0.5/share/lua/5.1/?.lua;/home/ronan/.cache/nvim/packer_hererocks/2.0.5/share/lua/5.1/?/init.lua;/home/ronan/.cache/nvim/packer_hererocks/2.0.5/lib/luarocks/rocks-5.1/?.lua;/home/ronan/.cache/nvim/packer_hererocks/2.0.5/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/ronan/.cache/nvim/packer_hererocks/2.0.5/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  ["Dockerfile.vim"] = {
    loaded = false,
    needs_bufread = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/Dockerfile.vim"
  },
  FastFold = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/FastFold"
  },
  ["barbar.nvim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/barbar.nvim"
  },
  ["clever-f.vim"] = {
    keys = { { "", "f" }, { "", "t" } },
    loaded = false,
    needs_bufread = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/clever-f.vim"
  },
  ["conflict-marker.vim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/conflict-marker.vim"
  },
  ["diffview.nvim"] = {
    config = { "require('config.git').post()" },
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/diffview.nvim"
  },
  ["emmet-vim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/emmet-vim"
  },
  ["fugitive-gitlab.vim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/fugitive-gitlab.vim"
  },
  ["galaxyline.nvim"] = {
    config = { "require('config.galaxyline').post()" },
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/galaxyline.nvim"
  },
  ["git-messenger.vim"] = {
    keys = { { "", "<leader>gm" } },
    loaded = false,
    needs_bufread = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/git-messenger.vim"
  },
  ["goyo.vim"] = {
    commands = { "Goyo" },
    loaded = false,
    needs_bufread = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/goyo.vim"
  },
  ["haskell-vim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/haskell-vim"
  },
  hiPairs = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/hiPairs"
  },
  ["indent-blankline.nvim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/indent-blankline.nvim"
  },
  ["is.vim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/is.vim"
  },
  ["iswap.nvim"] = {
    config = { "require('iswap').setup({})" },
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/iswap.nvim"
  },
  ["line-targets.vim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/line-targets.vim"
  },
  ["lsp-status.nvim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/lsp-status.nvim"
  },
  ["lsp_signature.nvim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/lsp_signature.nvim"
  },
  ["lspkind-nvim"] = {
    config = { "require('lspkind').init({with_text = false})" },
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/lspkind-nvim"
  },
  ["lua-dev.nvim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/lua-dev.nvim"
  },
  ["markdown-preview.nvim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/markdown-preview.nvim"
  },
  ["nvim-compe"] = {
    after_files = { "/home/ronan/.cache/nvim/site/pack/packer/opt/nvim-compe/after/plugin/compe.vim" },
    config = { "require('config.compe').post()" },
    loaded = false,
    needs_bufread = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/nvim-compe"
  },
  ["nvim-dap"] = {
    config = { "require('config.dap').post()" },
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-dap"
  },
  ["nvim-dap-python"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-dap-python"
  },
  ["nvim-dap-ui"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-dap-ui"
  },
  ["nvim-lspconfig"] = {
    config = { "require('config.lsp').post()" },
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-lspconfig"
  },
  ["nvim-lspinstall"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-lspinstall"
  },
  ["nvim-luapad"] = {
    commands = { "Luapad" },
    loaded = false,
    needs_bufread = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/nvim-luapad"
  },
  ["nvim-tree.lua"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-tree.lua"
  },
  ["nvim-treesitter"] = {
    config = { "require('config.treesitter').post()" },
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["nvim-treesitter-refactor"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-treesitter-refactor"
  },
  ["nvim-treesitter-textobjects"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-treesitter-textobjects"
  },
  ["nvim-ts-context-commentstring"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-ts-context-commentstring"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/packer.nvim"
  },
  ["pears.nvim"] = {
    config = { "require('config.autopairs').post()" },
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/pears.nvim"
  },
  playground = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/playground"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["popup.nvim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/popup.nvim"
  },
  ["splitjoin.vim"] = {
    keys = { { "", "gS" }, { "", "gJ" } },
    loaded = false,
    needs_bufread = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/splitjoin.vim"
  },
  tabular = {
    after_files = { "/home/ronan/.cache/nvim/site/pack/packer/opt/tabular/after/plugin/TabularMaps.vim" },
    commands = { "Tabularize" },
    loaded = false,
    needs_bufread = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/tabular"
  },
  ["targets.vim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/targets.vim"
  },
  ["telescope-dap.nvim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/telescope-dap.nvim"
  },
  ["telescope-fzf-native.nvim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/telescope-fzf-native.nvim"
  },
  ["telescope.nvim"] = {
    config = { "require('config.telescope').post()" },
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/telescope.nvim"
  },
  ["twilight.nvim"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/twilight.nvim"
  },
  ["vim-abolish"] = {
    commands = { "S" },
    loaded = false,
    needs_bufread = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-abolish"
  },
  ["vim-asterisk"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-asterisk"
  },
  ["vim-commentary"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-commentary"
  },
  ["vim-dadbod-completion"] = {
    after_files = { "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-dadbod-completion/after/plugin/vim_dadbod_completion.vim" },
    loaded = false,
    needs_bufread = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-dadbod-completion"
  },
  ["vim-dadbod-ui"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-dadbod-ui"
  },
  ["vim-doge"] = {
    commands = { "DogeGenerate" },
    loaded = false,
    needs_bufread = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-doge"
  },
  ["vim-eunuch"] = {
    commands = { "Rename", "Delete", "Remove", "Chmod" },
    loaded = false,
    needs_bufread = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-eunuch"
  },
  ["vim-floaterm"] = {
    commands = { "FloatermNew" },
    loaded = false,
    needs_bufread = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-floaterm"
  },
  ["vim-fugitive"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-fugitive"
  },
  ["vim-hexokinase"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-hexokinase"
  },
  ["vim-log-highlighting"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-log-highlighting"
  },
  ["vim-mundo"] = {
    commands = { "MundoToggle" },
    loaded = false,
    needs_bufread = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-mundo"
  },
  ["vim-rhubarb"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-rhubarb"
  },
  ["vim-sandwich"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-sandwich"
  },
  ["vim-signify"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-signify"
  },
  ["vim-subversive"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-subversive"
  },
  ["vim-test"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-test"
  },
  ["vim-twiggy"] = {
    commands = { "Twiggy" },
    loaded = false,
    needs_bufread = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-twiggy"
  },
  ["vim-ultest"] = {
    config = { "require('config.ultest').post()" },
    keys = { { "", "<Plug>(ultest-run-nearest)" }, { "", "<Plug>(ultest-run-file)" }, { "", "<Plug>(ultest-summary-toggle)" } },
    loaded = false,
    needs_bufread = false,
    path = "/home/ronan/.cache/nvim/site/pack/packer/opt/vim-ultest"
  },
  ["vim-unimpaired"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-unimpaired"
  },
  ["vim-vsnip"] = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vim-vsnip"
  },
  vimspector = {
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/vimspector"
  },
  ["zen-mode.nvim"] = {
    config = { "require('config.zen')" },
    loaded = true,
    path = "/home/ronan/.cache/nvim/site/pack/packer/start/zen-mode.nvim"
  }
}

time([[Defining packer_plugins]], false)
-- Config for: telescope.nvim
time([[Config for telescope.nvim]], true)
require('config.telescope').post()
time([[Config for telescope.nvim]], false)
-- Config for: iswap.nvim
time([[Config for iswap.nvim]], true)
require('iswap').setup({})
time([[Config for iswap.nvim]], false)
-- Config for: diffview.nvim
time([[Config for diffview.nvim]], true)
require('config.git').post()
time([[Config for diffview.nvim]], false)
-- Config for: lspkind-nvim
time([[Config for lspkind-nvim]], true)
require('lspkind').init({with_text = false})
time([[Config for lspkind-nvim]], false)
-- Config for: nvim-dap
time([[Config for nvim-dap]], true)
require('config.dap').post()
time([[Config for nvim-dap]], false)
-- Config for: pears.nvim
time([[Config for pears.nvim]], true)
require('config.autopairs').post()
time([[Config for pears.nvim]], false)
-- Config for: galaxyline.nvim
time([[Config for galaxyline.nvim]], true)
require('config.galaxyline').post()
time([[Config for galaxyline.nvim]], false)
-- Config for: nvim-lspconfig
time([[Config for nvim-lspconfig]], true)
require('config.lsp').post()
time([[Config for nvim-lspconfig]], false)
-- Config for: zen-mode.nvim
time([[Config for zen-mode.nvim]], true)
require('config.zen')
time([[Config for zen-mode.nvim]], false)
-- Config for: nvim-treesitter
time([[Config for nvim-treesitter]], true)
require('config.treesitter').post()
time([[Config for nvim-treesitter]], false)

-- Command lazy-loads
time([[Defining lazy-load commands]], true)
vim.cmd [[command! -nargs=* -range -bang -complete=file DogeGenerate lua require("packer.load")({'vim-doge'}, { cmd = "DogeGenerate", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file MundoToggle lua require("packer.load")({'vim-mundo'}, { cmd = "MundoToggle", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Tabularize lua require("packer.load")({'tabular'}, { cmd = "Tabularize", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Goyo lua require("packer.load")({'goyo.vim'}, { cmd = "Goyo", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file FloatermNew lua require("packer.load")({'vim-floaterm'}, { cmd = "FloatermNew", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Twiggy lua require("packer.load")({'vim-twiggy'}, { cmd = "Twiggy", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Remove lua require("packer.load")({'vim-eunuch'}, { cmd = "Remove", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Luapad lua require("packer.load")({'nvim-luapad'}, { cmd = "Luapad", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Delete lua require("packer.load")({'vim-eunuch'}, { cmd = "Delete", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Chmod lua require("packer.load")({'vim-eunuch'}, { cmd = "Chmod", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file S lua require("packer.load")({'vim-abolish'}, { cmd = "S", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
vim.cmd [[command! -nargs=* -range -bang -complete=file Rename lua require("packer.load")({'vim-eunuch'}, { cmd = "Rename", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]]
time([[Defining lazy-load commands]], false)

-- Keymap lazy-loads
time([[Defining lazy-load keymaps]], true)
vim.cmd [[noremap <silent> <leader>gm <cmd>lua require("packer.load")({'git-messenger.vim'}, { keys = "<lt>leader>gm", prefix = "" }, _G.packer_plugins)<cr>]]
vim.cmd [[noremap <silent> <Plug>(ultest-run-nearest) <cmd>lua require("packer.load")({'vim-ultest'}, { keys = "<lt>Plug>(ultest-run-nearest)", prefix = "" }, _G.packer_plugins)<cr>]]
vim.cmd [[noremap <silent> <Plug>(ultest-summary-toggle) <cmd>lua require("packer.load")({'vim-ultest'}, { keys = "<lt>Plug>(ultest-summary-toggle)", prefix = "" }, _G.packer_plugins)<cr>]]
vim.cmd [[noremap <silent> gS <cmd>lua require("packer.load")({'splitjoin.vim'}, { keys = "gS", prefix = "" }, _G.packer_plugins)<cr>]]
vim.cmd [[noremap <silent> t <cmd>lua require("packer.load")({'clever-f.vim'}, { keys = "t", prefix = "" }, _G.packer_plugins)<cr>]]
vim.cmd [[noremap <silent> <Plug>(ultest-run-file) <cmd>lua require("packer.load")({'vim-ultest'}, { keys = "<lt>Plug>(ultest-run-file)", prefix = "" }, _G.packer_plugins)<cr>]]
vim.cmd [[noremap <silent> f <cmd>lua require("packer.load")({'clever-f.vim'}, { keys = "f", prefix = "" }, _G.packer_plugins)<cr>]]
vim.cmd [[noremap <silent> gJ <cmd>lua require("packer.load")({'splitjoin.vim'}, { keys = "gJ", prefix = "" }, _G.packer_plugins)<cr>]]
time([[Defining lazy-load keymaps]], false)

vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
time([[Defining lazy-load filetype autocommands]], true)
vim.cmd [[au FileType Dockerfile ++once lua require("packer.load")({'Dockerfile.vim'}, { ft = "Dockerfile" }, _G.packer_plugins)]]
vim.cmd [[au FileType dockerfile ++once lua require("packer.load")({'Dockerfile.vim'}, { ft = "dockerfile" }, _G.packer_plugins)]]
time([[Defining lazy-load filetype autocommands]], false)
  -- Event lazy-loads
time([[Defining lazy-load event autocommands]], true)
vim.cmd [[au InsertEnter * ++once lua require("packer.load")({'nvim-compe'}, { event = "InsertEnter *" }, _G.packer_plugins)]]
time([[Defining lazy-load event autocommands]], false)
vim.cmd("augroup END")
vim.cmd [[augroup filetypedetect]]
time([[Sourcing ftdetect script at: /home/ronan/.cache/nvim/site/pack/packer/opt/Dockerfile.vim/ftdetect/Dockerfile.vim]], true)
vim.cmd [[source /home/ronan/.cache/nvim/site/pack/packer/opt/Dockerfile.vim/ftdetect/Dockerfile.vim]]
time([[Sourcing ftdetect script at: /home/ronan/.cache/nvim/site/pack/packer/opt/Dockerfile.vim/ftdetect/Dockerfile.vim]], false)
time([[Sourcing ftdetect script at: /home/ronan/.cache/nvim/site/pack/packer/opt/Dockerfile.vim/ftdetect/docker-compose.vim]], true)
vim.cmd [[source /home/ronan/.cache/nvim/site/pack/packer/opt/Dockerfile.vim/ftdetect/docker-compose.vim]]
time([[Sourcing ftdetect script at: /home/ronan/.cache/nvim/site/pack/packer/opt/Dockerfile.vim/ftdetect/docker-compose.vim]], false)
vim.cmd("augroup END")
if should_profile then save_profiles() end

end)

if not no_errors then
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
