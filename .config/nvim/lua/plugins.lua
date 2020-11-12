require'colorizer'.setup({
  vim = {names = false};
}, {
  RGB      = true;         -- #RGB hex codes
  RRGGBB   = true;         -- #RRGGBB hex codes
  names    = true;         -- "Name" codes like Blue
  RRGGBBAA = false;        -- #RRGGBBAA hex codes
  rgb_fn   = false;        -- CSS rgb() and rgba() functions
  hsl_fn   = false;        -- CSS hsl() and hsla() functions
  css      = false;        -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
  css_fn   = false;        -- Enable all CSS *functions*: rgb_fn, hsl_fn
  -- Available modes: foreground, background
  mode     = 'background'; -- Set the display mode.
})

local dap = require('dap')

dap.adapters.python = {
  type = 'executable';
  command = 'python';
  args = { '-m', 'debugpy.adapter' };
}

dap.configurations.python = {
  {
    type = 'python';
    request = 'launch';
    name = "Launch file";
    program = "${file}";
    --console= "internalConsole"; --one out of "internalConsole", "integratedTerminal", "externalConsole"
    pythonPath = function()
      return 'python'
      --local cwd = vim.fn.getcwd()
      --if vim.fn.executable(cwd .. '/venv/bin/python') then
        --return cwd .. '/venv/bin/python'
      --elseif vim.fn.executable(cwd .. '/.venv/bin/python') then
        --return cwd .. '/.venv/bin/python'
      --else
        --return '/usr/bin/python'
      --end
    end;
  },
}
