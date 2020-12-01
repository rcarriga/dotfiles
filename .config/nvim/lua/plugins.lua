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
