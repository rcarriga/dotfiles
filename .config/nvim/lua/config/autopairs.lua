local M = {}

function M.post()
  require "pears".setup(
    function(conf)
      conf.preset "tag_matching"
      conf.expand_on_enter(true)
      conf.on_enter(
        function(pears_handle)
          if vim.fn.pumvisible() == 1 and vim.fn.complete_info().selected ~= -1 then
            return vim.fn["compe#confirm"]("<CR>")
          else
            pears_handle()
          end
        end
      )
    end
  )
end
return M
