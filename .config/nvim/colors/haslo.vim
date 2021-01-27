" Vim color file - haslo
let g:colors_name = "haslo"
if get(g:, "haslo_loaded") 
  finish
endif
let g:haslo_loaded = 1
lua require("colors")
