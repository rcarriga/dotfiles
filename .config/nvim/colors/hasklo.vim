" Vim color file - hasklo
set background=dark
if version > 580
	hi clear
	if exists("syntax_on")
		syntax reset
	endif
endif

let g:colors_name = "hasklo"
scriptencoding utf-8

" ==========================
" Highlighting Function
" ==========================
"  >> (inspired by https://github.com/tomasiser/vim-code-dark and https://github.com/chriskempson/base16-vim)
fun! <sid>hi(group, fg, bg, attr)
  if !empty(a:fg)
    exec "hi " . a:group . " guifg=" . a:fg.gui . " ctermfg=" .  a:fg.cterm256
  endif
  if !empty(a:bg)
    exec "hi " . a:group . " guibg=" . a:bg.gui . " ctermbg=" .  a:bg.cterm256
  endif
  if a:attr != ""
    exec "hi " . a:group . " gui=" . a:attr . " cterm=" . a:attr
  endif
endfun

" ==========================
" Color Variables
" ==========================
let s:haskloBlack = {"gui": "#121212", "cterm256": "233"}
let s:haskloBlack2 = {"gui": "#262626", "cterm256": "235"}
let s:haskloGray1 = {"gui": "#3E3D32", "cterm256": "237"}
let s:haskloGray2 = {"gui": "#49483E", "cterm256": "239"}
let s:haskloGray3 = {"gui": "#8B8B8B", "cterm256": "242"}
let s:haskloSteel = {"gui": "#DEDEDE", "cterm256": "253"}
let s:haskloWhite = {"gui": "#F8F8F8", "cterm256": "231"}
let s:haskloViolet = {"gui": "#DE00F2", "cterm256": "170"}
let s:haskloBlue = {"gui": "#429ADD", "cterm256": "110"}
let s:haskloCyan = {"gui": "#00FFFA", "cterm256": "110"}
let s:haskloLightBlue = {"gui": "#89c6f4", "cterm256": "111"}
let s:haskloLightGreen = {"gui": "#A9FF68", "cterm256": "71"}
let s:haskloGreen = {"gui": "#00D122", "cterm256": "71"}
let s:haskloYellow = {"gui": "#FFF200", "cterm256": "191"}
let s:haskloGold = {"gui": "#F0D50C", "cterm256": "220"}
let s:haskloOrange = {"gui": "#FF8716", "cterm256": "202"}
let s:haskloLightRed = {"gui": "#FF5661", "cterm256": "1"}
let s:haskloRed = {"gui": "#F70067", "cterm256": "1"}
let s:none = {"gui": "NONE", "cterm256": "NONE"}

" Vim
call <sid>hi("Cursor", s:haskloBlack, s:haskloWhite, "none")
call <sid>hi("CursorLine", s:none, s:haskloGray1, "none")
call <sid>hi("CursorColumn", s:haskloGray1, s:none, "none")
call <sid>hi("ColorColumn", s:none, s:haskloGray1, "none")
call <sid>hi("LineNr", s:haskloGray3, s:haskloBlack2, "none")
call <sid>hi("CursorLineNr", s:haskloSteel, s:none, "none")
call <sid>hi("VertSplit", s:haskloGray3, s:haskloGray3, "none")
call <sid>hi("MatchParen", s:haskloRed, s:none, "bold")
call <sid>hi("StatusLine", s:haskloGray2, s:haskloSteel, "none")
call <sid>hi("StatusLineNC", s:haskloGray3, s:none, "none")
call <sid>hi("Pmenu", s:haskloLightBlue, s:none, "none")
call <sid>hi("PmenuSel", s:none, s:haskloGray2, "none")
call <sid>hi("IncSearch", s:haskloBlack, s:haskloGray3, "none")
call <sid>hi("Search", s:haskloWhite, s:haskloGray2, "none")
call <sid>hi("Directory", s:haskloGreen, s:none, "none")
call <sid>hi("Folded", s:haskloGray2, s:none, "none")

" General
call <sid>hi("Normal", s:haskloWhite, s:none, "none")
call <sid>hi("Visual", s:none, s:haskloGray2, "none")
call <sid>hi("Boolean", s:haskloViolet, s:none, "none")
call <sid>hi("Character", s:haskloGold, s:none, "none")
call <sid>hi("Comment", s:haskloGray3, s:none, "none")
call <sid>hi("Conditional", s:haskloRed, s:none, "none")
call <sid>hi("Constant", s:haskloViolet, s:none, "none")
call <sid>hi("Define", s:haskloCyan, s:none, "none")
call <sid>hi("DiffAdd", s:none, s:haskloGreen, "none")
call <sid>hi("DiffChange", s:none, s:haskloGold, "none")
call <sid>hi("DiffDelete", s:none, s:haskloRed, "none")
call <sid>hi("DiffText", s:none, s:haskloRed, "none")
call <sid>hi("ErrorMsg", s:haskloRed, s:none, "none")
call <sid>hi("WarningMsg", s:haskloOrange, s:none, "none")
call <sid>hi("Float", s:haskloViolet, s:none, "none")
call <sid>hi("Function", s:haskloCyan, s:none, "none")
call <sid>hi("Identifier", s:haskloOrange, s:none, "none")
call <sid>hi("Keyword", s:haskloRed, s:none, "none")
call <sid>hi("Label", s:haskloGreen, s:none, "none")
call <sid>hi("NonText", s:haskloSteel, s:none, "none")
call <sid>hi("Number", s:haskloViolet, s:none, "none")
call <sid>hi("Operator", s:haskloOrange, s:none, "none")
call <sid>hi("PreProc", s:haskloGreen, s:none, "none")
call <sid>hi("Special", s:haskloCyan, s:none, "none")
call <sid>hi("SpecialKey", s:haskloCyan, s:none, "none")
call <sid>hi("Statement", s:haskloRed, s:none, "none")
call <sid>hi("StorageClass", s:haskloOrange, s:none, "none")
call <sid>hi("String", s:haskloLightBlue, s:none, "none")
call <sid>hi("Tag", s:haskloRed, s:none, "none")
call <sid>hi("Title", s:haskloOrange, s:none, "bold")
call <sid>hi("Todo", s:haskloWhite, s:none, "inverse,bold")
call <sid>hi("Type", s:haskloCyan, s:none, "none")
call <sid>hi("Underlined", s:haskloGray3, s:none, "underline")
call <sid>hi("SpellBad", s:none, s:none, "undercurl")
call <sid>hi("SpellCap", s:none, s:none, "undercurl")
call <sid>hi("SpellLocal", s:none, s:none, "undercurl")
call <sid>hi("TabLineFill ", s:haskloGray3, s:none,"none")
call <sid>hi("WildMenu", s:haskloCyan, s:none, "none")
call <sid>hi("SignColumn", s:haskloGreen, s:none,"none")
call <sid>hi("SpecialComment", s:haskloGray2, s:none, "none")
call <sid>hi("Typedef", s:haskloCyan, s:none, "none")
call <sid>hi("PreCondit", s:haskloGreen, s:none, "none")
call <sid>hi("Include", s:haskloGreen, s:none, "none")
call <sid>hi("StatusLineNC", s:haskloGray3, s:none, "none")
call <sid>hi("Ignore", s:haskloGray3, s:none, "none")
call <sid>hi("Debug", s:haskloSteel, s:none, "none")
call <sid>hi("PMenuSbar", s:none, s:haskloBlack2, "none")
call <sid>hi("SpecialChar", s:haskloRed, s:none, "none") 
call <sid>hi("Delimiter", s:haskloGray3, s:none, "none") 
call <sid>hi("Question", s:haskloCyan, s:none, "none") 
call <sid>hi("VisualNOS", s:haskloGray2, s:haskloGold, "none")
call <sid>hi("ModeMsg", s:haskloGold, s:none, "none")
call <sid>hi("CursorColumn", s:none, s:haskloGray2, "none")
call <sid>hi("FoldColumn", s:haskloWhite, s:none, "none")
call <sid>hi("MoreMsg", s:haskloGold, s:none, "none")
call <sid>hi("Exception", s:haskloGreen, s:none, "none")
call <sid>hi("Error", s:haskloRed, s:none, "none")
call <sid>hi("PMenuThumb", s:haskloCyan, s:none, "none")
call <sid>hi("Repeat", s:haskloRed, s:none, "none")
call <sid>hi("Structure", s:haskloCyan, s:none, "none")
call <sid>hi("Macro", s:haskloGold, s:none, "none")
call <sid>hi("cursorim", s:haskloViolet, s:none, "none")

" Viml
call <sid>hi("vimOption", s:haskloCyan, s:none, "none")
call <sid>hi("vimCommand", s:haskloRed, s:none, "none")


" Haskell - Used with haskell-vim: https://github.com/neovimhaskell/haskell-vim
call <sid>hi("haskellIdentifier", s:haskloViolet, s:none, "none")
call <sid>hi("haskellType", s:haskloOrange, s:none, "none")
call <sid>hi("haskellImportKeywords", s:haskloViolet, s:none, "none")
call <sid>hi("haskellDecl", s:haskloCyan, s:none, "none")
call <sid>hi("haskellOperators", s:haskloRed, s:none, "none")
call <sid>hi("haskellDelimiter", s:haskloRed, s:none, "none")

" JSON
call <sid>hi("jsonKeyword", s:haskloRed, s:none, "none")

" Vim Fugitive
call <sid>hi("diffRemoved", s:haskloRed, s:none, "none")
call <sid>hi("diffAdded", s:haskloGreen, s:none, "none")

" HTML
call <sid>hi("htmlTagN", s:haskloGreen, s:none, "none")

" Vim Signify
call <sid>hi("SignifySignAdd", s:haskloGreen, s:none, "bold")
call <sid>hi("SignifySignDelete", s:haskloRed, s:none, "bold")
call <sid>hi("SignifySignChange", s:haskloYellow, s:none, "bold")

" Semshi
call <sid>hi("semshiSelected", s:haskloWhite, s:haskloSteel, "none")
