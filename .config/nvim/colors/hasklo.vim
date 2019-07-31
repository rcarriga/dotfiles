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
" Highlighting Functions
" ==========================
" Inspired by https://github.com/jaredgorski/SpaceCamp
"             https://github.com/tomasiser/vim-code-dark 
"             https://github.com/chriskempson/base16-vim
function! SetHi(group, fg, bg, attr)
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

augroup PluginOverrides
    au!
    au FileType python call SetSemshi()
augroup END

function! SetSemshi()
    call SetHi("semshiLocal",           s:haskloGold,       s:haskloBackground, "none")
    call SetHi("semshiGlobal",          s:haskloOrange,     s:haskloBackground, "none")
    call SetHi("semshiImported",        s:haskloOrange,     s:haskloBackground, "none")
    call SetHi("semshiParameter",       s:haskloBlue,       s:haskloBackground, "none")
    call SetHi("semshiParameterUnused", s:haskloGray3,      s:haskloBackground, "none")
    call SetHi("semshiFree",            s:haskloBlue,       s:haskloBackground, "none")
    call SetHi("semshiBuiltin",         s:haskloViolet,     s:haskloBackground, "none")
    call SetHi("semshiAttribute",       s:haskloCyan,       s:haskloBackground, "none")
    call SetHi("semshiSelf",            s:haskloSteel,      s:haskloBackground, "none")
    call SetHi("semshiUnresolved",      s:haskloGray3,      s:haskloBackground, "none")
    call SetHi("semshiSelected",        s:haskloBackground, s:haskloBackground, "underline")
    call SetHi("semshiErrorSign",       s:haskloRed,        s:haskloBackground, "none")
    call SetHi("semshiErrorChar",       s:haskloRed,        s:haskloBackground, "none")
endfunction

" ==========================
" Color Variables
" ==========================
let s:haskloBlack           = {"gui": "#262626", "cterm256": "235"}
let s:haskloGray1           = {"gui": "#3E3D32", "cterm256": "237"}
let s:haskloGray2           = {"gui": "#49483E", "cterm256": "239"}
let s:haskloGray3           = {"gui": "#8B8B8B", "cterm256": "242"}
let s:haskloSteel           = {"gui": "#DEDEDE", "cterm256": "253"}
let s:haskloWhite           = {"gui": "#F8F8F8", "cterm256": "231"}
let s:haskloViolet          = {"gui": "#D484FF", "cterm256": "170"}
let s:haskloBlue            = {"gui": "#429ADD", "cterm256": "110"}
let s:haskloCyan            = {"gui": "#52D3D1", "cterm256": "110"}
let s:haskloLightBlue       = {"gui": "#68DEFF", "cterm256": "111"}
let s:haskloLightGreen      = {"gui": "#A9FF68", "cterm256": "71"}
let s:haskloGreen           = {"gui": "#96F291", "cterm256": "71"}
let s:haskloLime            = {"gui": "#C4E572", "cterm256": "71"}
let s:haskloYellow          = {"gui": "#FFEC63", "cterm256": "191"}
let s:haskloDirtyYellow     = {"gui": "#F2F293", "cterm256": "191"}
let s:haskloGold            = {"gui": "#F0D50C", "cterm256": "220"}
let s:haskloOrange          = {"gui": "#FF9F63", "cterm256": "202"}
let s:haskloLightRed        = {"gui": "#FF5661", "cterm256": "1"}
let s:haskloRed             = {"gui": "#F70067", "cterm256": "1"}
let s:haskloFloatBackground = {"gui": "#262626", "cterm256": "NONE"}
let s:haskloBackground      = {"gui": "NONE", "cterm256": "NONE"}

" Vim
call SetHi("Cursor",                s:haskloBlack,       s:haskloWhite,           "none")
call SetHi("CursorLine",            s:haskloBackground,  s:haskloGray2,           "none")
call SetHi("CursorColumn",          s:haskloGray1,       s:haskloBackground,      "none")
call SetHi("ColorColumn",           s:haskloGray1,       s:haskloBackground,      "none")
call SetHi("LineNr",                s:haskloGray3,       s:haskloBackground,      "none")
call SetHi("CursorLineNr",          s:haskloSteel,       s:haskloBackground,      "none")
call SetHi("VertSplit",             s:haskloGray3,       s:haskloGray2,           "none")
call SetHi("MatchParen",            s:haskloRed,         s:haskloBackground,      "underline")
call SetHi("StatusLine",            s:haskloGray3,       s:haskloBackground,      "none")
call SetHi("StatusLineNC",          s:haskloSteel,       s:haskloBackground,      "none")
call SetHi("Pmenu",                 s:haskloLightBlue,   s:haskloFloatBackground, "none")
call SetHi("PmenuSel",              s:haskloBackground,  s:haskloGray2,           "none")
call SetHi("IncSearch",             s:haskloBlack,       s:haskloGray3,           "none")
call SetHi("Search",                s:haskloWhite,       s:haskloGray2,           "none")
call SetHi("Directory",             s:haskloBlue,        s:haskloBackground,      "none")
call SetHi("Folded",                s:haskloGray3,       s:haskloBackground,      "none")
call SetHi("WildMenu",              s:haskloCyan,        s:haskloBackground,      "none")
call SetHi("PMenuSbar",             s:haskloBackground,  s:haskloBlack,           "none")
call SetHi("VisualNOS",             s:haskloGray2,       s:haskloGold,            "none")
call SetHi("ModeMsg",               s:haskloGold,        s:haskloBackground,      "none")
call SetHi("FoldColumn",            s:haskloWhite,       s:haskloBackground,      "none")
call SetHi("MoreMsg",               s:haskloGold,        s:haskloBackground,      "none")
call SetHi("PMenuThumb",            s:haskloCyan,        s:haskloBackground,      "none")
call SetHi("cursorim",              s:haskloViolet,      s:haskloBackground,      "none")

" General
call SetHi("Normal",                s:haskloWhite,       s:haskloBackground,      "none")
call SetHi("Visual",                s:haskloBackground,  s:haskloGray2,           "none")
call SetHi("Boolean",               s:haskloViolet,      s:haskloBackground,      "none")
call SetHi("Character",             s:haskloGold,        s:haskloBackground,      "none")
call SetHi("Comment",               s:haskloGray3,       s:haskloBackground,      "none")
call SetHi("Conditional",           s:haskloRed,         s:haskloBackground,      "none")
call SetHi("Constant",              s:haskloViolet,      s:haskloBackground,      "none")
call SetHi("Define",                s:haskloCyan,        s:haskloBackground,      "none")
call SetHi("DiffAdd",               s:haskloGreen,       s:haskloBackground,      "none")
call SetHi("DiffChange",            s:haskloYellow,      s:haskloBackground,      "none")
call SetHi("DiffDelete",            s:haskloRed,         s:haskloBackground,      "none")
call SetHi("DiffText",              s:haskloRed,         s:haskloBackground,      "none")
call SetHi("ErrorMsg",              s:haskloRed,         s:haskloBackground,      "none")
call SetHi("WarningMsg",            s:haskloOrange,      s:haskloBackground,      "none")
call SetHi("Float",                 s:haskloViolet,      s:haskloBackground,      "none")
call SetHi("Function",              s:haskloCyan,        s:haskloBackground,      "none")
call SetHi("Identifier",            s:haskloOrange,      s:haskloBackground,      "none")
call SetHi("Keyword",               s:haskloRed,         s:haskloBackground,      "none")
call SetHi("Label",                 s:haskloGreen,       s:haskloBackground,      "none")
call SetHi("NonText",               s:haskloGray3,       s:haskloBackground,      "none")
call SetHi("Number",                s:haskloViolet,      s:haskloBackground,      "none")
call SetHi("Operator",              s:haskloOrange,      s:haskloBackground,      "none")
call SetHi("PreProc",               s:haskloGreen,       s:haskloBackground,      "none")
call SetHi("Special",               s:haskloViolet,      s:haskloBackground,      "none")
call SetHi("SpecialKey",            s:haskloCyan,        s:haskloBackground,      "none")
call SetHi("Statement",             s:haskloRed,         s:haskloBackground,      "none")
call SetHi("StorageClass",          s:haskloOrange,      s:haskloBackground,      "none")
call SetHi("String",                s:haskloDirtyYellow, s:haskloBackground,      "none")
call SetHi("Tag",                   s:haskloRed,         s:haskloBackground,      "none")
call SetHi("Title",                 s:haskloOrange,      s:haskloBackground,      "bold")
call SetHi("Todo",                  s:haskloWhite,       s:haskloBackground,      "standout")
call SetHi("Type",                  s:haskloCyan,        s:haskloBackground,      "none")
call SetHi("Underlined",            s:haskloGray3,       s:haskloBackground,      "underline")
call SetHi("SpellBad",              s:haskloBackground,  s:haskloBackground,      "undercurl")
call SetHi("SpellCap",              s:haskloBackground,  s:haskloBackground,      "undercurl")
call SetHi("SpellLocal",            s:haskloBackground,  s:haskloBackground,      "undercurl")
call SetHi("SignColumn",            s:haskloGreen,       s:haskloBackground,      "none")
call SetHi("SpecialComment",        s:haskloGray2,       s:haskloBackground,      "none")
call SetHi("Typedef",               s:haskloCyan,        s:haskloBackground,      "none")
call SetHi("PreCondit",             s:haskloGreen,       s:haskloBackground,      "none")
call SetHi("Include",               s:haskloGreen,       s:haskloBackground,      "none")
call SetHi("Ignore",                s:haskloGray3,       s:haskloBackground,      "none")
call SetHi("Debug",                 s:haskloSteel,       s:haskloBackground,      "none")
call SetHi("SpecialChar",           s:haskloRed,         s:haskloBackground,      "none")
call SetHi("Delimiter",             s:haskloGray3,       s:haskloBackground,      "none")
call SetHi("Question",              s:haskloCyan,        s:haskloBackground,      "none")
call SetHi("Exception",             s:haskloGreen,       s:haskloBackground,      "none")
call SetHi("Error",                 s:haskloRed,         s:haskloBackground,      "none")
call SetHi("Repeat",                s:haskloRed,         s:haskloBackground,      "none")
call SetHi("Structure",             s:haskloCyan,        s:haskloBackground,      "none")
call SetHi("Macro",                 s:haskloGold,        s:haskloBackground,      "none")
call SetHi("TabLineFill ",          s:haskloGray3,       s:haskloBackground,      "none")

" Viml
call SetHi("vimOption",             s:haskloLightBlue,   s:haskloBackground,      "none")
call SetHi("vimCommand",            s:haskloRed,         s:haskloBackground,      "none")
call SetHi("vimVar",                s:haskloLightGreen,  s:haskloBackground,      "none")

" Haskell - Used with haskell-vim: https://github.com/neovimhaskell/haskell-vim
call SetHi("haskellIdentifier",     s:haskloLightBlue,   s:haskloBackground,      "none")
call SetHi("haskellType",           s:haskloViolet,      s:haskloBackground,      "none")
call SetHi("haskellImportKeywords", s:haskloOrange,      s:haskloBackground,      "none")
call SetHi("haskellDecl",           s:haskloOrange,      s:haskloBackground,      "none")
call SetHi("haskellOperators",      s:haskloRed,         s:haskloBackground,      "none")
call SetHi("haskellDelimiter",      s:haskloRed,         s:haskloBackground,      "none")

" JSON
call SetHi("jsonKeyword",           s:haskloRed,         s:haskloBackground,      "none")

" Vim Fugitive
call SetHi("diffRemoved",           s:haskloRed,         s:haskloBackground,      "none")
call SetHi("diffAdded",             s:haskloGreen,       s:haskloBackground,      "none")

" HTML
call SetHi("htmlTagN",              s:haskloGreen,       s:haskloBackground,      "none")

" Vim Signify
call SetHi("SignifySignAdd",        s:haskloGreen,       s:haskloBackground,      "bold")
call SetHi("SignifySignDelete",     s:haskloRed,         s:haskloBackground,      "bold")
call SetHi("SignifySignChange",     s:haskloYellow,      s:haskloBackground,      "bold")

" Coc.nvim
call SetHi("CocErrorSign",          s:haskloRed,         s:haskloBackground,      "none")
call SetHi("CocWarningSign",        s:haskloOrange,      s:haskloBackground,      "none")
call SetHi("CocInfoSign",           s:haskloYellow,      s:haskloBackground,      "none")
call SetHi("CocHintSign",           s:haskloCyan,        s:haskloBackground,      "none")
call SetHi("CocHighlightText",      s:haskloBackground,  s:haskloBackground,      "underline")

" Python
call SetHi("pythonDecoratorName",   s:haskloViolet,      s:haskloBackground,      "none")
call SetHi("pythonStatement",       s:haskloRed,         s:haskloBackground,      "none")
call SetHi("pythonFunction",        s:haskloLightBlue,   s:haskloBackground,      "none")
call SetHi("pythonOperator",        s:haskloRed,         s:haskloBackground,      "none")

" ZSH
call SetHi("zshDeref",              s:haskloViolet,      s:haskloBackground,      "none")
call SetHi("zshCommands",           s:haskloLightBlue,   s:haskloBackground,      "none")
call SetHi("zshOperator",           s:haskloRed,         s:haskloBackground,      "none")
call SetHi("zshRedirect",           s:haskloRed,         s:haskloBackground,      "none")

" INI Files
call SetHi("dosiniLabel",           s:haskloRed,         s:haskloBackground,      "none")
call SetHi("dosiniValue",           s:haskloLightBlue,   s:haskloBackground,      "none")
call SetHi("dosiniHeader",          s:haskloViolet,      s:haskloBackground,      "none")

" ALE
call SetHi("ALEWarningSign",        s:haskloYellow,      s:haskloBackground,      "none")
call SetHi("ALEVirtualTextError",   s:haskloRed,         s:haskloBackground,      "none")
call SetHi("ALEVirtualTextWarning", s:haskloYellow,      s:haskloBackground,      "none")
call SetHi("ALEVirtualTextInfo",    s:haskloCyan,        s:haskloBackground,      "none")

" Denite
call SetHi("deniteSource_grepFile", s:haskloViolet,      s:haskloBackground,      "none")
call SetHi("deniteSource_grep",     s:haskloWhite,       s:haskloBackground,      "none")
call SetHi("deniteSource_file_rec", s:haskloViolet,      s:haskloBackground,      "none")
call SetHi("deniteMatchedChar",     s:haskloOrange,      s:haskloBackground,      "bold")

" Spelunker
call SetHi("SpelunkerSpellBad",     s:haskloBackground,  s:haskloBackground,      "undercurl")

" NERDTree
call SetHi("NERDTreeFile",          s:haskloSteel,       s:haskloBackground,      "none")
call SetHi("NERDTreeDir",           s:haskloLightBlue,   s:haskloBackground,      "none")
call SetHi("NERDTreeFlags",         s:haskloOrange,      s:haskloBackground,      "none")
call SetHi("NERDTreeCWD",           s:haskloDirtyYellow, s:haskloBackground,      "none")
call SetHi("NERDTreeUp",            s:haskloGray2,       s:haskloBackground,      "none")

" Vim Sneak
call SetHi("Sneak", s:haskloRed, s:haskloBackground, "undercurl")
call SetHi("SneakLabel", s:haskloRed, s:haskloSteel, "undercurl")
