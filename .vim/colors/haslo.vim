" Vim color file - haslo
let g:colors_name = "haslo"

" ==========================
" Highlighting Functions
" ==========================
function! s:SetHi(group, ...)
  let fg = a:1
  let bg = s:Background
  let attrs = "none"
  if  type(get(a:, 2)) == v:t_dict
    let bg = a:2
    if  type(get(a:, 3)) == v:t_string
      let attrs = a:3
    endif
  elseif type(get(a:, 2)) == v:t_string
    let attrs = a:2
  endif
  exec "hi ".a:group." guifg=".fg.gui." ctermfg=". fg.cterm256." guibg=".bg.gui." ctermbg=". bg.cterm256." gui=".attrs." cterm=".attrs
endfun

function! s:loadHighlights(highlights)
  for [group, groupArgs] in items(a:highlights)
    call call("s:SetHi", [group] + groupArgs)
  endfor
endfunction

augroup PluginOverrides
    au!
    au FileType python call s:SetSemshi()
augroup END

function! s:ShiftLeft(x, count) abort
    let result = a:x
    let c = a:count
    while c > 0
        let result = result * 2
        let c = c - 1
    endwhile
endfunction

function! s:To8Bit(colour) abort
    let red = str2nr(a:colour[1:2], 16)
    let green = str2nr(a:colour[3:4], 16)
    let blue = str2nr(a:colour[5:6], 16)
    let term_colour = s:ShiftLeft(round(red * 255/7), 5) + s:ShiftLeft(round(green * 255/7), 2) + round(blue * 255/3)
    return str2nr(string(term_colour))
endfunction

function! s:ConstructColour(colour)
    return {"gui": a:colour, "cterm256":s:To8Bit(a:colour)}
endfunction

" ==========================
" Color Variables
" ==========================
let s:Black             = s:ConstructColour("#262626")
let s:Grey1             = s:ConstructColour("#3E3D32")
let s:Grey2             = s:ConstructColour("#49483E")
let s:Grey3             = s:ConstructColour("#8B8B8B")
let s:Steel             = s:ConstructColour("#bdbdbd")
let s:White             = s:ConstructColour("#F8F8F8")
let s:Violet            = s:ConstructColour("#D484FF")
let s:Blue              = s:ConstructColour("#1e88e5")
let s:Cyan              = s:ConstructColour("#4DD0E1")
let s:LightBlue         = s:ConstructColour("#4FC3F7")
let s:Green             = s:ConstructColour("#A9FF68")
let s:Yellow            = s:ConstructColour("#FFEB3B")
let s:LightYellow       = s:ConstructColour("#FFF59D")
let s:Orange            = s:ConstructColour("#F57C00")
let s:LightRed          = s:ConstructColour("#ef9a9a")
let s:Red               = s:ConstructColour("#F70067")


let s:Normal = s:White
let s:Decoration = s:Steel
let s:Hidden = s:Grey3
let s:BuiltIn = s:Red
let s:VarName = s:Green
let s:FuncName = s:LightBlue
let s:TypeName = s:Violet
let s:Key = s:Cyan
let s:Val = s:Violet
let s:String = s:LightYellow
let s:Operator = s:Orange
let s:Success = s:Green
let s:Warning = s:Yellow
let s:Info = s:Cyan
let s:Error = s:Red

let FloatBackground   = s:ConstructColour("#233444")
let Background = &background == "dark" ? {"gui": "NONE", "cterm256": "NONE"} : s:White
let s:Background = Background

let highlights = {}
" Vim
call s:loadHighlights({
\ "Cursor": [s:Black, s:Red],
\ "CursorLine": [Background, s:Grey1],
\ "CursorColumn": [Background, s:Grey1],
\ "ColorColumn": [Background, s:Grey1],
\ "LineNr": [s:Hidden],
\ "CursorLineNr": [s:Success, "bold"],
\ "VertSplit": [s:Hidden],
\ "MatchParen": [s:Success, "underline"],
\ "StatusLine": [s:Hidden],
\ "StatusLineNC": [s:Steel],
\ "IncSearch": [s:Green, "bold,underline"],
\ "Search": [s:Green, "bold,underline"],
\ "Directory": [s:Blue],
\ "Folded": [s:Grey3],
\ "WildMenu": [s:Cyan],
\ "VisualNOS": [s:Grey2, s:LightYellow],
\ "ModeMsg": [s:LightYellow],
\ "FoldColumn": [s:Steel],
\ "MoreMsg": [s:LightYellow],
\ "cursorim": [s:Violet],
\ "Pmenu": [s:Steel, FloatBackground],
\ "PmenuSel": [Background, s:Grey2, "bold"],
\ "PMenuSbar": [Background, FloatBackground],
\ "PMenuThumb": [Background, s:Steel],
\ "Visual": [Background, s:Grey1, "bold"],
\ "EndOfBuffer": [s:Black],
\ "Underlined": [Background, "underline"],
\ "SpellBad": [Background, "undercurl"],
\ "SpellCap": [Background, "undercurl"],
\ "SpellLocal": [Background, "undercurl"],
\ "SignColumn": [s:Key],
\ "Question": [s:Info],
\ "TabLineFill": [s:Grey3],
\ })

" General
call s:loadHighlights({
\ "Normal": [s:Normal],
\ "Boolean": [s:Val],
\ "Character": [s:Val],
\ "Comment": [s:Hidden],
\ "Conditional": [s:BuiltIn],
\ "Constant": [s:VarName],
\ "Define": [s:BuiltIn],
\ "DiffAdd": [s:Success],
\ "DiffChange": [s:Warning],
\ "DiffDelete": [s:Error],
\ "DiffText": [s:Error],
\ "ErrorMsg": [s:Error],
\ "WarningMsg": [s:Warning],
\ "Float": [s:Val],
\ "Function": [s:FuncName],
\ "Identifier": [s:VarName],
\ "Keyword": [s:BuiltIn],
\ "Label": [s:Key],
\ "NonText": [s:Hidden],
\ "Number": [s:Val],
\ "Operator": [s:Operator],
\ "PreProc": [s:Key],
\ "Special": [s:Cyan],
\ "SpecialKey": [s:BuiltIn],
\ "Statement": [s:BuiltIn],
\ "String": [s:String],
\ "Tag": [s:Key],
\ "Title": [s:Normal, "bold"],
\ "Todo": [s:Normal, "bold"],
\ "Type": [s:TypeName],
\ "SpecialComment": [s:Hidden, "bold"],
\ "Typedef": [s:TypeName],
\ "PreCondit": [s:BuiltIn],
\ "Include": [s:BuiltIn],
\ "Ignore": [s:BuiltIn],
\ "Delimiter": [s:Decoration],
\ "Error": [s:Error],
\ "Conceal": [s:Hidden],
\ })

" Viml
call s:loadHighlights({
\ "vimContinue": [s:Decoration],
\ "vimFunction": [s:FuncName],
\ "vimIsCommand": [s:VarName],
\ })

" Haskell
call s:loadHighlights({
\ "haskellIdentifier": [s:FuncName],
\ "haskellDecl": [s:BuiltIn],
\ })

" Vim Fugitive
call s:loadHighlights({
\ "diffRemoved": [s:Error],
\ "diffAdded": [s:Success],
\ })

" HTML
call s:loadHighlights({
\ "htmlTagName": [s:Key],
\ "htmlSpecialTagName": [s:TypeName],
\ "htmlTag": [s:Decoration],
\ "htmlEndTag": [s:Decoration],
\ "htmlArg": [s:VarName],
\ })

" Vim Signify
call s:loadHighlights({
\ "SignifySignAdd": [s:Success, "bold"],
\ "SignifySignDelete": [s:Error, "bold"],
\ "SignifySignChange": [s:Warning, "bold"],
\ })

" Coc.nvim
call s:loadHighlights({
\ "CocErrorSign": [s:Error],
\ "CocWarningSign": [s:Warning],
\ "CocInfoSign": [s:Info],
\ "CocHintSign": [s:Info],
\ "CocHighlightText": [Background, "underline"],
\ "CocCodeLens": [s:Hidden, "bold"],
\ "CocListFgGreen": [s:Green],
\ "CocListFgRed": [s:Red],
\ "CocListFgBlack": [s:Black],
\ "CocListFgYellow": [s:Yellow],
\ "CocListFgBlue": [s:Blue],
\ "CocListFgMagenta": [s:Violet],
\ "CocListFgCyan": [s:Cyan],
\ "CocListFgWhite": [s:White],
\ "CocListFgGrey": [s:Grey3],
\ })

" ALE
call s:loadHighlights({
\ "ALEWarningSign": [s:Warning],
\ "ALEVirtualTextError": [s:Error],
\ "ALEVirtualTextWarning": [s:Warning],
\ "ALEVirtualTextInfo": [s:Info],
\ })

" Markdown
call s:loadHighlights({
\ "markdownHeadingDelimiter": [s:BuiltIn],
\ "markdownCodeDelimiter": [s:BuiltIn],
\ "markdownRule": [s:BuiltIn],
\ })

" Semshi
function! s:SetSemshi()
  call s:loadHighlights({
    \ "semshiLocal": [s:VarName],
    \ "semshiGlobal": [s:TypeName],
    \ "semshiImported": [s:TypeName],
    \ "semshiParameter": [s:VarName],
    \ "semshiParameterUnused": [s:Hidden],
    \ "semshiFree": [s:VarName],
    \ "semshiBuiltin": [s:BuiltIn],
    \ "semshiAttribute": [s:Key],
    \ "semshiSelf": [s:Key, "bold"],
    \ "semshiUnresolved": [s:Warning],
    \ "semshiSelected": [s:Background, "underline"],
    \ "semshiErrorSign": [s:Error],
    \ "semshiErrorChar": [s:Error],
    \ })
endfunction

" Makefile
call s:loadHighlights({
\ "makeCommands": [s:Normal, "bold"],
\ })

" vim-signature
call s:loadHighlights({
\ "SignatureMarkText": [s:TypeName, "bold"],
\ })

" Vista.vim
call s:loadHighlights({
\ "VistaScope": [s:TypeName, "bold"],
\ "VistaTag": [s:FuncName],
\ })

" LeaderF
call s:loadHighlights({
\ "Lf_hl_popup_window": [s:Normal, FloatBackground],
\ "Lf_hl_popup_blank": [s:Hidden, FloatBackground],
\ "Lf_hl_popup_inputText": [s:Key, FloatBackground],
\ "Lf_hl_cursorline": [s:Info, FloatBackground  , "bold"],
\ })

" vim-which-key
call s:loadHighlights({
\ "WhichKeySeperator": [s:BuiltIn],
\ "WhichKeyFloating": [s:VarName, FloatBackground, "bold"],
\ "WhichKeyGroup": [s:TypeName],
\ "WhichKey": [s:VarName],
\ "WhichKeyDesc": [s:Info, "bold"],
\ })

" TypeScript
call s:loadHighlights({
\ "typescriptVariable": [s:BuiltIn],
\ "typescriptImport": [s:BuiltIn],
\ "typescriptExport": [s:BuiltIn],
\ "typescriptCall": [s:VarName],
\ "typescriptTypeReference": [s:TypeName],
\ "typescriptArrowFunc": [s:BuiltIn],
\ "typescriptBraces": [s:Decoration],
\ "typescriptMember": [s:Green],
\ "typescriptObjectLabel": [s:Key],
\ "typescriptStringLiteralType": [s:TypeName],
\ "typescriptInterfaceName": [s:TypeName],
\ "typescriptFuncType": [s:VarName],
\ "typescriptFuncTypeArrow": [s:BuiltIn],
\ })

" JSX/TSX
call s:loadHighlights({
\ "jsxTagName": [s:Key],
\ "jsxComponentName": [s:TypeName],
\ "jsxAttrib": [s:Green],
\ })
"
" Javascript
call s:loadHighlights({
\ "jsImport": [s:BuiltIn],
\ "jsExport": [s:BuiltIn],
\ "jsVariableType": [s:BuiltIn],
\ "jsAssignmentEqual": [s:BuiltIn],
\ })

" vim-jumpmotion
call s:loadHighlights({
\ "JumpMotion": [s:Red, "bold"],
\ "JumpMotionTail": [s:Yellow],
\ })

call s:loadHighlights({
  \ "hiPairs_matchPair": [s:Success, "bold,underline"],
  \ "hiPairs_unmatchPair": [s:Error, "bold,underline"]
  \ })
