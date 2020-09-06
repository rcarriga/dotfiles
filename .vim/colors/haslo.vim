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
    let term_colour = s:ShiftLeft(round(red * 7/255), 5) + s:ShiftLeft(round(green * 7/255), 2) + round(blue * 3/255)
    return str2nr(string(term_colour))
endfunction

function! s:ConstructColour(colour)
    return {"gui": a:colour, "cterm256":s:To8Bit(a:colour)}
endfunction

    let s:Transparent = {"gui": "NONE", "cterm256": "NONE"}
" ==========================
" Color Variables
" ==========================
    if &background == "dark"
  let s:Grey1             = s:ConstructColour("#262626")
  let s:Grey2             = s:ConstructColour("#8B8B8B")
  let s:Grey3             = s:ConstructColour("#bdbdbd")
  let s:Grey4             = s:ConstructColour("#F8F8F8")

  let s:Violet            = s:ConstructColour("#D484FF")
  let s:Cyan              = s:ConstructColour("#00f1f5")
  let s:Green             = s:ConstructColour("#A9FF68")
  let s:Yellow            = s:ConstructColour("#FFF59D")
  let s:Orange            = s:ConstructColour("#F79000")
  let s:Red               = s:ConstructColour("#F70067")
  let FloatBackground   = s:ConstructColour("#233444")
    let s:Background = {"gui": "NONE", "cterm256": "NONE"}
else
  let s:Grey1             = s:ConstructColour("#424242")
  let s:Grey2             = s:ConstructColour("#bdbdbd")
  let s:Grey3             = s:ConstructColour("#8B8B8B")
  let s:Grey4             = s:ConstructColour("#262626")

  let s:Violet            = s:ConstructColour("#8400da")
  let s:Cyan              = s:ConstructColour("#00d8ec")
  let s:Green             = s:ConstructColour("#2E7D32")
  let s:Yellow            = s:ConstructColour("#f9a928")
  let s:Orange            = s:ConstructColour("#f77100")
  let s:Red               = s:ConstructColour("#B71C1C")
  let FloatBackground     = s:ConstructColour("#90A4AE")
  let s:Background        = s:ConstructColour("#ECEFF1")
endif

let s:Normal = s:Grey4
let s:Decoration = s:Grey3
let s:Hidden = s:Grey2
let s:BuiltIn = s:Red
let s:VarName = s:Green
let s:FuncName = s:Cyan
let s:TypeName = s:Violet
let s:Key = s:Cyan
let s:Val = s:Violet
let s:String = s:Yellow
let s:Operator = s:Orange
let s:Success = s:Green
let s:Warning = s:Yellow
let s:Info = s:Cyan
let s:Error = s:Red

call s:loadHighlights({
  \ "User1": [s:Success, FloatBackground],
  \ "User2": [s:Warning, FloatBackground],
  \ "User3": [s:Error,   FloatBackground],
  \ "User4": [s:Grey1, s:Info]
  \ })

let highlights = {}
" Vim
call s:loadHighlights({
    \ "Cursor": [s:Grey1, s:Red],
    \ "CursorLine": [s:Transparent, s:Grey1],
    \ "CursorColumn": [s:Transparent, s:Grey1],
    \ "ColorColumn": [s:Transparent, s:Grey1],
    \ "LineNr": [s:Hidden],
    \ "CursorLineNr": [s:Success, "bold"],
    \ "VertSplit": [s:Hidden],
    \ "MatchParen": [s:Success, "underline"],
    \ "StatusLine": [s:Normal, FloatBackground],
    \ "StatusLineNC": [s:Normal, s:Grey1],
    \ "IncSearch": [s:Green, "bold,underline"],
    \ "Search": [s:Green, "bold,underline"],
    \ "Directory": [s:Cyan],
    \ "Folded": [s:Grey2],
    \ "WildMenu": [s:Cyan],
    \ "VisualNOS": [s:Grey2, s:Yellow],
    \ "ModeMsg": [s:Yellow],
    \ "FoldColumn": [s:Grey3],
    \ "MoreMsg": [s:Yellow],
    \ "cursorim": [s:Violet],
    \ "Pmenu": [s:Grey3, FloatBackground],
    \ "PmenuSel": [s:Transparent, s:Grey2, "bold"],
    \ "PMenuSbar": [s:Transparent, FloatBackground],
    \ "PMenuThumb": [s:Transparent, s:Grey3],
    \ "Visual": [s:Transparent, s:Grey1, "bold"],
    \ "EndOfBuffer": [s:Grey1],
    \ "Underlined": [s:Transparent, "underline"],
    \ "SpellBad": [s:Transparent, "undercurl"],
    \ "SpellCap": [s:Transparent, "undercurl"],
    \ "SpellLocal": [s:Transparent, "undercurl"],
    \ "SignColumn": [s:Key],
    \ "Question": [s:Info],
    \ "TabLineFill": [s:Grey2],
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
    \ "SpecialComment": [s:Info, "bold"],
    \ "Typedef": [s:TypeName],
    \ "PreCondit": [s:BuiltIn],
    \ "Include": [s:BuiltIn],
    \ "Ignore": [s:BuiltIn],
    \ "Delimiter": [s:Decoration],
    \ "Error": [s:Error],
    \ "Conceal": [s:Transparent, "bold"],
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
    \ "htmlSpecialTagName": [s:BuiltIn],
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
    \ "CocHighlightText": [s:Transparent, "underline"],
    \ "CocCodeLens": [s:Hidden, "bold"],
    \ "CocListFgGreen": [s:Green],
    \ "CocListFgRed": [s:Red],
    \ "CocListFgBlack": [s:Grey1],
    \ "CocListFgYellow": [s:Yellow],
    \ "CocListFgBlue": [s:Cyan],
    \ "CocListFgMagenta": [s:Violet],
    \ "CocListFgCyan": [s:Cyan],
    \ "CocListFgWhite": [s:Grey4],
    \ "CocListFgGrey": [s:Grey2],
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
    \ "markdownUrl": [s:Key],
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
    \ "semshiSelf": [s:Normal, "bold"],
    \ "semshiUnresolved": [s:Warning],
    \ "semshiSelected": [s:Transparent, "underline"],
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
    \ "Lf_hl_cursorline": [s:Normal, FloatBackground  , "bold"],
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
    \ "jsParens": [s:Decoration],
    \ "jsObjectBraces": [s:Decoration],
    \ "jsFunctionBraces": [s:Decoration],
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

call s:loadHighlights({
    \ "texBeginEndName": [s:FuncName]
\})

call s:loadHighlights({
  \ "yamlBlockMappingKey": [s:Key]
  \ })
