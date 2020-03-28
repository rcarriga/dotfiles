" Vim color file - haslo
set background=dark
if version > 580
    hi clear
    if exists("syntax_on")
        syntax reset
    endif
endif

let g:colors_name = "haslo"
scriptencoding utf-8

" ==========================
" Highlighting Functions
" ==========================
" Inspired by https://github.com/jaredgorski/SpaceCamp
"             https://github.com/tomasiser/vim-code-dark
"             https://github.com/chriskempson/base16-vim
function! s:SetHi(group, fg, bg, attr)
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
let s:hasloBlack           = s:ConstructColour("#262626")
let s:hasloGrey1           = s:ConstructColour("#3E3D32")
let s:hasloGrey2           = s:ConstructColour("#49483E")
let s:hasloGrey3           = s:ConstructColour("#8B8B8B")
let s:hasloSteel           = s:ConstructColour("#bdbdbd")
let s:hasloWhite           = s:ConstructColour("#F8F8F8")
let s:hasloViolet          = s:ConstructColour("#D484FF")
let s:hasloPurple          = s:ConstructColour("#9C27B0")
let s:hasloBlue            = s:ConstructColour("#1e88e5")
let s:hasloCyan            = s:ConstructColour("#4DD0E1")
let s:hasloLightBlue       = s:ConstructColour("#4FC3F7")
let s:hasloGreen           = s:ConstructColour("#A9FF68")
let s:hasloYellow          = s:ConstructColour("#FFEB3B")
let s:hasloLightYellow     = s:ConstructColour("#FFF59D")
let s:hasloOrange          = s:ConstructColour("#F57C00")
let s:hasloLightRed        = s:ConstructColour("#ef9a9a")
let s:hasloRed             = s:ConstructColour("#F70067")
let s:hasloFloatBackground = s:ConstructColour("#162228")
let s:hasloBackground      = {"gui": "NONE", "cterm256": "NONE"}

" Vim
call s:SetHi("Cursor"                      , s:hasloBlack       , s:hasloWhite           , "none")
call s:SetHi("CursorLine"                  , s:hasloBackground  , s:hasloGrey2           , "none")
call s:SetHi("CursorColumn"                , s:hasloGrey1       , s:hasloBackground      , "none")
call s:SetHi("ColorColumn"                 , s:hasloGrey1       , s:hasloBackground      , "none")
call s:SetHi("LineNr"                      , s:hasloGrey3       , s:hasloBackground      , "none")
call s:SetHi("CursorLineNr"                , s:hasloSteel       , s:hasloBackground      , "none")
call s:SetHi("VertSplit"                   , s:hasloGrey3       , s:hasloBackground      , "none")
call s:SetHi("MatchParen"                  , s:hasloRed         , s:hasloBackground      , "underline")
call s:SetHi("StatusLine"                  , s:hasloGrey3       , s:hasloBackground      , "none")
call s:SetHi("StatusLineNC"                , s:hasloSteel       , s:hasloBackground      , "none")
call s:SetHi("IncSearch"                   , s:hasloGreen       , s:hasloBackground      , "bold,underline")
call s:SetHi("Search"                      , s:hasloGreen       , s:hasloBackground      , "bold,underline")
call s:SetHi("Directory"                   , s:hasloBlue        , s:hasloBackground      , "none")
call s:SetHi("Folded"                      , s:hasloGrey3       , s:hasloBackground      , "none")
call s:SetHi("WildMenu"                    , s:hasloCyan        , s:hasloBackground      , "none")
call s:SetHi("VisualNOS"                   , s:hasloGrey2       , s:hasloLightYellow     , "none")
call s:SetHi("ModeMsg"                     , s:hasloLightYellow , s:hasloBackground      , "none")
call s:SetHi("FoldColumn"                  , s:hasloSteel       , s:hasloBackground      , "none")
call s:SetHi("MoreMsg"                     , s:hasloLightYellow , s:hasloBackground      , "none")
call s:SetHi("cursorim"                    , s:hasloViolet      , s:hasloBackground      , "none")
call s:SetHi("Pmenu"                       , s:hasloSteel       , s:hasloFloatBackground , "none")
call s:SetHi("PmenuSel"                    , s:hasloBackground  , s:hasloGrey2           , "none")
call s:SetHi("PMenuSbar"                   , s:hasloBackground  , s:hasloFloatBackground , "none")
call s:SetHi("PMenuThumb"                  , s:hasloBackground  , s:hasloSteel           , "none")

" General
call s:SetHi("Normal"                      , s:hasloWhite       , s:hasloBackground      , "none")
call s:SetHi("Visual"                      , s:hasloBackground  , s:hasloBlack           , "bold")
call s:SetHi("Boolean"                     , s:hasloViolet      , s:hasloBackground      , "none")
call s:SetHi("Character"                   , s:hasloLightYellow , s:hasloBackground      , "none")
call s:SetHi("Comment"                     , s:hasloGrey3       , s:hasloBackground      , "none")
call s:SetHi("Conditional"                 , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("Constant"                    , s:hasloViolet      , s:hasloBackground      , "none")
call s:SetHi("Define"                      , s:hasloCyan        , s:hasloBackground      , "none")
call s:SetHi("DiffAdd"                     , s:hasloGreen       , s:hasloBackground      , "none")
call s:SetHi("DiffChange"                  , s:hasloLightYellow , s:hasloBackground      , "none")
call s:SetHi("DiffDelete"                  , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("DiffText"                    , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("ErrorMsg"                    , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("WarningMsg"                  , s:hasloOrange      , s:hasloBackground      , "none")
call s:SetHi("Float"                       , s:hasloViolet      , s:hasloBackground      , "none")
call s:SetHi("Function"                    , s:hasloCyan        , s:hasloBackground      , "none")
call s:SetHi("Identifier"                  , s:hasloWhite         , s:hasloBackground      , "none")
call s:SetHi("Keyword"                     , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("Label"                       , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("NonText"                     , s:hasloGrey3       , s:hasloBackground      , "none")
call s:SetHi("EndOfBuffer"                 , s:hasloBlack       , s:hasloBackground      , "none")
call s:SetHi("Number"                      , s:hasloViolet      , s:hasloBackground      , "none")
call s:SetHi("Operator"                    , s:hasloOrange      , s:hasloBackground      , "none")
call s:SetHi("PreProc"                     , s:hasloRed       , s:hasloBackground      , "none")
call s:SetHi("Special"                     , s:hasloCyan        , s:hasloBackground      , "none")
call s:SetHi("SpecialKey"                  , s:hasloCyan        , s:hasloBackground      , "none")
call s:SetHi("Statement"                   , s:hasloBlue        , s:hasloBackground      , "none")
call s:SetHi("StorageClass"                , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("String"                      , s:hasloLightYellow , s:hasloBackground      , "none")
call s:SetHi("Tag"                         , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("Title"                       , s:hasloRed         , s:hasloBackground      , "bold")
call s:SetHi("Todo"                        , s:hasloWhite       , s:hasloBackground      , "standout")
call s:SetHi("Type"                        , s:hasloCyan        , s:hasloBackground      , "none")
call s:SetHi("Underlined"                  , s:hasloGrey3       , s:hasloBackground      , "underline")
call s:SetHi("SpellBad"                    , s:hasloBackground  , s:hasloBackground      , "undercurl")
call s:SetHi("SpellCap"                    , s:hasloBackground  , s:hasloBackground      , "undercurl")
call s:SetHi("SpellLocal"                  , s:hasloBackground  , s:hasloBackground      , "undercurl")
call s:SetHi("SignColumn"                  , s:hasloGreen       , s:hasloBackground      , "none")
call s:SetHi("SpecialComment"              , s:hasloGreen       , s:hasloBackground      , "none")
call s:SetHi("Typedef"                     , s:hasloCyan        , s:hasloBackground      , "none")
call s:SetHi("PreCondit"                   , s:hasloGreen       , s:hasloBackground      , "none")
call s:SetHi("Include"                     , s:hasloGreen       , s:hasloBackground      , "none")
call s:SetHi("Ignore"                      , s:hasloGrey3       , s:hasloBackground      , "none")
call s:SetHi("Debug"                       , s:hasloSteel       , s:hasloBackground      , "none")
call s:SetHi("SpecialChar"                 , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("Delimiter"                   , s:hasloGrey3       , s:hasloBackground      , "none")
call s:SetHi("Question"                    , s:hasloCyan        , s:hasloBackground      , "none")
call s:SetHi("Exception"                   , s:hasloGreen       , s:hasloBackground      , "none")
call s:SetHi("Error"                       , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("Repeat"                      , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("Structure"                   , s:hasloCyan        , s:hasloBackground      , "none")
call s:SetHi("Macro"                       , s:hasloLightYellow , s:hasloBackground      , "none")
call s:SetHi("TabLineFill"                 , s:hasloGrey3       , s:hasloBackground      , "none")
call s:SetHi("Conceal"                     , s:hasloGrey3       , s:hasloBackground      , "none")

" Viml
call s:SetHi("vimOption"                   , s:hasloLightBlue   , s:hasloBackground      , "none")
call s:SetHi("vimCommand"                  , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("vimVar"                      , s:hasloGreen       , s:hasloBackground      , "none")
call s:SetHi("vimFunction"                 , s:hasloLightBlue   , s:hasloBackground      , "none")

" Haskell
call s:SetHi("haskellIdentifier"           , s:hasloLightBlue   , s:hasloBackground      , "none")
call s:SetHi("haskellType"                 , s:hasloViolet      , s:hasloBackground      , "none")
call s:SetHi("haskellImportKeywords"       , s:hasloBlue        , s:hasloBackground      , "none")
call s:SetHi("haskellDecl"                 , s:hasloOrange      , s:hasloBackground      , "none")
call s:SetHi("haskellOperators"            , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("haskellDelimiter"            , s:hasloRed         , s:hasloBackground      , "none")

" Vim Fugitive
call s:SetHi("diffRemoved"                 , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("diffAdded"                   , s:hasloGreen       , s:hasloBackground      , "none")

" HTML
call s:SetHi("htmlSpecialTagName"          , s:hasloLightBlue   , s:hasloBackground      , "none")

" Vim Signify
call s:SetHi("SignifySignAdd"              , s:hasloGreen       , s:hasloBackground      , "bold")
call s:SetHi("SignifySignDelete"           , s:hasloRed         , s:hasloBackground      , "bold")
call s:SetHi("SignifySignChange"           , s:hasloLightYellow , s:hasloBackground      , "bold")

" Coc.nvim
call s:SetHi("CocErrorSign"                , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("CocWarningSign"              , s:hasloOrange      , s:hasloBackground      , "none")
call s:SetHi("CocInfoSign"                 , s:hasloLightYellow , s:hasloBackground      , "none")
call s:SetHi("CocHintSign"                 , s:hasloCyan        , s:hasloBackground      , "none")
call s:SetHi("CocHighlightText"            , s:hasloBackground  , s:hasloBackground      , "underline")
call s:SetHi("CocCodeLens"                 , s:hasloGrey2       , s:hasloBackground      , "bold")
call s:SetHi("CocListFgGreen"              , s:hasloGreen       , s:hasloBackground      , "none")
call s:SetHi("CocListFgRed"                , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("CocListFgBlack"              , s:hasloBlack       , s:hasloBackground      , "none")
call s:SetHi("CocListFgRed"                , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("CocListFgGreen"              , s:hasloGreen       , s:hasloBackground      , "none")
call s:SetHi("CocListFgYellow"             , s:hasloYellow      , s:hasloBackground      , "none")
call s:SetHi("CocListFgBlue"               , s:hasloBlue        , s:hasloBackground      , "none")
call s:SetHi("CocListFgMagenta"            , s:hasloViolet      , s:hasloBackground      , "none")
call s:SetHi("CocListFgCyan"               , s:hasloCyan        , s:hasloBackground      , "none")
call s:SetHi("CocListFgWhite"              , s:hasloWhite       , s:hasloBackground      , "none")
call s:SetHi("CocListFgGrey"               , s:hasloGrey3       , s:hasloBackground      , "none")


" Python
call s:SetHi("pythonDecoratorName"         , s:hasloViolet      , s:hasloBackground      , "none")
call s:SetHi("pythonStatement"             , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("pythonFunction"              , s:hasloLightBlue   , s:hasloBackground      , "none")
call s:SetHi("pythonOperator"              , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("pythonInclude"               , s:hasloRed         , s:hasloBackground      , "none")


" ZSH
call s:SetHi("zshDeref"                    , s:hasloViolet      , s:hasloBackground      , "none")
call s:SetHi("zshCommands"                 , s:hasloLightBlue   , s:hasloBackground      , "none")
call s:SetHi("zshOperator"                 , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("zshRedirect"                 , s:hasloRed         , s:hasloBackground      , "none")

" INI Files
call s:SetHi("dosiniLabel"                 , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("dosiniValue"                 , s:hasloLightBlue   , s:hasloBackground      , "none")
call s:SetHi("dosiniHeader"                , s:hasloViolet      , s:hasloBackground      , "none")

" ALE
call s:SetHi("ALEWarningSign"              , s:hasloLightYellow , s:hasloBackground      , "none")
call s:SetHi("ALEVirtualTextError"         , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("ALEVirtualTextWarning"       , s:hasloLightYellow , s:hasloBackground      , "none")
call s:SetHi("ALEVirtualTextInfo"          , s:hasloCyan        , s:hasloBackground      , "none")

" Denite
call s:SetHi("deniteSource_grepFile"       , s:hasloViolet      , s:hasloBackground      , "none")
call s:SetHi("deniteSource_grep"           , s:hasloWhite       , s:hasloBackground      , "none")
call s:SetHi("deniteSource_file_rec"       , s:hasloViolet      , s:hasloBackground      , "none")
call s:SetHi("deniteMatchedChar"           , s:hasloOrange      , s:hasloBackground      , "bold")

" Spelunker
call s:SetHi("SpelunkerSpellBad"           , s:hasloBackground  , s:hasloBackground      , "undercurl")

" Vim Sneak
call s:SetHi("Sneak"                       , s:hasloRed         , s:hasloBackground      , "undercurl")
call s:SetHi("SneakLabel"                  , s:hasloRed         , s:hasloSteel           , "undercurl")

" Markdown
call s:SetHi("MarkdownBold"                , s:hasloOrange      , s:hasloBackground      , "bold")
call s:SetHi("htmlBold"                    , s:hasloOrange      , s:hasloBackground      , "bold")
call s:SetHi("markdownItalic"              , s:hasloViolet      , s:hasloBackground      , "italic")
call s:SetHi("htmlItalic"                  , s:hasloViolet      , s:hasloBackground      , "italic")
call s:SetHi("markdownCode"                , s:hasloBlue        , s:hasloBackground      , "bold")
call s:SetHi("mkdCode"                     , s:hasloBlue        , s:hasloBackground      , "bold")
call s:SetHi("markdownCodeDelimiter"       , s:hasloCyan        , s:hasloBackground      , "bold")
call s:SetHi("mkdListItem"                 , s:hasloRed         , s:hasloBackground      , "bold")

" Semshi
function! s:SetSemshi()
    call s:SetHi("semshiLocal"             , s:hasloYellow      , s:hasloBackground      , "none")
    call s:SetHi("semshiGlobal"            , s:hasloOrange      , s:hasloBackground      , "none")
    call s:SetHi("semshiImported"          , s:hasloOrange      , s:hasloBackground      , "none")
    call s:SetHi("semshiParameter"         , s:hasloBlue        , s:hasloBackground      , "none")
    call s:SetHi("semshiParameterUnused"   , s:hasloGrey3       , s:hasloBackground      , "none")
    call s:SetHi("semshiFree"              , s:hasloBlue        , s:hasloBackground      , "none")
    call s:SetHi("semshiBuiltin"           , s:hasloViolet      , s:hasloBackground      , "none")
    call s:SetHi("semshiAttribute"         , s:hasloCyan        , s:hasloBackground      , "none")
    call s:SetHi("semshiSelf"              , s:hasloSteel       , s:hasloBackground      , "none")
    call s:SetHi("semshiUnresolved"        , s:hasloGrey3       , s:hasloBackground      , "none")
    call s:SetHi("semshiSelected"          , s:hasloBackground  , s:hasloBackground      , "underline")
    call s:SetHi("semshiErrorSign"         , s:hasloRed         , s:hasloBackground      , "none")
    call s:SetHi("semshiErrorChar"         , s:hasloRed         , s:hasloBackground      , "none")
endfunction

" Makefile
call s:SetHi("makeIdent"                   , s:hasloLightBlue   , s:hasloBackground      , "none")
call s:SetHi("makeTarget"                  , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("makeCommands"                , s:hasloSteel       , s:hasloBackground      , "bold")

" vim-signature
call s:SetHi("SignatureMarkText"           , s:hasloViolet      , s:hasloBackground      , "bold")

" Vista.vim
call s:SetHi("VistaScope"                  , s:hasloViolet      , s:hasloBackground      , "bold")
call s:SetHi("VistaTag"                    , s:hasloBlue        , s:hasloBackground      , "none")

" Log files
call s:SetHi("logHexNumber"                , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("logFloatNumber"              , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("logFilePath"                 , s:hasloCyan        , s:hasloBackground      , "none")

" LeaderF
call s:SetHi("Lf_hl_popup_window"          , s:hasloWhite       , s:hasloFloatBackground , "none")
call s:SetHi("Lf_hl_popup_blank"           , s:hasloSteel       , s:hasloFloatBackground , "none")
call s:SetHi("Lf_hl_popup_inputText"       , s:hasloSteel       , s:hasloFloatBackground , "none")
call s:SetHi("Lf_hl_cursorline"            , s:hasloYellow      , s:hasloFloatBackground , "bold")
" call s:SetHi("Lf_hl_popup_total"         , s:hasloBackground  , s:hasloBackground      , "bold")

" Nvim-R
call s:SetHi("routNormal"                  , s:hasloWhite       , s:hasloBackground      , "none")
call s:SetHi("routFloat"                   , s:hasloCyan        , s:hasloBackground      , "none")
call s:SetHi("routNumber"                  , s:hasloCyan        , s:hasloBackground      , "none")

" vim-which-key
call s:SetHi("WhichKeySeperator"           , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("WhichKeyFloating"            , s:hasloGreen       , s:hasloFloatBackground , "bold")
call s:SetHi("WhichKeyDesc"                , s:hasloYellow      , s:hasloBackground      , "none")

" TypeScript
call s:SetHi("typescriptVariable"          , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("typescriptTypeReference"     , s:hasloViolet      , s:hasloBackground      , "none")
call s:SetHi("typescriptMember"            , s:hasloGreen       , s:hasloBackground      , "none")
call s:SetHi("typescriptObjectLabel"       , s:hasloCyan        , s:hasloBackground      , "none")
call s:SetHi("typescriptNumber"            , s:hasloWhite       , s:hasloBackground      , "none")
call s:SetHi("typescriptBraces"            , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("typescriptArrowFunc"         , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("typescriptImport"            , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("typescriptExport"            , s:hasloBlue        , s:hasloBackground      , "none")
call s:SetHi("typescriptStringLiteralType" , s:hasloCyan        , s:hasloBackground      , "none")

" TSX
call s:SetHi("tsxTag"                      , s:hasloSteel       , s:hasloBackground      , "none")
call s:SetHi("tsxTagName"                  , s:hasloCyan        , s:hasloBackground      , "none")
call s:SetHi("tsxIntrinsicTagName"         , s:hasloLightBlue   , s:hasloBackground      , "none")
call s:SetHi("tsxAttrib"                   , s:hasloGreen       , s:hasloBackground      , "none")

" JSX
call s:SetHi("jsxTag"                      , s:hasloSteel       , s:hasloBackground      , "none")
call s:SetHi("jsxTagName"                  , s:hasloOrange      , s:hasloBackground      , "none")
call s:SetHi("jsxComponentName"            , s:hasloViolet      , s:hasloBackground      , "none")
call s:SetHi("jsxAttrib"                   , s:hasloGreen       , s:hasloBackground      , "none")

" Javascript
call s:SetHi("jsImport"                    , s:hasloRed         , s:hasloBackground      , "none")
call s:SetHi("jsExport"                    , s:hasloRed         , s:hasloBackground      , "none")

" vim-jumpmotion
call s:SetHi("JumpMotion"                  , s:hasloRed         , s:hasloBackground      , "bold")
call s:SetHi("JumpMotionTail"              , s:hasloOrange      , s:hasloBackground      , "none")
