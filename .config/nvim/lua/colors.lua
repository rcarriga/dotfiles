function ConstructColour(colour)
  return {gui = colour}
end

local Transparent = ConstructColour("NONE")
local Grey1 = ConstructColour("#262626")
local Grey2 = ConstructColour("#8B8B8B")
local Grey3 = ConstructColour("#bdbdbd")
local Grey4 = ConstructColour("#F8F8F8")

local Violet = ConstructColour("#D484FF")
local Blue = ConstructColour("#2f628e")
local Cyan = ConstructColour("#00f1f5")
local Green = ConstructColour("#A9FF68")
local Green2 = ConstructColour("#2f7366")
local Yellow = ConstructColour("#FFF59D")
local Orange = ConstructColour("#F79000")
local Red = ConstructColour("#F70067")
local FloatBackground = ConstructColour("#132434")
local Background = ConstructColour("NONE")

local function setHighlight(group, args)
  local fg = args[1]
  local bg = Background
  local attrs = "none"
  if type(args[2]) == "table" then
    bg = args[2]
    if type(args[3]) == "string" then
      attrs = args[3]
    end
  elseif type(args[2]) == "string" then
    attrs = args[2]
  end
  vim.cmd("hi " .. group .. " guifg=" .. fg.gui .. " guibg=" .. bg.gui .. " gui=" .. attrs)
end

local function loadHighlights(highlights)
  for group, groupArgs in pairs(highlights) do
    setHighlight(group, groupArgs)
  end
end

local Normal = Grey4
local Border = Orange
local Decoration = Orange
local Hidden = Grey2
local BuiltIn = Red
local VarName = Grey4
local FuncName = Cyan
local TypeName = Violet
local Key = Cyan
local Val = Violet
local Parameter = Green
local String = Yellow
local Operator = Orange
local Success = Green
local Warning = Yellow
local Info = Cyan
local Error = Red

-- For reference elsewhere
loadHighlights(
  {
    Normal = {Grey4},
    Border = {Orange},
    Decoration = {Orange},
    Hidden = {Grey2},
    BuiltIn = {Red},
    VarName = {Grey4, "bold"},
    FuncName = {Cyan},
    TypeName = {Violet},
    Key = {Cyan},
    Val = {Violet},
    String = {Yellow},
    Operator = {Orange},
    Success = {Green},
    Warning = {Yellow},
    Info = {Cyan},
    Error = {Red},
    User1 = {Success, FloatBackground},
    User2 = {Warning, FloatBackground},
    User3 = {Error, FloatBackground},
    User4 = {Grey1, Info},
    -- Vim
    Cursor = {Grey1, Red},
    CursorLine = {Transparent, Grey1},
    CursorColumn = {Transparent, Grey1},
    ColorColumn = {Transparent, Grey1},
    LineNr = {Hidden},
    CursorLineNr = {Success, "bold"},
    VertSplit = {Hidden},
    MatchParen = {Success, "underline"},
    StatusLine = {Normal},
    StatusLineNC = {Hidden},
    IncSearch = {Green, "bold,underline"},
    Search = {Green, "bold,underline"},
    Directory = {Cyan},
    Folded = {Grey2},
    WildMenu = {Cyan},
    VisualNOS = {Grey2, Yellow},
    ModeMsg = {Yellow},
    FoldColumn = {Grey3},
    MoreMsg = {Yellow},
    cursorim = {Violet},
    Pmenu = {Grey3, FloatBackground},
    PmenuSel = {Transparent, Grey2, "bold"},
    PMenuSbar = {Transparent, FloatBackground},
    PMenuThumb = {Transparent, Grey3},
    Visual = {Transparent, Grey1, "bold,underline"},
    EndOfBuffer = {Grey1},
    Underlined = {Transparent, "underline"},
    SpellBad = {Transparent, "undercurl"},
    SpellCap = {Transparent, "undercurl"},
    SpellLocal = {Transparent, "undercurl"},
    SignColumn = {Key},
    Question = {Info},
    TabLineFill = {Grey2},
    NotificationInfo = {Normal, FloatBackground},
    NotificationError = {Error, FloatBackground},
    NotificationWarning = {Warning, FloatBackground},
    -- General
    Boolean = {Val},
    Character = {Val},
    Comment = {Hidden},
    Conditional = {BuiltIn},
    Constant = {VarName},
    Define = {BuiltIn},
    DiffAdd = {Success},
    DiffChange = {Warning},
    DiffDelete = {Error},
    DiffText = {Error},
    ErrorMsg = {Error},
    WarningMsg = {Warning},
    Float = {Val},
    Function = {FuncName},
    Identifier = {VarName},
    Keyword = {BuiltIn},
    Label = {Key},
    NonText = {Hidden},
    Number = {Val},
    PreProc = {Key},
    Special = {Cyan},
    SpecialKey = {BuiltIn},
    Statement = {BuiltIn},
    Tag = {Key},
    Title = {Normal, "bold"},
    Todo = {Normal, "bold"},
    Type = {TypeName},
    SpecialComment = {Info, "bold"},
    Typedef = {TypeName},
    PreCondit = {BuiltIn},
    Include = {BuiltIn},
    Ignore = {BuiltIn},
    Delimiter = {Decoration},
    Conceal = {Transparent, "bold"},
    -- Viml
    vimContinue = {Decoration},
    vimFunction = {FuncName},
    vimIsCommand = {VarName},
    -- Haskell
    haskellIdentifier = {FuncName},
    haskellDecl = {BuiltIn},
    haskellDeclKeyword = {BuiltIn},
    haskellLet = {BuiltIn},
    -- Vim Fugitive
    diffRemoved = {Error},
    diffAdded = {Success},
    -- HTML
    htmlTagName = {Key},
    htmlSpecialTagName = {BuiltIn},
    htmlTag = {Decoration},
    htmlEndTag = {Decoration},
    htmlArg = {VarName},
    -- Vim Signify
    SignifySignAdd = {Success, "bold"},
    SignifySignDelete = {Error, "bold"},
    SignifySignChange = {Warning, "bold"},
    --Floaterm
    FloatermBorder = {Border},
    -- Coc.nvim
    CocErrorSign = {Error},
    CocWarningSign = {Warning},
    CocInfoSign = {Info},
    CocHintSign = {Info},
    CocHighlightText = {Transparent, "underline"},
    CocCodeLens = {Hidden, "bold"},
    CocListFgGreen = {Green},
    CocListFgRed = {Red},
    CocListFgBlack = {Grey1},
    CocListFgYellow = {Yellow},
    CocListFgBlue = {Cyan},
    CocListFgMagenta = {Violet},
    CocListFgCyan = {Cyan},
    CocListFgWhite = {Grey4},
    CocListFgGrey = {Grey2},
    -- ALE
    ALEWarningSign = {Warning},
    ALEVirtualTextError = {Error},
    ALEVirtualTextWarning = {Warning},
    ALEVirtualTextInfo = {Info},
    -- Markdown
    markdownHeadingDelimiter = {BuiltIn},
    markdownCodeDelimiter = {BuiltIn},
    markdownRule = {BuiltIn},
    markdownUrl = {Key},
    -- Makefile
    makeCommands = {Normal, "bold"},
    -- vim-signature
    SignatureMarkText = {TypeName, "bold"},
    -- Vista.vim
    VistaScope = {TypeName, "bold"},
    VistaTag = {FuncName},
    -- LeaderF
    Lf_hl_popup_window = {Normal, FloatBackground},
    Lf_hl_popup_blank = {Hidden, FloatBackground},
    Lf_hl_popup_inputText = {Key, FloatBackground},
    Lf_hl_cursorline = {Normal, FloatBackground, "bold"},
    -- vim-which-key
    WhichKeySeperator = {BuiltIn},
    WhichKeyFloating = {VarName, FloatBackground, "bold"},
    WhichKeyGroup = {TypeName},
    WhichKey = {VarName},
    WhichKeyDesc = {Info, "bold"},
    -- JSX/TSX
    jsxTagName = {Key},
    jsxComponentName = {TypeName},
    jsxAttrib = {Green},
    -- Javascript
    jsImport = {BuiltIn},
    jsExport = {BuiltIn},
    jsVariableType = {BuiltIn},
    jsAssignmentEqual = {BuiltIn},
    jsParens = {Decoration},
    jsObjectBraces = {Decoration},
    jsFunctionBraces = {Decoration},
    -- vim-jumpmotion
    JumpMotion = {Red, "bold"},
    JumpMotionTail = {Yellow},
    -- TypeScript
    typescriptVariable = {BuiltIn},
    typescriptImport = {BuiltIn},
    typescriptExport = {BuiltIn},
    typescriptCall = {VarName},
    typescriptTypeReference = {TypeName},
    typescriptArrowFunc = {BuiltIn},
    typescriptBraces = {Decoration},
    typescriptMember = {Green},
    typescriptObjectLabel = {Key},
    typescriptStringLiteralType = {TypeName},
    typescriptInterfaceName = {TypeName},
    typescriptFuncType = {VarName},
    typescriptFuncTypeArrow = {BuiltIn},
    --hiPairs
    hiPairs_matchPair = {Success, "bold,underline"},
    hiPairs_unmatchPair = {Error, "bold,underline"},
    --LaTex
    texBeginEndName = {FuncName},
    --YAML
    yamlBlockMappingKey = {Key},
    --ini
    dosiniLabel = {Key},
    dosiniValue = {Val},
    dosiniHeader = {BuiltIn},
    -- Conflict Markers
    ConflictMarkerBegin = {Transparent, Green2},
    ConflictMarkerOurs = {Transparent, Green2},
    ConflictMarkerTheirs = {Transparent, Blue},
    ConflictMarkerEnd = {Transparent, Blue},
    ConflictMarkerCommonAncestorsHunk = {Transparent, Red},
    -- TreeSitter
    TSError = {Error},
    TSComment = {Hidden},
    TSPunctDelimiter = {Decoration},
    TSPunctBracket = {Decoration},
    TSPunctSpecial = {Decoration},
    TSConstant = {VarName},
    TSConstBuiltin = {BuiltIn},
    TSConstMacro = {BuiltIn},
    TSString = {String},
    TSStringRegex = {Operator},
    TSStringEscape = {Operator},
    TSCharacter = {Val},
    TSNumber = {Val},
    TSBoolean = {Val},
    TSFloat = {Val},
    TSFunction = {FuncName},
    TSFuncBuiltin = {BuiltIn},
    TSFuncMacro = {BuiltIn},
    TSParameter = {Green},
    TSParameterReference = {Green},
    TSMethod = {FuncName},
    TSField = {FuncName},
    TSProperty = {Key},
    TSConstructor = {TypeName},
    TSConditional = {BuiltIn},
    TSRepeat = {BuiltIn},
    TSLabel = {Key},
    TSOperator = {Operator},
    TSKeyword = {BuiltIn},
    TSKeywordFunction = {BuiltIn},
    TSException = {Error},
    TSType = {TypeName},
    TSTypeBuiltin = {TypeName},
    TSStructure = {Error},
    TSInclude = {BuiltIn},
    TSAnnotation = {String},
    TSText = {String},
    TSStrong = {Transparent, "bold"},
    TSEmphasis = {Transparent, "bold,underline"},
    TSUnderline = {Transparent, "underline"},
    TSTitle = {BuiltIn},
    TSLiteral = {Decoration},
    TSURI = {Info},
    TSVariable = {VarName},
    TSVariableBuiltin = {BuiltIn},
    TSDefinition = {Transparent, "bold,underline"},
    TSDefinitionUsage = {Transparent, "bold,underline"},
    TSCurrentScope = {Transparent, "bold"},
    -- Golang
    goFunctionCall = {FuncName},
    goVarDefs = {VarName},
    -- Telescope
    TelescopeBorder = {Border},
    -- barbar
    BufferCurrent = {Normal, FloatBackground},
    BufferCurrentMod = {Info, FloatBackground, "bold"},
    BufferCurrentSign = {Info, FloatBackground},
    BufferCurrentTarget = {Info, Grey1, "bold"},
    BufferVisible = {Normal, Grey1, "bold"},
    BufferVisibleMod = {Normal, Grey1, "bold,underline"},
    BufferVisibleSign = {Info, Grey1},
    BufferVisibleTarget = {Error, "bold,underline"},
    BufferInactive = {Grey2, Grey1},
    BufferInactiveMod = {Grey2, Grey1, "underline"},
    BufferInactiveSign = {Grey2, Grey1},
    BufferInactiveTarget = {Error, Grey1, "bold"},
    BufferTabpages = {Info, "bold"},
    BufferTabpageFill = {Grey2},
    -- LSP
    LspDiagnosticsDefaultError = {Error},
    LspDiagnosticsDefaultWarning = {Warning},
    LspDiagnosticsDefaultInformation = {Info},
    LspDiagnosticsDefaultHint = {Hidden},
    -- Lsp saga
    LspFloatWinBorder = {Border},
    ProviderTruncateLine = {Hidden},
    LspSagaFinderSelection = {Green, "bold"},
    LspSagaBorderTitle = {BuiltIn, "bold"},
    TargetWord = {BuiltIn},
    ReferencesCount = {Val},
    DefinitionCount = {Val},
    TargetFileName = {Operator},
    DefinitionIcon = {Decoration},
    ReferencesIcon = {Decoration},
    SagaShadow = {Transparent, Grey1},
    DiagnosticTruncateLine = {Hidden},
    DiagnosticError = {Error},
    DiagnosticWarning = {Warning},
    DiagnosticInformation = {Info},
    DiagnosticHint = {Hidden},
    DefinitionPreviewTitle = {BuiltIn, "bold"},
    LspSagaDiagnosticBorder = {Border},
    LspSagaDiagnosticHeader = {BuiltIn},
    LspSagaDiagnostcTruncateLine = {Hidden},
    LspDiagnosticsFloatingError = {Error},
    LspDiagnosticsFloatingWarn = {Warning},
    LspDiagnosticsFloatingInfor = {Info},
    LspDiagnosticsFloatingHint = {Hidden},
    LspSagaShTruncateLine = {Hidden},
    LspSagaDocTruncateLine = {Hidden},
    LspSagaCodeActionTitle = {BuiltIn},
    LspSagaCodeActionTruncateLine = {Hidden},
    LspSagaCodeActionContent = {Grey3},
    LspSagaRenamePromptPrefix = {Decoration},
    LspSagaRenameBorder = {Border},
    LspSagaHoverBorder = {Border},
    LspSagaSignatureHelpBorder = {Border},
    LspSagaLspFinderBorder = {Border},
    LspSagaCodeActionBorder = {Border},
    LspSagaAutoPreview = {Yellow},
    LspSagaDefPreviewBorder = {Border}
  }
)
