" Colorscheme:  Sherlock
" Maintainer:   Sebastian Bengtegård <sebbebook@gmail.com>
" Last Change:  18-01-2014
" URL:	        https://github.com/trumtomte/runcom/blob/master/sherlock.vim

" =========================
" Colors
" =========================
" gui: #fbf8ff  cterm: 7        :white
" gui: #fff6d9  cterm: 144      :beige
" gui: #fad46b  cterm: 179      :yellow
" gui: #eb3652  cterm: 197      :red
" gui: #90aede  cterm: 110      :blue

set background=dark

hi clear
if exists("syntax_on")
    syntax reset
endif

let g:colors_name="sherlock"

" Sherlock Color Groups
hi SherlockWhite    guifg=#fbf8ff gui=none cterm=none ctermfg=7
hi SherlockGold     guifg=#fff6d9 gui=none cterm=none ctermfg=144
hi SherlockYellow   guifg=#fad46b gui=none cterm=none ctermfg=179
hi SherlockRed      guifg=#eb3652 gui=none cterm=none ctermfg=197
hi SherlockBlue     guifg=#90aede gui=none cterm=none ctermfg=110

" Statusline Colors
hi User1 guifg=#c1ae6e ctermfg=179 guibg=#181818 ctermbg=234
hi User2 guifg=#cc2f47 ctermfg=197 guibg=#181818 ctermbg=234
hi User3 guifg=#7c96bf ctermfg=11 guibg=#181818 ctermbg=234
hi User4 guifg=#777777 ctermfg=243 guibg=#181818 ctermbg=234

hi LineNr       guibg=#222222 guifg=#3a3a3a ctermbg=235 ctermfg=240
hi Normal       guibg=#222222 guifg=#fff6d9 ctermbg=235 ctermfg=144
hi Comment      guifg=#777777 gui=none ctermbg=235 ctermfg=243 cterm=none
hi CursorLine   guibg=#333333 gui=none ctermbg=236 cterm=none
hi CursorLineNr guifg=#fad46b guibg=#2b2b2b ctermfg=179 ctermbg=235
hi NonText      guifg=#777777 ctermfg=243

hi VertSplit    guibg=#2b2b2b guifg=#fad46b gui=none ctermfg=179 ctermbg=235 cterm=none
hi Todo         guifg=#000000 guibg=#fad46b gui=none ctermfg=0 ctermbg=110 cterm=none
hi Folded       guifg=#fad46b guibg=#555555 gui=none ctermfg=179 ctermbg=240 cterm=none
hi MatchParen   guifg=#fad46b guibg=#eb3652 ctermfg=179 ctermbg=197
hi Visual       guibg=#444444 ctermbg=238

" Netrw
hi link netrwDir SherlockYellow
hi netrwTreeBar guifg=#5d5d5d

" Tabs
hi TabLine ctermbg=236 ctermfg=179 cterm=none
hi TabLineSel ctermbg=239 ctermfg=179 cterm=none
hi TabLineFill ctermbg=236 ctermfg=0 cterm=none

hi! link Function    SherlockYellow
hi! link Conditional SherlockYellow
hi! link Repeat      SherlockYellow
hi! link Label       SherlockYellow
hi! link Statement   SherlockYellow
hi! link String      SherlockRed
hi! link Number      SherlockRed
hi! link Boolean     SherlockRed
hi! link Constant    SherlockRed
hi! link Identifier  SherlockGold
hi! link Operator    SherlockWhite
hi! link Special     SherlockWhite
hi! link Structure   SherlockWhite
hi! link PreProc     SherlockBlue
hi! link Type        SherlockBlue

hi! link Keyword    SherlockGold

" =========================
" Filetype specific
" =========================

" PHP
hi link PhpVarSelector  SherlockGold
hi link PhpStatement    SherlockBlue
hi link phpStorageClass SherlockBlue
hi link phpStructure    SherlockWhite

" CSS
hi link cssProp         SherlockGold
hi link cssAttr         SherlockBlue
hi link cssTagName      SherlockBlue
hi link cssValueLength  SherlockRed
hi link cssValueNumber  SherlockRed
hi link cssColor        SherlockRed
hi link cssStringQ      SherlockRed

" SASS
hi link sassDefinition      SherlockGold
hi link sassClass           SherlockBlue
hi link sassId              SherlockBlue
hi link sassColor           SherlockRed
hi link sassImportStr       SherlockRed
hi link sassAmpersand       SherlockRed
hi link sassVariable        SherlockYellow
hi link sassExtend          SherlockYellow
hi link sassCssAttribute    SherlockYellow
hi link sassFunction        SherlockYellow

" HTML
hi link htmlArg SherlockYellow

hi link htmlTag SherlockGold

hi link htmlTagName         SherlockBlue
hi link htmlTagN            SherlockBlue
hi link htmlSpecialTagName  SherlockBlue

hi link htmlString  SherlockRed
hi link htmlValue   SherlockRed

hi link htmlLink    SherlockWhite
hi link htmlH1      SherlockWhite
hi link htmlH2      SherlockWhite
hi link htmlH3      SherlockWhite
hi link htmlH4      SherlockWhite
hi link htmlH5      SherlockWhite
hi link htmlH6      SherlockWhite

" JavaScript
hi link javaScriptStringD   SherlockRed
hi link javaScriptStringS   SherlockRed
hi link javaScriptBoolean   SherlockRed
hi link javaScriptNull      SherlockRed

hi link javaScriptNumber        SherlockBlue
hi link javaScriptConditional   SherlockBlue
hi link javaScriptRepeat        SherlockBlue
hi link javaScriptFunction      SherlockBlue
hi link javaScriptIdentifier    SherlockBlue

hi link javaScriptOperator  SherlockWhite
hi link javaScriptBraces    SherlockWhite
hi link javaScriptParens    SherlockWhite
hi link javaScriptSpecial   SherlockWhite
hi link javaScriptMessage   SherlockWhite
hi link javaScriptGlobal    SherlockWhite
hi link javaScriptMember    SherlockWhite

hi link javaScript SherlockGold

" JavaScript ES6
" Improved JavaScript syntax
hi link javaScriptBrowserObjects    SherlockYellow
hi link javaScriptOperator          SherlockYellow

hi link javaScriptHtmlElemProperties    SherlockGold
hi link javascriptDOMProperties         SherlockGold
hi link javaScriptEventListenerKeywords SherlockGold
hi link javaScriptDOMObjects            SherlockGold

hi link javaScriptFuncArg       SherlockGold
hi link javaScriptFuncKeyword   SherlockBlue
hi link javaScriptLogger        SherlockBlue

hi link javaScriptBraces        SherlockWhite
hi link javaScriptOpSymbols     SherlockWhite
hi link javaScriptLogicSymbols  SherlockWhite
hi link javaScriptSemiColon     SherlockWhite
hi link javaScriptComma         SherlockWhite
hi link javaScriptColon         SherlockWhite
hi link javaScriptDot           SherlockWhite
hi link javaScriptPlus          SherlockWhite

hi link javascriptImport        SherlockBlue
hi link javascriptImportBlock   SherlockYellow
hi link javascriptExport        SherlockYellow
hi link javascriptVariable      SherlockBlue
hi link javascriptBOM           SherlockGold
hi link javascriptBOMWindowProp SherlockGold
hi link javascriptDOMDocProp    SherlockGold
hi link javascriptDOMElemProp   SherlockGold
hi link javascriptURLUtilsProp  SherlockGold
hi link javascriptDOMEventProp  SherlockGold
hi link javascriptGlobal        SherlockGold
hi link javascriptGlobalMethod  SherlockGold
hi link javascriptDOMNodeProp   SherlockGold
hi link javascriptEndColons     SherlockWhite
hi link javascriptLogicSymbol   SherlockWhite
hi link javascriptOpSymbol      SherlockWhite

hi link javascriptArrowFunc         SherlockWhite
hi link javascriptArrowFuncArg      SherlockGold
hi link javascriptArrowFuncDef      SherlockGold

hi link javascriptConditionalElse   SherlockBlue

" Jinja
hi link jinjaVariable   SherlockGold
hi link jinjaString     SherlockRed
hi link jinjaNumber     SherlockBlue

" Python
hi link pythonBuiltinFunc SherlockYellow

hi link pythonBuiltinObj    SherlockRed
hi link pythonDecorator     SherlockRed
hi link pythonDottedName    SherlockRed
hi link pythonTripleString  SherlockRed

hi link pythonPreCondit     SherlockBlue
hi link pythonOperator      SherlockBlue
hi link pythonConditional   SherlockBlue
hi link pythonRepeat        SherlockBlue
hi link pythonStatement     SherlockBlue

" SQL
hi link sqlKeyword SherlockBlue
hi link sqlSpecial SherlockRed

" Haskell
hi link hsDelimiter SherlockWhite

" Haml
hi link hamlId      SherlockYellow
hi link hamlClass   SherlockYellow

" Jade
hi link jadeId      SherlockYellow
hi link jadeClass   SherlockYellow

" Go
hi link goStructDef SherlockGold
hi link goDeclaration SherlockBlue

" Markdown
hi link markdownCode SherlockBlue

" XML
hi link xmlEndTag SherlockYellow

" Markdown
hi link markdownItalic              SherlockYellow
hi link markdownBold                SherlockYellow
hi link markdownLinkText            SherlockBlue
hi link markdownCodeDelimiter       SherlockRed
hi link markdownCode                SherlockRed
hi link markdownLinkTextDelimiter   SherlockYellow
hi link markdownLinkDelimiter       SherlockYellow
