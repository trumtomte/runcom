" Colorscheme:  Sherlock
" Maintainer:   Sebastian Bengtegård <sebbebook@gmail.com>
" Last Change:  25-02-2018
" URL:	        https://github.com/trumtomte/runcom/blob/master/sherlock.vim

" ======
" Colors
" ======
" White:    cterm=7
" Beige:    cterm=233
" Yellow:   cterm=179
" Red:      cterm=167
" Blue:     cterm=109

set background=dark

hi clear
if exists("syntax_on")
    syntax reset
endif

let g:colors_name="sherlock"

" ============
" Color Groups
" ============
hi SherlockWhite    ctermfg=7       cterm=none 
hi SherlockBeige    ctermfg=223     cterm=none 
hi SherlockYellow   ctermfg=179     cterm=none 
hi SherlockRed      ctermfg=167     cterm=none 
hi SherlockBlue     ctermfg=109     cterm=none 

hi Normal           ctermbg=235     ctermfg=223
hi LineNr           ctermbg=235     ctermfg=240
hi Comment          ctermbg=235     ctermfg=243     cterm=none
hi CursorLine       ctermbg=236     cterm=none
hi CursorLineNr     ctermbg=235     ctermfg=179
hi NonText          ctermbg=235     ctermfg=243
hi ColorColumn      ctermbg=236
hi VertSplit        ctermbg=235     ctermfg=179     cterm=none
hi Todo             ctermbg=109     ctermfg=0       cterm=none
hi Folded           ctermbg=240     ctermfg=179     cterm=none
hi MatchParen       ctermbg=167     ctermfg=179 
hi Visual           ctermbg=238

" Statusline Colors
hi User1            ctermbg=234     ctermfg=179
hi User2            ctermbg=234     ctermfg=167
hi User3            ctermbg=234     ctermfg=109
hi User4            ctermbg=234     ctermfg=109

" Tabline
hi TabLine          ctermbg=236     ctermfg=179     cterm=none
hi TabLineSel       ctermbg=239     ctermfg=179     cterm=none
hi TabLineFill      ctermbg=236     ctermfg=0       cterm=none

hi! link Function       SherlockYellow
hi! link Conditional    SherlockYellow
hi! link Repeat         SherlockYellow
hi! link Label          SherlockYellow
hi! link Statement      SherlockYellow
hi! link String         SherlockRed
hi! link Number         SherlockRed
hi! link Boolean        SherlockRed
hi! link Constant       SherlockRed
hi! link Identifier     SherlockBeige
hi! link Operator       SherlockWhite
hi! link Special        SherlockWhite
hi! link Structure      SherlockWhite
hi! link PreProc        SherlockBlue
hi! link Type           SherlockBlue
hi! link Keyword        SherlockYellow

" ====
" Misc
" ====

" PHP
hi link PhpVarSelector  SherlockBeige
hi link PhpStatement    SherlockBlue
hi link phpStorageClass SherlockBlue
hi link phpStructure    SherlockWhite

" CSS
hi link cssProp         SherlockBeige
hi link cssAttr         SherlockBlue
hi link cssTagName      SherlockBlue
hi link cssValueLength  SherlockRed
hi link cssValueNumber  SherlockRed
hi link cssColor        SherlockRed
hi link cssStringQ      SherlockRed

" SASS
hi link sassDefinition      SherlockBeige
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
hi link htmlArg             SherlockYellow
hi link htmlTag             SherlockBeige
hi link htmlTagName         SherlockBlue
hi link htmlTagN            SherlockBlue
hi link htmlSpecialTagName  SherlockBlue
hi link htmlString          SherlockRed
hi link htmlValue           SherlockRed
hi link htmlLink            SherlockWhite
hi link htmlH1              SherlockWhite
hi link htmlH2              SherlockWhite
hi link htmlH3              SherlockWhite
hi link htmlH4              SherlockWhite
hi link htmlH5              SherlockWhite
hi link htmlH6              SherlockWhite

" JavaScript
hi link javaScriptStringD       SherlockRed
hi link javaScriptStringS       SherlockRed
hi link javaScriptBoolean       SherlockRed
hi link javaScriptNull          SherlockRed
hi link javaScriptNumber        SherlockBlue
hi link javaScriptConditional   SherlockBlue
hi link javaScriptRepeat        SherlockBlue
hi link javaScriptFunction      SherlockBlue
hi link javaScriptIdentifier    SherlockBlue
hi link javaScriptOperator      SherlockWhite
hi link javaScriptBraces        SherlockWhite
hi link javaScriptParens        SherlockWhite
hi link javaScriptSpecial       SherlockWhite
hi link javaScriptMessage       SherlockWhite
hi link javaScriptGlobal        SherlockWhite
hi link javaScriptMember        SherlockWhite
hi link javaScript              SherlockBeige
hi link javascriptObjectLabel   SherlockBeige

" JavaScript ES6
hi link javaScriptBrowserObjects        SherlockYellow
hi link javaScriptOperator              SherlockYellow
hi link javaScriptHtmlElemProperties    SherlockBeige
hi link javascriptDOMProperties         SherlockBeige
hi link javaScriptEventListenerKeywords SherlockBeige
hi link javaScriptDOMObjects            SherlockBeige
hi link javaScriptFuncArg               SherlockBeige
hi link javaScriptFuncKeyword           SherlockBlue
hi link javaScriptLogger                SherlockBlue
hi link javaScriptBraces                SherlockWhite
hi link javaScriptOpSymbols             SherlockWhite
hi link javaScriptLogicSymbols          SherlockWhite
hi link javaScriptSemiColon             SherlockWhite
hi link javaScriptComma                 SherlockWhite
hi link javaScriptColon                 SherlockWhite
hi link javaScriptDot                   SherlockWhite
hi link javaScriptPlus                  SherlockWhite
hi link javascriptImport                SherlockBlue
hi link javascriptImportBlock           SherlockYellow
hi link javascriptExport                SherlockYellow
hi link javascriptVariable              SherlockBlue
hi link javascriptBOM                   SherlockBeige
hi link javascriptBOMWindowProp         SherlockBeige
hi link javascriptDOMDocProp            SherlockBeige
hi link javascriptDOMElemProp           SherlockBeige
hi link javascriptURLUtilsProp          SherlockBeige
hi link javascriptDOMEventProp          SherlockBeige
hi link javascriptGlobal                SherlockBeige
hi link javascriptGlobalMethod          SherlockBeige
hi link javascriptDOMNodeProp           SherlockBeige
hi link javascriptEndColons             SherlockWhite
hi link javascriptLogicSymbol           SherlockWhite
hi link javascriptOpSymbol              SherlockWhite
hi link javascriptBOMWindowMethod       SherlockBeige
hi link javascriptArrowFunc             SherlockWhite
hi link javascriptArrowFuncArg          SherlockBeige
hi link javascriptArrowFuncDef          SherlockBeige
hi link javascriptConditionalElse       SherlockBlue
hi link javaScriptIdentifierName        SherlockBeige
hi link javascriptFunctionMethod        SherlockBeige
hi link javascriptArrayMethod           SherlockBeige
hi link javascriptDOMEventTargetMethod  SherlockBeige
hi link javascriptDOMNodeMethod         SherlockBeige
hi link javascriptCacheMethod           SherlockBeige
hi link javascriptBOMLocationMethod     SherlockBeige
hi link javascriptDOMDocMethod          SherlockBeige
hi link javascriptDOMElemMethod         SherlockBeige
hi link javascriptStringMethod          SherlockBeige
hi link javascriptJSONStaticMethod      SherlockBeige
hi link javascriptMathStaticMethod      SherlockBeige
hi link javascriptRegExpMethod          SherlockBeige
hi link javascriptBrackets              SherlockWhite

hi link javascriptClassKeyword          SherlockBlue
hi link javascriptClassExtends          SherlockBlue
hi link javascriptClassName             SherlockBeige
hi link javascriptClassSuperName        SherlockBeige
hi link javascriptConsoleMethod         SherlockBeige

hi link jsDestructuringNoise    SherlockWhite
hi link jsDestructuringBraces   SherlockWhite
hi link jsBraces                SherlockWhite
hi link jsBrackets              SherlockWhite
hi link jsFuncBraces            SherlockWhite
hi link jsFuncParens            SherlockWhite
hi link jsObjectBraces          SherlockWhite
hi link jsParens                SherlockWhite
hi link jsFuncArgCommas         SherlockWhite
hi link jsClassKeywords         SherlockBlue
hi link jsClassProperty         SherlockYellow
hi link jsArrowFunction         SherlockWhite
hi link jsConditional           SherlockBlue
hi link jsThis                  SherlockBlue
hi link jsSuper                 SherlockYellow
hi link jsTemplateBraces        SherlockYellow
hi link jsTemplateVar           SherlockYellow
hi link jsNoise                 SherlockWhite
hi link jsObjectSeparator       SherlockWhite
hi link jsModuleDefault         SherlockYellow
hi link jsGlobalObjects         SherlockBeige
hi link jsNull                  SherlockRed
hi link jsUndefined             SherlockRed
hi link jsClassBraces           SherlockWhite
hi link jsAsyncKeyword          SherlockBlue

" Jinja
hi link jinjaVariable   SherlockBeige
hi link jinjaString     SherlockRed
hi link jinjaNumber     SherlockBlue

" Python
hi link pythonBuiltinFunc   SherlockYellow
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
hi link sqlKeyword  SherlockBlue
hi link sqlSpecial  SherlockRed

" Haskell
hi link hsDelimiter SherlockWhite

" Haml
hi link hamlId      SherlockYellow
hi link hamlClass   SherlockYellow

" Jade
hi link jadeId      SherlockYellow
hi link jadeClass   SherlockYellow

" Go
hi link goStructDef     SherlockBeige
hi link goDeclaration   SherlockBlue

" XML
hi link xmlEndTag   SherlockYellow

" Markdown
hi link markdownItalic              SherlockYellow
hi link markdownBold                SherlockYellow
hi link markdownLinkText            SherlockBlue
hi link markdownCodeDelimiter       SherlockRed
hi link markdownCode                SherlockRed
hi link markdownLinkTextDelimiter   SherlockYellow
hi link markdownLinkDelimiter       SherlockYellow
