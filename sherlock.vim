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

" =========================
" Filetype specific
" =========================

" PHP
hi link PhpVarSelector  SherlockGold
hi link PhpStatement    SherlockBlue
hi link phpStorageClass SherlockBlue
hi link phpStructure    SherlockWhite

" CSS/SASS
hi link sassDefinition  SherlockGold

hi link sassClass       SherlockBlue
hi link sassId          SherlockBlue

hi link sassColor       SherlockRed
hi link sassImportStr   SherlockRed
hi link sassAmpersand   SherlockRed

hi link sassVariable        SherlockYellow
hi link sassExtend          SherlockYellow
hi link sassCssAttribute    SherlockYellow
hi link sassFunction        SherlockYellow

hi link cssTagName SherlockBlue

hi link cssTextProp             SherlockGold
hi link cssPositioningProp      SherlockGold
hi link cssUIProp               SherlockGold
hi link cssBorderOutlineProp    SherlockGold
hi link cssDimensionProp        SherlockGold
hi link cssPaddingProp          SherlockGold
hi link cssListProp             SherlockGold
hi link cssMarginProp           SherlockGold
hi link cssFontProp             SherlockGold
hi link cssTransitionProp       SherlockGold
hi link cssColorProp            SherlockGold
hi link cssBackgroundProp       SherlockGold
hi link cssTableProp            SherlockGold
hi link cssBoxProp              SherlockGold
hi link cssTransformProp        SherlockGold
hi link cssAnimationProp        SherlockGold
hi link cssFlexibleBoxProp      SherlockGold
hi link cssGeneratedContentProp SherlockGold

hi link cssValueLength  SherlockRed
hi link cssValueNumber  SherlockRed
hi link cssColor        SherlockRed
hi link cssStringQ      SherlockRed

hi link cssFontAttr         SherlockBlue
hi link cssCommonAttr       SherlockBlue
hi link cssTextAttr         SherlockBlue
hi link cssPositionAttr     SherlockBlue
hi link cssBoxAttr          SherlockBlue
hi link cssPseudoClassId    SherlockBlue

" HTML
hi link htmlArg SherlockYellow

hi link htmlTag SherlockGold

hi link htmlTagName         SherlockBlue
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

" Improved JavaScript syntax
hi link javaScriptBrowserObjects    SherlockYellow
hi link javaScriptOperator          SherlockYellow
hi link javaScriptFuncArg           SherlockYellow

hi link javaScriptHtmlElemProperties    SherlockGold
hi link javascriptDOMProperties         SherlockGold
hi link javaScriptEventListenerKeywords SherlockGold
hi link javaScriptDOMObjects            SherlockGold

hi link javaScriptFuncArg       SherlockBlue
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

" Coffeescript
hi link coffeeConditional   SherlockBlue
hi link coffeeRepeat        SherlockBlue
hi link coffeeSpecialIdent  SherlockBlue

hi link coffeeObject        SherlockGold

hi link coffeeParens SherlockYellow

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
