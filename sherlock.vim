" Colorscheme:  Sherlock
" Maintainer:   Sebastian Bengtegård <sebbebook@gmail.com>
" Last Change:  01-11-2013
" URL:	        https://github.com/trumtomte/runcom/blob/master/sherlock.vim

" =========================
" Colors
" =========================
" gui: #fbf8ff  cterm: 7        :white
" gui: #fff6d9  cterm: 144      :gold
" gui: #fad46b  cterm: 179      :yellow
" gui: #eb3652  cterm: 197      :red
" gui: #90aede  cterm: 11       :blue

" TODO - file needs major refactoring

set background=dark

hi clear
if exists("syntax_on")
    syntax reset
endif

let g:colors_name="sherlock"

hi LineNr guibg=#323232 guifg=#5d5d5d ctermbg=236 ctermfg=240
hi Normal guibg=#2b2b2b guifg=#fff6d9 ctermbg=235 ctermfg=144
hi Comment guifg=#777777 gui=italic ctermbg=235 ctermfg=243 cterm=none
hi CursorLine guibg=#333333 gui=none ctermbg=236 cterm=none
hi CursorLineNr guifg=yellow guibg=#2b2b2b ctermfg=11 ctermbg=235
hi NonText guifg=#777777 ctermfg=243
" hi VertSplit, vertical bar that splits windows
hi Todo guifg=#000000 guibg=yellow gui=none ctermfg=0 ctermbg=11 cterm=none
hi Folded guifg=#fad46b guibg=#555555 gui=none ctermfg=179 ctermbg=240 cterm=none
hi Directory guifg=#fad46b ctermfg=179
hi MatchParen guifg=#fad46b guibg=#eb3652 ctermfg=179 ctermbg=197
" Tabs
hi TabLine ctermbg=236 ctermfg=179 cterm=none
hi TabLineSel ctermbg=239 ctermfg=179 cterm=none
hi TabLineFill ctermbg=236 ctermfg=0 cterm=none
" Visual
hi Visual guibg=#444444 ctermbg=238

hi Function guifg=#fad46b gui=none ctermfg=179 cterm=none
hi Conditional guifg=#fad46b gui=none ctermfg=179 cterm=none
hi Repeat guifg=#fad46b gui=none ctermfg=179 cterm=none
hi Label guifg=#fad46b gui=none ctermfg=179 cterm=none
hi Statement guifg=#fad46b gui=none ctermfg=179 cterm=none

hi String guifg=#eb3652 gui=none ctermfg=197 cterm=none
hi Number guifg=#eb3652 gui=none ctermfg=197 cterm=none
hi Boolean guifg=#eb3652 gui=none ctermfg=197 cterm=none
hi Constant guifg=#eb3652 gui=none ctermfg=197 cterm=none

hi Identifier guifg=#fff6d9 gui=none ctermfg=144 cterm=none

hi Operator guifg=#fbf8ff gui=none ctermfg=7 cterm=none
hi Special guifg=#fbf8ff gui=none ctermfg=7 cterm=none
hi Structure guifg=#fbf8ff gui=none ctermfg=7 cterm=none

hi PreProc guifg=#90aede gui=none ctermfg=111 cterm=none
hi Type guifg=#90aede ctermfg=111 cterm=none


" =========================
" Filetype
" =========================

" PHP
hi link PhpVarSelector Identifier
hi link PhpStatement PreProc
hi link phpStorageClass PreProc
hi link phpStructure Structure

" SCSS
hi link scssDefinition Identifier
hi link scssClass PreProc
hi link scssId PreProc
hi link scssColor String
hi link scssVariable Function
hi link scssImportStr String
hi link scssExtend Function

" CSS
hi link cssTextProp scssDefinition
hi link cssUIProp scssDefinition
hi link cssTagName scssClass
hi link cssColor scssColor
hi link cssColorProp scssDefinition
hi link cssBoxProp scssDefinition
hi link cssRenderProp scssDefinition
hi link cssUIAttr PreProc
hi link cssFontProp scssDefinition
hi link cssGeneratedContentProp scssDefinition
hi link cssFunction scssDefinition
hi link cssFontAttr scssDefinition
hi link cssCommonAttr String
hi link cssFontDescriptorAttr scssDefinition
hi link cssColorAttr scssDefinition
hi link cssTextAttr String
hi link cssBoxAttr PreProc
hi link cssGeneratedContentAttr PreProc
hi link cssAuralAttr PreProc
hi link cssPagingAttr PreProc
hi link cssRenderAttr PreProc
hi link cssTableAttr scssDefinition

" HTML
hi link htmlEndTag Identifier
hi htmlEndTag gui=bold cterm=bold
hi link htmlArg Function
hi htmlArg gui=none cterm=none
hi link htmlLink Operator
hi htmlLink gui=underline cterm=underline

hi link htmlTag Identifier
hi link htmlTagName PreProc
hi link htmlSpecialTagName PreProc
hi link htmlString String
hi link htmlValue String

hi link htmlH1 Operator
hi link htmlH2 Operator
hi link htmlH3 Operator
hi link htmlH4 Operator
hi link htmlH5 Operator
hi link htmlH6 Operator

" JavaScript
hi link javaScriptStringD String
hi link javaScriptStringS String
hi link javaScriptBoolean String
hi link javaScriptNull String

hi link javaScriptNumber PreProc
hi link javaScriptConditional PreProc
hi link javaScriptRepeat PreProc
hi link javaScriptFunction PreProc
hi link javaScriptIdentifier PreProc

hi link javaScriptOperator Operator
hi link javaScriptBraces Operator
hi link javaScriptParens Operator
hi link javaScriptSpecial Operator
hi link javaScriptMessage Operator
hi link javaScriptGlobal Operator
hi link javaScriptMember Operator

hi link javaScript Identifier

" Improved JavaScript syntax
hi link javaScriptBrowserObjects Function
hi link javaScriptOperator Function
hi link javaScriptHtmlElemProperties Identifier
hi link javascriptDOMProperties Identifier
hi link javaScriptBraces Operator
hi link javaScriptFuncArg PreProc
hi link javaScriptEventListenerKeywords Identifier
hi link javaScriptDOMObjects Identifier

hi link javaScriptOpSymbols Operator
hi link javaScriptLogicSymbols Operator
hi link javaScriptSemiColon Operator
hi link javaScriptComma Operator
hi link javaScriptColon Operator
hi link javaScriptDot Operator
hi link javaScriptPlus Operator
hi link javaScriptFuncArg Function
hi link javaScriptFuncKeyword PreProc
hi link javaScriptLogger PreProc

" Coffeescript
hi link coffeeConditional PreProc
hi link coffeeRepeat PreProc
hi link coffeeObject Identifier
hi link coffeeSpecialIdent PreProc
hi link coffeeParens Statement

" Jinja
hi link jinjaVariable Identifier
hi link jinjaString String
hi link jinjaNumber PerProc

" Python
hi link pythonBuiltinFunc Function

hi link pythonBuiltinObj String
hi link pythonDecorator String
hi link pythonDottedName String
hi link pythonTripleString String

hi link pythonPreCondit PreProc
hi link pythonOperator PreProc
hi link pythonConditional PreProc
hi link pythonRepeat PreProc
hi link pythonStatement PreProc

" SQL
hi link sqlKeyword PreProc
hi link sqlSpecial String

" Haskell
hi link hsDelimiter Operator

" Haml
hi link hamlId Function
hi link hamlClass Function

" Jade
hi link jadeId Function
hi link jadeClass Function
