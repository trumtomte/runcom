" Vim color file
" Maintainer:   Sebastian Book <youremail@something.com>
" Last Change:  05-08-2012
" URL:	github.com/trumtomte
"
" --- COLORS ---
" - White
" gui: #fbf8ff  cterm: 7
" - BrownGold (light)
" gui: #cfcaaa  cterm: 144
" - BrownGold (dark)
" gui: #c1ae6e  cterm: 179
" - Red
" gui: #cc2f47  cterm: 197
" - Blue
" gui: #7c96bf  cterm: 11

set background=dark

hi clear
if exists("syntax_on")
    syntax reset
endif

let g:colors_name="sherlock"

hi LineNr guibg=#323232 guifg=#5d5d5d ctermbg=236 ctermfg=240
hi Normal guibg=#2b2b2b guifg=#cfcaaa ctermbg=235 ctermfg=144
hi Comment guifg=#777777 gui=italic ctermbg=235 ctermfg=243 cterm=none
hi CursorLine guibg=#333333 gui=none ctermbg=236 cterm=none
hi CursorLineNr guifg=yellow guibg=#2b2b2b ctermfg=11 ctermbg=235
hi NonText guifg=#777777 ctermfg=243

hi Function guifg=#c1ae6e gui=none ctermfg=179 cterm=none
hi Conditional guifg=#c1ae6e gui=none ctermfg=179 cterm=none
hi Repeat guifg=#c1ae6e gui=none ctermfg=179 cterm=none
hi Label guifg=#c1ae6e gui=none ctermfg=179 cterm=none
hi Statement guifg=#c1ae6e gui=none ctermfg=179 cterm=none

hi String guifg=#cc2f47 gui=none ctermfg=197 cterm=none
hi Number guifg=#cc2f47 gui=none ctermfg=197 cterm=none
hi Boolean guifg=#cc2f47 gui=none ctermfg=197 cterm=none
hi Constant guifg=#cc2f47 gui=none ctermfg=197 cterm=none

hi Identifier guifg=#cfcaaa gui=none ctermfg=144 cterm=none

hi Operator guifg=#fbf8ff gui=none ctermfg=7 cterm=none
hi Special guifg=#fbf8ff gui=none ctermfg=7 cterm=none
hi Structure guifg=#fbf8ff gui=none ctermfg=7 cterm=none

hi PreProc guifg=#7c96bf gui=none ctermfg=111 cterm=none
hi Type guifg=#7c96bf ctermfg=111 cterm=none

hi Todo guifg=#000000 guibg=yellow gui=none ctermfg=0 ctermbg=11 cterm=none
hi Folded guifg=#c1ae6e guibg=#555555 gui=none ctermfg=179 ctermbg=240 cterm=none
hi Directory guifg=#c1ae6e ctermfg=179
hi MatchParen guifg=#c1ae6e guibg=#cc2f47 ctermfg=179 ctermbg=197

hi TabLine ctermbg=0 ctermfg=179 cterm=none
hi TabLineSel ctermbg=239 ctermfg=179 cterm=none
hi TabLineFill ctermbg=0 ctermfg=0 cterm=none

hi Visual guibg=#444444 ctermbg=238


" --- FILETYPE SPECIFIC --- "

" PHP
hi link PhpVarSelector Identifier
hi link PhpStatement PreProc
hi link phpStorageClass PreProc
hi link phpStructure Structure

" CSS / SASS(SCSS)
hi link sassAmpersand Function

hi link cssColor String

hi link cssBraces Operator
hi link cssFunctionName Operator
hi link sassIdChar Operator
hi link sassClassChar Operator

hi link sassMixin Identifier
hi link sassExtend Identifier
hi link sassInclude Identifier
hi link StorageClass Identifier

hi link sassVariable PreProc
hi link sassFunction PreProc
hi link sassId PreProc
hi link sassClass PreProc
hi link cssFunction PreProc
hi link cssTagName PreProc
hi link cssFontAttr PreProc
hi link cssCommonAttr PreProc
hi link cssFontDescriptorAttr PreProc
hi link cssColorAttr PreProc
hi link cssTextAttr PreProc
hi link cssBoxAttr PreProc
hi link cssGeneratedContentAttr PreProc
hi link cssAuralAttr PreProc
hi link cssPagingAttr PreProc
hi link cssUIAttr PreProc
hi link cssRenderAttr PreProc
hi link cssTableAttr PreProc

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
