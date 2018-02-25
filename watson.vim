" Colorscheme:  Watson
" Maintainer:   Sebastian Bengtegård <sebbebook@gmail.com>
" Last Change:  25-02-2018
" URL:	        https://github.com/trumtomte/runcom/blob/master/sherlock-light.vim

" =========================
" Colors
" =========================
" gui: #fbf8ff  cterm: 7        :white
" gui: #ffd7af  cterm: 223      :beige
" gui: #d7af5f  cterm: 179      :yellow
" gui: #d75f5f  cterm: 167      :red
" gui: #87afaf  cterm: 109      :blue
"
" white: cterm 7
" black: cterm 1
" red: cterm 2, ev 9
" blue: cterm 3
" orange: cterm 4

set background=light

hi clear
if exists("syntax_on")
    syntax reset
endif

let g:colors_name="watson"

" Watson Color Groups
hi WatsonWhite    ctermfg=7       cterm=none
hi WatsonBlack    ctermfg=0       cterm=none
hi WatsonRed      ctermfg=1       cterm=none
hi WatsonGreen     ctermfg=2       cterm=none
hi WatsonOrange   ctermfg=3       cterm=none
hi WatsonGrey     ctermfg=249     cterm=none

hi Normal           ctermbg=7       ctermfg=0
hi Comment          ctermbg=7       ctermfg=249     cterm=none
hi LineNr           ctermbg=7       ctermfg=252
hi CursorLine       ctermbg=255     ctermfg=0       cterm=none
hi CursorLineNr     ctermbg=7       ctermfg=0
hi ColorColumn      ctermbg=255
hi VertSplit        ctermbg=7       ctermfg=0       cterm=none
hi Todo             ctermbg=255     ctermfg=0       cterm=none
hi Visual           ctermbg=255
hi MatchParen       ctermbg=253     ctermfg=0
hi Folded           ctermbg=255     ctermfg=0
hi NonText                          ctermfg=255
hi Search           ctermbg=253     ctermfg=0       cterm=none
hi IncSearch        ctermbg=253     ctermfg=0       cterm=none
hi DiffAdd          ctermbg=254     ctermfg=2       cterm=none
hi DiffChange       ctermbg=254     ctermfg=1       cterm=none
hi DiffDelete       ctermbg=254     ctermfg=1       cterm=none
hi Error            ctermbg=254     ctermfg=1       cterm=none

" Statusline Colors
hi User1            ctermbg=255     ctermfg=2
hi User2            ctermbg=255     ctermfg=1
hi User3            ctermbg=255     ctermfg=0
hi User4            ctermbg=255     ctermfg=0

hi! link Function       WatsonGreen
hi! link PreProc        WatsonGreen
hi! link Identifier     WatsonGreen
hi! link Statement      WatsonGreen
hi! link Special        WatsonRed
hi! link Operator       WatsonBlack
hi! link Type           WatsonRed
hi! link Keyword        WatsonGreen
hi! link Structure      WatsonGreen
hi! link Number         WatsonGreen
hi! link String         WatsonRed
hi! link Boolean        WatsonRed
hi! link Constant       WatsonGreen

" "hi! link Conditional    WatsonRed
" "hi! link Repeat         WatsonRed
" "hi! link Label          WatsonRed
" 
" =========================
" Filetype specific
" =========================

hi link xmlTag          WatsonGreen
hi link xmlTagName      WatsonGreen
hi link xmlAttrib       WatsonGrey

hi link jsThis              WatsonGreen
hi link jsArrowFunction     WatsonBlack
hi link jsStorageClass      WatsonGreen
hi link jsFunction          WatsonGreen
hi link jsGlobalObjects     WatsonGreen
hi link jsTemplateBraces    WatsonGreen
hi link jsTemplateVar       WatsonRed

hi link pythonFunction      WatsonRed

hi link djangoTagBlock      WatsonBlack
hi link djangoStatement     WatsonGreen

hi link htmlTitle           WatsonBlack
hi link htmlArg             WatsonGrey
hi link htmlLink            WatsonBlack
hi link htmlTagN            WatsonGreen
hi link htmlH1              WatsonBlack
hi link htmlH2              WatsonBlack
hi link htmlH3              WatsonBlack
hi link htmlH4              WatsonBlack
hi link htmlH5              WatsonBlack
hi link htmlH6              WatsonBlack
