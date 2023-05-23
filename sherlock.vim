" Colorscheme:  Sherlock
" Maintainer:   Sebastian Bengtegård <sebastianbengtegard@protonmail.com>
" Last Change:  29-04-2022
" URL:	        https://github.com/trumtomte/runcom

" Colors
" ======
" White:        7
" Beige:        223     (foreground)
" Yellow:       179
" Red:          167
" Blue:         109
" Green:        71
" Gray:         243
" Background:   235

set background=dark

hi clear
if exists("syntax_on")
    syntax reset
endif

let g:colors_name="sherlock"

" Groups 
hi SherlockWhite    ctermfg=7       cterm=none 
hi SherlockBeige    ctermfg=223     cterm=none 
hi SherlockYellow   ctermfg=179     cterm=none 
hi SherlockRed      ctermfg=167     cterm=none 
hi SherlockBlue     ctermfg=109     cterm=none 
hi SherlockGreen    ctermfg=71      cterm=none 

" Editor
hi Normal               ctermbg=235     ctermfg=223
hi LineNr               ctermbg=235     ctermfg=240
hi Comment              ctermbg=235     ctermfg=243     cterm=none
hi NonText              ctermbg=235     ctermfg=243
hi Underlined           ctermbg=235     ctermfg=109     cterm=none
hi Todo                 ctermbg=238     ctermfg=179     cterm=none
hi CursorLine           ctermbg=236                     cterm=none
hi CursorLineNr         ctermbg=235     ctermfg=179
hi CursorColumn         ctermbg=237                     cterm=none
hi ColorColumn          ctermbg=236
hi SignColumn           ctermbg=234     ctermfg=179
hi VertSplit            ctermbg=235     ctermfg=179     cterm=none
hi Folded               ctermbg=234     ctermfg=179     cterm=none
hi MatchParen           ctermbg=235     ctermfg=167 
hi Visual               ctermbg=238
hi Pmenu                ctermbg=236     ctermfg=223     cterm=none
hi PmenuSel             ctermbg=234     ctermfg=179     cterm=none
hi PmenuSbar            ctermbg=237     ctermfg=109     cterm=none
hi PmenuThumb           ctermbg=179     ctermfg=167     cterm=none
hi StatusLine           ctermbg=235     ctermfg=109     cterm=none
hi StatusLineNC         ctermbg=235     ctermfg=244     cterm=none
hi WildMenu             ctermbg=235     ctermfg=179     cterm=none

hi link netrwDir        SherlockYellow
hi link netrwExe        SherlockRed
hi link netrwSymLink    SherlockBlue

" Syntax 
hi! link Boolean        SherlockGreen
hi! link Character      SherlockRed
hi! link Conditional    SherlockBlue
hi! link Constant       SherlockRed
hi! link Define         SherlockBlue
hi! link Delimiter      SherlockWhite
hi! link Float          SherlockRed
hi! link Function       SherlockWhite
hi! link Identifier     SherlockWhite
hi! link Include        SherlockBlue
hi! link Keyword        SherlockYellow
hi! link Label          SherlockYellow
hi! link Noise          SherlockWhite
hi! link Number         SherlockRed
hi! link Operator       SherlockWhite
hi! link PreProc        SherlockBlue
hi! link Repeat         SherlockBlue
hi! link Special        SherlockWhite
hi! link SpecialChar    SherlockRed
hi! link Statement      SherlockYellow
hi! link StorageClass   SherlockBlue
hi! link String         SherlockRed
hi! link Structure      SherlockBlue
hi! link Tag            SherlockWhite
hi! link Title          SherlockWhite
hi! link Type           SherlockYellow
hi! link Typedef        SherlockYellow
