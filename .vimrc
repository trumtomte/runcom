set nocompatible

" =========================
" Vim-Plug
" =========================
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'tpope/vim-surround'
Plugin 'mxw/vim-jsx'
Plugin 'pangloss/vim-javascript'
Plugin 'chr4/nginx.vim'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'elixir-editors/vim-elixir'
Plugin 'hail2u/vim-css3-syntax'
call vundle#end()

colorscheme watson            " Colorscheme
syntax on                       " Enable syntax highlightning

" =========================
" Settings
" =========================
filetype plugin on
filetype indent on

set t_Co=256                    
set nuw=5
set backspace=indent,eol,start  " Allow backspace in insertmode
set expandtab                   " Tabs/Indent
set autoindent                  " --
set shiftwidth=4                " --
set softtabstop=4               " --
set hlsearch                    " Enable search highlighting
set incsearch                   " Enable incremental search
set number                      " Show line numbers
set nobackup                    " Disable swap files
set noswapfile                  " --
set nowb                        " --
set undodir=~/.vim/undo         " where to save undo histories NOTE requires the dir to exist
set undofile                    " Save undo's after file closes
set scrolloff=3                 " Show two extra lines when scrolling
set mousehide                   " Hide mouse when moving/writing
set wildmenu                    " Enable wildmenu for tab-completion
set wildmode=longest:list       " --
set wildignore+=*/*git/*,*/*hg/*,*/*svn/*,*/*sass-cache/*,*/*node_modules/*,*DS_Store*,*pyc*,*/*_site/*,*.png,*.jpg,*.gif
set laststatus=2                " Always have a status line at the last window
set nowrap                      " Don't wrap lines
set cursorline                  " Enable cursorline
set splitright                  " Vsp to Right
set splitbelow                  " Sp to bottom
set autoread                    " Auto update file if it changes outside of vim
set autochdir                   " --
set foldmethod=marker           " automatic folds with {{{ }}}
set cc=80

" Leader key
let mapleader = "\<Space>"

" =========================
" Statusline
" =========================
set statusline=
set statusline +=%2*\ %r\ %m\   " Read-only + Modified?
set statusline +=%1*\ %f\       " File + Path
set statusline +=%=             " Separator
set statusline +=%3*\ %y\       " Filetype

" =========================
" ctrlP
" =========================
nmap <leader>f :CtrlP<CR>
nmap <leader><S-f> :CtrlPLine<CR>
nmap <leader><C-f> :CtrlPMRUFiles<CR>
nmap <leader>, :CtrlPBuffer<CR>
nmap <leader><tab> :CtrlPBuffer<CR><CR>
let g:ctrlp_max_height = 25
let g:ctrlp_custom_ignore = "node_modules\|DS_store\|git\|sass-cache\|_site\|pyc"

" =========================
" Netrw
" =========================
let g:netrw_liststyle = 3       " Use tree-mode as default view
let g:netrw_browse_split = 4    " Open file in previous buffer
let g:netrw_preview = 1         " preview window shown in a vertically split
let g:netrw_winsize = 10        " netrw window size (10%)
let g:netrw_list_hide = '\.sass-cache\|\.DS_Store'
nmap <leader>3 :Vex <cr>

" =========================
" JSX
" =========================
let g:jsx_ext_required = 0

" =========================
" Indent guides
" =========================
let g:indent_guides_auto_colors = 0
let g:indent_guides_enable_on_vim_startup = 1

" =========================
" Markdown
" =========================
let g:vim_markdown_folding_disabled = 1
autocmd BufNewFile,BufRead *.md set filetype=markdown
autocmd BufNewFile,BufRead *.md set wrap

" Jekyll
autocmd BufNewFile,BufRead *.md syntax match Comment /\%^---\_.\{-}---$/
" syntax for old highlight via pygments
autocmd BufNewFile,BufRead *.md syntax match Operator /^{%\shighlight.*%}\_.\{-}{%\sendhighlight\s%}$/

" =========================
" Bindings
" =========================
" Jump between tabs
nmap <leader>1 gT
nmap <leader>2 gt
" Jump between windows with the arrow-keys
nmap  <Up>    <C-w>k
nmap  <Down>  <C-w>j
nmap  <Left>  <C-w>h
nmap  <Right> <C-w>l
" Remap escape in insert-mode
inoremap jj <ESC>
" Jump up/down a paragraph
nmap K {
nmap J }
" Scroll down/up 5 lines
nmap <C-j> 5<C-e>
nmap <C-k> 5<C-y>
" Go to start/end of the current line in insert/visual mode
nmap <C-h> ^
nmap <C-l> <END>
vmap <C-h> ^
vmap <C-l> <END>
" Make 'U' work as redo
nmap <S-u> <C-r>
" Maps -, § to horizontal and vertical split respectively
nmap - :sp
nmap _ :vsp
" Easy hotkey to repeat commands
nmap § .
" Tab through windows
nmap <Tab> <C-W>w
nmap <S-Tab> <C-W>h
" Indenting multilines in visual mode keeps the selection afterwards
vmap < <gv
vmap > >gv
" Sudo write + regular write
noremap <leader>W :w !sudo tee %<CR>
nmap <leader>s :w<CR>
nmap <leader>w :w<CR>
" Close file
nmap <leader>q :q<CR>
" Remove highlight of found matches
nmap <leader>n :noh <CR>
" Disable arrow-keys in insert-mode
inoremap <Left>  <NOP>
inoremap <Right> <NOP>
inoremap <Up>    <NOP>
inoremap <Down>  <NOP>
" Unbind command history
map q: <NOP>
nnoremap q: <NOP>
nnoremap Q <NOP>
" Find ...
nmap <leader>r /
" Search and replace %
nmap <leader>R :%s/

" =========================
" Misc
" =========================

" Remove trailing spaces
function! StripTrailingWhitespace()
    let l:previousPosition = getpos('.')
    let l:previousSearch = @/
    %s/\s\+$//e
    let @/ = l:previousSearch
    call setpos('.', l:previousPosition)
endfunction
nnoremap <silent> <leader>5 :call StripTrailingWhitespace()<CR>

" Show higlight group
nmap <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
            \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
            \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" ====================
" Auto commands
" ====================

autocmd BufNewFile,BufRead *.go setfiletype go
" ugly hack for js
autocmd BufNewFile,BufRead *.js call matchadd('Operator', '[')
autocmd BufNewFile,BufRead *.js call matchadd('Operator', ']')
" For pep8
autocmd BufNewFile,BufRead *.py set cc=80
" Misc
autocmd BufNewFile,BufRead *.schema setfiletype json
autocmd BufNewFile,BufRead *.hbs setfiletype html
" TMP
" autocmd FileType javascript,json setlocal foldmarker={,}
