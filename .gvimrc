" Load Vim plugins with Pathogen
call pathogen#infect('~/.vim/bundle')
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

" Directory for virtualenv
let g:virtualenv_directory = '/Users/sebbe/Python'

syntax on                       " Enable syntax highlightning
filetype plugin indent on       " Enable filetype specific features
set encoding=utf-8              " Set fileencoding for files to UTF-8
setglobal fileencoding=utf-8
set nocompatible                " Disable Vi - compability
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set hlsearch                    " Enable search highlighting
set incsearch                   " Enable incremental search
set noignorecase                " Case-sensitive search
set nosmartcase
set number                      " Show line numbers
set ruler                       " Show line number in statusbar
set backupdir=~/.vim/swp//      " Directories for swap files
set dir^=~/.vim/swp//
set scrolloff=2                 " Show two extra lines when scrolling
set mouse=a                     " Enable mouse by default
set mousehide                   " Hide mouse when moving/writing
set wildmenu                    " Enable wildmenu for tab-completion
set wildmode=longest:full       " Configure wildmenu to behave more like bash
set wildignore=*.py[co]         " Avoid to open python bytecode
set laststatus=2                " Always have a status line at the last window
set nowrap                      " Don't wrap lines
set cul                         " Enable cursorline

" Change tab width to 2 for HTML files
autocmd FileType html setlocal shiftwidth=2 tabstop=2
autocmd FileType jinja setlocal shiftwidth=2 tabstop=2

if has('gui_running')
    colorscheme desert  " Default colorscheme
    set guioptions-=T   " Remove GUI features
    set guioptions-=m
    set guioptions-=r
    set guioptions-=L
    set lines=50        " Window size
    set columns=160
else
    set t_Co=256
    colorscheme solarized
    let g:solarized_termtrans=1
    let g:solarized_termcolors=256
    set background=dark
endif

" Basic apperance
hi LineNr guibg=#323232 guifg=#5d5d5d
hi Normal guibg=#2b2b2b
hi Comment guifg=#777777 gui=italic
hi cursorline guibg=#333333

" Made darker, looks better on some screens
"hi LineNr guibg=#080808 guifg=#5d5d5d
"hi Normal guibg=#000000
"hi Comment guifg=#777777 gui=italic
"hi cursorline guibg=#111111
"hi NonText guibg=#000000 guifg=#222222
"set guifont=Terminus


" Function for relative line numbers
function! g:ToggleNuMode()
    if(&rnu == 1)
        set nu
    else
        set rnu
    endif
endfunc

" Previous and Next tab
nmap <F1> gT
nmap <F2> gt

" activate nerdtree
nmap <F3> :NERDTree <cr>
" run python script
nmap <F6> :!python % <cr>

" Disable arrow-keys in insert-mode
inoremap <Left>  <NOP>
inoremap <Right> <NOP>
inoremap <Up>    <NOP>
inoremap <Down>  <NOP>

" Jump between windows with arrow-keys ( + § for left window)
nmap  <Up>    <C-w>k
nmap  <Down>  <C-w>j
nmap  <Left>  <C-w>h
nmap  <Right> <C-w>l

" Remap escape in insert-mode
inoremap jj <ESC>

" Jump up/down a paragraph
nmap K {
nmap J }

" Scroll down/up 3 lines
nmap <C-j> 3<C-e>
nmap <C-k> 3<C-y>

" Go to start/end of the currnet line
nmap <C-h> _
nmap <C-l> <END>

" Make 'U' work as redo
nmap <S-u> <C-r>

" Run Syntastic for errors
nmap <silent> <F4> :SyntasticCheck<cr><bar>:Errors<cr>

" Remove trailing spaces
nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

" Toggle relative line numbers
nnoremap <C-n> :call g:ToggleNuMode()<cr>

" Maps Alt-[up, down, left, right] to resizing a window split
nmap <silent> <A-Up> <C-W>+
nmap <silent> <A-Down> <C-W>-
nmap <silent> <A-Left> <C-w><
nmap <silent> <A-Right> <C-w>>

" Maps -, § to horizontal and vertical split respectively
nmap - <C-W>s<C-W><Down>
nmap § <C-W>v<C-W><Right>

" Tab through windows
nmap <Tab> <C-W>w
