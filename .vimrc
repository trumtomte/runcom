" Load Vim plugins with Pathogen
call pathogen#infect('~/.vim/bundle')
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

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
set wildignore+=*/tmp/*,*.so,*.swp,*.zip  " MacOSX/Linux
set wildignore+=tmp\*,*.swp,*.zip,*.exe   " Windows
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

    " Basic apperance
    hi LineNr guibg=#323232 guifg=#5d5d5d
    hi Normal guibg=#2b2b2b
    hi Comment guifg=#777777 gui=italic
    hi cursorline guibg=#333333
else
    set t_Co=256
    set background=dark

    " Basic apperance
    hi LineNr ctermbg=235 ctermfg=240
    hi Normal ctermbg=234
    hi Comment ctermfg=243 term=none
    hi cursorline ctermbg=236
    hi NonText ctermfg=242
endif

" ctrlP plugin
let g:ctrlp_working_path_mode = 1
nnoremap f :CtrlP<CR>
nnoremap <S-f> :CtrlPLine<CR>
nnoremap <C-f> :CtrlPMRUFiles<CR>
nnoremap , :CtrlPBuffer<CR>

" Directory for virtualenv
let g:virtualenv_directory = '/Users/sebbe/Python'

let g:netrw_liststyle = 3       " Use tree-mode as default view
let g:netrw_browse_split = 4    " Open file in previous buffer
let g:netrw_preview = 1         " preview window shown in a vertically split
let g:netrw_winsize = 20        " netrw window size (20%)

nmap <F3> :Vex <cr>

" Function for relative line numbers
function! g:ToggleNuMode()
    if(&rnu == 1)
        set nu
    else
        set rnu
    endif
endfunc

" Toggle relative line numbers
nnoremap <C-n> :call g:ToggleNuMode()<cr>

" Previous and Next tab
nmap <F1> gT
nmap <F2> gt

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
