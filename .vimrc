set nocompatible                " Vim settings

" Load Vim plugins with Pathogen
call pathogen#infect('~/.vim/bundle/{}')
call pathogen#incubate()
call pathogen#helptags()

" Colorscheme
colorscheme sherlock

if has('gui_running')
    set guioptions-=T           " Remove GUI features
    set guioptions-=m
    set guioptions-=r
    set guioptions-=L
    set lines=50                " Window size
    set columns=160
else
    set t_Co=256
endif

syntax on                       " Enable syntax highlightning
filetype plugin on
filetype indent on

set backspace=indent,eol,start  " Allow backspace in insertmode
set history=1000
set showcmd
set showmode
set hidden
set expandtab
set autoindent
set smartindent
set shiftwidth=4
set softtabstop=4
set hlsearch                    " Enable search highlighting
set incsearch                   " Enable incremental search
set number                      " Show line numbers
set nobackup                    " Disable swap files
set noswapfile
set nowb
set undofile                    " Save undo's after file closes
set undodir=~/.vim/undo//       " where to save undo histories
set scrolloff=5                 " Show two extra lines when scrolling
set mousehide                   " Hide mouse when moving/writing
set wildmenu                    " Enable wildmenu for tab-completion
set wildmode=longest:list
set wildignore+=*/*git/*,*/*hg/*,*/*svn/*,*/*sass-cache/*,*DS_Store*,*.png,*.jpg,*.gif
set laststatus=2                " Always have a status line at the last window
set nowrap                      " Don't wrap lines
set cursorline                  " Enable cursorline
set splitright                  " Vsp to Right
set splitbelow                  " Sp to bottom
set autoread                    " Auto update file if it changes outside of vim

let mapleader = "\<Space>"

" =========================
" Statusline
" =========================
set statusline=
set statusline +=%1*\ %<%F\     " File + Path
set statusline +=%2*\ %r\ %m\   " Read-only + Modified?
set statusline +=%=             " Separator
set statusline +=%3*\ %y\       " Filetype
set statusline +=%4*\ col\ %c,  " Column
set statusline +=%4*\ line\ %l  " Line
set statusline +=%4*\ of\ %L    " Lines
set statusline +=%4*\ (%p%%)\   " %-Lines
" =========================
" Statusline Colors
" =========================
hi User1 guifg=#c1ae6e ctermfg=179 guibg=#222222 ctermbg=234
hi User2 guifg=#cc2f47 ctermfg=197 guibg=#222222 ctermbg=234
hi User3 guifg=#7c96bf ctermfg=11 guibg=#222222 ctermbg=234
hi User4 guifg=#777777 ctermfg=243 guibg=#222222 ctermbg=234

" =========================
" Binds for ctrlP plugin
" =========================
nnoremap <leader>f :CtrlP<CR>
nnoremap <leader><S-f> :CtrlPLine<CR>
nnoremap <leader><C-f> :CtrlPMRUFiles<CR>
nnoremap <leader>, :CtrlPBuffer<CR>
let g:ctrlp_working_path_mode = 'a'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\.git$\|\.hg$\|\.svn$\|\.sass-cache$',
  \ 'file': '\.exe$\|\.so$\|\.dat$\|\.DS_Store$'
  \ }

" =========================
" Netrw settings
" =========================
let g:netrw_liststyle = 3       " Use tree-mode as default view
let g:netrw_browse_split = 4    " Open file in previous buffer
let g:netrw_preview = 1         " preview window shown in a vertically split
let g:netrw_winsize = 20        " netrw window size (20%)
let g:netrw_list_hide = '\.sass-cache\|\.DS_Store'
nmap <leader>3 :Vex <cr>

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

" Scroll down/up 3 lines
nmap <C-j> 3<C-e>
nmap <C-k> 3<C-y>

" Go to start/end of the current line
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
vnoremap < <gv
vnoremap > >gv

" Sudo write
noremap <leader>W :w !sudo tee %<CR>

" Remove highlight of found matches
noremap <leader>n :noh <CR>

" Fullscreen
if has('gui_running')
    set nofullscreen
    nnoremap <leader>4 :set fullscreen!<CR>
endif

" Disable arrow-keys in insert-mode
inoremap <Left>  <NOP>
inoremap <Right> <NOP>
inoremap <Up>    <NOP>
inoremap <Down>  <NOP>

" Unbind command history
nnoremap q: <NOP>

" Change fontsize
" :silent set guifont=Menlo:h13 lines=65<CR>

" Remove trailing spaces
function! StripTrailingWhitespace()
    let l:previousPosition = getpos('.')
    let l:previousSearch = @/
    %s/\s\+$//e
    let @/ = l:previousSearch
    call setpos('.', l:previousPosition)
endfunction
nnoremap <silent> <leader>5 :call StripTrailingWhitespace()<CR>
