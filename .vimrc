" Load Vim plugins with Pathogen
call pathogen#infect('~/.vim/bundle')
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()
" colorscheme
colorscheme watson

syntax on                       " Enable syntax highlightning
filetype plugin indent on       " Enable filetype specific features
set encoding=utf-8              " Set fileencoding for files to UTF-8
setglobal fileencoding=utf-8
set nocompatible                " Disable Vi - compability
set softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
set hlsearch                    " Enable search highlighting
set incsearch                   " Enable incremental search
set noignorecase                " Case-sensitive search
set nosmartcase
set number                      " Show line numbers
set ruler                       " Show line number in statusbar
" Backup
set backup
set backupdir=~/.vim/swp//      " Directories for swap files
set directory=~/.vim/swp//
set scrolloff=3                 " Show two extra lines when scrolling
set mouse=a                     " Enable mouse by default
set mousehide                   " Hide mouse when moving/writing
set wildmode=longest:list
set wildmenu                    " Enable wildmenu for tab-completion
set wildignore+=*/*git/*,*/*hg/*,*/*svn/*,*/*sass-cache/*,*DS_Store*,*.png,*.jpg,*.gif
set laststatus=2                " Always have a status line at the last window
set nowrap                      " Don't wrap lines
set cul                         " Enable cursorline
set nofoldenable
" Split windows to right/below
set splitright
set splitbelow
set autoread
" fix slight delay after pressing ESC then O
" http://ksjoberg.com/vim-esckeys.html
" set noesckeys
set timeout timeoutlen=1000 ttimeoutlen=100
" <leader> key
let mapleader=","
" autocommands for filetypes
autocmd FileType html,haml,jinja,jade setlocal shiftwidth=4 softtabstop=4

if has('gui_running')
    set guioptions-=T   " Remove GUI features
    set guioptions-=m
    set guioptions-=r
    set guioptions-=L
    set lines=50        " Window size
    set columns=160
else
    set t_Co=256
endif
" Statusline
set statusline=
set statusline +=%1*\ %<%F\     " File + Path
set statusline +=%2*\ %r\ %m\   " Read-only + Modified?
set statusline +=%=             " Separator
set statusline +=%3*\ %y\       " Filetype
set statusline +=%4*\ col\ %c,  " Column
set statusline +=%4*\ line\ %l  " Line
set statusline +=%4*\ of\ %L    " Lines
set statusline +=%4*\ (%p%%)\   " %-Lines
" Statusline Colors
hi User1 guifg=#c1ae6e ctermfg=179 guibg=#222222 ctermbg=234
hi User2 guifg=#cc2f47 ctermfg=197 guibg=#222222 ctermbg=234
hi User3 guifg=#7c96bf ctermfg=11 guibg=#222222 ctermbg=234
hi User4 guifg=#777777 ctermfg=243 guibg=#222222 ctermbg=234
" Binds for ctrlP plugin
nnoremap <leader>f :CtrlP<CR>
nnoremap <leader><S-f> :CtrlPLine<CR>
nnoremap <leader><C-f> :CtrlPMRUFiles<CR>
nnoremap <leader>, :CtrlPBuffer<CR>

let g:ctrlp_working_path_mode = 'a'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\.git$\|\.hg$\|\.svn$\|\.sass-cache$',
  \ 'file': '\.exe$\|\.so$\|\.dat$\|\.DS_Store$'
  \ }

" Directory for virtualenv
let g:virtualenv_directory = '/Users/sebbe/Python'
" netrw settings
let g:netrw_liststyle = 3       " Use tree-mode as default view
let g:netrw_browse_split = 4    " Open file in previous buffer
let g:netrw_preview = 1         " preview window shown in a vertically split
let g:netrw_winsize = 20        " netrw window size (20%)
let g:netrw_list_hide = '\.sass-cache\|\.DS_Store'
" Open netrw (vertical)
nmap <F3> :Vex <cr>
nmap <leader>t :Vex <cr>
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
nmap <leader>1 gT
nmap <leader>2 gt
" Call python from the current file
nmap <F6> :!python % <cr>
" Disable arrow-keys in insert-mode
inoremap <Left>  <NOP>
inoremap <Right> <NOP>
inoremap <Up>    <NOP>
inoremap <Down>  <NOP>
" Unbind history
nnoremap q: <NOP>
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
" Remove trailing spaces
function! StripTrailingWhitespace()
    let l:previousPosition = getpos('.')
    let l:previousSearch = @/
    %s/\s\+$//e
    let @/ = l:previousSearch
    call setpos('.', l:previousPosition)
endfunction

nnoremap <silent> <F5> :call StripTrailingWhitespace()<CR>

" Maps Alt-[up, down, left, right] to resizing a window split
nmap <silent> <A-Up> <C-W>+
nmap <silent> <A-Down> <C-W>-
nmap <silent> <A-Left> <C-w><
nmap <silent> <A-Right> <C-w>>
" Maps -, § to horizontal and vertical split respectively
nmap - :sp
nmap _ :vsp
" Easy hotkey to repeat commands
nmap § .
" Tab through windows
nmap <Tab> <C-W>w
nmap <S-Tab> <C-W>h
" Sudo write
noremap <leader>W :w !sudo tee %<CR>
" Remove highlight of found matches
noremap <leader>¨ :noh <CR>
" Show syntax highlighting groups for word under cursor
nmap <C-S-P> :call <SID>SynStack()<CR>
" Indenting multilines in visual mode keeps the selection afterwards
vnoremap < <gv
vnoremap > >gv
" Print highlight group
function! <SID>SynStack()
    if !exists("*synstack")
        return
    endif
    echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc
" FocusMode
let g:curmode = ''
function! ToggleFocusMode()
    if (empty(g:curmode))
        let g:curmode = 'focusmode'
        hi Normal guibg=#1b1b1b
        hi FoldColumn guibg=#1b1b1b
        hi LineNr guibg=#1b1b1b guifg=#1b1b1b
        hi VertSplit guifg=#1b1b1b guibg=#000000
        hi NonText guifg=#1b1b1b
        hi CursorLineNr guifg=#000000 guibg=#333333
        hi foldBraces guifg=#cc2f47
        set columns=240
        " set columns=165
        if (&guifont == 'Menlo:h11')
            set lines=75
        else
            set lines=65
        endif
        set noruler
        set fuoptions=background:#001b1b1b
        set fullscreen
    else
        let g:curmode = ''
        set ruler
        set lines=50
        set columns=160
        set nofullscreen
        execute 'colorscheme ' . g:colors_name
    endif
endfunc
" FocusMode on F4
nnoremap <F4> :call ToggleFocusMode()<cr>
" Change fontsize
if has("gui_running")
    set guifont=Menlo:h11
    "nnoremap <C-Up> :silent let &guifont=substitute(&guifont, ':h\zs\d\+', '\=submatch(0)+1', '')<CR>
    "nnoremap <C-Down> :silent let &guifont=substitute(&guifont, ':h\zs\d\+', '\=submatch(0)-1', '')<CR>
    nnoremap <C-Up> :silent set guifont=Menlo:h13 lines=65<CR>
    nnoremap <C-Down> :silent set guifont=Menlo:h11 lines=75<CR>
endif
