" Load Vim plugins with Pathogen
call pathogen#infect('~/.vim/bundle')
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

colorscheme sherlock

syntax on                       " Enable syntax highlightning
filetype plugin indent on       " Enable filetype specific features
set encoding=utf-8              " Set fileencoding for files to UTF-8
setglobal fileencoding=utf-8
set nocompatible                " Disable Vi - compability
"set tabstop=4
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
set backupdir=~/.vim/swp//      " Directories for swap files
set dir^=~/.vim/swp//
set scrolloff=3                 " Show two extra lines when scrolling
set mouse=a                     " Enable mouse by default
set mousehide                   " Hide mouse when moving/writing
set wildmenu                    " Enable wildmenu for tab-completion
set wildmode=longest:full       " Configure wildmenu to behave more like bash
set wildignore=*.py[co]         " Avoid to open python bytecode
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,.DS_Store  " MacOSX/Linux
set wildignore+=tmp\*,*.swp,*.zip,*.exe   " Windows
set laststatus=2                " Always have a status line at the last window
set nowrap                      " Don't wrap lines
set cul                         " Enable cursorline
set nofoldenable
set splitright
set splitbelow

" <leader> key
let mapleader=","

" autocommands for filetypes
" autocmd FileType html setlocal shiftwidth=4 softtabstop=4
" autocmd FileType haml setlocal shiftwidth=4 softtabstop=4
" autocmd FileType jinja setlocal shiftwidth=4 softtabstop=4
 
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

" Binds for ctrlP plugin
nnoremap <leader>f :CtrlP<CR>
nnoremap <leader><S-f> :CtrlPLine<CR>
nnoremap <leader><C-f> :CtrlPMRUFiles<CR>
nnoremap <leader>, :CtrlPBuffer<CR>
let g:ctrlp_working_path_mode = 'a'
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn|DS_Store)$'
let g:ctrlp_user_command = 'find %s -type fd'

" Directory for virtualenv
let g:virtualenv_directory = '/Users/sebbe/Python'

" netrw settings
let g:netrw_liststyle = 3       " Use tree-mode as default view
let g:netrw_browse_split = 4    " Open file in previous buffer
let g:netrw_preview = 1         " preview window shown in a vertically split
let g:netrw_winsize = 20        " netrw window size (20%)

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

" Run Syntastic for errors
" nmap <silent> <F4> :SyntasticCheck<cr><bar>:Errors<cr>

" Remove trailing spaces
nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

" Maps Alt-[up, down, left, right] to resizing a window split
nmap <silent> <A-Up> <C-W>+
nmap <silent> <A-Down> <C-W>-
nmap <silent> <A-Left> <C-w><
nmap <silent> <A-Right> <C-w>>

" Maps -, § to horizontal and vertical split respectively
"nmap - <C-W>s<C-W><Down>
"nmap § <C-W>v<C-W><Right>
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
        set laststatus=0
        set noruler
        set fuoptions=background:#001b1b1b
        set fullscreen
    else
        let g:curmode = ''
        set laststatus=2
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

" Indenting multilines in visual mode keeps the selection afterwards
vnoremap < <gv
vnoremap > >gv
