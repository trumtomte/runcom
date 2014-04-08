set nocompatible

" =========================
" Pathogen
" =========================
call pathogen#infect('~/.vim/bundle/{}')
call pathogen#incubate()
call pathogen#helptags()

colorscheme sherlock            " Colorscheme

if has('gui_running')           " Remove GUI features and set window size
    set guioptions-=T
    set guioptions-=m
    set guioptions-=r
    set guioptions-=L
    set lines=50
    set columns=160
else
    set t_Co=256
endif

" =========================
" Settings
" =========================
syntax on                       " Enable syntax highlightning
filetype plugin on
filetype indent on

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
set undofile                    " Save undo's after file closes
set undodir=~/.vim/undo//       " where to save undo histories
set scrolloff=3                 " Show two extra lines when scrolling
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
" Statusline Colors
" =========================
hi User1 guifg=#c1ae6e ctermfg=179 guibg=#181818 ctermbg=234
hi User2 guifg=#cc2f47 ctermfg=197 guibg=#181818 ctermbg=234
hi User3 guifg=#7c96bf ctermfg=11 guibg=#181818 ctermbg=234
hi User4 guifg=#777777 ctermfg=243 guibg=#181818 ctermbg=234

" =========================
" ctrlP
" =========================
nmap <leader>f :CtrlP<CR>
nmap <leader><S-f> :CtrlPLine<CR>
nmap <leader><C-f> :CtrlPMRUFiles<CR>
nmap <leader>, :CtrlPBuffer<CR>
nmap <leader><tab> :CtrlPBuffer<CR><CR>
" let g:ctrlp_working_path_mode = 'c'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\.git$\|\.hg$\|\.svn$\|\.sass-cache$',
  \ 'file': '\.exe$\|\.so$\|\.dat$\|\.DS_Store$'
  \ }

" =========================
" Markdown
" =========================
let g:vim_markdown_folding_disabled=1

" =========================
" Netrw
" =========================
let g:netrw_liststyle = 3       " Use tree-mode as default view
let g:netrw_browse_split = 4    " Open file in previous buffer
let g:netrw_preview = 1         " preview window shown in a vertically split
let g:netrw_winsize = 20        " netrw window size (20%)
let g:netrw_list_hide = '\.sass-cache\|\.DS_Store'
nmap <leader>3 :Vex <cr>

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
nnoremap q: <NOP>
" Find ...
nmap <leader>r /
" Search and replace %
nmap <leader>R :%s/
" Fullscreen
if has('gui_running')
    set nofullscreen
    nnoremap <leader>4 :set fullscreen!<CR>
endif

" =========================
" Other
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

" Change fontsize
" :silent set guifont=Menlo:h13 lines=65<CR>

" More subtle environment when writing markdown
function! MarkdownMode()
    if (&foldcolumn != 2)
        set foldcolumn=2
        set nocursorline
        hi FoldColumn guibg=#222222
        hi LineNr guibg=#222222 guifg=#222222
        hi Normal guibg=#222222
        hi NonText guifg=#222222
    else
        " Reset
        set foldcolumn=0
        set cursorline
        hi LineNr guibg=#222222 guifg=#3a3a3a
        hi Normal guibg=#222222
        hi NonText guifg=#777777
    endif
endfunc
nnoremap <leader>6 :call MarkdownMode()<CR>

" Temporary fix?
autocmd BufNewFile,BufRead *.go setfiletype go
