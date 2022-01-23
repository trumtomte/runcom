set nocompatible
syntax on
filetype plugin indent on
colorscheme sherlock

" Settings
" ========
set t_Co=256
set backspace=indent,eol,start  " Allow backspace in insertmode
set autoread                    " Auto update file if it changes outside of vim
set expandtab                   " Use spaces for tabs instead
set shiftwidth=2                " Number of spaces when autoindenting
set softtabstop=2               " Number of spaces that counts as one <Tab>
set autoindent                  " Copy indent from previous line
set colorcolumn=81              " Show eol-border
set textwidth=80
set hlsearch                    " Show highlights when searching
set incsearch                   " Show highlights when typing
set number                      " Show line numbers
" set numberwidth=5               " Minimum width for line numbers
" set nobackup                   
set noswapfile                  
set undofile                    " Persist undo files
set undodir=~/.vim/undo         " Directory for undo files
" set scrolloff=3                 " Show two extra lines when scrolling
set wildmenu                    " Enable wildmenu for tab-completion
" set laststatus=1                " Always have a status line at the last window
set showbreak=...               " Wrapped linebreaks begin with `...`
set cursorline                  " Highlight current line
" set splitright                  " Vsp to Right
" set splitbelow                  " Sp to bottom
" set statusline=                 " Customize the status line
" set statusline +=%2*\ %r\ %m\   " Read-only + Modified
" set statusline +=%1*\ %f\       " File + Path
" set statusline +=%=             " Separator
" set statusline +=%3*\ %y\       " Filetype

" Bindings
" ========
let mapleader = "\<Space>"
inoremap jj <ESC>
nmap <C-j> 5<C-e>
nmap <C-k> 5<C-y>
nmap <leader>w :w<CR>
nmap <leader>q :q<CR>
nmap <leader>n :noh <CR>
nmap <leader>e :set rnu!<CR>
" fzf.vim
nmap <leader>o :Files<CR>
nmap <leader>f :Rg<CR>
nmap <leader>F :BLines<CR>
nmap <leader>b :Buffers<CR>

" Print highlight group
nmap <F9> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . ">"<CR>

" Local .vimrc config
if filereadable(expand("~/.vimrc.local"))
    source ~/.vimrc.local
endif
