set nocompatible
syntax on
filetype plugin indent on
colorscheme sherlock

" Settings
" ========
set t_Co=256
set autoindent                  " Copy indent from previous line
set autoread                    " Auto update file if it changes outside of vim
set backspace=indent,eol,start  " Allow backspace in insertmode
set colorcolumn=81              " Show eol-border
set cursorline                  " Highlight current line
set expandtab                   " Use spaces for tabs instead
set hlsearch                    " Show highlights when searching
set incsearch                   " Show highlights when typing
set noswapfile
set nowrap
set number                      " Show line numbers
set shiftwidth=2                " Number of spaces when autoindenting
set showbreak=...               " Wrapped linebreaks begin with `...`
set softtabstop=2               " Number of spaces that counts as one <Tab>
set textwidth=80
set undodir=~/.vim/undo         " Directory for undo files
set undofile                    " Persist undo files
set wildmenu                    " Enable wildmenu for tab-completion

" Bindings
" ========
let mapleader = "\<Space>"
inoremap jj <ESC>
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
