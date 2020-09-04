set nocompatible

" Vim-Vundle
" ==========
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" Visually show indent guides 
" Plugin 'nathanaelkane/vim-indent-guides'
" Improved focus mode
Plugin 'junegunn/goyo.vim'
" Better syntax highlights for JS
" Plugin 'pangloss/vim-javascript'
" Better syntax highlights for JSX
" Plugin 'MaxMEllon/vim-jsx-pretty'
" Better syntax highlights for CSS
" Plugin 'hail2u/vim-css3-syntax'
" Better syntax highlights for nginx
" Plugin 'chr4/nginx.vim'
" Better syntax highlights for elixir
" Plugin 'elixir-editors/vim-elixir'

" syntax pack
Plugin 'sheerun/vim-polyglot'
call vundle#end()

" Enables fzf
set rtp+=/usr/local/opt/fzf

" Settings
" ========
syntax on
filetype plugin on
filetype indent on
colorscheme sherlock

set t_Co=256
set backspace=indent,eol,start  " Allow backspace in insertmode
set expandtab                   " Use spaces for tabs instead
set autoindent                  " Copy indent from previous line
set shiftwidth=4                " Number of spaces when autoindenting
set softtabstop=4               " Number of spaces that counts as one <Tab>
set hlsearch                    " Show highlights when searching
set incsearch                   " Show highlights when typing
set number                      " Show line numbers
set numberwidth=5               " Minimum width for line numbers
set nobackup                    " Disable swap files
set noswapfile                  " --
set undofile                    " Persist undo files
set undodir=~/.vim/undo         " Directory for undo files
set scrolloff=3                 " Show two extra lines when scrolling
set mousehide                   " Hide mouse when moving/writing
set wildmenu                    " Enable wildmenu for tab-completion
set laststatus=2                " Always have a status line at the last window
set showbreak=...               " Wrapped linebreaks begin with `...`
set cursorline                  " Highlight current line
set splitright                  " Vsp to Right
set splitbelow                  " Sp to bottom
set autoread                    " Auto update file if it changes outside of vim
set colorcolumn=81
set textwidth=80
set statusline=
set statusline +=%2*\ %r\ %m\   " Read-only + Modified?
set statusline +=%1*\ %f\       " File + Path
set statusline +=%=             " Separator
set statusline +=%3*\ %y\       " Filetype

" Custom settings
" ===============
let mapleader = "\<Space>"
" let g:indent_guides_auto_colors = 0
" let g:indent_guides_enable_on_vim_startup = 1
let g:jsx_ext_required = 0

" Bindings
" ========
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
nmap - :sp
nmap _ :vsp
nmap § .
nmap <Tab> <C-W>w
nmap <S-Tab> <C-W>h
" Keep selection when indenting
vmap < <gv
vmap > >gv

" Leader bindings
nmap <leader>w :w<CR>
nmap <leader>q :q<CR>
nmap <leader>n :noh <CR>
nmap <leader>g :Goyo<CR>
nmap <leader>c :set cursorcolumn!<CR>
nmap <leader>r /

" Print highlight group
nmap <F9> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . ">"<CR>

" Auto commands
" =============
autocmd BufNewFile,BufRead *.js call matchadd('Operator', '[')
autocmd BufNewFile,BufRead *.js call matchadd('Operator', ']')
" autocmd BufNewFile,BufRead *.js setlocal shiftwidth=2
" autocmd BufNewFile,BufRead *.js setlocal softtabstop=2
autocmd BufNewFile,BufRead *.schema set filetype=json
autocmd BufNewFile,BufRead *.hbs set filetype=html
autocmd BufNewFile,BufRead *.md set filetype=markdown
autocmd BufNewFile,BufRead *.md setlocal wrap
autocmd BufNewFile,BufRead *.md setlocal tw=80
autocmd BufNewFile,BufRead *.md syntax match Comment /\%^---\_.\{-}---$/
autocmd BufNewFile,BufRead *.md syntax match Operator /^{%\shighlight.*%}\_.\{-}{%\sendhighlight\s%}$/
autocmd BufNewFile,BufRead *.html syntax match Comment /\%^---\_.\{-}---$/
autocmd BufNewFile,BufRead *.ts set filetype=javascript
autocmd BufNewFile,BufRead *.svelte set filetype=html
autocmd BufNewFile,BufRead *.vue set filetype=html
autocmd BufNewFile,BufRead *.vue setlocal shiftwidth=2
autocmd BufNewFile,BufRead *.vue setlocal softtabstop=2
