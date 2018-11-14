""""""""""""""""
" vim-plug
""""""""""""""""
" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" Auto Complete
Plug 'lifepillar/vim-mucomplete'
Plug 'jiangmiao/auto-pairs'
Plug 'scrooloose/nerdcommenter'

" Format
Plug 'sbdchd/neoformat'

" Git
Plug 'tpope/vim-fugitive'

" Make
Plug 'neomake/neomake'

" Navigation
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'Yggdroot/indentLine'

" Plug 'fatih/vim-go'

" Python
Plug 'python-mode/python-mode', { 'branch': 'develop' }

" Rust
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'

" Status
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Theme
Plug 'altercation/vim-colors-solarized'

" Syntastic Check
" Plug 'vim-syntastic/syntastic'

" Initialize plugin system
call plug#end()

""""""""""""""""
" My Config
""""""""""""""""
filetype plugin on
syntax enable

" Auto Complete
set completeopt+=menuone,noselect
let g:mucomplete#enable_auto_at_startup = 1
let g:NERDSpaceDelims = 1

" Format
augroup fmt
  autocmd!
  autocmd BufWritePre * undojoin | Neoformat
augroup END
let g:neoformat_basic_format_retab = 1  " Enable tab to spaces conversion
let g:neoformat_basic_format_trim = 1  " Enable trimmming of trailing whitespace

" Make
call neomake#configure#automake('w')  " When writing a buffer (no delay).

" Navigation
set foldmethod=syntax
set foldlevel=99

" Python
let g:pymode_python = 'python3'

" Rust
let g:racer_experimental_completer = 1
autocmd FileType rust nmap gd <Plug>(rust-def)
autocmd FileType rust nmap gs <Plug>(rust-def-split)
autocmd FileType rust nmap gx <Plug>(rust-def-vertical)
autocmd FileType rust nmap <leader>gd <Plug>(rust-doc)

" Shortcut
nmap <Leader>r :source ~/.config/nvim/init.vim<CR>
nmap <Leader>ff :FZF<CR>
nmap <Leader>fb :Buffers<CR>
nmap <Leader>t :NERDTree %<CR>
nmap <Leader>bd :bp<bar>sp<bar>bn<bar>bd<CR>
nmap <C-o> :b#<CR>

" Status
let g:airline#extensions#tabline#enabled = 1

" Tab
set tabstop=2  " Show existing tab with 2 spaces width
set shiftwidth=2  " When indenting with '>', use 2 spaces width
set expandtab  " On pressing tab, insert spaces
" autocmd FileType vim,yaml setlocal tabstop=2 shiftwidth=2

" Theme
set number
set relativenumber
set background=dark
colorscheme solarized
set cursorline
set cursorcolumn
