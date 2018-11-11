""""""""""""""""
" vim-plug
""""""""""""""""
" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" Auto Complete
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'ervandew/supertab'
Plug 'jiangmiao/auto-pairs'

" Buffer
Plug 'lotabout/skim', { 'dir': '~/.skim', 'do': './install' }

" Make
Plug 'neomake/neomake'

" Plug 'fatih/vim-go'

" Directory
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'Xuyuanp/nerdtree-git-plugin'

" FZF
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Python
Plug 'python-mode/python-mode', { 'branch': 'develop' }

" Rust
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'

" Status
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Syntastic Check
" Plug 'vim-syntastic/syntastic'

" Initialize plugin system
call plug#end()

""""""""""""""""
" My Config
""""""""""""""""
" System Config
set autoread
set hidden
set tabstop=2  " Show existing tab with 2 spaces width
set shiftwidth=2  " When indenting with '>', use 2 spaces width
set expandtab  " On pressing tab, insert 2 spaces
set number
set relativenumber

" Auto Complete
let g:deoplete#enable_at_startup = 1

" Make
" When writing a buffer (no delay).
call neomake#configure#automake('w')

" Shortcut
let mapleader = ","
nmap <leader>f :FZF<CR>
nmap <leader>t :NERDTree<CR>
nmap <leader>bd :lclose<bar>b#<bar>bd #<CR>
nmap <C-o> :b#<CR>

" Python
let g:pymode_python = 'python3'
au FileType python nmap <leader>= :PymodeLintAuto<CR>

" Rust
let g:rustfmt_autosave = 1
let g:racer_experimental_completer = 1
au FileType rust nmap gd <Plug>(rust-def)
au FileType rust nmap gs <Plug>(rust-def-split)
au FileType rust nmap gx <Plug>(rust-def-vertical)
au FileType rust nmap <leader>gd <Plug>(rust-doc)

" Status
let g:airline#extensions#tabline#enabled = 1
