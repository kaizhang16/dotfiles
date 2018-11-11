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

" Buffer
Plug 'lotabout/skim', { 'dir': '~/.skim', 'do': './install' }

" Make
Plug 'neomake/neomake'

" Plug 'fatih/vim-go'

" Directory
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'Xuyuanp/nerdtree-git-plugin'

" Python
Plug 'python-mode/python-mode', { 'branch': 'develop' }

" Rust
Plug 'rust-lang/rust.vim'

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
" Auto Complete
let g:deoplete#enable_at_startup = 1

" Make
" When writing a buffer (no delay).
call neomake#configure#automake('w')

" Shortcut
let mapleader = ","
nmap <leader>t :NERDTree<CR>
nmap <leader>= :PymodeLintAuto<CR>
nmap <leader>bd :lclose<bar>b#<bar>bd #<CR>
nmap <C-o> :b#<CR>
set hidden

" Python
let g:pymode_python = 'python3'

" Rust
let g:rustfmt_autosave = 1

" Status
set number
set relativenumber
