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

" Auto Complete
set completeopt+=menuone,noselect
let g:mucomplete#enable_auto_at_startup = 1
let g:NERDSpaceDelims = 1

" Make
call neomake#configure#automake('w')  " When writing a buffer (no delay).

" Navigation
set foldmethod=syntax
hi CursorLine   cterm=NONE ctermbg=darkred ctermfg=white guibg=darkred guifg=white
hi CursorColumn cterm=NONE ctermbg=darkred ctermfg=white guibg=darkred guifg=white

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

" Shortcut
nmap <Leader>r :source ~/.config/nvim/init.vim<CR>
nmap <Leader>ff :FZF<CR>
nmap <Leader>fb :Buffers<CR>
nmap <Leader>t :NERDTree<CR>
nmap <Leader>bd :bp<bar>sp<bar>bn<bar>bd<CR>
nmap <C-o> :b#<CR>

" Status
let g:airline#extensions#tabline#enabled = 1

" Tab
set tabstop=4  " Show existing tab with 4 spaces width
set shiftwidth=4  " When indenting with '>', use 4 spaces width
set expandtab  " On pressing tab, insert spaces
autocmd FileType yaml setlocal tabstop=2 shiftwidth=2

" Theme
set number
set relativenumber
syntax enable
set background=dark
colorscheme solarized
set cursorline
set cursorcolumn
