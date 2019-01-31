func! myspacevim#after() abort
  " Navigation
  set wrap
  set foldmethod=indent
  set foldlevel=99

  " Shortcuts
  " Find files
  nmap <Leader>ff :FZF ~/projects<CR>
  " Last buffer
  nmap <C-o> :b#<CR>
endf
