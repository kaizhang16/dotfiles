# Environment Variables
set -x GOPATH ~/apps
set -x PATH ~/.local/bin $GOPATH/bin ~/.config/yarn/global/node_modules/.bin $PATH
set -x EDITOR vim
set -x LANG en_US.UTF-8
set -x LC_CTYPE zh_CN.UTF-8     # 中文处理
set -x LC_COLLATE zh_CN.UTF-8   # 中文排序
set -x TZ Asia/Shanghai
set -x XDG_CONFIG_HOME $HOME/.config
set -x RUST_SRC_PATH (rustc --print sysroot)/lib/rustlib/src/rust/src

# Alias
alias tmux "tmux -2"
alias zz "z (cat ~/.local/share/z/data | sort -t'|' -k2,2 -nr | awk -F'|' '{print \$1}' | fzf +s)"


# Start X at login
if status --is-login
  if test -z "$DISPLAY" -a "$XDG_VTNR" = 1
    exec startx -- -keeptty
  end
end
