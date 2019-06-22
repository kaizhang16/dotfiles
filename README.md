# dotfiles

[![Build Status](https://travis-ci.org/kaizhang91/dotfiles.svg?branch=master)](https://travis-ci.org/kaizhang91/dotfiles)

## Deploy

```
VERSION=2.2.1
OS=linux  # or OS=macos
curl -L https://github.com/kaizhang91/dotfiles/releases/download/${VERSION}/dotfiles-${OS} -o dotfiles-bin
chmod u+x dotfiles-bin

git clone https://github.com/kaizhang91/dotfiles.git
./dotfiles-bin dotfiles/templates/
```
