# dotfiles

[![Build Status](https://travis-ci.org/kaizhang91/dotfiles.svg?branch=master)](https://travis-ci.org/kaizhang91/dotfiles)

## Deploy

```
VERSION=2.1.0
OS=linux  # or OS=macos
curl -L https://github.com/kaizhang91/dotfiles/releases/download/${VERSION}/dotfiles-${OS} -o dotfiles
chmod u+x dotfiles

git clone https://github.com/kaizhang91/dotfiles.git
./dotfiles dotfiles/templates/
```
