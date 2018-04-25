# dotfiles

[![Build Status](https://travis-ci.org/kaizhang91/dotfiles.svg?branch=master)](https://travis-ci.org/kaizhang91/dotfiles)

## Get released package

```
version=1.2.0
os=linux  # or darwin
curl -LO https://github.com/kaizhang91/dotfiles/releases/download/${version}/dotfiles-${os}.tar.xz
tar -xvJf dotfiles-${os}.tar.xz
```

## Run

```
dotfiles --help
```

## Compile

```
git clone https://github.com/kaizhang91/dotfiles.git
cd dotfiles/
stack install
```
