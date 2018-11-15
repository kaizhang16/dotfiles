# dotfiles

[![Build Status](https://travis-ci.org/kaizhang91/dotfiles.svg?branch=master)](https://travis-ci.org/kaizhang91/dotfiles)

## Deploy

```
git clone https://github.com/kaizhang91/dotfiles.git
cd dotfiles/
sudo pacman -S rustup
rustup install nightly
rustup default nightly
cargo build --release
./target/release/dotfiles templates/
```
