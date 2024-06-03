#!/bin/bash

sudo apt update
sudo apt upgrade -y
sudo apt install -y git

mkdir -p ~/src
cd ~/src

git clone https://github.com/0xcefaedfe/dotfiles
cd dotfiles

sudo cp keyboard /etc/default/keyboard
sudo cp console-setup /etc/default/console-setup
cp Xresources ~/.Xresources
exit 0

sudo apt install -y console-setup unzip git neovim firefox-esr i3 dmenu fonts-roboto wget alacritty

wget -P ~/.local/share/fonts https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/RobotoMono.zip
cd ~/.local/share/fonts
unzip RobotoMono.zip
rm RobotoMono.zip
fc-cache -fv

sudo timedatectl set-timezone Europe/Amsterdam

wget -c https://ftpmirror.gnu.org/emacs/emacs-29.3.tar.gz
sudo apt build-dep emacs -y
tar -xzf emacs-29.3.tar.gz
cd emacs-29.3
./configure --with-native-compilation=aot --with-tree-sitter --with-gif --with-png --with-jpeg --with-rsvg --with-tiff --with-imagemagick --with-x-toolkit=lucid --with-json --with-mailutils
make clean
make -j8

rm -rf ~/src/emacs-29.3

cd ~
