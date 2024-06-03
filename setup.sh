#!/bin/bash

# Create src directory in the home directory and change to it
mkdir -p ~/src
cd ~/src

# Update package list and install packages
sudo apt update
sudo apt install -y console-setup unzip git neovim firefox-esr i3 dmenu fonts-roboto wget alacritty

# Copy keyboard and console-setup files to /etc/default
sudo cp keyboard /etc/default/keyboard
sudo cp console-setup /etc/default/console-setup
cp Xresources ~/.Xresources

# Download and install RobotoMono Nerd Font
wget -P ~/.local/share/fonts https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/RobotoMono.zip
cd ~/.local/share/fonts
unzip RobotoMono.zip
rm RobotoMono.zip
fc-cache -fv

# Set timezone to Europe/Amsterdam
sudo timedatectl set-timezone Europe/Amsterdam


# Download Emacs source
wget -c https://ftpmirror.gnu.org/emacs/emacs-29.3.tar.gz

# Install build dependencies for Emacs
sudo apt build-dep emacs -y

# Unpack the downloaded Emacs source
tar -xzf emacs-29.3.tar.gz
cd emacs-29.3

# Configure, clean, and build Emacs
./configure --with-native-compilation=aot --with-tree-sitter --with-gif --with-png --with-jpeg --with-rsvg --with-tiff --with-imagemagick --with-x-toolkit=lucid --with-json --with-mailutils
make clean
make -j8

# Clean up
rm -rf ~/src/emacs-29.2
