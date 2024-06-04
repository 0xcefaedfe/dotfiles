#!/bin/bash

# Define a temporary directory
TMP_DIR="/tmp/setup_script"
mkdir -p $TMP_DIR

# Function to clean up temporary directory
cleanup() {
  echo "Cleaning up temporary directory..."
  rm -rf $TMP_DIR
}

# Set trap to clean up on EXIT signal
trap cleanup EXIT

# Update and upgrade the system
sudo apt update && sudo apt upgrade -y
if [ $? -ne 0 ]; then
  echo "Failed to update and upgrade the system"
  exit 1
fi

# Install git
sudo apt install -y git
if [ $? -ne 0 ]; then
  echo "Failed to install git"
  exit 1
fi

# Create src directory under /tmp and navigate to it
mkdir -p $TMP_DIR/src
cd $TMP_DIR/src
if [ $? -ne 0 ]; then
  echo "Failed to create and change to src directory under /tmp"
  exit 1
fi

# Clone the dotfiles repository
git clone https://github.com/0xcefaedfe/dotfiles
if [ $? -ne 0 ]; then
  echo "Failed to clone the dotfiles repository"
  exit 1
fi
cd dotfiles
if [ $? -ne 0 ]; then
  echo "Failed to change to dotfiles directory"
  exit 1
fi

# Copy keyboard and console-setup files
sudo cp keyboard /etc/default/keyboard
if [ $? -ne 0 ]; then
  echo "Failed to copy keyboard file"
  exit 1
fi

sudo cp console-setup /etc/default/console-setup
if [ $? -ne 0 ]; then
  echo "Failed to copy console-setup file"
  exit 1
fi

# Ensure .Xresources file is copied
cp Xresources ~/.Xresources
if [ $? -ne 0 ]; then
  echo "Failed to copy Xresources file"
  exit 1
fi

# Copy configs
mkdir -p ~/.config
cp $TMP_DIR/src/dotfiles/config/* ~/.config/ 
if [ $? -ne 0 ]; then
  echo "Failed to copy configs" 
  exit 1
fi

# Install the required packages
sudo apt install -y console-setup unzip git neovim firefox-esr i3 dmenu fonts-roboto wget alacritty libmagickwand-dev libjpeg-dev libpng-dev libtiff-dev libgif-dev libx11-dev libxpm-dev litree-sitter-dev
if [ $? -ne 0 ]; then
  echo "Failed to install required packages"
  exit 1
fi

# Check if Roboto Mono is installed and install it if not
if ! fc-list | grep -i "Roboto Mono" > /dev/null 2>&1; then
  mkdir -p ~/.local/share/fonts
  wget -P ~/.local/share/fonts https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/RobotoMono.zip
  if [ $? -ne 0 ]; then
    echo "Failed to download RobotoMono Nerd Font"
    exit 1
  fi

  cd ~/.local/share/fonts
  unzip RobotoMono.zip
  if [ $? -ne 0 ]; then
    echo "Failed to unzip RobotoMono Nerd Font"
    exit 1
  fi

  rm RobotoMono.zip
  fc-cache -fv
  if [ $? -ne 0 ]; then
    echo "Failed to refresh font cache"
    exit 1
  fi
else
  echo "Roboto Mono is already installed"
fi

# Set timezone to Europe/Amsterdam
sudo timedatectl set-timezone Europe/Amsterdam
if [ $? -ne 0 ]; then
  echo "Failed to set timezone to Europe/Amsterdam"
  exit 1
fi

# Download and build Emacs
wget -c https://ftpmirror.gnu.org/emacs/emacs-29.3.tar.gz
if [ $? -ne 0 ]; then
  echo "Failed to download Emacs source"
  exit 1
fi

sudo apt build-dep emacs -y
if [ $? -ne 0 ]; then
  echo "Failed to install build dependencies for Emacs"
  exit 1
fi

tar -xzf emacs-29.3.tar.gz
if [ $? -ne 0 ]; then
  echo "Failed to unpack Emacs source"
  exit 1
fi

cd emacs-29.3
./configure --with-native-compilation=aot --with-tree-sitter --with-gif --with-png --with-jpeg --with-rsvg --with-tiff --with-imagemagick --with-x-toolkit=lucid --with-json --with-mailutils
if [ $? -ne 0 ]; then
  echo "Failed to configure Emacs"
  exit 1
fi

make clean
make -j8
if [ $? -ne 0 ]; then
  echo "Failed to build Emacs"
  exit 1
fi

# Install Emacs
sudo make install

# Install Emacs config
cd ~/.config/emacs
if [ $? -ne 0 ]; then
  echo "Failed to change to emacs config source directory"
  exit 1
fi
make clean
make 
if [ $? -ne 0 ]; then
  echo "Failed to install Emacs config"
  exit 1
fi

# Log in to GitHub
sudo apt install -y gh
if [ $? -ne 0 ]; then
  echo "Failed to install gh"
  exit 1
fi

gh auth login

# Return to home directory
cd ~
if [ $? -ne 0 ]; then
  echo "Failed to change to home directory"
  exit 1
fi

echo "Job's done."

