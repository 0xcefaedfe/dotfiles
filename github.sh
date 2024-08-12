#!/usr/bin/bash

# Generate ssh key
ssh-keygen -t ed25519
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519

# GitHub
sudo apt install -y gh
gh auth login
gh auth refresh -h github.com -s admin:public_key
gh ssh-key add ~/.ssh/id_ed25519.pub

