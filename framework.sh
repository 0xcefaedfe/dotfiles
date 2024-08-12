#!/usr/bin/bash

# things to run on debian on a framework computer
# https://wiki.debian.org/InstallingDebianOn/FrameWork/Laptop13/AMD_7040_Series

# firmware updates
sudo apt install fwupd
sudo fwupdmgr refresh
sudo fwupdmgr update

# amd gpu
cd ~/Downloads/
wget https://gitlab.com/kernel-firmware/linux-firmware/-/archive/main/linux-firmware-main.zip?path=amdgpu -O amdgpu.zip
unzip amdgpu.zip
cd linux-firmware-main-amdgpu/amdgpu/ && sudo cp gc_11_0_1*.bin dcn_3_1_4_dmcub.bin psp_13_0_4*.bin sdma_6_0_1.bin vcn_4_0_2.bin /lib/firmware/amdgpu/

