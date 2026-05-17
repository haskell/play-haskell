#!/bin/bash
#set -e
sed -i '/^_apt:/d' /etc/passwd  # See https://github.com/containers/bubblewrap/issues/210

apt update
apt install -y dialog  # apt needs this to show stuff, so let's install it first

# These two files are chown'd by dpkg during installation, and chown is not supported within bubblewrap. Fortunately we don't care about these files so let's just skip them.
# See also: https://github.com/containers/bubblewrap/issues/395
apt upgrade -y -o Dpkg::Options::="--path-exclude=/usr/sbin/pam_extrausers_chkpwd" -o Dpkg::Options::="--path-exclude=/usr/sbin/unix_chkpwd"

sed -i '/^_apt:/d' /etc/passwd

apt install -y build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libtinfo6 locales zlib1g-dev gawk

# GHC LLVM 5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
# 8.4.4    x                                             
# 8.6.5       x                                          
# 8.8.4          x                                       
# 8.10.7               x  x  x  x                        
# 9.0.2                x  x  x  x                        
# 9.2.8                x  x  x  x                        
# 9.4.8                   x  x  x  x  x                  
# 9.6.7                      x  x  x  x  x               
# 9.8.4                      x  x  x  x  x               
# 9.10.3                           x  x  x               
# 9.12.2                           x  x  x  x  x  x  x   
# 9.14.1                           x  x  x  x  x  x  x  x

apt install -y llvm-15 clang-15

PATH="$PATH:/usr/sbin" dpkg-reconfigure locales
