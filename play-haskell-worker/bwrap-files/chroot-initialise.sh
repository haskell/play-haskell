#!/bin/bash
#set -e
sed -i '/^_apt:/d' /etc/passwd  # See https://github.com/containers/bubblewrap/issues/210
apt update
apt install -y dialog  # apt needs this to show stuff, so let's install it first
apt upgrade -y
sed -i '/^_apt:/d' /etc/passwd
apt install -y build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 locales zlib1g-dev gawk
PATH="$PATH:/usr/sbin" dpkg-reconfigure locales
