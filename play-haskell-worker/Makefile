.PHONY: all chroot builders

all: chroot bwrap-files/systemd-run-shim bwrap-files/systemd-kill-shim builders

chroot: bwrap-files/ubuntu-base
	bwrap --bind bwrap-files/ubuntu-base / --ro-bind /etc/resolv.conf /etc/resolv.conf --tmpfs /tmp --dev /dev --proc /proc --new-session --unshare-all --share-net --die-with-parent --gid 0 --uid 0 --chdir / --ro-bind bwrap-files/chroot-initialise.sh /tmp/chinit.sh /bin/bash /tmp/chinit.sh

builders:
	bwrap-files/mkallbuildscripts.sh

bwrap-files/ubuntu-base:
	mkdir '$@'
	curl -L 'http://cdimage.ubuntu.com/ubuntu-base/releases/20.04/release/ubuntu-base-20.04.1-base-amd64.tar.gz' | tar -C '$@' -xz

bwrap-files/systemd-run-shim: bwrap-files/systemd-run-shim.c
	gcc -Wall -Wextra -std=c11 -O2 -o $@ $<
	@echo "Will now mark systemd-run-shim as setuid root:"
	sudo chown root:root $@ && sudo chmod +s $@ || rm $@

bwrap-files/systemd-kill-shim: bwrap-files/systemd-kill-shim.c
	gcc -Wall -Wextra -std=c11 -O2 -o $@ $<
	@echo "Will now mark systemd-kill-shim as setuid root:"
	sudo chown root:root $@ && sudo chmod +s $@ || rm $@
