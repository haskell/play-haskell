.PHONY: all chroot frontend frontend-dependencies reload-pages

# These are not rule dependencies because we want to run them sequentially,
# even if -j is in effect: chroot is interactive, and frontend-dependencies
# needs to be run before frontend.
all:
	$(MAKE) chroot
	$(MAKE) frontend-dependencies
	$(MAKE) frontend

chroot: bwrap-files/ubuntu-base
	bwrap --bind bwrap-files/ubuntu-base / --ro-bind /etc/resolv.conf /etc/resolv.conf --tmpfs /tmp --dev /dev --proc /proc --new-session --unshare-all --share-net --die-with-parent --gid 0 --uid 0 --chdir / --ro-bind bwrap-files/chroot-initialise.sh /tmp/chinit.sh /bin/bash /tmp/chinit.sh

bwrap-files/ubuntu-base:
	mkdir '$@'
	curl -L 'http://cdimage.ubuntu.com/ubuntu-base/releases/20.04/release/ubuntu-base-20.04.1-base-amd64.tar.gz' | tar -C '$@' -xz

frontend:
	$(MAKE) -C static/

frontend-dependencies:
	$(MAKE) -C static/ dependencies

reload-pages:
	kill -s USR1 $$(ps -o pid,cmd -ww a | grep '/pastebin-haskell$$' | awk '{print $$1}')
