.PHONY: all frontend frontend-dependencies reload-pages

# These are not rule dependencies because we want to run them sequentially,
# even if -j is in effect: frontend-dependencies needs to run to completion
# before frontend works.
all:
	$(MAKE) frontend-dependencies
	$(MAKE) frontend

frontend:
	$(MAKE) -C static/

frontend-dependencies:
	$(MAKE) -C static/ dependencies

reload-pages:
	kill -s USR1 $$(ps -o pid,cmd -ww a -e | grep '^ *[0-9]* *[^ ]*/pastebin-haskell\b' | awk '{print $$1}')
