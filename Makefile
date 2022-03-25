all:
	kill -s USR1 $$(ps -o pid,cmd -ww a | grep '/pastebin-haskell$$' | awk '{print $$1}')

frontend:
	$(MAKE) -C static/

frontend-dependencies:
	$(MAKE) -C static/ dependencies

.PHONY: frontend frontend-dependencies
