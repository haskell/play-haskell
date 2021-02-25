all:
	kill -s USR1 $$(ps -o pid,cmd -ww a | grep '/pastebin-haskell$$' | awk '{print $$1}')
