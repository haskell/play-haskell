all: play-index.js

play-index.js: play-index.ts
	npx tsc --lib dom,es2015 play-index.ts

dependencies:
	npm install
	git submodule update --init --recursive

clean:
	rm -f play-index.js

.PHONY: clean dependencies
