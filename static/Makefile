all: bundle.js

play-index.js: play-index.ts
	npx tsc --lib dom,es2015 play-index.ts

bundle.js: play-index.js
	npx browserify play-index.js > bundle.js

dependencies:
	npm install

clean:
	rm -f bundle.js play-index.js

.PHONY: clean dependencies
