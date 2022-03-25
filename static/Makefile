all: bundle.js

play-index.js: play-index.ts
	tsc play-index.ts

bundle.js: play-index.js
	browserify play-index.js > bundle.js

dependencies:
	npm install
	npm install -g browserify

clean:
	rm bundle.js play-index.js

.PHONY: clean dependencies
