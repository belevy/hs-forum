.PHONY: build dev

build: dist/bundle.js dist/style.css

node_modules: package.json package-lock.json
	@npm i
	@touch $@

.spago: packages.dhall spago.dhall node_modules
	@npm run spago:install
	@touch $@

output: .spago $(wildcard purs/src/**/*)
	@npm run spago:build 
	@touch $@

dist/index.js: output
	@npm run spago:bundle
	@touch $@

dist/bundle.js: dist/index.js $(wildcard ts/**/*)
	@npm run bundle:prod
	@touch $@

dist/style.css: $(wildcard styles/**/*) node_modules 
	@npm run sass:build 
	@touch $@

dev: output dist/style.css $(wildcard ts/**/*)
	@npm run bundle:dev
	@touch dist/bundle.js

clean:
	@rm -rf dist/*

nuke: clean
	@rm -rf .spago
	@rm -rf node_modules
