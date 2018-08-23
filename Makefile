all: test

test:
	@echo 'Hello, World!'

dev:
	@chokidar '{{src,example}/**/*.elm,example/*.html}' | make build

build:
	@rm -rf dist
	@mkdir dist
	@cp example/index.html dist
	@cd example && elm make Example.elm --output=../dist/out.js
