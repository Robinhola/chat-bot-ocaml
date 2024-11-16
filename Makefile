build:
	cd chat; dune build

watch:
	cd chat; dune build -w

run: build
	cd chat; dune exec chat

test: build
	cd chat; dune runtest --auto-promote

format:
	cd chat; dune fmt

fmt: format

client:
	./chat/_build/default/bin/main.exe client

server:
	./chat/_build/default/bin/main.exe server
