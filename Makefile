default: build

.PHONY: build clean check run dev

build:
	pack build pact-api && pack build pact-wai && pack build pact-server

clean:
	pack clean pact-api && pack clean pact-wai && pack clean pact-server && pack clean pact-todomvc

check:
	pack typecheck pact-todomvc

run:
	pack run pact-todomvc

build-todomvc:
	pack build pact-todomvc

run-todomvc: build-todomvc
	./todomvc/build/exec/pact-todomvc

watch:
	find src | entr make check

hot:
	find src | entr -r make run