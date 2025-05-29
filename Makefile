default: build

.PHONY: build clean check run dev

build:
	pack build pact-api && pack build pact-wai && pack build pact-server

clean:
	pack clean pact-api && pack clean pact-wai && pack clean pact-server && pack clean pact-sample

check:
	pack typecheck pact-sample

run:
	pack run pact-sample

build-sample:
	pack build pact-sample

run-sample: build-sample
	./sample/build/exec/pact-sample

watch:
	find src | entr make check

hot:
	find src | entr -r make run