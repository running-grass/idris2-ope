default: build

.PHONY: build clean check run dev

build: check
	pack build sample.ipkg

clean:
	pack clean pact-api && pack clean pact-wai && pack clean pact-server && pack clean pact-sample

check:
	pack typecheck pact-sample

run:
	pack run pact-sample

watch:
	find src | entr make check

hot:
	find src | entr -r make run