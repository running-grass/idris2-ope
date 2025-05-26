default: build

.PHONY: build clean check run dev

build: check
	pack build ope.ipkg

clean:
	pack clean ope.ipkg && pack clean sample.ipkg

check:
	pack typecheck ope.ipkg && pack typecheck sample.ipkg && echo "\n\nCheck done\n\n"

run:
	pack run sample.ipkg

watch:
	find src | entr make check

hot:
	find src | entr -r make run