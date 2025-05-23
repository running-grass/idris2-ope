default: build

.PHONY: build clean check run dev

build: check
	pack build

clean:
	pack clean

check:
	pack typecheck && echo "\n\nCheck done\n\n"

run:
	pack run

watch:
	find src | entr make check

hot:
	find src | entr -r make run