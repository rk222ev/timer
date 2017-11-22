SHELL = /bin/sh

TARGET = timer

all:manifest build

build:
	buildapp \
		--manifest-file quicklisp-manifest.txt \
		--load-system cl-ppcre \
		--load-system timer \
		--load packages.lisp \
		--load timer.lisp \
		--entry timer:main \
		--output $(TARGET)

clean:
	rm ./$(TARGET) ./quicklisp-manifest.txt


manifest:
	@sbcl --no-userinit --no-sysinit --non-interactive \
    --load ~/quicklisp/setup.lisp \
    --eval '(ql:write-asdf-manifest-file "quicklisp-manifest.txt")' \

.PHONY: manifest
