SHELL = /bin/sh

TARGET = timer

build:
	buildapp --load packages.lisp --load timer.lisp --entry timer:main --output $(TARGET)

clean:
	rm ./$(TARGET)
