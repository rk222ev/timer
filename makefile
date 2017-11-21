SHELL = /bin/sh

TARGET = timer

build:
	buildapp --load timer.lisp --entry timer:main --output $(TARGET)

clean:
	rm ./$(TARGET)
