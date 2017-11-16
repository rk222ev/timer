SHELL = /bin/sh

TARGET = timer

build: 
	buildapp --load timer.lisp --entry main --output $(TARGET)

clean:
	rm ./$(TARGET)
