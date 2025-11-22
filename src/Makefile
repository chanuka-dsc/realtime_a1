all: build

build: cyclic

cyclic: cyclic.adb
	gnatmake cyclic.adb

run: cyclic
	./cyclic

clean:
	rm *.o *.ali cyclic