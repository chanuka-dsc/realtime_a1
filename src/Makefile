all: build

build: cyclic prot_q

cyclic: cyclic.adb
	gnatmake realtime_a1.adb

prot_q: realtime_a4.adb
	gnatmake realtime_a4.adb

run: cyclic
	./realtime_a1

clean:
	rm -f *.o *.ali realtime_a1 realtime_a4