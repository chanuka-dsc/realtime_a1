all: build

build: cyclic prot_q

cyclic: cyclic.adb
	gnatmake cyclic.adb

prot_q: realtime_a4.adb
	gnatmake realtime_a4.adb

run: cyclic
	./cyclic

clean:
	rm -f *.o *.ali cyclic realtime_a4