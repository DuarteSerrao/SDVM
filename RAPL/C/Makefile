NTIMES=10

export NTIMES

compile:
	gcc -O2 -o quick quicksort.c

run:
	./quick

measure: 
	sudo ../RAPL/main "quick" $(NTIMES)


mem: 
	{ /usr/bin/time -v ./quick > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"

clean:
	rm -rf quick *.o