NTIMES=10
ARRAY = 2 5 1 9 0
export INPUTSIZE
export NTIMES

compile:
	gcc -O2 -o quick quicksort.c

run:
	./quick $(ARRAY)

measure: 
	sudo ../RAPL/main "quick $(ARRAY)" $(NTIMES)


mem: 
	{ /usr/bin/time -v ./quick $(ARRAY) > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"

clean:
	rm -rf quick *.o