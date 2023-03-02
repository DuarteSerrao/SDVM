
NTIMES=10

export NTIMES

compile:
	gcc -O2 -o quick quicksort.c
	gcc -O2 -o bubble bubblesort.c


run:
	./quick
	./bubble 

measure: 
	sudo ../RAPL/main "quick" $(NTIMES)
	sudo ../RAPL/main "bubble" $(NTIMES)



mem: 
	{ /usr/bin/time -v ./quick  > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"
	{ /usr/bin/time -v ./bubble  > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"

clean:
	rm -rf quick *.o
	rm -rf bubble *.o
	rm -rf quick.J
	rm -rf bubble.J
