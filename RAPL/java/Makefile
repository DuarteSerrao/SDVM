NTIMES=10

export NTIMES

compile:
	javac QuickSort.c

run:
	java QuickSort

measure: 
	sudo ../RAPL/main "java QuickSort" $(NTIMES)


mem: 
	{ /usr/bin/time -v java QuickSort > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"

clean:
	rm -rf QuickSort *.o