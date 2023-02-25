NTIMES=10

export NTIMES

compile:
	javac QuickSort.c
	javac BubbleSort.c

run:
	java QuickSort
	java BubbleSort

measure: 
	sudo ../RAPL/main "java QuickSort" $(NTIMES)
	sudo ../RAPL/main "java BubbleSort" $(NTIMES)

mem: 
	{ /usr/bin/time -v java QuickSort > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"
	{ /usr/bin/time -v java BubbleSort > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"

clean:
	rm -rf QuickSort *.o
	rm -rf BubbleSort *.o