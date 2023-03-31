NTIMES=10
export NTIMES

compile:
	python3 bubblesort.py

run:
	./bubble
	./insertion
	./quick

measure: 
	sudo ../RAPL/main "C" "bubble" $(NTIMES)
	sudo ../RAPL/main "C" "insertion" $(NTIMES)
	sudo ../RAPL/main "C" "quick" $(NTIMES)


mem: 
	{ /usr/bin/time -v ./bubble > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"
	{ /usr/bin/time -v ./insertion > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"
	{ /usr/bin/time -v ./quick > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"

clean:
	rm -rf bubble insertion quick *.o