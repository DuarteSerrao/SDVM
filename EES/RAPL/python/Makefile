NTIMES=10
export NTIMES

compile:
	pyinstaller --onefile --console  "bubblesort.py"
	pyinstaller --onefile --console  "insertionsort.py"
	pyinstaller --onefile --console  "quicksort.py"
	rm -rf *.spec build
	mv dist/bubblesort.exe bubblesort.exe
	mv dist/insertionsort.exe insertionsort.exe
	mv dist/quicksort.exe quicksort.exe
	rm -rf dist


run:
	./dist/bubble.exe
	./insertion
	./quick

measure: 
	sudo ../RAPL/main "python" "bubble" $(NTIMES)
	sudo ../RAPL/main "python" "insertion" $(NTIMES)
	sudo ../RAPL/main "python" "quick" $(NTIMES)


mem: 
	{ /usr/bin/time -v ./bubble > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"
	{ /usr/bin/time -v ./insertion > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"
	{ /usr/bin/time -v ./quick > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"

clean:
	rm -rf bubble insertion quick *.o *.exe