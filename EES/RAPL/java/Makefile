NTIMES=10

export NTIMES

compile:
	javac QuickSort.java
	jar -cvmf Quick/manifest.txt quick.jar QuickSort.class
	echo '#!/usr/bin/java -jar' > quick
	cat quick.jar >> quick
	chmod +x quick
	javac BubbleSort.java
	jar -cvmf Bubble/manifest.txt bubble.jar BubbleSort.class
	echo '#!/usr/bin/java -jar' > bubble
	cat bubble.jar >> bubble
	chmod +x bubble
	javac InsertionSort.java
	jar -cvmf Insertion/manifest.txt insertion.jar InsertionSort.class
	echo '#!/usr/bin/java -jar' > insertion
	cat insertion.jar >> insertion
	chmod +x insertion
	rm -rf QuickSort.class BubbleSort.classInsertionSort.class
	rm -rf quick.jar bubble.jar insertion.jar

run:
	./quick
	./bubble
	./insertion
	
measure: 
	sudo ../RAPL/main "Java" "quick" $(NTIMES)
	sudo ../RAPL/main "Java" "bubble" $(NTIMES)
	sudo ../RAPL/main "Java" "insertion" $(NTIMES)

mem: 
	{ /usr/bin/time -v quick > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"
	{ /usr/bin/time -v bubble > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"
	{ /usr/bin/time -v insertion > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"

clean:
	rm -rf quick.J bubble.J insertion.J
	rm -rf quick bubble insertion
