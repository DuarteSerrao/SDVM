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

run:
	./quick
	./bubble
	
measure: 
	sudo ../RAPL/main "quick" $(NTIMES)
	sudo ../RAPL/main "bubble" $(NTIMES)

mem: 
	{ /usr/bin/time -v quick > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"
	{ /usr/bin/time -v bubble > /dev/null; } 2>&1 | grep "Maximum resident" #| sed "s/[^0-9]\+\([0-9]\+\).*/\1/"

clean:
	rm -rf QuickSort.class
	rm -rf BubbleSort.class
	rm -rf quick.jar
	rm -rf bubble.jar
	rm -rf quick.J
	rm -rf bubble.J
	rm -rf quick
	rm -rf bubble
