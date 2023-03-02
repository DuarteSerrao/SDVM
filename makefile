NTIMES=10

export NTIMES

compile:
	cd C/ && $(MAKE) compile
	cd ..
	cd java/ && $(MAKE) compile
	cd ..

measure:
	sudo RAPL/main "C/quick" $(NTIMES)
	sudo RAPL/main "C/bubble" $(NTIMES)
	sudo RAPL/main "java/quick" $(NTIMES)
	sudo RAPL/main "java/bubble" $(NTIMES)
	cat C/quick.J >> energy.csv
	cat C/bubble.J >> energy.csv
	cat java/quick.J >> energy.csv
	cat java/bubble.J >> energy.csv


clean:
	cd C/ && $(MAKE) clean
	cd ..
	cd java/ && $(MAKE) clean
	cd ..
	rm -rf energy.csv