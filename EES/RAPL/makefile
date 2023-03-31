NTIMES=10

export NTIMES

compile:
	cd RAPL/ && $(MAKE) 
	cd ..
	cd C/ && $(MAKE) compile
	cd ..
	cd java/ && $(MAKE) compile
	cd ..

measure:
	cd C/ && $(MAKE) measure
	cd ..
	cd java/ && $(MAKE) measure
	cd ..
	"Program, Package, Core(s), Time (milisec) " >> energy.csv
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
