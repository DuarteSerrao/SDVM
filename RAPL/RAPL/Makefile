

main : main.c rapl.o
	gcc -O2  -o main main.c rapl.o -lm
	chmod +x main

rapl.o : rapl.c rapl.h
	gcc -O2  -c rapl.c  -lm

clean:
	rm -rf main.o rapl.o
