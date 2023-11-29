all: build run clean

run: quicksort
	./quicksort

build: main.c quicksort.c
	gcc -o quicksort main.c quicksort.c

clean:
	rm quicksort
