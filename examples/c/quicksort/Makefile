all: build run clean

run: quicksort
	./quicksort B B B B B B B B B B B B B B B B B B B A A A A A A A A A A A A A A A A

build: main.c quicksort.c
	gcc -o quicksort main.c quicksort.c

clean:
	rm quicksort
