all: build run

run: quicksort input.txt
	@./quicksort
	@./quicksort
	@./quicksort
	@./quicksort

build: build-four

build-single: main.c quicksort.c
	gcc -o quicksort -DMAX_THREADS=1  main.c quicksort.c

build-two: main.c quicksort.c
	gcc -o quicksort -DMAX_THREADS=2  main.c quicksort.c

build-four: main.c quicksort.c
	gcc -o quicksort -DMAX_THREADS=4  main.c quicksort.c

setup: input.rkt
	racket input.rkt

clean:
	rm quicksort
