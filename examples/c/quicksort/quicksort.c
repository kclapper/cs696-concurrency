#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>

struct Args {
    char *array;
    int low;
    int high;
};

void swap(char *array, int l, int r) {
	int temp = array[r];
	array[r] = array[l];
	array[l] = temp;
}

int partition(char *array, int low, int high) {
    char pivot = array[low];
    int lo = low;
    int hi = high + 1;

    while (1) {
        while (array[++lo] < pivot);
        while (array[--hi] > pivot);

        if (hi <= lo) {
            break;
        }

	swap(array, lo, hi);
    }

    swap(array, low, hi);

    return hi;
}

typedef pthread_mutex_t mutex_t;

mutex_t lock;

void *sort(void *argStruct) {
    struct Args *args = (struct Args *)argStruct;

    char *array = args->array;
    int low = args->low;
    int high = args->high;

    if (low >= high) {
	return NULL;
    }

    int pivot = partition(array, low, high);

    struct Args leftArgs; 
    leftArgs.array = array;
    leftArgs.low = low;
    leftArgs.high = pivot - 1;
    sort(&leftArgs);

    struct Args rightArgs; 
    rightArgs.array = array;
    rightArgs.low = pivot + 1;
    rightArgs.high = high;
    sort(&rightArgs);

    return NULL;
}

void parallel(char* array, int low, int high) {

    if (pthread_mutex_init(&lock, NULL) != 0) {
	return;
    }

    int pivot = partition(array, low, high);

    pthread_t left;
    struct Args leftArgs;
    leftArgs.array = array;
    leftArgs.low = low;
    leftArgs.high = pivot - 1;
    pthread_create( &left, NULL, sort, &leftArgs );
    
    pthread_t right;
    struct Args rightArgs;
    rightArgs.array = array;
    rightArgs.low = pivot + 1;
    rightArgs.high = high;
    pthread_create( &right, NULL, sort, &rightArgs );

    pthread_join(left, NULL);
    pthread_join(right, NULL);
}
