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

pthread_mutex_t lock;
int num_threads = 1;

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

    struct Args rightArgs; 
    rightArgs.array = array;
    rightArgs.low = pivot + 1;
    rightArgs.high = high;

    pthread_t left;
    pthread_t right;

    pthread_mutex_lock(&lock);
    if (num_threads < MAX_THREADS) {
	num_threads++;
	pthread_mutex_unlock(&lock);
    	pthread_create( &left, NULL, sort, &leftArgs );
    } else {
	pthread_mutex_unlock(&lock);
    	sort(&leftArgs);
    }
    
    pthread_mutex_lock(&lock);
    if (num_threads < MAX_THREADS) {
	num_threads++;
	pthread_mutex_unlock(&lock);
    	pthread_create( &right, NULL, sort, &rightArgs );
    } else {
	pthread_mutex_unlock(&lock);
    	sort(&rightArgs);
    }


    if (left != NULL) {
        pthread_join(left, NULL);
	//pthread_mutex_lock(&lock);
	//num_threads--;
	//pthread_mutex_unlock(&lock);
    }
    if (right != NULL) {
        pthread_join(right, NULL);
	//pthread_mutex_lock(&lock);
	//num_threads--;
	//pthread_mutex_unlock(&lock);
    }
    return NULL;
}

void parallel(char* array, int low, int high) {

    if (pthread_mutex_init(&lock, NULL) != 0) {
	return;
    }

    struct Args args;
    args.array = array;
    args.low = low;
    args.high = high;
    sort(&args);

    //int pivot = partition(array, low, high);

    //pthread_t left;
    //struct Args leftArgs;
    //leftArgs.array = array;
    //leftArgs.low = low;
    //leftArgs.high = pivot - 1;
    //pthread_create( &left, NULL, sort, &leftArgs );
    //
    //pthread_t right;
    //struct Args rightArgs;
    //rightArgs.array = array;
    //rightArgs.low = pivot + 1;
    //rightArgs.high = high;
    //pthread_create( &right, NULL, sort, &rightArgs );

    //pthread_join(left, NULL);
    //pthread_join(right, NULL);
}
