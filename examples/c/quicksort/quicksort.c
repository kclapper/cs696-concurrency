#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>

struct Args {
    char *array;
    int low;
    int high;
};

int partition(char *array, int low, int high) {
    if (low >= high) {
        return low;
    }

    char pivot = array[low];
    int lo = low + 1;
    int hi = high;

    char temp;
    while (1) {
        while (array[lo] - pivot <= 0 && lo < high) {
            lo++;
        }
        while (array[hi] - pivot >= 0 && hi > low) {
            hi--;
        }

        if (hi <= lo) {
            break;
        }

        temp = array[lo];
        array[lo] = array[hi];
        array[hi] = temp;
    }

    temp = array[hi];
    array[hi] = pivot;
    array[low] = temp;

    return hi;
}

const int MAX_THREADS = 4;
int THREAD_COUNT = 1;

void *sort(void *argStruct) {
    struct Args *args = (struct Args *)argStruct;
    char *array = args->array;
    int low = args->low;
    int high = args->high;

    int pivot = partition(array, low, high);

    // Parallel recursion
    pthread_t left = NULL;
    if (pivot - 1 > low) {
        struct Args leftArgs = { array, low, pivot - 1 };

        if (THREAD_COUNT < MAX_THREADS) {
            printf("Create thread %i\n", ++THREAD_COUNT);
            pthread_create( &left, NULL, sort, (void *)&leftArgs );

        } else {
            sort(&leftArgs);
        }
    }

    // Serial recursion
    if (pivot + 1 < high) {
        struct Args rightArgs = { array, pivot + 1, high };
        //args->low = pivot + 1;
        sort(&rightArgs);
    }

    // Parallel cleanup
    if (left != NULL) {
        pthread_join(left, NULL);
        printf("Delete thread %i\n", THREAD_COUNT--);
    }

    return 0;
}

void parallel(char* array, int low, int high) {
    struct Args args = { array, low, high };
    sort(&args);
}

void serial(char* array, int low, int high) {
    int pivot = partition(array, low, high);

    if (pivot - 1 > low) {
        serial(array, low, pivot - 1);
    }
    if (pivot + 1 < high) {
        serial(array, pivot + 1, high);
    }
}
