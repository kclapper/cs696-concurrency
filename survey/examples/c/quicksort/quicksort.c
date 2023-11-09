#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>

struct Args {
    char *array;
    int low;
    int high;
};

struct Args* makeArgs(char* array, int low, int high) {
    struct Args *args = malloc(sizeof(struct Args));
    args->array = array;
    args->low = low;
    args->high = high;
    return args;
}

int partition(char *array, int low, int high) {
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

void *sort(void *argStruct) {
    struct Args *args = (struct Args *)argStruct;

    char *array = args->array;
    int low = args->low;
    int high = args->high;

    if (high <= low) {
        return NULL;
    }

    int pivot = partition(array, low, high);

    //printf("(left, %i, %i, %i) ", low, pivot, high);
    struct Args *leftArgs = makeArgs(array, low, pivot - 1);
    sort(leftArgs);
    free(leftArgs);

    //printf("(right, %i, %i, %i)\n", low, pivot, high);
    struct Args *rightArgs = makeArgs(array, pivot + 1, high);
    sort(rightArgs);
    free(rightArgs);

    return NULL;
}

void parallel(char* array, int low, int high) {

    pthread_t left;
    //pthread_t right = NULL;

    pthread_create( &left, NULL, sort, makeArgs(array, low, high) );
    //pthread_create( &right, NULL, sort, makeArgs(array, pivot + 1, high) );

    pthread_join(left, NULL);
    //pthread_join(right, NULL);
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
