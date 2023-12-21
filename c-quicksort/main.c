#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <quicksort.h>

/*
** A parallel implementation of a program that
** takes in an array of letters from the command
** line, sorts them, then prints the result out.
*/

/*
** Print a character array to stdout.
*/
void printChars(char* chars, int length) {
    for (int i = 0; i < length; i++) {
        printf("%c", chars[i]);
    }
    printf("\n");
    return;
}

/*
** Read stdin and return an array of input letters.
*/
char* readStdin(int argc, char *argv[]) {
    char *letters = malloc(argc * sizeof(char));
    for (int i = 0; i < argc; i++) {
        letters[i] = *argv[i];
    }
    return letters;
}

/*
** Max number of chars to read from input file.
*/
const int size = 250000;

/*
** Read characters from input file.
*/
char* readFile(char *path) {
    FILE *file = fopen(path, "r");

    char *buff = malloc(sizeof(char[size]));
    fgets(buff, size, file);

    fclose(file);

    return buff;
}

int main(int argc, char *argv[]) {
    char *letters = readFile("input.txt");
    
    clock_t start, end;

    start = clock();
    parallel(letters, 0, size - 1);
    end = clock();

    printf("Sort took %f seconds using %i thread(s)\n", ((double) end - start) / CLOCKS_PER_SEC, MAX_THREADS);

    //printChars(letters, size - 1);

    return 0;
}
