#include <stdio.h>
#include <stdlib.h>

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
    //char *letters = readStdin(argc - 1, &argv[1]);
    //parallel(letters, 0, argc - 2);
    //printChars(letters, argc - 1);

    char *letters = readFile("input.txt");
    parallel(letters, 0, size - 1);
    printChars(letters, size - 1);

    return 0;
}
