# CS696 Independent Study - Concurrency 

This repository contains the result of my Fall 2023 independent
study with Stephen Chang focusing on concurrent programming. Here
you will find a written report of what I've learned and several
projects involving concurrent programming.

## Repository Contents

### Writeup
In the `writeup` folder you'll find the LaTeX source for a report
detailing the things I've learned this semester. 

### C Quicksort
In the `c-quicksort` folder you'll find a parallel implementation of
quicksort written in C. It uses POSIX threads and is an example of 
the shared memory concurrency model. This program reads in a list of
random letters and prints them sorted alphabetically. Of note, even
on large inputs (GB), this program does not run faster than a single
threaded implementation. This is largely due to the overhead of thread
creation. A better implementation may reduce the number of thread
creations and deletions to achieve faster performance. On a slower
computer the parallel implementation **may** be quicker. 

### Tic Tac Toe
In the `tic-tac-toe` folder you'll find a concurrent implementation
of a Tic-Tac-Toe game server written in Erlang. This game server requires
Erlang clients but may be used either by communicating Erlang processes on
the same or different machines, or over the network.

### Miniprojects
In the `miniproject_*` folders you'll find solutions to the mini projects
offered as part of the _Concurrent Programming in Java_ Coursera course.
I went through this course's material during the semester, you can find it online
[here](https://www.coursera.org/learn/concurrent-programming-in-java/home/week/1).

