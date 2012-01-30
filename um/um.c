#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[])
{
  if (argc != 2) {
    printf("Usage: um <program name>\n");
    exit(-1);
  }
  exit(0);
}