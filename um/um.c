#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Google C++ Style Guide : 
    http://google-styleguide.googlecode.com/svn/trunk/cppguide.xml
*/

/*
  platter : unsigned 32-bit numbers - unsigned int, uint32_t
  input, output : unsigned char
  execution finger : ip
*/

/*
                          A     C
                          |     |
                          vvv   vvv
  .--------------------------------.
  |VUTSRQPONMLKJIHGFEDCBA9876543210|
  `--------------------------------'
   ^^^^                      ^^^
   |                         |
   operator number           B
*/

#define OP_MASK = 0xF0000000
#define RA_MASK = 0x000001c0
#define RB_MASK = 0x00000038
#define RC_MASK = 0x00000007

void OpenFileOrDie(char *fname)
{
  FILE *file = fopen(fname, "r");

  unsigned char ch = 0;

  if (file == NULL) {
    printf("Error reading program file.\n");
    exit(-1);
  }

  unsigned int new_platter = 0;
  while ((ch = getc(file)) != EOF) {
    new_platter |=  (ch << 24);
    if ((ch = getc(file)) != EOF) {
      new_platter |=  (ch << 16);
      if ((ch = getc(file)) != EOF) {
        new_platter |=  (ch << 8);
        if ((ch = getc(file)) != EOF) {
          new_platter |=  ch;
        }
      }
    }
    printf("%u\n", new_platter);
    /*break;*/
  }

  fclose(file);

  printf("Program loaded.\n");
}

void spincycle() {
  return;
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Usage: um <program name>\n");
    exit(-1);
  }

  unsigned int reg[8] = {0, 0, 0, 0, 0, 0, 0, 0};
  unsigned int coll[100];
  unsigned int ip = 0;

  /*
  When reading programs from legacy "unsigned 8-bit character"
  scrolls, a series of four bytes A,B,C,D should be interpreted with
  'A' as the most magnificent byte, and 'D' as the most shoddy, with
  'B' and 'C' considered lovely and mediocre respectively.
  */

  OpenFileOrDie(argv[1]);

  exit(0);
}