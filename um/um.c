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

#define CODE_BUF_SIZE 40000000

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
#define OP_MASK 0xF0000000
#define RA_MASK 0x000001c0
#define RB_MASK 0x00000038
#define RC_MASK 0x00000007

#define COND_MOVE 0x00000000
#define ARR_INDEX 0x10000000
#define STORE     0x20000000
#define ADD       0x30000000
#define MULT      0x40000000
#define DIV       0x50000000
#define NAND      0x60000000
#define HALT      0x70000000
#define ALLOC     0x80000000
#define FREE      0x90000000
#define OUT       0xA0000000
#define IN        0xB0000000
#define JUMP      0xC0000000
#define IMM       0xD0000000


unsigned int reg[8] = {1, 1, 1, 1, 1, 1, 1, 1};

void printRegister() {
	printf("[ %X %X %X %X %X %X %X %X ]", reg[0], reg[1], reg[2], reg[3], reg[4], reg[5], reg[6], reg[7]);
}

/*
  When reading programs from legacy "unsigned 8-bit character"
  scrolls, a series of four bytes A,B,C,D should be interpreted with
  'A' as the most magnificent byte, and 'D' as the most shoddy, with
  'B' and 'C' considered lovely and mediocre respectively.
*/
unsigned int * OpenFileOrDie(char *fname)
{
  FILE *file = fopen(fname, "r");

  if (file == NULL) {
    printf("Error reading program file.\n");
    exit(-1);
  }

  unsigned int * platter_array = (unsigned int *) malloc(CODE_BUF_SIZE * sizeof(unsigned int));

  int ch = 0;
  unsigned int new_platter = 0;
  int i = 0;
  while ((ch = getc(file)) != EOF) {
    new_platter = (ch << 24);
    if ((ch = getc(file)) != EOF) {
      new_platter |= (ch << 16);
      if ((ch = getc(file)) != EOF) {
        new_platter |= (ch << 8);
        if ((ch = getc(file)) != EOF) {
          new_platter |= ch;
        } else {
          exit(-1);
        }
      } else {
        exit(-1);
      }
    } else {
      exit(-1);
    }
    platter_array[i] = new_platter;
    i++;
  }

  fclose(file);

  printf("Program loaded.\n");

  return platter_array;
}

void spincycle(unsigned int instruction) {
  int a = instruction & RA_MASK >> 6;
  int b = instruction & RB_MASK >> 3;
  int c = instruction & RC_MASK;
  int in;
  
  printRegister();
  switch(instruction & OP_MASK) {
    case COND_MOVE:
      if(reg[c] != 0) {
      	reg[a] = reg[b];
      }
    break;
  	
  	case ADD:
  	  reg[a] = reg[b] + reg[c];
  	break;

  	case MULT:
      reg[a] = reg[b] * reg[c];
  	break;
 
    case DIV:
      reg[a] = reg[b] / reg[c];
    break;

    case NAND:
      reg[a] = ~(reg[b] & reg[c]);
    break;

    case HALT:
      exit(-1);
    break;

    case OUT:
      printf(">> %d", reg[c]);
    break;

    case IN:
      scanf("%d", &in);
      reg[c] = in;
    break;
  }
  printRegister();
  return;
}


int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Usage: um <program name>\n");
    exit(-1);
  }

  unsigned int ip = 0;
  int pc = 0;

  unsigned int * coll = OpenFileOrDie(argv[1]);

  printf("%X", reg[1]);
  spincycle(IN | 0x312);

//  while(1) {
//  	spincycle(coll[pc]);
//  	pc++;
//  }
//  printf(coll);

  exit(0);
}