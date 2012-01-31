#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Google C++ Style Guide : 
    http://google-styleguide.googlecode.com/svn/trunk/cppguide.xml
*/

#define CODE_BUF_SIZE 40000000
#define MEMO_BUF_SIZE 100000

/*
                          A     C
                          vvv   vvv
  .--------------------------------.
  |VUTSRQPONMLKJIHGFEDCBA9876543210|
  `--------------------------------'
   ^^^^                      ^^^
   operator number           B

       A  
       vvv
  .--------------------------------.
  |VUTSRQPONMLKJIHGFEDCBA9876543210|
  `--------------------------------'
   ^^^^   ^^^^^^^^^^^^^^^^^^^^^^^^^
   operator number      value
*/
#define OP_MASK 0xF0000000
#define RA_MASK 0x000001c0
#define RB_MASK 0x00000038
#define RC_MASK 0x00000007
#define OA_MASK 0x0E000000
#define OV_MASK 0x01FFFFFF

/*
#0. Conditional Move.
#1. Array Index.
#2. Array Amendment.
#3. Addition.
#4. Multiplication.
#5. Division.
#6. Not-And.
#7. Halt.
#8. Allocation.
#9. Abandonment.
#10. Output.
#11. Input.
#12. Load Program.
#13. Orthography.
*/
#define OP_IFM 0x00000000
#define OP_ARI 0x10000000
#define OP_ARA 0x20000000
#define OP_ADD 0x30000000
#define OP_MUL 0x40000000
#define OP_DIV 0x50000000
#define OP_NOT 0x60000000
#define OP_HLT 0x70000000
#define OP_ALC 0x80000000
#define OP_ABD 0x90000000
#define OP_OUT 0xA0000000
#define OP_INP 0xB0000000
#define OP_LDP 0xC0000000
#define OP_ORT 0xD0000000

typedef struct {
  unsigned int reg[8];
  unsigned int ip;
  unsigned int pp;
  unsigned int * collection[MEMO_BUF_SIZE];
  unsigned int pp_lb;
} UniversalMachine;

/*
  When reading programs from legacy "unsigned 8-bit character"
  scrolls, a series of four bytes A,B,C,D should be interpreted with
  'A' as the most magnificent byte, and 'D' as the most shoddy, with
  'B' and 'C' considered lovely and mediocre respectively.
*/
unsigned int * LoadPlatterArrayOrDie(char *fname) {
  unsigned int *pa;
  unsigned int new_platter = 0, ch = 0, i = 0;

  FILE *file = fopen(fname, "r");

  if (file == NULL) {
    printf("Error reading program file.\n");
    exit(-1);
  }

  pa = calloc(CODE_BUF_SIZE, sizeof(unsigned int));

  while ((ch = getc(file)) != EOF) {
    /* leave entry 0 for storing size */
    i++;
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
    pa[i] = new_platter;
  }

  /* store array size in entry 0 */
  pa[0] = i;

  fclose(file);

  return pa;
}

void SpinCycle(UniversalMachine * um) {
  unsigned int byte_code, op, reg_a, reg_b, reg_c;
  unsigned char ch;
  unsigned int *zero_arr = um->collection[0];
  unsigned int *regs = um->reg;
  unsigned int ip = um->ip;
  while (ip <= zero_arr[0]) {
    byte_code = zero_arr[ip + 1];
    op = (byte_code & OP_MASK);
    reg_a = (byte_code & RA_MASK) >> 6;
    reg_b = (byte_code & RB_MASK) >> 3;
    reg_c = (byte_code & RC_MASK);
    switch (op) {
      case OP_IFM:
        if (regs[reg_c] != 0) {
          regs[reg_a] = regs[reg_b];
        }
        break;
      case OP_ARI:
        regs[reg_a] = um->collection[regs[reg_b]][regs[reg_c] + 1];
        break;
      case OP_ARA:
        um->collection[regs[reg_a]][regs[reg_b] + 1] = regs[reg_c];
        break;
      case OP_ADD:
        regs[reg_a] = regs[reg_b] + regs[reg_c];
        break;
      case OP_MUL:
        regs[reg_a] = regs[reg_b] * regs[reg_c];
        break;
      case OP_DIV:
        regs[reg_a] = regs[reg_b] / regs[reg_c];
        break;
      case OP_NOT:
        regs[reg_a] = ~(regs[reg_b] & regs[reg_c]);
        break;
      case OP_HLT:
        return;
        break;
      case OP_ALC:
        /* TODO: implement more robust/less naive alloc and free */
        /* Search from last known lower bound on free cell */
        while (um->collection[um->pp_lb] != NULL) {
          um->pp_lb++;
        }
        um->pp = um->pp_lb++;
        um->collection[um->pp] = calloc(
            (regs[reg_c] + 1),
            sizeof(unsigned int));
        um->collection[um->pp][0] = regs[reg_c];
        regs[reg_b] = um->pp;
        break;
      case OP_ABD:
        free(um->collection[regs[reg_c]]);
        um->collection[regs[reg_c]] = NULL;
        /* Keep track of known lower bound */
        if (regs[reg_c] < um->pp_lb) {
          um->pp_lb = regs[reg_c];
        }
        if (regs[reg_c] == um->pp) {
          um->pp--;
        }
        break;
      case OP_OUT:
        putchar((unsigned char)regs[reg_c]);
        break;
      case OP_INP:
        regs[reg_c] = ((ch = getchar()) != EOF) ? ch : 0xFF;
        break;
      case OP_LDP:
        /* TODO: verify it's behaving */
        /* TODO: also all this memcpy seems slow */
        memcpy(
          zero_arr,
          um->collection[regs[reg_b]],
          (um->collection[regs[reg_b]][0] + 1) * sizeof(unsigned int));
        /* subtract 1 since we'll increment below */
        ip = regs[reg_c] - 1;
        break;
      case OP_ORT:
        regs[(byte_code & OA_MASK) >> 25] = (byte_code & OV_MASK);
        break;
      default:
        printf("Unknown byte_code: %u\n", byte_code);
        return;
    }
    ip++;
  }
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Usage: um <program.um>\n");
    exit(-1);
  }

  UniversalMachine um = {
    {0, 0, 0, 0, 0, 0, 0, 0},
    0,
    0,
    {LoadPlatterArrayOrDie(argv[1])},
    0
  };

  SpinCycle(&um);

  exit(0);
}