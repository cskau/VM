#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Google C++ Style Guide : 
    http://google-styleguide.googlecode.com/svn/trunk/cppguide.xml
*/

#define CODE_BUF_SIZE 32000000
#define MEMO_BUF_SIZE 32000000

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

/* OP codes */
#define OP_IFM 0x00000000  // 0. Conditional Move.
#define OP_ARI 0x10000000  // 1. Array Index.
#define OP_ARA 0x20000000  // 2. Array Amendment.
#define OP_ADD 0x30000000  // 3. Addition.
#define OP_MUL 0x40000000  // 4. Multiplication.
#define OP_DIV 0x50000000  // 5. Division.
#define OP_NOT 0x60000000  // 6. Not-And.
#define OP_HLT 0x70000000  // 7. Halt.
#define OP_ALC 0x80000000  // 8. Allocation.
#define OP_ABD 0x90000000  // 9. Abandonment.
#define OP_OUT 0xA0000000  // 10. Output.
#define OP_INP 0xB0000000  // 11. Input.
#define OP_LDP 0xC0000000  // 12. Load Program.
#define OP_ORT 0xD0000000  // 13. Orthography.

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

void SpinCycle(unsigned int *pa) {
  unsigned int regs[] = {0, 0, 0, 0, 0, 0, 0, 0};
  unsigned int **collection;
  unsigned int ip = 0, pp = 0, pp_lb = 0;

  unsigned int byte_code, op;
  unsigned int a, b, c;
  unsigned char ch;
  unsigned int last_ldp = 0;

  collection = (unsigned int *) calloc(MEMO_BUF_SIZE, sizeof(unsigned int *));
  collection[0] = pa;

  while (ip <= collection[0][0]) {
    byte_code = collection[0][ip + 1];
    op = (byte_code & OP_MASK);
    a = (byte_code & RA_MASK) >> 6;
    b = (byte_code & RB_MASK) >> 3;
    c = (byte_code & RC_MASK);
    switch (op) {
      case OP_IFM:
        if (regs[c] != 0) {
          regs[a] = regs[b];
        }
        break;
      case OP_ARI:
        regs[a] = collection[regs[b]][regs[c] + 1];
        break;
      case OP_ARA:
        /* Copy-on-write */
        if (last_ldp != 0) {
          collection[0] = malloc(
              ((collection[0][0] + 1) * sizeof(unsigned int)));
          memcpy(
              collection[0],
              collection[last_ldp],
              (collection[last_ldp][0] + 1) * sizeof(unsigned int));
          last_ldp = 0;
        }
        collection[regs[a]][regs[b] + 1] = regs[c];
        break;
      case OP_ADD:
        regs[a] = regs[b] + regs[c];
        break;
      case OP_MUL:
        regs[a] = regs[b] * regs[c];
        break;
      case OP_DIV:
        regs[a] = regs[b] / regs[c];
        break;
      case OP_NOT:
        regs[a] = ~(regs[b] & regs[c]);
        break;
      case OP_HLT:
        return;
        break;
      case OP_ALC:
        /* TODO: implement more robust/less naive alloc and free */
        /* Search from last known lower bound on free cell */
        while (collection[pp_lb] != NULL) {
          pp_lb++;
        }
        pp = pp_lb;
        collection[pp] = calloc((regs[c] + 1), sizeof(unsigned int));
        collection[pp][0] = regs[c];
        regs[b] = pp;
        break;
      case OP_ABD:
        /* make sure we're not shadowing it */
        if (regs[c] != last_ldp) {
          free(collection[regs[c]]);
        } else {
          /* this memory is now exclusively collection[0] */
          last_ldp = 0;
        }
        collection[regs[c]] = NULL;
        /* Keep track of known lower bound */
        if (regs[c] < pp_lb) {
          pp_lb = regs[c];
        }
        if (regs[c] == pp) {
          pp--;
        }
        break;
      case OP_OUT:
        putchar((unsigned char)regs[c]);
        break;
      case OP_INP:
        regs[c] = ((ch = getchar()) != EOF) ? ch : 0xFF;
        break;
      case OP_LDP:
        /* subtract 1 since we'll increment below */
        ip = regs[c] - 1;
        /* Shadow the copied memory to speed up "copy" */
        if (regs[b] != 0) {
          free(collection[0]);
          collection[0] = collection[regs[b]];
          last_ldp = regs[b];
        }
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

  free(collection);
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Usage: um <program.um>\n");
    exit(-1);
  }

  SpinCycle(LoadPlatterArrayOrDie(argv[1]));

  exit(0);
}