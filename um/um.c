#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* Google C++ Style Guide : 
    http://google-styleguide.googlecode.com/svn/trunk/cppguide.xml
*/

#define CODE_BUF_SIZE 32000000

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
#define OP_IFM 0x00000000  /* 0. Conditional Move. */
#define OP_ARI 0x10000000  /* 1. Array Index. */
#define OP_ARA 0x20000000  /* 2. Array Amendment. */
#define OP_ADD 0x30000000  /* 3. Addition. */
#define OP_MUL 0x40000000  /* 4. Multiplication. */
#define OP_DIV 0x50000000  /* 5. Division. */
#define OP_NOT 0x60000000  /* 6. Not-And. */
#define OP_HLT 0x70000000  /* 7. Halt. */
#define OP_ALC 0x80000000  /* 8. Allocation. */
#define OP_ABD 0x90000000  /* 9. Abandonment. */
#define OP_OUT 0xA0000000  /* 10. Output. */
#define OP_INP 0xB0000000  /* 11. Input. */
#define OP_LDP 0xC0000000  /* 12. Load Program. */
#define OP_ORT 0xD0000000  /* 13. Orthography. */

// Global machine state.
typedef void (*NativeCode)();
NativeCode* native_code = NULL;

uint32_t* byte_code = NULL;
uint32_t ip = 0;
uint32_t reg[8] = { 0 };
uint32_t op, a, b, c;
uint32_t instr;

/*
  When reading programs from legacy "unsigned 8-bit character"
  scrolls, a series of four bytes A,B,C,D should be interpreted with
  'A' as the most magnificent byte, and 'D' as the most shoddy, with
  'B' and 'C' considered lovely and mediocre respectively.
*/
uint32_t * LoadPlatterArrayOrDie(char *fname) {
  uint32_t *pa;
  uint32_t new_platter = 0, ch = 0, i = 0;

  FILE *file = fopen(fname, "r");

  if (file == NULL) {
    printf("Error reading program file.\n");
    exit(-1);
  }

  /* leave entry 0 for storing size */
  pa = calloc(CODE_BUF_SIZE, sizeof(uint32_t));

  for (i = 1; (ch = getc(file)) != EOF; i++) {
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

  /* store array size in first entry */
  pa[0] = i;

  fclose(file);

  return pa;
}

void ifm() {
  if (reg[c] != 0) {
    reg[a] = reg[b];
  }
  ip++;
}

NativeCode compile() {
  switch (op) {
    case OP_IFM:
      return (native_code[(ip + 1)] = ifm);
    default:
      return NULL;
  }
}

void SpinCycle(uint32_t *pa) {
  uint32_t tmp;
  uint8_t ch;

  byte_code = pa;
  native_code = calloc(byte_code[0], sizeof(uint32_t));

  while (1) {
    op = (byte_code[(ip + 1)] & OP_MASK);
    a = (byte_code[(ip + 1)] & RA_MASK) >> 6;
    b = (byte_code[(ip + 1)] & RB_MASK) >> 3;
    c = (byte_code[(ip + 1)] & RC_MASK);
    NativeCode code = native_code[(ip + 1)];
    if (code == NULL) { code = compile(); }
    if (code != NULL) { code(); continue; }
    switch (op) {
      case OP_IFM:
        ifm(); continue;
        break;
      case OP_ARI:
        reg[a] = (reg[b] == 0 ? byte_code : (uint32_t*)reg[b])[(reg[c] + 1)];
        break;
      case OP_ARA:
        (reg[a] == 0 ? byte_code : (uint32_t*)reg[a])[(reg[b] + 1)] = reg[c];
        break;
      case OP_ADD:
        reg[a] = (reg[b] + reg[c]) % 0x100000000;  /* 2^32 */
        break;
      case OP_MUL:
        reg[a] = (reg[b] * reg[c]) % 0x100000000;  /* 2^32 */
        break;
      case OP_DIV:
        reg[a] = (reg[b] % 0x100000000) / (reg[c] % 0x100000000);
        break;
      case OP_NOT:
        reg[a] = ~(reg[b] & reg[c]);
        break;
      case OP_HLT:
        return;
        break;
      case OP_ALC:
        tmp = reg[c];
        reg[b] = calloc((reg[c] + 1), sizeof(uint32_t));
        ((uint32_t*)reg[b])[0] = tmp;
        break;
      case OP_ABD:
        free(((uint32_t*)reg[c]));
        break;
      case OP_OUT:
        putchar((uint8_t)reg[c]);
        break;
      case OP_INP:
        reg[c] = ((ch = getchar()) != EOF) ? ch : 0xFF;
        break;
      case OP_LDP:
        /* NOTE: 0 refers to the special Platter Array 0 */
        /* don't bother copy if we're jumping within the same array */
        if (reg[b] != 0 && ((uint32_t*)reg[b]) != byte_code) {
          free(byte_code);
          byte_code = malloc((((uint32_t*)reg[b])[0] + 1) * sizeof(uint32_t));
          memcpy(
              byte_code,
              ((uint32_t*)reg[b]),
              ((((uint32_t*)reg[b])[0] + 1) * sizeof(uint32_t)));
          native_code = calloc(byte_code[0], sizeof(uint32_t));
        }
        /* subtract 1 since we'll increment below */
        ip = (reg[c] - 1);
        break;
      case OP_ORT:
        reg[(byte_code[(ip + 1)] & OA_MASK) >> 25] =
            (byte_code[(ip + 1)] & OV_MASK);
        break;
      default:
        printf("Unknown byte_code: %u\n", instr);
        return;
    }
    ip++;
  }

  /* free the original platter array */
  /* TODO(cskau): Is this a good idea? (Double free, not our memory, ..) */
  free(pa);
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Usage: um <program.um>\n");
    exit(-1);
  }

  SpinCycle(LoadPlatterArrayOrDie(argv[1]));

  exit(0);
}