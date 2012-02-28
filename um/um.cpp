#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "v8.h"
#include "assembler.h"
#include "ia32/assembler-ia32-inl.h"

#define __ assembler.

v8::internal::Operand ExternalOperand(void* address) {
  return v8::internal::Operand(
      reinterpret_cast<int32_t>(address),
      v8::internal::RelocInfo::EXTERNAL_REFERENCE);
}

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

void set_code(uint32_t target) {
  if (target != 0) {
    delete[] byte_code;
    delete[] native_code;
    uint32_t* array = ((uint32_t*)target);
    size_t size = array[0];
    byte_code = new uint32_t[size];
    native_code = new NativeCode[size];
    for (unsigned int i = 0; i < size; ++i) {
      byte_code[i] = array[i];
      native_code[i] = NULL;
    }
  }
}

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
  pa = (uint32_t*)calloc(CODE_BUF_SIZE, sizeof(uint32_t));

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
  /* TODO(cskau): compile whole blocks to first jump */
  using namespace v8::internal;
  /* this is slow, but if we cache we shouldn't get here too often */
  /* TODO(cskau): maybe simply instanciate and store globally ? */
  Assembler assembler(Isolate::Current(), NULL, 0);
  CodeDesc desc;
  Label skip;
  size_t allocated;
  uint32_t ip_tmp = ip;
  while (ip_tmp < byte_code[0]) {
    op = (byte_code[(ip_tmp + 1)] & OP_MASK);
    a = (byte_code[(ip_tmp + 1)] & RA_MASK) >> 6;
    b = (byte_code[(ip_tmp + 1)] & RB_MASK) >> 3;
    c = (byte_code[(ip_tmp + 1)] & RC_MASK);
    switch (op) {
      case OP_IFM:
        /* since we're looping we need to clear label every round */
        skip = Label();
        __ mov(eax, ExternalOperand(&reg[c]));
        __ test(eax, eax);
        __ j(zero, &skip, Label::kNear);
        if (b != c) {
          __ mov(eax, ExternalOperand(&reg[b]));
        }
        __ mov(ExternalOperand(&reg[a]), eax);
        __ bind(&skip);
        break;
      case OP_ARI:
        skip = Label();
        __ mov(eax, ExternalOperand(&reg[b])); // array
        __ mov(ecx, ExternalOperand(&reg[c])); // index
        __ test(eax, eax); // array != 0 ?
        __ j(not_zero, &skip, Label::kNear);
        // TODO(cskau): make sure we invalidate this when we touch byte_code
        __ mov(eax, Immediate(reinterpret_cast<Address>(byte_code)));
        __ bind(&skip);
        __ mov(eax, Operand(eax, ecx, times_4, 4));
        __ mov(ExternalOperand(&reg[a]), eax);
        break;
      case OP_ADD:
        __ mov(eax, ExternalOperand(&reg[b]));
        if (b == c) {
          __ add(eax, eax);
        } else {
          __ add(eax, ExternalOperand(&reg[c]));
        }
        __ mov(ExternalOperand(&reg[a]), eax);
        break;
      case OP_MUL:
        __ mov(eax, ExternalOperand(&reg[b]));
        __ mov(edx, ExternalOperand(&reg[c]));
        __ mul(edx);
        __ mov(ExternalOperand(&reg[a]), eax);
        break;
      case OP_DIV:
        __ mov(eax, ExternalOperand(&reg[b]));
        __ mov(ecx, ExternalOperand(&reg[c]));
        //__ mov(edx, Immediate(0));
        __ xor_(edx, edx); // clear edx
        __ div(ecx);
        __ mov(ExternalOperand(&reg[a]), eax);
        break;
      case OP_NOT:
        __ mov(eax, ExternalOperand(&reg[b]));
        if (b != c) {
          __ and_(eax, ExternalOperand(&reg[c]));
        }
        __ not_(eax);
        __ mov(ExternalOperand(&reg[a]), eax);
        break;
      case OP_ORT:
        __ mov(
            ExternalOperand(&reg[(byte_code[(ip_tmp + 1)] & OA_MASK) >> 25]),
            Immediate((byte_code[(ip_tmp + 1)] & OV_MASK)));
        break;
      default:
        if (assembler.pc_offset() == 0) {
          /* if we can't compile even the first instruction .. */
          return NULL;
        }
        printf("%x\n", op);
        goto finish; /* Yippee-ki-yay ! */
        break;
    }
    __ add(ExternalOperand(&ip), Immediate(1)); /* increment ip */
    ip_tmp++;
  }
  finish:
  __ ret(0);
  // assembler.Print();
  assembler.GetCode(&desc);
  ASSERT(desc.reloc_size == 0);
  byte* instructions = reinterpret_cast<byte*>(
      OS::Allocate(desc.instr_size, &allocated, true));  // executable = true.
  /* Allocation might (and consistently do) fail */
  if (instructions == NULL) {
    return NULL;
  }
  memcpy(instructions, desc.buffer, desc.instr_size);
  return reinterpret_cast<NativeCode>(instructions);
}

void SpinCycle(uint32_t *pa) {
  v8::V8::Initialize();
  uint32_t tmp;
  uint8_t ch;

  byte_code = pa;
  native_code = (NativeCode*)calloc((byte_code[0] + 1), sizeof(uint32_t));

  while (1) {
    op = (byte_code[(ip + 1)] & OP_MASK);
    a = (byte_code[(ip + 1)] & RA_MASK) >> 6;
    b = (byte_code[(ip + 1)] & RB_MASK) >> 3;
    c = (byte_code[(ip + 1)] & RC_MASK);
    NativeCode code = native_code[(ip + 1)];
    if (code == NULL) { code = (native_code[(ip + 1)] = compile()); }
    if (code != NULL) { code(); continue; }
    switch (op) {
      case OP_IFM:
        if (reg[c] != 0) {
          reg[a] = reg[b];
        }
        break;
      case OP_ARI:
        reg[a] = (reg[b] == 0 ? byte_code : (uint32_t*)reg[b])[(reg[c] + 1)];
        break;
      case OP_ARA:
        /* TODO(cskau): invalidate native code cache on write to 0 */
        /*
        if (reg[a] == 0) {
          free(native_code);
          native_code = (NativeCode*)calloc(byte_code[0], sizeof(uint32_t));
        }
        */
        (reg[a] == 0 ? byte_code : (uint32_t*)reg[a])[(reg[b] + 1)] = reg[c];
        break;
      case OP_ADD:
        reg[a] = (reg[b] + reg[c]) & 0xFFFFFFFF;  /* 2^32 */
        break;
      case OP_MUL:
        reg[a] = (reg[b] * reg[c]) & 0xFFFFFFFF;  /* 2^32 */
        break;
      case OP_DIV:
        reg[a] = (reg[b] & 0xFFFFFFFF) / (reg[c] & 0xFFFFFFFF);
        break;
      case OP_NOT:
        reg[a] = ~(reg[b] & reg[c]);
        break;
      case OP_HLT:
        return;
        break;
      case OP_ALC:
        tmp = reg[c];
        reg[b] = (uint32_t)calloc((reg[c] + 1), sizeof(uint32_t));
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
          for (int i = 1; i < byte_code[0]; i++) {
            if (native_code[i] != NULL) {
              /* free each piece of V8 native code so we dont orphane them */
              v8::internal::OS::Free((uint8_t*)native_code[i], 0x1000);
            }
          }
          free(native_code);
          free(byte_code);
          byte_code = (uint32_t*)malloc(
              (((uint32_t*)reg[b])[0] + 1) * sizeof(uint32_t));
          if (byte_code == NULL) {
            printf("ERROR: couldn't allocate new memory (%x) (size: %x)\n", byte_code, (((uint32_t*)reg[b])[0] + 1));
            exit(-1);
          }
          memcpy(
              byte_code,
              ((uint32_t*)reg[b]),
              ((((uint32_t*)reg[b])[0] + 1) * sizeof(uint32_t)));
          native_code = (NativeCode*)calloc((byte_code[0] + 1), sizeof(uint32_t));
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

  free(native_code);

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
