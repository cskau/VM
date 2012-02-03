#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "types.h"

/* Google C++ Style Guide : 
    http://google-styleguide.googlecode.com/svn/trunk/cppguide.xml
*/

/* read binary data, adjust endianess */
uint32_t Read32OrDie(FILE *file) {
  uint32_t res = 0;
  res |= getc(file);
  res |= (getc(file) << 8);
  res |= (getc(file) << 16);
  res |= (getc(file) << 24);
  return res;
}

uint16_t Read16OrDie(FILE *file) {
  uint16_t res = 0;
  res |= getc(file);
  res |= (getc(file) << 8);
  return res;
}

uint8_t Read8OrDie(FILE *file) {
  return getc(file);
}

typedef struct {
  /* Program limits */
  uint16_t max_glo, max_tmp, max_res;
  /* String pool */
  uint32_t string_count;
  struct {
    uint32_t length;
    char *string;
  } *string_pool;
  /* Symbol pool */
  uint32_t symbol_count;
  struct {
    uint32_t length;
    char *symbol;
  } *symbol_pool;
  /* Lambda abstraction table */
  uint32_t lambda_count;
  struct {
    int8_t arity;
    uint32_t code;
  } *lambda_pool;
  /* Code */
  uint32_t code_length;
  unsigned char *instructions;
  /* Compiler signature */
  uint32_t signature_length;
  unsigned char *compiler_signature;
} VMLDSB;

/* VML DSB file layout:
  • Magic word
  • Program limits
  • String pool
  • Symbol pool
  • Lambda abstraction table
  • Code
  • Compiler signature
  • Magic word
*/
VMLDSB *LoadDSBOrDie(char *fname) {
  uint16_t max_glo, max_tmp, max_res;
  uint32_t string_count, string_length;
  uint32_t symbol_count, symbol_length;
  uint32_t lambda_count, lambda_code;
  int8_t lambda_arity;
  uint32_t code_length;
  int i;

  unsigned char *strings;
  unsigned char *symbols;
  unsigned char *lambdas;
  unsigned char *instructions;
  unsigned char *compiler_signature;

  VMLDSB *new_dsb = (VMLDSB *) malloc(sizeof(VMLDSB));

  FILE *file = fopen(fname, "r");

  if (file == NULL) {
    printf("Error reading program file.\n");
    exit(-1);
  }

  if (Read32OrDie(file) != VML_MAGIC) {
    printf("Error: Magic missing at beginning of file.\n");
    exit(-1);
  }

  /* Program limits */
  new_dsb->max_glo = Read16OrDie(file);
  new_dsb->max_tmp = Read16OrDie(file);
  new_dsb->max_res = Read16OrDie(file);

  /* String pool */
  new_dsb->string_count = Read32OrDie(file);
  new_dsb->string_pool = malloc(
      (new_dsb->string_count * sizeof(new_dsb->string_pool)));
  for (i = 0; i < new_dsb->string_count; i++) {
    new_dsb->string_pool[i].length = Read32OrDie(file);
    new_dsb->string_pool[i].string = malloc(
        new_dsb->string_pool[i].length * sizeof(char));
    fread(
        new_dsb->string_pool[i].string,
        sizeof(char),
        new_dsb->string_pool[i].length,
        file);
  }
  if (Read8OrDie(file) != VML_EOS) {
    printf("Error: After strings, EOS missing.\n");
    exit(-1);
  }

  /* Symbol pool */
  new_dsb->symbol_count = Read32OrDie(file);
  new_dsb->symbol_pool = malloc(
      (new_dsb->symbol_count * sizeof(new_dsb->symbol_pool)));
  for (i = 0; i < new_dsb->symbol_count; i++) {
    new_dsb->symbol_pool[i].length = Read32OrDie(file);
    new_dsb->symbol_pool[i].symbol = malloc(
        new_dsb->symbol_pool[i].length * sizeof(char));
    fread(
        new_dsb->symbol_pool[i].symbol,
        sizeof(char),
        new_dsb->symbol_pool[i].length,
        file);
  }
  if (Read8OrDie(file) != VML_EOS) {
    printf("Error: After symbols, EOS missing.\n");
    exit(-1);
  }

  /* Lambda abstraction table */
  new_dsb->lambda_count = Read32OrDie(file);
  new_dsb->lambda_pool = malloc(
      new_dsb->lambda_count * sizeof(new_dsb->lambda_pool));
  for (i = 0; i < new_dsb->symbol_count; i++) {
    new_dsb->lambda_pool[i].arity = (int8_t)Read8OrDie(file);
    new_dsb->lambda_pool[i].code = Read32OrDie(file);
  }
  if (Read8OrDie(file) != VML_EOS) {
    printf("Error: After lambdas, EOS missing.\n");
    exit(-1);
  }

  /* Code */
  new_dsb->code_length = Read32OrDie(file);
  new_dsb->instructions = malloc(new_dsb->code_length * sizeof(char));
  fread(new_dsb->instructions, sizeof(char), new_dsb->code_length, file);
  if (Read8OrDie(file) != VML_EOS) {
    printf("Error: After code, EOS missing.\n");
    exit(-1);
  }

  /* Compiler Signature */
  new_dsb->signature_length = Read32OrDie(file);
  new_dsb->compiler_signature = malloc(
      new_dsb->signature_length * sizeof(char));
  fread(
      new_dsb->compiler_signature, 
      sizeof(char), 
      new_dsb->signature_length, 
      file);

  if (Read32OrDie(file) != VML_MAGIC) {
    printf("Error: Magic missing at end of file.\n");
    exit(-1);
  }

  fclose(file);

  return new_dsb;
}

void Run(VMLDSB *vmldsb) {
  unsigned char* instructions = vmldsb->instructions;
  uint32_t *env_lib, *env_lex, *aux_vec;
  uint32_t *env_glo, *env_tmp, *aux_res;
  uint32_t *cont;
  
  uint8_t op = 0;
  unsigned int ip = 0;
  /* opcode parameters */
  uint32_t *l, *x;
  uint16_t *i, *j, *n;
  uint8_t q, s, t, v;

  aux_res = malloc(vmldsb->max_res * sizeof(uint32_t));

  while (1) {
    op = instructions[ip];
    switch (op) {
      case OP_NOP:
        break;
      case OP_LOAD:
        printf("Unimplemented opcode: OP_LOAD\n");
        v = (uint8_t*) instructions[ip + 1];
        x = (uint32_t*) instructions[ip + 2];
        t = (uint8_t*) instructions[ip + 6];
        j = (uint16_t*) instructions[ip + 7];
        ip += 8;
        DSValue *new_value = CreateValue(*v, *x);
        switch (*t) {
          case SCP_LIB:
            env_lib[*j] = new_value;
            break;
          case SCP_GLO:
            env_glo[*j] = new_value;
            break;
          case SCP_RES:
            aux_res[*j] = new_value;
            break;
          case SCP_TMP:
            env_tmp[*j] = new_value;
            break;
          case SCP_VEC:
            aux_vec[*j] = new_value;
            break;
          default:
            env_lex[*t][*j] = new_value;
            break;
        }
        break;
      case OP_MOVE:
        printf("Unimplemented opcode: OP_MOVE\n");
        s = (uint8_t*) instructions[ip + 1];
        i = (uint16_t*) instructions[ip + 2];
        t = (uint8_t*) instructions[ip + 4];
        j = (uint16_t*) instructions[ip + 5];
        ip += 8;
        break;
      case OP_NEW_VEC:
        printf("Unimplemented opcode: OP_NEW_VEC\n");
        n = (uint16_t*) instructions[ip + 1];
        ip += 2;
        aux_vec = calloc(n, sizeof(DSValue*));
        break;
      case OP_EXTEND:
        printf("Unimplemented opcode: OP_EXTEND\n");
        break;
      case OP_JUMP:
        printf("Unimplemented opcode: OP_JUMP\n");
        l = (uint32_t*) instructions[ip + 1];
        ip += 4;
        break;
      case OP_JUMP_IF_FALSE:
        printf("Unimplemented opcode: OP_JUMP_IF_FALSE\n");
        q = (uint8_t*) instructions[ip + 1];
        i = (uint16_t*) instructions[ip + 2];
        l = (uint32_t*) instructions[ip + 4];
        ip += 7;
        break;
      case OP_TAIL_CALL:
        printf("Unimplemented opcode: OP_TAIL_CALL\n");
        q = (uint8_t*) instructions[ip + 1];
        i = (uint16_t*) instructions[ip + 2];
        ip += 3;
        break;
      case OP_CALL:
        printf("Unimplemented opcode: OP_CALL\n");
        q = (uint8_t*) instructions[ip + 1];
        i = (uint16_t*) instructions[ip + 2];
        n = (uint16_t*) instructions[ip + 4];
        ip += 5;
        break;
      case OP_RETURN:
        printf("Unimplemented opcode: OP_RETURN\n");
        if (cont == NULL) {
          printf("%s\n", aux_res[0]);
        }
        break;
      default:
        printf("Unknown opcode: %u\n", op);
        exit(-1);
    }
    ip++;
  }
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Usage: dsvm <program.dsb>\n");
    exit(-1);
  }

  VMLDSB *vmldsb = LoadDSBOrDie(argv[1]);
  Run(vmldsb);

  exit(0);
}