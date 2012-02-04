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
  for (i = 0; i < new_dsb->lambda_count; i++) {
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

void PrintValue(DSValue *value) {
  switch (value->type) {
    case NIL:
      printf("nil\n");
      break;
    case BOOL:
      printf("%s\n", value->value ? "True" : "False");
      break;
    case INT:
      printf("%i\n", value->value);
      break;
    case CHAR:
      printf("%c\n", value->code);
      break;
    case STR:
      printf("%u\n", value->index);
      break;
    case SYM:
      printf("%u\n", value->index);
      break;
    case CLOSE_FLAT:
      printf("close-flat (%u)\n", value->index);
      break;
    case CLOSE_DEEP:
      printf("close-deep (%u)\n", value->index);
      break;
    case VOID:
      printf("void\n");
      break;
    default:
      printf("Unimplemented printing method: %i\n", value->type);
  }
}

DSValue *Lib(uint16_t i, DSValue **aux_vec) {
  switch (i) {
    case LIB_INTEGERQ:
      printf("Unimplemented library function: LIB_INTEGERQ\n");
      break;
    case LIB_PLUS:
      return CreateValue(INT, (aux_vec[0]->value + aux_vec[1]->value));
      break;
    default:
      printf("Unimplemented or unknown library function: %i\n", i);
  }
  return NULL;
}

/* TODO: inline, and possibly add macro to auto assign all vector params */
DSValue **GetVector(
    int8_t t,
    DSValue **env_lib, DSValue **env_glo, DSValue **aux_res,
    DSValue **env_tmp, DSValue **aux_vec, DSValue **env_lex) {
  switch (t) {
    case SCP_LIB:
      return env_lib;
    case SCP_GLO:
      return env_glo;
    case SCP_RES:
      return aux_res;
    case SCP_TMP:
      return env_tmp;
    case SCP_VEC:
      return aux_vec;
    default:
      return ((uint32_t *)env_lex[t]);
  }
  return NULL;
}

void Run(VMLDSB *vmldsb) {
  unsigned char* instructions = vmldsb->instructions;
  DSValue **env_lex;
  DSValue **env_lib, **aux_vec;
  DSValue **env_glo, **env_tmp, **aux_res;
  uint32_t *cont = NULL;
  
  uint8_t op = 0;
  unsigned int ip = 0;

  /* opcode parameters */
  uint32_t l, x;
  uint16_t i, j, n;
  int8_t q, s, t, v;

  aux_res = malloc(vmldsb->max_res * sizeof(DSValue*));
  env_tmp = malloc(vmldsb->max_tmp * sizeof(DSValue*));
  env_glo = malloc(vmldsb->max_glo * sizeof(DSValue*));

  while (1) {
    op = instructions[ip];
    switch (op) {
      case OP_NOP:
        break;
      case OP_LOAD:
        printf("Unimplemented opcode: OP_LOAD\n");
        v = instructions[ip + 1];
        x = instructions[ip + 2];
        t = instructions[ip + 6];
        j = instructions[ip + 7];
        ip += 8;
        printf("(v, x, t, j) = (%i, %i, %i, %i)\n", v, x, t, j);
        GetVector(
            t,
            &env_lib, &env_glo, &aux_res,
            &env_tmp, &aux_vec, &env_lex
            )[j] = CreateValue(v, x);
        break;
      case OP_MOVE:
        printf("Unimplemented opcode: OP_MOVE\n");
        s = instructions[ip + 1];
        i = instructions[ip + 2];
        t = instructions[ip + 4];
        j = instructions[ip + 5];
        printf("(s, i, t, j) = (%i, %i, %i, %i)\n", s, i, t, j);
        ip += 6;
        /* Unknown: should moved-from vector entry be zeroed after move ? */
        /* TODO: my eyes.. !! */
        GetVector(
            s,
            &env_lib, &env_glo, &aux_res,
            &env_tmp, &aux_vec, &env_lex
            )[i] = GetVector(
                t,
                &env_lib, &env_glo, &aux_res,
                &env_tmp, &aux_vec, &env_lex
                )[j];
        break;
      case OP_NEW_VEC:
        printf("Unimplemented opcode: OP_NEW_VEC\n");
        n = instructions[ip + 1];
        ip += 2;
        aux_vec = calloc(n, sizeof(DSValue*));
        break;
      case OP_EXTEND:
        printf("Unimplemented opcode: OP_EXTEND\n");
        DSValue **new_env_lex = malloc(2 * sizeof(DSValue **));
        new_env_lex[1] = env_lex;
        env_lex = new_env_lex[0];
        break;
      case OP_JUMP:
        printf("Unimplemented opcode: OP_JUMP\n");
        l = instructions[ip + 1];
        ip += 4;
        /* Compensate for increment before looping */
        ip = (l - 1);
        break;
      case OP_JUMP_IF_FALSE:
        printf("Unimplemented opcode: OP_JUMP_IF_FALSE\n");
        q = instructions[ip + 1];
        i = instructions[ip + 2];
        l = instructions[ip + 4];
        printf("(q, i, l) = (%i, %i, %i)\n", q, i, l);
        ip += 7;
        DSValue *vector = GetVector(
            q,
            &env_lib, &env_glo, &aux_res,
            &env_tmp, &aux_vec, &env_lex
            )[i];
        if (vector->type == BOOL && vector->value == 0) {
          /* Compensate for increment before looping */
          ip = (l - 1);
        }
        break;
      case OP_TAIL_CALL:
        printf("Unimplemented opcode: OP_TAIL_CALL\n");
        q = instructions[ip + 1];
        i = instructions[ip + 2];
        printf("(q, i) = (%i, %i)\n", q, i);
        ip += 3;
        if (q == SCP_LIB) {
          aux_res = Lib(i, aux_vec);
          /* you saw that right, a goto ! */
          goto op_return;
        }
        break;
      case OP_CALL:
        printf("Unimplemented opcode: OP_CALL\n");
        q = instructions[ip + 1];
        i = instructions[ip + 2];
        n = instructions[ip + 4];
        ip += 5;
        break;
      case OP_RETURN:
        printf("Unimplemented opcode: OP_RETURN\n");
        /* see OP_TAIL_CALL above */
        op_return:
        if (cont == NULL) {
          PrintValue(&aux_res[0]);
          return;
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