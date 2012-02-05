#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "types.h"

/* Google C++ Style Guide : 
    http://google-styleguide.googlecode.com/svn/trunk/cppguide.xml
*/

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

typedef struct DSVector_ {
  unsigned int length;
  union {
    DSValue **values;
    struct DSVector_ **vectors;
  }
} DSVector;

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

void PrintValue(VMLDSB* dsb, DSValue *value, char* seperator) {
  switch (value->type) {
    case NIL:
      printf("nil");
      break;
    case BOOL:
      printf("%s", value->value ? "#t" : "#f");
      break;
    case INT:
      printf("%i", value->value);
      break;
    case CHAR:
      printf("#\\%c", value->code);
      break;
    case STR:
      printf("\"%s\"", dsb->string_pool[value->index].string);
      break;
    case SYM:
      printf("%u", value->index);
      break;
    case CLOSE_FLAT:
      printf("close-flat (%u)", value->index);
      break;
    case CLOSE_DEEP:
      printf("close-deep (%u)", value->index);
      break;
    case VOID:
      printf("void");
      break;
    case FUNC_LIB:
      printf("lib(%u)", value->index);
      break;
    default:
      printf("Unimplemented printing method: %i", value->type);
  }
  printf(seperator);
}

void PrintVector(DSVector *vector, int depth, char* seperator) {
  int i;
  if (vector != NULL) {
    printf("(");
    for (i = 0; i < vector->length; i++) {
      if (depth > 0 && vector->vectors && vector->vectors[i] != NULL) {
        PrintVector(vector->vectors[i], (depth - 1), (i < (vector->length - 1) ? ", " : ""));
      } else if (vector->values && vector->values[i] != NULL) {
        /*vmldsb*/
        PrintValue(NULL, vector->values[i], (i < (vector->length - 1) ? ", " : ""));
      } else {
        printf("uninitialized value");
      }
    }
    printf("){%u}%s", vector->length, seperator);
  } else {
    printf("uninitialized vector\n");
  }
}

void PrintCore(
    unsigned int ip,
    DSVector *env_lib, DSVector *env_glo, DSVector *aux_res,
    DSVector *env_tmp, DSVector *aux_vec, DSVector *env_lex) {
  int i;
  printf("ip: %u\n", ip);
  printf("env_lex: ");
  PrintVector(env_lex, 1, "\n");
  /*
  for (i = 0; env_lex && i < env_lex->length; i++) {
    printf("env_lex[%u]: ", i);
    PrintVector(env_lex->vectors[i], 0, "\n");
  }
  */
  printf("env_tmp: ");
  PrintVector(env_tmp, 0, "\n");
  printf("env_glo: ");
  PrintVector(env_glo, 0, "\n");
  printf("aux_vec: ");
  PrintVector(aux_vec, 0, "\n");
  printf("aux_res: ");
  PrintVector(aux_res, 0, "\n");
  /*
  printf("env_lib: ");
  PrintVector(env_lib, 0, "\n");
  */
}

DSValue *Lib(uint16_t i, DSVector *aux_vec) {
  switch (i) {
    case LIB_INTEGERQ:
      CheckArityOrDie(1, aux_vec->length);
      return CreateValue(
          BOOL,
          (aux_vec->values[0]->type == INT ? 1 : 0));
      break;
    case LIB_PLUS:
      CheckArityOrDie(2, aux_vec->length);
      return CreateValue(
          INT,
          (aux_vec->values[0]->value + aux_vec->values[1]->value));
      break;
    case LIB_MINUS:
      CheckArityOrDie(2, aux_vec->length);
      return CreateValue(
          INT,
          (aux_vec->values[0]->value - aux_vec->values[1]->value));
      break;
    case LIB_TIMES:
      CheckArityOrDie(2, aux_vec->length);
      return CreateValue(
          INT,
          (aux_vec->values[0]->value * aux_vec->values[1]->value));
      break;
    case LIB_QUOTIENT:
      CheckArityOrDie(2, aux_vec->length);
      return CreateValue(
          INT,
          (aux_vec->values[0]->value / aux_vec->values[1]->value));
      break;
    case LIB_REMAINDER:
      CheckArityOrDie(2, aux_vec->length);
      return CreateValue(
          INT,
          (aux_vec->values[0]->value % aux_vec->values[1]->value));
      break;
    case LIB_LT:
      CheckArityOrDie(2, aux_vec->length);
      return CreateValue(
          BOOL,
          (aux_vec->values[0]->value < aux_vec->values[1]->value));
      break;
    case LIB_LE:
      CheckArityOrDie(2, aux_vec->length);
      return CreateValue(
          BOOL,
          (aux_vec->values[0]->value <= aux_vec->values[1]->value));
      break;
    case LIB_EQ:
      /* TODO: type checking? */
      CheckArityOrDie(2, aux_vec->length);
      return CreateValue(
          BOOL,
          (aux_vec->values[0]->value == aux_vec->values[1]->value));
      break;
    case LIB_GE:
      CheckArityOrDie(2, aux_vec->length);
      return CreateValue(
          BOOL,
          (aux_vec->values[0]->value >= aux_vec->values[1]->value));
      break;
    case LIB_GT:
      CheckArityOrDie(2, aux_vec->length);
      return CreateValue(
          BOOL,
          (aux_vec->values[0]->value > aux_vec->values[1]->value));
      break;
    case LIB_BOOLEANQ:
      CheckArityOrDie(1, aux_vec->length);
      return CreateValue(
          BOOL,
          (aux_vec->values[0]->type == BOOL));
      break;
    case LIB_SYMBOLQ:
      CheckArityOrDie(1, aux_vec->length);
      return CreateValue(
          BOOL,
          (aux_vec->values[0]->type == SYM));
      break;
    case LIB_CHARQ:
      CheckArityOrDie(1, aux_vec->length);
      return CreateValue(
          BOOL,
          (aux_vec->values[0]->type == CHAR));
      break;
    case LIB_STRINGQ:
      CheckArityOrDie(1, aux_vec->length);
      return CreateValue(
          BOOL,
          (aux_vec->values[0]->type == STR));
      break;
    default:
      printf("Unimplemented or unknown library function: %i\n", i);
  }
  return NULL;
}

/* TODO: inline, and possibly add macro to auto assign all vector params */
DSVector *GetVector(
    int8_t t,
    DSVector *env_lib, DSVector *env_glo, DSVector *aux_res,
    DSVector *env_tmp, DSVector *aux_vec, DSVector *env_lex) {
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
      return env_lex->vectors[t];
  }
  return NULL;
}

/* TODO: inline the below ! */
DSVector *CreateVector(unsigned int length) {
  DSVector *new_vec = malloc(sizeof(DSVector));
  new_vec->length = length;
  new_vec->values = calloc(length, sizeof(DSValue*));
  return new_vec;
}

DSVector *ExtendVector(DSVector *env_lex, DSVector *aux_vec) {
  DSVector *new_vec = malloc(sizeof(DSVector));
  new_vec->length = (env_lex != NULL) ? (env_lex->length + 1) : 1;
  new_vec->vectors = malloc(new_vec->length * sizeof(DSVector*));
  new_vec->vectors[0] = aux_vec;
  if (env_lex != NULL) {
    memcpy(
        &new_vec->vectors[1],
        env_lex->vectors,
        (env_lex->length * sizeof(DSVector*)));
    /*free(env_lex);*/
  }
  return new_vec;
}

void CheckArityOrDie(int want, int got) {
  if (want != got && want != -1) {
    printf(
        "Error: Arity mismatch!\n  Want %i, but got %i\n",
        want, got);
    exit(-1);
  }
}

void Run(VMLDSB *vmldsb) {
  DSVector *env_glo, *env_tmp, *aux_res;
  DSVector *env_lib, *aux_vec;
  DSVector *env_lex = NULL;
  DSVector *cont = NULL;
  
  unsigned char* instructions = vmldsb->instructions;
  uint8_t op = 0;
  unsigned int ip = 0;

  /* opcode parameters */
  uint32_t l, x;
  uint16_t i, j, n;
  int8_t q, s, t, v;

  unsigned int k;

  /* init from dsb info */
  aux_res = CreateVector(vmldsb->max_res);
  env_tmp = CreateVector(vmldsb->max_tmp);
  env_glo = CreateVector(vmldsb->max_glo);

  /* TODO: allow overriding lib functions */
  env_lib = CreateVector(55);
  for (k = 0; k < 55; k++) {
    /* Use special type to refer to lib func */
    env_lib->values[k] = CreateValue(FUNC_LIB, k);
  }

  while (1) {
    /**
    PrintCore(
        ip,
        env_lib, env_glo, aux_res,
        env_tmp, aux_vec, env_lex);
    /**/
    op = instructions[ip];
    switch (op) {
      case OP_NOP:
        break;
      case OP_LOAD:
        v = instructions[ip + 1];
        x = instructions[ip + 2];
        t = instructions[ip + 6];
        j = instructions[ip + 7];
        printf(
            "Unimplemented opcode: OP_LOAD (v, x, t, j) = (%i, %i, %i, %i)\n",
            v, x, t, j);
        /**
          printf("aux_vec: "); PrintVector(aux_vec, 0, "\n");
          printf("env_lex: "); PrintVector(env_lex, 1, "\n");
        /**/
        ip += 8;
        /**
        if (v == CLOSE_FLAT) {
          PrintVector(aux_vec, 0, "\n");
        }
        /**/
        /* TODO: do we do anything special to load closures ? */
        GetVector(
            t,
            env_lib, env_glo, aux_res,
            env_tmp, aux_vec, env_lex
            )->values[j] = CreateValue(v, x);
        break;
      case OP_MOVE:
        s = instructions[ip + 1];
        i = instructions[ip + 2];
        t = instructions[ip + 4];
        j = instructions[ip + 5];
        printf("OP_MOVE (s, i, t, j) = (%i, %i, %i, %i)\n", s, i, t, j);
          if (env_tmp && env_tmp->values) {printf("env_tmp: "); printf("%u\n", env_tmp->values[0]); /*PrintVector(env_tmp, 0, "\n");*/}
        ip += 6;
        DSValue *from = GetVector(
            s,
            env_lib, env_glo, aux_res,
            env_tmp, aux_vec, env_lex
            )->values[i];
        GetVector(
            t,
            env_lib, env_glo, aux_res,
            env_tmp, aux_vec, env_lex
            )->values[j] = CreateValue(from->type, from->value);
        /* TODO: should moved-from vector entry be zero/void/nil after move ? */
        break;
      case OP_NEW_VEC:
        n = instructions[ip + 1];
        printf("OP_NEW_VEC (n) = (%i)\n", n);
        ip += 2;
        aux_vec = CreateVector(n);
        break;
      case OP_EXTEND:
        printf("OP_EXTEND\n");
        env_lex = ExtendVector(env_lex, aux_vec);
        break;
      case OP_JUMP:
        l = instructions[ip + 1];
        printf("OP_JUMP (l) = (%i)\n", l);
        ip += 4;
        /* Compensate for increment before looping */
        ip = (l - 1);
        break;
      case OP_JUMP_IF_FALSE:
        q = instructions[ip + 1];
        i = instructions[ip + 2];
        l = instructions[ip + 4];
        printf("OP_JUMP_IF_FALSE (q, i, l) = (%i, %i, %i)\n", q, i, l);
        ip += 7;
        DSValue *value = GetVector(
            q,
            env_lib, env_glo, aux_res,
            env_tmp, aux_vec, env_lex
            )->values[i];
        if (value->type == BOOL && value->value == 0) {
          /* Compensate for increment before looping */
          ip = (l - 1);
        }
        break;
      case OP_TAIL_CALL:
    /**
    PrintCore(
        ip,
        env_lib, env_glo, aux_res,
        env_tmp, aux_vec, env_lex);
    /**/
        q = instructions[ip + 1];
        i = instructions[ip + 2];
        printf("Unimplemented opcode: OP_TAIL_CALL (q, i) = (%i, %i)\n", q, i);
          printf("aux_vec: "); PrintVector(aux_vec, 0, "\n");
          printf("env_lex: "); PrintVector(env_lex, 1, "\n");
        ip += 3;
        op_tail_call:
        if (q == SCP_LIB) {
          /* TODO: do we always just get one result value? */
          aux_res->values[0] = Lib(i, aux_vec);
          /* you saw that right, a goto ! */
          goto op_return;
        } else {
          DSValue *close = GetVector(
              q,
              env_lib, env_glo, aux_res,
              env_tmp, aux_vec, env_lex
              )->values[i];
          if (close->type == CLOSE_FLAT || close->type == CLOSE_DEEP) {
            CheckArityOrDie(
                vmldsb->lambda_pool[close->index].arity,
                aux_vec->length);
            env_lex = ExtendVector(env_lex, aux_vec);
            /* Compensate for increase before looping */
            ip = (vmldsb->lambda_pool[close->index].code - 1);
          } else if (close->type == FUNC_LIB) {
            aux_res->values[0] = Lib(close->value, aux_vec);
            goto op_return;
          }

    /**
    PrintCore(
        ip,
        env_lib, env_glo, aux_res,
        env_tmp, aux_vec, env_lex);
    /**/
        }
        break;
      case OP_CALL:
    /**
    PrintCore(
        ip,
        env_lib, env_glo, aux_res,
        env_tmp, aux_vec, env_lex);
    /**/
        q = instructions[ip + 1];
        i = instructions[ip + 2];
        n = instructions[ip + 4];
        printf(
            "Unimplemented opcode: OP_CALL (q, i, n) = (%i, %i, %i)\n",
            q, i, n);
        ip += 5;
        /* push to continuation stack */
        DSVector *new_vec = CreateVector((n + 3));
        new_vec->values[0] = cont;
        new_vec->values[1] = env_lex;
        new_vec->values[2] = ip;
        for (; n > 0; n--) {
          new_vec->values[(2 + n)] = CopyValue(&env_tmp[(n - 1)]);
        }
        cont = new_vec;
        /* TODO: look at possibility of fall-through, since call leads to 
          tail-call, and tail-call leads to return */
        goto op_tail_call;
        break;
      case OP_RETURN:
        printf("Unimplemented opcode: OP_RETURN\n");
        /* see OP_TAIL_CALL above */
        op_return:
        if (cont != NULL) {
          /* TODO: free old cont */
          /* pop from continuation stack */
          for (k = 0; k < (cont->length - 3); k++) {
            env_tmp->values[k] = cont->values[(3 + k)];
          }
          ip = cont->values[2];
          env_lex = cont->values[1];
          cont = cont->values[0];
        } else {
          PrintValue(vmldsb, aux_res->values[0], "\n");
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