#include <stdint.h>
#include <string.h>

#define  VML_MAGIC  0xDA15CE03
#define  VML_EOS    0x80

/* 5.2 Types and Values */

/* non-negative value for the lexical scope corresponding to that number */
#define  SCP_LIB  -1
#define  SCP_GLO  -2
#define  SCP_RES  -3
#define  SCP_TMP  -4
#define  SCP_VEC  -5

typedef struct DSValue DSValue;
typedef struct DSVector DSVector;

DSVector *CopyVector(DSVector *old_vector, unsigned int from);
DSVector *CreateVector(unsigned int length);

enum ValueTypes {
  NIL = 0,
  BOOL = 1,
  INT = 2,
  CHAR = 3,
  STR = 4,
  SYM = 5,
  CLOSE_FLAT = 6,
  CLOSE_DEEP = 7,
  VOID = 8,
  /* This is impl specific. Refers to lib function. */
  FUNC_LIB = 9,
};

struct DSValue {
  enum ValueTypes type;
  union {
    int32_t value;
    uint32_t index;
    char code;
  };
  /* for closures.. */
  DSVector *env_lex;
};

struct DSVector {
  unsigned int length;
  union {
    DSValue **values;
    DSVector **vectors;
  };
};

int IsUnboxedInt(DSValue *value) {
  return ((int32_t)value) & 0x1;
}

int32_t GetUnboxedInt(DSValue *value) {
  return ((int32_t)value - 1) / 2;
}

DSValue *CreateValue(enum ValueTypes type, uint32_t value, DSVector *env_lex) {
  /* unboxed int */
  if (type == INT) {
    if (value & 0x40000000) {
      printf("Value overflows sign: %i\n", value);
      exit(-1);
    }
    return ((int32_t)value * 2) + 1;
  }
  DSValue *new_value = malloc(sizeof(DSValue));
  new_value->type = type;
  if (new_value->type == CHAR) {
    new_value->code = (char)value;
  } else {
    new_value->index = (uint32_t)value;
  }
  if (new_value->type == CLOSE_FLAT) {
    new_value->env_lex = CreateVector(1);
    new_value->env_lex->vectors[0] = CopyVector(env_lex, 0);
  } else if (new_value->type == CLOSE_DEEP) {
    new_value->env_lex = CopyVector(env_lex, 1);
    new_value->env_lex = env_lex;
  } else {
    new_value->env_lex = NULL;
  }
  return new_value;
};

DSValue *CopyValue(DSValue *old_value) {
  /* NULL or unboxed int ? */
  if (old_value == NULL || IsUnboxedInt(old_value)) {
    return old_value;
  }
  DSValue *new_value = malloc(sizeof(DSValue));
  new_value->type = old_value->type;
  new_value->value = old_value->value;
  /* TODO: deep copy ?? */
  new_value->env_lex = old_value->env_lex;
  return new_value;
};

DSVector *CreateVector(unsigned int length) {
  DSVector *new_vec = malloc(sizeof(DSVector));
  new_vec->length = length;
  new_vec->values = calloc(length, sizeof(DSValue*));
  return new_vec;
}

DSVector *CopyVector(DSVector *old_vector, unsigned int depth) {
  unsigned int i = 0;
  /* TODO(cskau): should this be here ?? */
  /*
  if (old_vector == NULL) {
    return old_vector;
  }
  */
  DSVector *new_vector = malloc(sizeof(DSVector));
  if (old_vector == NULL) {
    new_vector->length = 0;
    new_vector->vectors = NULL;
  } else {
    new_vector->length = old_vector->length;
    if (depth > 0) {
      new_vector->vectors = malloc(new_vector->length * sizeof(DSVector*));
      for (i = 0; i < new_vector->length ; i++) {
        new_vector->vectors[i] = CopyVector(old_vector->vectors[i], (depth - 1));
      }
    } else {
      new_vector->values = malloc(new_vector->length * sizeof(DSValue*));
      for (i = 0; i < new_vector->length ; i++) {
        new_vector->values[i] = CopyValue(old_vector->values[i]);
      }
    }
  }
  return new_vector;
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


/* 5.3 The Instructions */

#define  OP_NOP            0
#define  OP_LOAD           1
#define  OP_MOVE           2
#define  OP_NEW_VEC        3
#define  OP_EXTEND         4
#define  OP_JUMP           5
#define  OP_JUMP_IF_FALSE  6
#define  OP_TAIL_CALL      7
#define  OP_CALL           8
#define  OP_RETURN         9


/* 6 Builtin Library Functions */

/* Integers: */
#define  LIB_INTEGERQ              0
#define  LIB_PLUS                  1
#define  LIB_MINUS                 2
#define  LIB_TIMES                 3
#define  LIB_QUOTIENT              4
#define  LIB_REMAINDER             5
#define  LIB_LT                    6
#define  LIB_LE                    7
#define  LIB_EQ                    8
#define  LIB_GE                    9
#define  LIB_GT                   10
/* Booleans: */
#define  LIB_BOOLEANQ             11
/* Symbols: */
#define  LIB_SYMBOLQ              12
/* Characters: */
#define  LIB_CHARQ                13
#define  LIB_CHAR_INTEGER         14
#define  LIB_INTEGER_CHAR         15
/* Strings: */
#define  LIB_STRING               16
#define  LIB_MAKE_STRING          17
#define  LIB_STRINGQ              18
#define  LIB_STRING_LENGTH        19
#define  LIB_STRING_APPEND        20
#define  LIB_STRINGEQ             21
#define  LIB_STRING_REF           22
#define  LIB_STRING_SYMBOL        23
#define  LIB_SYMBOL_STRING        24
/* Pairs: */
#define  LIB_PAIRQ                25
#define  LIB_CONS                 26
#define  LIB_CAR                  27
#define  LIB_CDR                  28
#define  LIB_SET_CARB             29
#define  LIB_SET_CDRB             30
/* Lists: */
#define  LIB_NULLQ                31
/* Vectors: */
#define  LIB_VECTOR               32
#define  LIB_MAKE_VECTOR          33
#define  LIB_VECTORQ              34
#define  LIB_VECTOR_LENGTH        35
#define  LIB_VECTOR_REF           36
#define  LIB_VECTOR_SETB          37
/* Procedures: */
#define  LIB_PROCEDUREQ           38
#define  LIB_APPLY                39
/* Misc: */
#define  LIB_EQVQ                 40
/* Control: */
#define  LIB_CALL_CC              41
#define  LIB_EXIT                 42
/* Input: */
#define  LIB_OPEN_INPUT_FILE      43
#define  LIB_INPUT_PORTQ          44
#define  LIB_CLOSE_INPUT_PORT     45
#define  LIB_CURRENT_INPUT_PORT   46
#define  LIB_READ_CHAR            47
#define  LIB_PEEK_CHAR            48
#define  LIB_EOF_OBJECTQ          49
/* Output: */
#define  LIB_OPEN_OUTPUT_FILE     50
#define  LIB_OUTPUT_PORTQ         51
#define  LIB_CLOSE_OUTPUT_PORT    52
#define  LIB_CURRENT_OUTPUT_PORT  53
#define  LIB_WRITE_CHAR           54
