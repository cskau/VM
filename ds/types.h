#include <stdint.h>

#define  VML_MAGIC  0xDA15CE03
#define  VML_EOS    0x80

/* 5.2 Types and Values */

/* non-negative value for the lexical scope corresponding to that number */
#define  SCP_LIB  -1
#define  SCP_GLO  -2
#define  SCP_RES  -3
#define  SCP_TMP  -4
#define  SCP_VEC  -5

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
};

typedef struct {
  enum ValueTypes type;
  union {
    int32_t value;
    uint32_t index;
    char code;
  };
} DSValue;

DSValue *CreateValue(enum ValueTypes type, uint32_t value) {
  DSValue *new_value = malloc(sizeof(DSValue));
  new_value->type = type;
  if (new_value->type == INT) {
    new_value->value = (int32_t)value;
  } else if (new_value->type == CHAR) {
    new_value->code = (char)value;
  } else {
    new_value->index = (uint32_t)value;
  }
  return new_value;
};


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
#define  INTEGERQ              0
#define  PLUS                  1
#define  MINUS                 2
#define  TIMES                 3
#define  QUOTIENT              4
#define  REMAINDER             5
#define  LT                    6
#define  LE                    7
#define  EQ                    8
#define  GE                    9
#define  GT                   10
/* Booleans: */
#define  BOOLEANQ             11
/* Symbols: */
#define  SYMBOLQ              12
/* Characters: */
#define  CHARQ                13
#define  CHAR_INTEGER         14
#define  INTEGER_CHAR         15
/* Strings: */
#define  STRING               16
#define  MAKE_STRING          17
#define  STRINGQ              18
#define  STRING_LENGTH        19
#define  STRING_APPEND        20
#define  STRINGEQ             21
#define  STRING_REF           22
#define  STRING_SYMBOL        23
#define  SYMBOL_STRING        24
/* Pairs: */
#define  PAIRQ                25
#define  CONS                 26
#define  CAR                  27
#define  CDR                  28
#define  SET_CARB             29
#define  SET_CDRB             30
/* Lists: */
#define  NULLQ                31
/* Vectors: */
#define  VECTOR               32
#define  MAKE_VECTOR          33
#define  VECTORQ              34
#define  VECTOR_LENGTH        35
#define  VECTOR_REF           36
#define  VECTOR_SETB          37
/* Procedures: */
#define  PROCEDUREQ           38
#define  APPLY                39
/* Misc: */
#define  EQVQ                 40
/* Control: */
#define  CALL_CC              41
#define  EXIT                 42
/* Input: */
#define  OPEN_INPUT_FILE      43
#define  INPUT_PORTQ          44
#define  CLOSE_INPUT_PORT     45
#define  CURRENT_INPUT_PORT   46
#define  READ_CHAR            47
#define  PEEK_CHAR            48
#define  EOF_OBJECTQ          49
/* Output: */
#define  OPEN_OUTPUT_FILE     50
#define  OUTPUT_PORTQ         51
#define  CLOSE_OUTPUT_PORT    52
#define  CURRENT_OUTPUT_PORT  53
#define  WRITE_CHAR           54
