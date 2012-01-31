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
void LoadDSBOrDie(char *fname) {
  uint16_t max_glo, max_tmp, max_res;
  uint32_t string_count, string_length;
  uint32_t symbol_count, symbol_length;
  uint32_t lambda_count, lambda_code;
  int8_t lambda_arity;
  uint32_t code_length;
  uint32_t i;

  unsigned char *strings;
  unsigned char *symbols;
  unsigned char *lambdas;
  unsigned char *instructions;
  unsigned char *compiler_signature;

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
  max_glo = Read16OrDie(file);
  max_tmp = Read16OrDie(file);
  max_res = Read16OrDie(file);

  /* String pool */
  string_count = Read32OrDie(file);
  strings = malloc(string_count * sizeof(unsigned char*));
  for (i = 0; i < string_count; i++) {
    string_length = Read32OrDie(file);
    strings[i] = malloc(string_length * sizeof(char));
    fread((char*)strings[i], sizeof(char), string_length, file);
  }
  if (Read8OrDie(file) != VML_EOS) {
    printf("Error: After strings, EOS missing.\n");
    exit(-1);
  }

  /* Symbol pool */
  symbol_count = Read32OrDie(file);
  symbols = malloc(symbol_count * sizeof(unsigned char*));
  for (i = 0; i < symbol_count; i++) {
    symbol_length = Read32OrDie(file);
    symbols[i] = malloc(symbol_length * sizeof(char));
    fread((char*)symbols[i], sizeof(char), symbol_length, file);
  }
  if (Read8OrDie(file) != VML_EOS) {
    printf("Error: After symbols, EOS missing.\n");
    exit(-1);
  }

  /* Lambda abstraction table */
  lambda_count = Read32OrDie(file);
  lambdas = malloc(lambda_count * sizeof(unsigned char*));
  for (i = 0; i < symbol_count; i++) {
    lambda_arity = (int8_t)Read8OrDie(file);
    lambda_code = Read32OrDie(file);
  }
  if (Read8OrDie(file) != VML_EOS) {
    printf("Error: After lambdas, EOS missing.\n");
    exit(-1);
  }

  /* Code */
  code_length = Read32OrDie(file);
  instructions = malloc(code_length * sizeof(unsigned char*));
  fread(instructions, sizeof(char), code_length, file);
  if (Read8OrDie(file) != VML_EOS) {
    printf("Error: After code, EOS missing.\n");
    exit(-1);
  }

  /* Compiler Signature */
  string_length = Read32OrDie(file);
  compiler_signature = malloc(string_length * sizeof(char));
  fread(compiler_signature, sizeof(char), string_length, file);

  if (Read32OrDie(file) != VML_MAGIC) {
    printf("Error: Magic missing at end of file.\n");
    exit(-1);
  }

  printf("%s\n", compiler_signature);

  fclose(file);
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Usage: dsvm <program.dsb>\n");
    exit(-1);
  }

  LoadDSBOrDie(argv[1]);

  exit(0);
}