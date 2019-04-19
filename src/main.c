#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "ast/tree.h"
#include "ast/pretty.h"
#include "ast/weeder.h"
#include "ast/symbol.h"
#include "ast/typecheck.h"
#include "ast/codegen.h"

#include "utils/mode.h"


PROGRAM *root = NULL;

int yylex();
int yyparse();

bool modeToken   = false;
bool modeParse   = false;
bool modeScan    = false;
bool modePretty  = false;
bool modeSymbol  = false;
bool modeType    = false;
bool modeCodegen = false;

bool isParseError = false;

FILE *fp;

int main(int argc, char *argv[]) {

  if (argc != 2 && argc != 3) {
    fprintf(stderr, "Error: Provide correct arguments [scan|tokens|parse|pretty|symbol|typecheck|codegen filename.min]");
    return 1;
  }

  if (strcmp(argv[1], "scan") == 0) {
    // Outputs OK if the input is lexically correct, or an appropriate error message
    modeScan = true;
  } else if (strcmp(argv[1], "tokens") == 0) {
    modeToken = true;
  } else if (strcmp(argv[1], "parse") == 0) {
    modeParse = true;
  } else if (strcmp(argv[1], "pretty") == 0) {
    modePretty = true;
  } else if (strcmp(argv[1], "symbol") == 0) {
    modeSymbol = true;
  } else if(strcmp(argv[1], "typecheck") == 0){
    modeType = true;
  } else if(strcmp(argv[1], "codegen") == 0) {
    modeCodegen = true;
  } else {
    fprintf(stderr, "Error: Provide correct arguments [scan|tokens|parse|pretty|symbol|typecheck|codegen");
    return 1;
  }

  yyparse();
  weed_Program(root);


  // need to return 1 when error occurs else invalid tests are not passing
  if (isParseError) exit(1);

  if (modeParse  && !isParseError) printf("OK\n");

  if (modePretty && !isParseError) pprint_Program(root);

  if (modeScan) printf("OK\n"); // if errors are in scanner, this part  wont be reache

  if (modeType || modeSymbol || modeCodegen) symbol_Program(root);

  if (modeType) {
    typecheck_Program(root);
    printf("OK\n");
  }

  if (modeCodegen){
    typecheck_Program(root);

    char* filename = strcat(argv[2], ".js");
    fp = fopen(filename, "w");
    codegen_Program(root);
    fclose(fp);
    printf("OK\n");
  }

  exit(0);
  return 0;
}
