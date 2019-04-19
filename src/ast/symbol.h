#ifndef __SYMBOL_H
#define __SYMBOL_H
#include "tree.h"

#define HashSize 317
typedef struct PROGRAM      PROGRAM;
typedef struct SYMBOL_TABLE SYMBOL_TABLE;
typedef struct SYMBOL       SYMBOL;
typedef struct FUNCTION     FUNCTION;
typedef struct PARAMETER    PARAMETER;
typedef struct FIELD        FIELD;
typedef enum   SymbolK      SymbolK;
typedef enum   CategoryK    CategoryK;
typedef enum   TypeK        TypeK;
typedef enum   BaseK        BaseK;

enum BaseK {
  boolK,
  intK,
  float64K,
  runeK,
  stringK
};

enum TypeK {
  sliceK,
  refK,
  arrayK,
  structK,
  baseK
};

struct TYPE {
  TypeK kind;
  union {
    TYPE *slice;
    struct { char *id;   TYPE *element;} ref;
    struct { int   size; TYPE  *element;} array;
    FIELD *field; // for struct
    BaseK baseType;
  } val;
};
struct PARAMETER {
  TYPE *type;
  PARAMETER *nxt;
};

struct FUNCTION {
  TYPE *result;
  PARAMETER *parameter;
};


enum CategoryK {
  category_constant,
  category_type,
  category_variable,
  category_field_name,
  category_function
};

struct SYMBOL {
  char *name; // name of the identifier?
  CategoryK category; // from specification
  union {
    BaseK constant;
    TYPE *type;
    FUNCTION *function;
    struct { TYPE *type; BRACKET_LIST *brackets; } variable_declaration;
  } val;
  struct SYMBOL *next;
};



struct FIELD {
  char  *name;
  TYPE  *type;
  FIELD *nxt;
};


struct SYMBOL_TABLE {
  int depth;
  SYMBOL *table[HashSize];
  SYMBOL_TABLE *parent;
};

void print_Type(TYPE *type, bool isRecursivePrint);
void symbol_Program(PROGRAM *root);
int in_Current_Scope(SYMBOL *symbol, SYMBOL_TABLE *t);
SYMBOL *get_Symbol(SYMBOL *symbol, SYMBOL_TABLE *t);
SYMBOL * new_Symbol(char *name);

void symbol_Top_Declaration_List(TOP_DECLARATION_LIST *list, SYMBOL_TABLE *t);
void symbol_Top_Declaration(TOP_DECLARATION *dclr, SYMBOL_TABLE *t);

void symbol_Type_Declaration_List(bool isTop, TYPE_DECLARATION_LIST *list, SYMBOL_TABLE *t);
void symbol_Type_Declaration(bool isTop, TYPE_DECLARATION *declaration, SYMBOL_TABLE *t);

void symbol_Var_Declaration(bool isTop, VAR_DECLARATION *dclr, SYMBOL_TABLE *t);

void symbol_Declaration_list(bool isTop, DECLARATION_LIST *list, SYMBOL_TABLE *t);
void symbol_Declaration(bool isTop, DECLARATION *declaration, SYMBOL_TABLE *t);


/* ============= FUNCTION DECLARATION ============ */

void symbol_Function_Declaration(FUNCTION_DECLARATION *fun_dclr, SYMBOL_TABLE *t);


void print_Parameter_List(PARAMETER_LIST *parameter_list);
void print_Parameter_Unit(PARAMETER_UNIT *unit);
void print_function_returnType(RESULT *result);

// void print_Identifier_list(IDENTIFIER_LIST *list);



/* ============= BRACKETS ============ */

void print_Bracket(BRACKET *bracket);
void print_Bracket_list(BRACKET_LIST *list);


/* ============ IDENTIFIERS ============ */

void symbol_Identifier_list(IDENTIFIER_LIST *list, BRACKET_LIST *brackets, char *type, SYMBOL_TABLE *t);

/* ============ STATEMENTS ============ */

void symbol_STMTS(STMTS *stmts, SYMBOL_TABLE *t);
void symbol_Expression_Statement(SIMPLE_STMT *simple_stmt, SYMBOL_TABLE *t);
void symbol_Return_Statement(EXP *exp, SYMBOL_TABLE *t);
void symbol_Short_Var_Decl(SIMPLE_STMT *simple_stmt, SYMBOL_TABLE *t);
void symbol_Assignment_List(SIMPLE_STMT *simple_stmt, SYMBOL_TABLE *t);
void symbol_Op_Assignment(SIMPLE_STMT *simple_stmt, SYMBOL_TABLE *t);
void symbol_Print_Statement(EXP_LIST *print, SYMBOL_TABLE *t);
void symbol_Println_Statement(EXP_LIST *println, SYMBOL_TABLE *t);
void symbol_For_Statement(FOR_STATEMENT *for_stmt, SYMBOL_TABLE *t);
void symbol_If_Statement(STMTS *stmt, SYMBOL_TABLE *t);
void symbol_Switch_On(SWITCH_ON *switch_on, SYMBOL_TABLE *t);
void symbol_Switch_Case(SWITCH_CASE *switch_case, SYMBOL_TABLE *t);
void symbol_Inc_Statement(SIMPLE_STMT *simple_stmt, SYMBOL_TABLE *t);
void symbol_Dec_Statement(SIMPLE_STMT *simple_stmt, SYMBOL_TABLE *t);

void symbol_Builtin(BuiltInExpression *builtInExp, SYMBOL_TABLE *t);

void symbol_Simple_Stmt(SIMPLE_STMT *simple_stmt, SYMBOL_TABLE *t);
// void symbol_Clause(CLAUSE *c, SYMBOL_TABLE *t);


/* ============ EXPRESSION ============ */

void symbol_expression(EXP *e, SYMBOL_TABLE *t);
void symbol_expression_list(EXP_LIST *exp_list, SYMBOL_TABLE *t);
void symbol_Primary_Expression(PrimaryExpression *primaryExp, SYMBOL_TABLE *t);
void symbol_Primary_Expression_arguments(PrimaryExpression *primaryExp, SYMBOL_TABLE *t);
void symbol_Primary_Expression_selector(PrimaryExpression *primaryExp, SYMBOL_TABLE *t);
void symbol_Primary_Expression_index(PrimaryExpression *primaryExp, SYMBOL_TABLE *t);
void symbol_Unary_Op(UnaryOpExpression *unaryOpExp, SYMBOL_TABLE *t);
void symbol_Binary_Op(BinaryOpExpression *binaryOpExp, SYMBOL_TABLE *t);

#endif
