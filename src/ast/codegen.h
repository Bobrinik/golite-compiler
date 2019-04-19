#ifndef __CODEGEN_H
#define __CODEGEN_H
#include "tree.h"

typedef struct ENVIRONMENT ENVIRONMENT;
typedef struct RECORD       RECORD;

#define HashSize 317
RECORD *table[HashSize];


struct ENVIRONMENT {
  int depth;
  RECORD *record;
  RECORD *table[HashSize];
  ENVIRONMENT *parent;
};

struct RECORD {
  bool isHiding;
  bool isFunction;
  char *name; // it's a key
  char *codegen_name;
  RECORD *hidden_record;
  RECORD *next;
};


// helper functions based on tree.h. passed return statements as arguments
void codegen_Program(PROGRAM *arg);
void codegen_Package(PACKAGE *arg);
void codegen_Top_Declaration(TOP_DECLARATION *arg, ENVIRONMENT *env);
void codegen_Top_Declaration_for_var(TOP_DECLARATION *arg);
void codegen_Top_Declaration_for_type(TOP_DECLARATION *arg);
void codegen_Top_Declaration_List(TOP_DECLARATION_LIST *arg, ENVIRONMENT *env);

void codegen_Declaration_id_list_type(DECLARATION *arg);
void codegen_Declaration_id_list_array(DECLARATION *arg);
void codegen_Declaration_id_list_exp_list(DECLARATION *arg);
void codegen_Declaration_id_list_type_exp_list(DECLARATION *arg);
void codegen_Declaration_list(DECLARATION_LIST *arg, ENVIRONMENT *env);
void codegen_Empty_Bracket(BRACKET *arg);
void codegen_Bracket(BRACKET *arg);

void codegen_Bracket_list(BRACKET_LIST *arg);
void codegen_Type_Declaration_list(TYPE_DECLARATION_LIST *arg);
void codegen_Type_Declaration(TYPE_DECLARATION *arg);
void codegen_Type_Struct_Declaration(TYPE_DECLARATION *arg);
void codegen_Var_Declaration_list(VAR_DECLARATION *arg);
void codegen_Var_Declaration(VAR_DECLARATION *arg, ENVIRONMENT *env);
void codegen_Var_Decl_Statement(STMTS *arg);
void codegen_Type_Decl_Statement(STMTS *arg);

void codegen_Declaration(DECLARATION *declaration, ENVIRONMENT *env);

void codegen_EXP_LIST(EXP_LIST *list);
void codegen_STMTS(STMTS *list, ENVIRONMENT *env);

void codegen_If_Statement(STMTS *if_stmt, ENVIRONMENT *env);
void codegen_Switch_On(SWITCH_ON *switch_on, ENVIRONMENT *env);
void codegen_Switch_Case(SWITCH_CASE *switch_case, ENVIRONMENT *env); 
void codegen_For_Statement(FOR_STATEMENT *for_stmt, ENVIRONMENT *env); 
void codegen_Print_Statement(EXP_LIST *print, ENVIRONMENT *env);
void codegen_Println_Statement(EXP_LIST *println, ENVIRONMENT *env);
void codegen_Return_Statement(EXP *exp, ENVIRONMENT *env);
void codegen_Assignment_List(SIMPLE_STMT *simple_stmt, ENVIRONMENT *env); 
void codegen_Expression_Statement(SIMPLE_STMT *simple_stmt, ENVIRONMENT *env); 
void codegen_Inc_Statement(SIMPLE_STMT *simple_stmt, ENVIRONMENT *env);
void codegen_Dec_Statement(SIMPLE_STMT *simple_stmt, ENVIRONMENT *env);
void codegen_Short_Var_Decl(SIMPLE_STMT *simple_stmt, ENVIRONMENT *env); 
void codegen_Op_Assignment(SIMPLE_STMT *simple_stmt, ENVIRONMENT *env);
void codegen_Simple_Stmt(SIMPLE_STMT *simple_stmt, ENVIRONMENT *env);

void codegen_Function_Declaration(FUNCTION_DECLARATION *arg, ENVIRONMENT *env);
void codegen_Signature(SIGNATURE *arg, ENVIRONMENT *env);
void codegen_Parameter_Unit(PARAMETER_UNIT *arg, ENVIRONMENT *env);
void codegen_Parameter_List(PARAMETER_LIST *arg, ENVIRONMENT *env);
void codegen_result(RESULT *result, ENVIRONMENT *env);

/* Expression */

void codegen_expression(EXP *e, ENVIRONMENT *env);
void codegen_expression_list(EXP_LIST *exp_list, ENVIRONMENT *env);

void codegen_Primary_Expression(PrimaryExpression *primaryExp, ENVIRONMENT *env);
void codegen_Primary_Expression_arguments(PrimaryExpression *primaryExp, ENVIRONMENT *env);
void codegen_Primary_Expression_selector(PrimaryExpression *primaryExp, ENVIRONMENT *env);
void codegen_Primary_Expression_index(PrimaryExpression *primaryExp, ENVIRONMENT *env);
void codegen_Unary_Op(UnaryOpExpression *unaryOpExp, ENVIRONMENT *env);
void codegen_Binary_Op(BinaryOpExpression *binaryOpExp, ENVIRONMENT *env);
void codegen_Builtin(BuiltInExpression *builtInExp, ENVIRONMENT *env);

#endif
