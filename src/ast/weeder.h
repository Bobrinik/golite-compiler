#ifndef __WEEDER_H
#define __WEEDER_H
#include "tree.h"

// helper functions based on tree.h. passed return statements as arguments
void weed_Program(PROGRAM *arg);
void weed_Package(PACKAGE *arg);
void weed_Top_Declaration(TOP_DECLARATION *arg);
void weed_Top_Declaration_for_var(TOP_DECLARATION *arg);
void weed_Top_Declaration_for_type(TOP_DECLARATION *arg);
void weed_Top_Declaration_List(TOP_DECLARATION_LIST *arg);
void weed_Declaration_id_list_type(DECLARATION *arg);
void weed_Declaration_id_list_array(DECLARATION *arg);
void weed_Declaration_id_list_exp_list(DECLARATION *arg);
void weed_Declaration_id_list_type_exp_list(DECLARATION *arg);
void weed_Declaration_list(DECLARATION_LIST *arg);
void weed_Empty_Bracket(BRACKET *arg);
void weed_Bracket(BRACKET *arg);

void weed_Bracket_list(BRACKET_LIST *arg);
void weed_Type_Declaration_list(TYPE_DECLARATION_LIST *arg);
void weed_Type_Declaration(TYPE_DECLARATION *arg);
void weed_Type_Struct_Declaration(TYPE_DECLARATION *arg);
void weed_Var_Declaration_list(VAR_DECLARATION *arg);
void weed_Var_Declaration(VAR_DECLARATION *arg);
void weed_Var_Decl_Statement(STMTS *arg);
void weed_Type_Decl_Statement(STMTS *arg);

void weed_Declaration(DECLARATION *declaration);

void weed_EXP_LIST(EXP_LIST *list);
void weed_STMTS(STMTS *list);

void weed_If_Statement(STMTS *if_stmt, int lineno);
void weed_Switch_On(SWITCH_ON *switch_on, int lineno);
void weed_Switch_Case(SWITCH_CASE *switch_case, int num, int lineno);
void weed_For_Statement(FOR_STATEMENT *for_stmt, int lineno);
void weed_Print_Statement(EXP_LIST *print);
void weed_Println_Statement(EXP_LIST *println);
void weed_Return_Statement(EXP *exp, int num, int lineno);
void weed_Assignment_List(SIMPLE_STMT *simple_stmt, int lineno);
void weed_Expression_Statement(SIMPLE_STMT *simple_stmt, int lineno);
void weed_Inc_Statement(SIMPLE_STMT *simple_stmt);
void weed_Dec_Statement(SIMPLE_STMT *simple_stmt);
void weed_Short_Var_Decl(SIMPLE_STMT *simple_stmt, int lineno);
void weed_Op_Assignment(SIMPLE_STMT *simple_stmt);
void weed_Simple_Stmt(SIMPLE_STMT *simple_stmt, int lineno);

void weed_Function_Declaration(FUNCTION_DECLARATION *arg);
void weed_Signature(SIGNATURE *arg);
void weed_Parameter_Unit(PARAMETER_UNIT *arg);
void weed_Parameter_List(PARAMETER_LIST *arg);
void weed_result(RESULT *result);

void weed_expression(EXP *e);
int weed_expression_list(EXP_LIST *exp_list);

void weed_Primary_Expression(PrimaryExpression *primaryExp);
void weed_Primary_Expression_arguments(PrimaryExpression *primaryExp);
void weed_Primary_Expression_selector(PrimaryExpression *primaryExp);
void weed_Primary_Expression_index(PrimaryExpression *primaryExp);
void weed_Unary_Op(UnaryOpExpression *unaryOpExp);
void weed_Binary_Op(BinaryOpExpression *binaryOpExp);
void weed_Builtin(BuiltInExpression *builtInExp);

#endif
