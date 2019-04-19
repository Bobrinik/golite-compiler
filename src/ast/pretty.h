#ifndef __PRETTY_H
#define __PRETTY_H
#include "tree.h"

// helper functions based on tree.h. passed return statements as arguments
void pprint_Program(PROGRAM *arg);
void pprint_Package(PACKAGE *arg);
void pprint_Top_Declaration(TOP_DECLARATION *arg);
void pprint_Top_Declaration_for_var(TOP_DECLARATION *arg);
void pprint_Top_Declaration_for_type(TOP_DECLARATION *arg);
void pprint_Top_Declaration_List(TOP_DECLARATION_LIST *arg);
void pprint_Declaration_id_list_type(DECLARATION *arg);
void pprint_Declaration_id_list_array(DECLARATION *arg);
void pprint_Declaration_id_list_exp_list(DECLARATION *arg);
void pprint_Declaration_id_list_type_exp_list(DECLARATION *arg);
void pprint_Declaration_list(DECLARATION_LIST *arg);
void pprint_Empty_Bracket(BRACKET *arg);
void pprint_Bracket(BRACKET *arg);

void pprint_Bracket_list(BRACKET_LIST *arg);
void pprint_Type_Declaration_list(TYPE_DECLARATION_LIST *arg);
void pprint_Type_Declaration(TYPE_DECLARATION *arg);
void pprint_Type_Struct_Declaration(TYPE_DECLARATION *arg);
void pprint_Var_Declaration_list(VAR_DECLARATION *arg);
void pprint_Var_Declaration(VAR_DECLARATION *arg);
void pprint_Var_Decl_Statement(STMTS *arg);
void pprint_Type_Decl_Statement(STMTS *arg);

void pprint_Declaration(DECLARATION *declaration);

void pprint_EXP_LIST(EXP_LIST *list);
void pprint_STMTS(STMTS *list);

void pprint_If_Statement(STMTS *if_stmt);
void pprint_Switch_On(SWITCH_ON *switch_on);
void pprint_Switch_Case(SWITCH_CASE *switch_case); 
void pprint_For_Statement(FOR_STATEMENT *for_stmt); 
void pprint_Print_Statement(EXP_LIST *print);
void pprint_Println_Statement(EXP_LIST *println); 
void pprint_Return_Statement(EXP *exp);
void pprint_Assignment_List(SIMPLE_STMT *simple_stmt); 
void pprint_Expression_Statement(SIMPLE_STMT *simple_stmt); 
void pprint_Inc_Statement(SIMPLE_STMT *simple_stmt);
void pprint_Dec_Statement(SIMPLE_STMT *simple_stmt);
void pprint_Short_Var_Decl(SIMPLE_STMT *simple_stmt); 
void pprint_Assignment_List(SIMPLE_STMT *simple_stmt); 
void pprint_Op_Assignment(SIMPLE_STMT *simple_stmt);
void pprint_Simple_Stmt(SIMPLE_STMT *simple_stmt);

void pprint_Function_Declaration(FUNCTION_DECLARATION *arg);
void pprint_Signature(SIGNATURE *arg);
void pprint_Parameter_Unit(PARAMETER_UNIT *arg);
void pprint_Parameter_List(PARAMETER_LIST *arg);
void pprint_result(RESULT *result);

/* Expression */

void pprint_expression(EXP *e);
void pprint_expression_list(EXP_LIST *exp_list);

void pprint_Primary_Expression(PrimaryExpression *primaryExp);
void pprint_Primary_Expression_arguments(PrimaryExpression *primaryExp);
void pprint_Primary_Expression_selector(PrimaryExpression *primaryExp);
void pprint_Primary_Expression_index(PrimaryExpression *primaryExp);
void pprint_Unary_Op(UnaryOpExpression *unaryOpExp);
void pprint_Binary_Op(BinaryOpExpression *binaryOpExp);
void pprint_Builtin(BuiltInExpression *builtInExp);

#endif
