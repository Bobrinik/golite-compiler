#ifndef __TYPECHECK_H
#define __TYPECHECK_H
#include "tree.h"

typedef struct EXP_TYPE_LIST EXP_TYPE_LIST;

struct EXP_TYPE_LIST {
        TYPE *type;
        EXP_TYPE_LIST *next;
};


/* ============= Utils for working with Types ============ */
bool   isTypeNodeKindOf(TYPE *type, TypeK kind);
bool   isBaseKindOf(TYPE *type, BaseK kind);
TYPE * getType(TYPE *type, TypeK kind);
TYPE * resolveType(TYPE *type, int lineno);

/* ============= Top Declarations ============ */
void typecheck_Program(PROGRAM *root);

void typecheck_Top_Declaration_List(TOP_DECLARATION_LIST *list);
void typecheck_Top_Declaration(TOP_DECLARATION *dclr);
void typecheck_Var_Declaration(VAR_DECLARATION *dclr);

void typecheck_Declaration_list(DECLARATION_LIST *list);
void typecheck_Declaration(DECLARATION *declaration);


/* ============= FUNCTION DECLARATION ============ */

void typecheck_Function_Declaration(FUNCTION_DECLARATION *fun_dclr);

/* ============= BRACKETS ============ */

//void typecheck_Bracket(BRACKET *bracket);
//void typecheck_Bracket_list(BRACKET_LIST *list);


/* ============ IDENTIFIERS ============ */

void typecheck_Identifier_list(IDENTIFIER_LIST *list, BRACKET_LIST *brackets, char *type);

/* ============ STATEMENTS ============ */

void typecheck_STMTS(FUNCTION_DECLARATION *fun_dclr, STMTS *stmts);
void typecheck_Expression_Statement(SIMPLE_STMT *simple_stmt);
void typecheck_Return_Statement(FUNCTION_DECLARATION *fun_dclr, EXP *exp, int lineno);
void typecheck_Short_Var_Decl(SIMPLE_STMT *simple_stmt);
void typecheck_Assignment_List(SIMPLE_STMT *simple_stmt);
TYPE *typecheck_Op_Assignment(SIMPLE_STMT *simple_stmt);
void typecheck_Print_Statement(EXP_LIST *print);
void typecheck_Println_Statement(EXP_LIST *println);
void typecheck_For_Statement(FUNCTION_DECLARATION *fun_dclr, FOR_STATEMENT *for_stmt, int lineno);
void typecheck_If_Statement(FUNCTION_DECLARATION *fun_dclr, STMTS *stmt);
TYPE *typecheck_Switch_On(SWITCH_ON *switch_on);
void typecheck_Switch_Case(FUNCTION_DECLARATION *fun_dclr, SWITCH_CASE *switch_case, TYPE *cond_type, int lineno);
void typecheck_Inc_Statement(SIMPLE_STMT *simple_stmt);
void typecheck_Dec_Statement(SIMPLE_STMT *simple_stmt);

TYPE *typecheck_Builtin(BuiltInExpression *builtInExp);

void typecheck_Simple_Stmt(SIMPLE_STMT *simple_stmt);
// void symbol_Clause(CLAUSE *c);


/* ============ EXPRESSION ============ */

TYPE *typecheck_expression(EXP *e);
EXP_TYPE_LIST *typecheck_expression_list(EXP_LIST *exp_list, bool isLHS);
TYPE *typecheck_Primary_Expression(PrimaryExpression *primaryExp);
TYPE *typecheck_Primary_Expression_arguments(PrimaryExpression *primaryExp);
TYPE *typecheck_Primary_Expression_selector(PrimaryExpression *primaryExp);
TYPE *typecheck_Primary_Expression_index(PrimaryExpression *primaryExp);
TYPE *typecheck_Unary_Op(UnaryOpExpression *unaryOpExp);
TYPE *typecheck_Binary_Op(BinaryOpExpression *binaryOpExp);

#endif
