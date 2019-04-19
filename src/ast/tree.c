#include "tree.h"
#include <stdlib.h>
#include <stdio.h>
#include "../utils/memory.h"
#include "../utils/util.h"

void assert_same_size(IDENTIFIER_LIST *ids, EXP_LIST *exps, int lineno){
  int l_count = 0, r_count = 0;

  while((ids = ids->nxt)) l_count++;
  while((exps = exps->nextExp)) r_count++;
  
  if(l_count != r_count) {
    fprintf(stderr,"Error: (line %d) variable declaration lhs(%d) != rhs(%d)", lineno, l_count, r_count);
    exit(1);
  }
}

SYMBOL * new_Identifier(char *name, int lineno) {
  SYMBOL *smbl = new_Symbol(name);
  return smbl;
}

PROGRAM * new_Program(PACKAGE *package, TOP_DECLARATION_LIST *top_declr_list) {
  PROGRAM *prog = NEW(PROGRAM);

  prog->package = package;
  prog->top_declaration_list = top_declr_list;

  return prog;
}

PACKAGE * new_Package(char *name, int lineno) {
  if (isBlankIdentifier(name)) {
    fprintf(stderr, "Error: (line %d) package name may not contain the blank identifier\n", lineno);
    exit(1);
  }

  PACKAGE *pckg = NEW(PACKAGE);

  pckg->name = name;
  pckg->lineno = lineno;

  return pckg;
}

IDENTIFIER_LIST * new_Identifier_list(char *current, IDENTIFIER_LIST *nxt, int lineno) {
  IDENTIFIER_LIST *lst = NEW(IDENTIFIER_LIST);
  
  lst->symbol = new_Symbol(current);
  lst->nxt = nxt;
  lst->lineno = lineno;

  return lst;
}


TOP_DECLARATION * new_Top_Declaration_for_var(VAR_DECLARATION *declaration) {
  TOP_DECLARATION *top_declr = NEW(TOP_DECLARATION);

  top_declr->kind = varK;
  top_declr->val.var_dclr = declaration;

  return top_declr;
}

TOP_DECLARATION * new_Top_Declaration_for_type(TYPE_DECLARATION_LIST *declaration) {
  TOP_DECLARATION *top_declr = NEW(TOP_DECLARATION);

  top_declr->kind = typeK;
  top_declr->val.type_dclr_list = declaration;

  return top_declr;
}

TOP_DECLARATION * new_Top_Declaration_for_func(FUNCTION_DECLARATION *func){
  TOP_DECLARATION *top_declr = NEW(TOP_DECLARATION);

  top_declr->kind = funcK;
  top_declr->val.function_dclr = func;

  return top_declr;
}


TOP_DECLARATION_LIST * new_Top_Declaration_List( TOP_DECLARATION *decl, TOP_DECLARATION_LIST *nxt) {
  TOP_DECLARATION_LIST *declr_list = NEW(TOP_DECLARATION_LIST);

  declr_list->top_declaration = decl;
  declr_list->nxt = nxt;

  return declr_list;
}

FUNCTION_DECLARATION * new_Function_Declaration(SYMBOL *name, SIGNATURE *signature, STMTS *body,int lineno){
  FUNCTION_DECLARATION *func_declr = NEW(FUNCTION_DECLARATION);
  
  func_declr->name = name;
  func_declr->signature = signature;
  func_declr->body = body;
  func_declr->lineno = lineno;

  return func_declr;
}

PARAMETER_UNIT * new_Parameter_Unit(IDENTIFIER_LIST *ids_list, SYMBOL *type) {
  PARAMETER_UNIT *unit = NEW(PARAMETER_UNIT);
  
  unit->isArray = false;
  unit->ids_list = ids_list;
  unit->brackets = NULL;
  unit->type = type;
  
  return unit;
}

PARAMETER_UNIT * new_Parameter_Unit_Array(IDENTIFIER_LIST *ids_list, BRACKET_LIST *brackets, SYMBOL *type) {
  if(brackets == NULL){
    fprintf(stderr, "Cannot be null array");
    exit(1);
  }

  PARAMETER_UNIT *unit = NEW(PARAMETER_UNIT);

  unit->isArray = true;
  unit->ids_list = ids_list;
  unit->brackets = brackets;
  unit->type = type;
  
  return unit;
}

PARAMETER_LIST * new_Parameter_List(PARAMETER_UNIT *unit, PARAMETER_LIST *nxt) {
  PARAMETER_LIST *list = NEW(PARAMETER_LIST);

  list->param = unit;
  list->nxt = nxt;
  
  return list;
}

SIGNATURE * new_Signature(PARAMETER_LIST *parameters, RESULT *result) { 
  SIGNATURE *sign = NEW(SIGNATURE);
  
  sign->parameters = parameters;
  sign->result = result;

  return sign;
}

RESULT *new_simple_result(SYMBOL *type){
  RESULT *result = NEW(RESULT);

  result->kind = simpleResultK;
  result->type = type;
  result->brackets = NULL;
  result->decl_list = NULL;

  return result;
}

RESULT *new_array_result(BRACKET_LIST *list, SYMBOL *type) {
  RESULT *result = NEW(RESULT);

  result->kind = arrayResultK;
  result->type = type;
  result->brackets = list;
  result->decl_list = NULL;

  return result;
}

RESULT *new_array_struct_result(BRACKET_LIST *b_list, DECLARATION_LIST *decl_list) {
	
	RESULT *result = NEW(RESULT);

  	result->kind = arrayStructResultK;
  	result->type = NULL;
  	result->brackets = b_list;
	result->decl_list = decl_list;

  	return result;	
}

RESULT *new_struct_result(DECLARATION_LIST *decl_list) {
	
	RESULT *result = NEW(RESULT);

        result->kind = structResultK;
        result->type = NULL;
        result->brackets = NULL;
        result->decl_list = decl_list;

        return result;
}

DECLARATION * new_Declaration_id_list_type(IDENTIFIER_LIST *ids, SYMBOL *type, int lineno) {
  DECLARATION *dclr = NEW(DECLARATION);
  dclr->lineno = lineno;

  dclr->lhs = ids;
  dclr->kind = Id_list_typek;

  dclr->type = type;


  return dclr;
}

DECLARATION * new_Declaration_id_list_array(IDENTIFIER_LIST *ids, BRACKET_LIST *brackets, SYMBOL *type ,int lineno) {
  DECLARATION *dclr = NEW(DECLARATION);
  dclr->lineno = lineno;

  dclr->lhs = ids;
  dclr->kind = Id_list_arrayk;
  
  dclr->val.indx_list = brackets;
  
  dclr->type = type;


  return dclr;
}

DECLARATION * new_Declaration_id_list_exp_list(IDENTIFIER_LIST *ids, EXP_LIST *exps, int lineno) {
  DECLARATION *dclr = NEW(DECLARATION);
  dclr->lineno = lineno;

  dclr->lhs = ids;
  dclr->kind = Id_list_eqk;

  dclr->type = NULL;
  dclr->val.rhs_list = exps;

  assert_same_size(ids, exps, lineno);

  return dclr;
}

DECLARATION * new_Declaration_id_list_type_exp_list(IDENTIFIER_LIST *ids, SYMBOL *type,  EXP_LIST *exps, int lineno) {
  DECLARATION *dclr = NEW(DECLARATION);
  dclr->lineno = lineno;

  dclr->lhs = ids;
  dclr->kind = Id_list_type_eqk;
 
  dclr->type = type;
  dclr->val.rhs_list = exps;

  assert_same_size(ids, exps, lineno);
  
  return dclr;
}

DECLARATION * new_Declaration_id_list_struct(IDENTIFIER_LIST *ids, DECLARATION_LIST *body, int lineno) {
	DECLARATION *dclr = NEW(DECLARATION);
	dclr->lineno = lineno;
	
	dclr->lhs = ids;
	dclr->kind = Id_list_structK;

	dclr->type = NULL;
	dclr->val.struct_body = body;

	return dclr;
}

DECLARATION * new_Declaration_id_list_array_struct(IDENTIFIER_LIST *ids, BRACKET_LIST *brackets, DECLARATION_LIST *body, int lineno) {
        DECLARATION *dclr = NEW(DECLARATION);
        dclr->lineno = lineno;

        dclr->lhs = ids;
        dclr->kind = Id_list_array_structK;

        dclr->type = NULL;
        dclr->val.struct_array.indx_list = brackets;
	dclr->val.struct_array.struct_body = body;

        return dclr;
}

DECLARATION * new_Declaration_id_list_array_exp_list(IDENTIFIER_LIST *ids, BRACKET_LIST *brackets, SYMBOL *type, EXP_LIST *exps, int lineno) {

	DECLARATION *dclr = NEW(DECLARATION);
        dclr->lineno = lineno;

	dclr->lhs = ids;
	dclr->kind = Id_list_array_eqK;

	dclr->type = type;
	dclr->val.array_eq.indx_list = brackets;
	dclr->val.array_eq.rhs_list = exps;

	return dclr;
}

DECLARATION_LIST * new_Declaration_list(DECLARATION *declaration, DECLARATION_LIST *nxt) {
    DECLARATION_LIST *lst = NEW(DECLARATION_LIST);

    lst->declaration = declaration;
    lst->nxt = nxt;

    return lst;
}


BRACKET * new_Empty_Bracket() {
  BRACKET *bracket = NEW(BRACKET);
  bracket->isEmpty = true;

  return bracket;
}

BRACKET * new_Bracket(char *idx) {
  BRACKET *bracket = NEW(BRACKET);
  bracket->isEmpty = false;
  bracket->idx = atoi(idx);

  return bracket;
}


BRACKET_LIST * new_Bracket_list(BRACKET *current, BRACKET_LIST *nxt) {
  BRACKET_LIST *lst = NEW(BRACKET_LIST);
  
  lst->bracket = current;
  lst->nxt = nxt;

  return lst;
}

TYPE_DECLARATION_LIST * new_Type_Declaration_list(TYPE_DECLARATION *type_dclr, TYPE_DECLARATION_LIST *nxt) {
  TYPE_DECLARATION_LIST *lst = NEW(TYPE_DECLARATION_LIST);

  lst->type_declaration = type_dclr;
  lst->nxt = nxt;

  return lst;
}

TYPE_DECLARATION * new_Type_Declaration(SYMBOL *name, SYMBOL *type, int lineno) {
  TYPE_DECLARATION *type_dclr = NEW(TYPE_DECLARATION);

  type_dclr->lineno = lineno;
  type_dclr->kind = type_simpleK;

  type_dclr->val.simple.name = name;
  type_dclr->val.simple.type = type;

  return type_dclr;
}

TYPE_DECLARATION * new_Type_Struct_Declaration(SYMBOL *name, DECLARATION_LIST *body, int lineno) {
  TYPE_DECLARATION *type_dclr = NEW(TYPE_DECLARATION);

  type_dclr->lineno = lineno;
  type_dclr->kind = type_structK;

  type_dclr->val.structure.name = name;
  type_dclr->val.structure.body = body;

  return type_dclr;
}

TYPE_DECLARATION * new_Type_Bracket_Declaration(SYMBOL *name, BRACKET_LIST *list, SYMBOL *type, int lineno) {
	TYPE_DECLARATION *type_dclr = NEW(TYPE_DECLARATION);

	type_dclr->lineno = lineno;
  	type_dclr->kind = type_arrayK;

  	type_dclr->val.array.name = name;
  	type_dclr->val.array.brackets = list;
	type_dclr->val.array.type = type;

	return type_dclr;
}

TYPE_DECLARATION * new_Type_Bracket_Struct_Declaration(SYMBOL *name, BRACKET_LIST *list,  DECLARATION_LIST *body, int lineno) {
        TYPE_DECLARATION *type_dclr = NEW(TYPE_DECLARATION);

        type_dclr->lineno = lineno;
        type_dclr->kind = type_struct_arrayK;

        type_dclr->val.struct_array.name = name;
        type_dclr->val.struct_array.brackets = list;
        type_dclr->val.struct_array.body = body;

        return type_dclr;
}

// NOT SURE WHAT THIS THING IS DOING
VAR_DECLARATION * new_Var_Declaration_list(DECLARATION_LIST *list, int lineno) {
  VAR_DECLARATION *dclr = NEW(VAR_DECLARATION);
  dclr->lineno = lineno;
  dclr->isList = true;

  dclr->val.declaration_list = list;

  return dclr;
}

VAR_DECLARATION * new_Var_Declaration(DECLARATION *decl, int lineno) {
  VAR_DECLARATION *dclr = NEW(VAR_DECLARATION);

  dclr->lineno = lineno;
  dclr->isList = false;

  dclr->val.declaration = decl;

  return dclr;
}


/* ================== Statements ====================== */

STMTS*  new_Block_Stmt(STMTS *stmts, bool isTop){
  if(isTop) return stmts;
  STMTS *s = NEW(STMTS);
  s->kind = blockK;
  s->val.block = stmts;
  return s;
}

STMTS * new_Var_Decl_Statement(VAR_DECLARATION *decl, int lineno) {
  STMTS *s = malloc(sizeof(STMTS));

  s->lineno = lineno;
  s->kind = varBodyK;
  s->val.var_dclr = decl;
  s->nextStmt = NULL;
  return s;
}

STMTS * new_Type_Decl_Statement(TYPE_DECLARATION_LIST *decl, int lineno) {
    STMTS *s = malloc(sizeof(STMTS));
    s->lineno = lineno;
    s->kind = typeBodyK;
    s->val.type_dclr = decl;
    return s;
}

STMTS * new_If_Statement(SIMPLE_STMT *simplestmt, EXP *condition, STMTS *block, STMTS *next, int lineno) {
    STMTS *s = malloc(sizeof(STMTS));
    s->lineno = lineno;
    s->kind = ifK;
    s->val.if_stmt.simplestmt = simplestmt;
    s->val.if_stmt.condition = condition;
    s->val.if_stmt.block = block;
    s->val.if_stmt.next = next;
    return s;
}

STMTS * new_Switch_Statement(SWITCH_ON *switch_on, SWITCH_CASE *switch_case, int lineno) {
    STMTS *s = malloc(sizeof(STMTS));
    s->lineno = lineno;
    s->kind = switchK;
    s->val.switch_stmt.switch_on = switch_on;
	s->val.switch_stmt.switch_case = switch_case;
    return s;
}

STMTS * new_Print_Statement(EXP_LIST *printArgs, int lineno) {
    STMTS *s = malloc(sizeof(STMTS));
    s->lineno = lineno;
    s->kind = printK;
    s->val.print = printArgs;
    return s;
}

STMTS * new_For_Statement(FOR_STATEMENT *for_stmt, int lineno) {
	STMTS *s = malloc(sizeof(STMTS));
    s->lineno = lineno;
    s->kind = forK;
    s->val.for_stmt = for_stmt;
    return s;
}

STMTS * new_Println_Statement(EXP_LIST *printArgs, int lineno) {
    STMTS *s = malloc(sizeof(STMTS));
    s->lineno = lineno;
    s->kind = printlnK;
    s->val.print = printArgs;
    return s;
}

STMTS * new_Continue_Statement(int lineno) {
    STMTS *s = malloc(sizeof(STMTS));
    s->lineno = lineno;
    s->kind = continueK;
    return s;
}

STMTS * new_Break_Statement(int lineno) {
    STMTS *s = malloc(sizeof(STMTS));
    s->lineno = lineno;
    s->kind = breakK;
    return s;
}

STMTS * new_Return_Statement(EXP *returnExp, int lineno) {
    STMTS *s = malloc(sizeof(STMTS));
    s->lineno = lineno;
    s->kind = returnK;
    s->val.exp = returnExp;
    return s;
}


STMTS * new_Simple_Statement(SIMPLE_STMT *simple, int lineno) {
	  STMTS *s = malloc(sizeof(STMTS));
    s->lineno = lineno;
    s->kind = simpleK;
    s->val.simple_stmt = simple;
    return s;
}

/* ================= Statement Helpers ==================== */

SIMPLE_STMT * new_Inc_Simple_Statement(EXP *incExp, int lineno) {
    SIMPLE_STMT *s = malloc(sizeof(SIMPLE_STMT));
    s->lineno = lineno;
    s->kind = incK;
    s->val.exp = incExp;
    return s;
}

SIMPLE_STMT * new_Dec_Simple_Statement(EXP *decExp, int lineno) {
    SIMPLE_STMT *s = malloc(sizeof(SIMPLE_STMT));
    s->lineno = lineno;
    s->kind = decK;
    s->val.exp = decExp;
    return s;
}

SIMPLE_STMT * new_Assign_ExpList_Simple_Statement(EXP_LIST *lhs, EXP_LIST *rhs, int lineno) {
    SIMPLE_STMT *s = malloc(sizeof(SIMPLE_STMT));
    s->lineno = lineno;
    s->kind = assignListK;
    s->val.assignment_list.lhs = lhs;
    s->val.assignment_list.rhs = rhs;
    return s;
}

SIMPLE_STMT * new_Assign_Exp_Simple_Statement(EXP *lhs, StatementK kind, EXP *rhs, int lineno) {
    SIMPLE_STMT *s = malloc(sizeof(SIMPLE_STMT));
    s->lineno = lineno;
    s->kind = kind;
    s->val.assignment.lhs = lhs;
    s->val.assignment.rhs = rhs;
    return s;
}

SIMPLE_STMT * new_Expr_Simple_Statement(EXP *exp, int lineno) {
    SIMPLE_STMT *s = malloc(sizeof(SIMPLE_STMT));
    s->lineno = lineno;
    s->kind = exprSimpleK;
    s->val.exp = exp;
    return s;
}

SIMPLE_STMT * new_Short_Var_Decl_Simple_Statement(EXP_LIST *identifierList, EXP_LIST *rhs, int lineno) {
    SIMPLE_STMT *s = malloc(sizeof(SIMPLE_STMT));
    s->lineno = lineno;
    s->kind = shortVarDeclK;
    s->val.assignment_short_var_decl.identifierList = identifierList;
    s->val.assignment_short_var_decl.rhs = rhs;
    return s;
}

SWITCH_CASE * new_Switch(SwitchCaseK kind, EXP_LIST *caseExpr, STMTS *case_stmts) {
	SWITCH_CASE *s = malloc(sizeof(SWITCH_CASE));
	s->kind = kind;
	s->caseExp = caseExpr;
	s->caseStmts = case_stmts;
	return s;
}

SWITCH_ON * new_Switch_On(SIMPLE_STMT *simple, EXP *condition) {
	SWITCH_ON *s = malloc(sizeof(SWITCH_CASE));
    s->simpleStmt = simple;
    s->condition= condition;
    return s;
}

FOR_STATEMENT * new_While_For(EXP *while_condition, STMTS *block) {
	FOR_STATEMENT *s = malloc(sizeof(FOR_STATEMENT));
	s->kind = whileK;
	s->val.while_condition = while_condition;
	s->block = block;
	return s;
}

FOR_STATEMENT * new_Clause_For(CLAUSE *clause, STMTS *block) {
    FOR_STATEMENT *s = malloc(sizeof(FOR_STATEMENT));
	s->kind = forClauseK;
	s->val.clause = clause;
    s->block = block;
    return s;
}

CLAUSE * new_Clause(SIMPLE_STMT *simple1, EXP *condition, SIMPLE_STMT *simple2) {
	CLAUSE *c = malloc(sizeof(CLAUSE));
	c->simple_stmt1 = simple1;
	c->simple_stmt2 = simple2;
	c->condition = condition;
    return c;
}

FOR_STATEMENT * new_Infinite_For(STMTS *block) {
    FOR_STATEMENT *s = malloc(sizeof(FOR_STATEMENT));
	s->kind = infiniteK;
	s->block = block;
	return s;
}


/* ================== Expressions ====================== */

EXP* new_Expression_primary(PrimaryExpression *primaryExp, ExpressionKind kind, int lineno) {
    EXP *e = NEW(EXP);
    e->lineno = lineno;
    e->kind = kind;
    e->val.primaryExpression = primaryExp;
    return e;
}

EXP* new_Expression_unaryOp(UnaryOpExpression *unaryOpExp, ExpressionKind kind, int lineno) {
    EXP *e = NEW(EXP);
    e->lineno = lineno;
    e->kind = kind;
    e->val.unaryOpExpression = unaryOpExp;
    return e;
}

EXP* new_Expression_binaryOp(BinaryOpExpression *binaryOpExp, ExpressionKind kind, int lineno) {
    EXP *e = NEW(EXP);
    e->lineno = lineno;
    e->kind = kind;
    e->val.binaryOpExpression = binaryOpExp;
    return e;
}

EXP* new_Expression_builtinOp(BuiltInExpression *builtinExp, ExpressionKind kind, int lineno) {
    EXP *e = NEW(EXP);
    e->lineno = lineno;
    e->kind = kind;
    e->val.builtinExpression = builtinExp;
    return e;
}

PrimaryExpression* new_Primary_Expression_identifier(SYMBOL *identifier, int lineno) {
    PrimaryExpression *newPrimaryExp = NEW(PrimaryExpression);
    
    if (isBlankIdentifier(identifier->name)) {
      newPrimaryExp->kind = blankExpK;
    } else {
      newPrimaryExp->kind = identifierK;
    }
    
    newPrimaryExp->val.identifier = identifier;
    newPrimaryExp->lineno = lineno;
    return newPrimaryExp;
}

PrimaryExpression* new_Primary_Expression_parenthesis(EXP *exp, int lineno) {
    PrimaryExpression *newPrimaryExp = NEW(PrimaryExpression);
    newPrimaryExp->kind = parenthesisExpK;
    newPrimaryExp->val.parenthesisExp = exp;
    newPrimaryExp->lineno = lineno;
    return newPrimaryExp;
}

PrimaryExpression* new_Primary_Expression_arguments(PrimaryExpression *primaryExp, EXP_LIST *expList, int lineno) {
    PrimaryExpression *newPrimaryExp = NEW(PrimaryExpression);
    newPrimaryExp->kind = primaryExpArgumentK;
    newPrimaryExp->val.argument.primaryExp = primaryExp;
    newPrimaryExp->val.argument.expList = expList;
    newPrimaryExp->lineno = lineno;
    return newPrimaryExp;
}

PrimaryExpression* new_Primary_Expression_index(PrimaryExpression *primaryExp, EXP *indexExp, int lineno) {
    PrimaryExpression *newPrimaryExp = NEW(PrimaryExpression);
    newPrimaryExp->kind = primaryExpIndexK;
    newPrimaryExp->val.index.primaryExp = primaryExp;
    newPrimaryExp->val.index.indexExp = indexExp;
    newPrimaryExp->lineno = lineno;
    return newPrimaryExp;
}

PrimaryExpression* new_Primary_Expression_selector(PrimaryExpression *primaryExp, char *identifier, int lineno) {
    PrimaryExpression *newPrimaryExp = NEW(PrimaryExpression);
    newPrimaryExp->kind = primaryExpSelectorK;
    newPrimaryExp->val.selector.primaryExp = primaryExp;
    newPrimaryExp->val.selector.identifier = identifier;
    newPrimaryExp->val.selector.type = NULL;
    newPrimaryExp->lineno = lineno;
    return newPrimaryExp;
}

PrimaryExpression* new_Literal_intval_decimal(char *val, int lineno) {
    PrimaryExpression *newPrimaryExp = NEW(PrimaryExpression);
    newPrimaryExp->kind = intLiteralK;
    newPrimaryExp->val.intLiteral = (int) strtol(val, NULL, 10);
    newPrimaryExp->lineno = lineno;
    return newPrimaryExp;
}

PrimaryExpression* new_Literal_intval_octal(char *val, int lineno) {
    PrimaryExpression *newPrimaryExp = NEW(PrimaryExpression);
    newPrimaryExp->kind = intLiteralK;
    newPrimaryExp->val.intLiteral = (int) strtol(val, NULL, 8);
    newPrimaryExp->lineno = lineno;
    return newPrimaryExp;
}

PrimaryExpression* new_Literal_intval_hex(char *val, int lineno) {
    PrimaryExpression *newPrimaryExp = NEW(PrimaryExpression);
    newPrimaryExp->kind = intLiteralK;
    newPrimaryExp->val.intLiteral = (int) strtol(val, NULL, 16);
    newPrimaryExp->lineno = lineno;
    return newPrimaryExp;
}


PrimaryExpression* new_Literal_floatval(float val, int lineno) {
    PrimaryExpression *newPrimaryExp = NEW(PrimaryExpression);
    newPrimaryExp->kind = floatLiteralK;
    newPrimaryExp->val.floatLiteral = val;
    newPrimaryExp->lineno = lineno;
    return newPrimaryExp;
}

PrimaryExpression* new_Literal_runeval(char val, int lineno) {
    PrimaryExpression *newPrimaryExp = NEW(PrimaryExpression);
    newPrimaryExp->kind = runeLiteralK;
    newPrimaryExp->val.runeLiteral.val = val;
    newPrimaryExp->val.runeLiteral.isRuneLiteralEscape = false;
    newPrimaryExp->lineno = lineno;
    return newPrimaryExp;
}

PrimaryExpression* new_Literal_runeval_escape(char val, int lineno) {
    PrimaryExpression *newPrimaryExp = NEW(PrimaryExpression);
    newPrimaryExp->kind = runeLiteralK;
    newPrimaryExp->val.runeLiteral.val = val;
    newPrimaryExp->val.runeLiteral.isRuneLiteralEscape = true;
    newPrimaryExp->lineno = lineno;
    return newPrimaryExp;
}

PrimaryExpression* new_Literal_stringval(char *val, int lineno) {
    PrimaryExpression *newPrimaryExp = NEW(PrimaryExpression);
    newPrimaryExp->kind = strLiteralK;
    newPrimaryExp->val.strLiteral = val;
    newPrimaryExp->lineno = lineno;
    return newPrimaryExp;
}

UnaryOpExpression* new_Unary_Op(EXP *unaryRHS, UnaryOpKind unaryOperatorKind, int lineno) {
    UnaryOpExpression *unaryOpExp = NEW(UnaryOpExpression);
    unaryOpExp->kind = unaryOperatorKind;
    unaryOpExp->unaryRHS = unaryRHS;
    unaryOpExp->lineno = lineno;
    return unaryOpExp;
}

BinaryOpExpression* new_Binary_Op(EXP *lhs, EXP *rhs, BinaryOpKind binaryOperatorKind, int lineno) {
    BinaryOpExpression *binaryOpExp = NEW(BinaryOpExpression);
    binaryOpExp->kind = binaryOperatorKind;
    binaryOpExp->lhs = lhs;
    binaryOpExp->rhs = rhs;
    binaryOpExp->lineno = lineno;
    // lineno?
    return binaryOpExp;
}

BuiltInExpression* new_Builtin_append(EXP *arg1, EXP *arg2, int lineno) {
    BuiltInExpression *builtInExp = NEW(BuiltInExpression);
    builtInExp->kind = appendK;
    builtInExp->val.append.arg1 = arg1;
    builtInExp->val.append.arg2 = arg2;
    builtInExp->lineno = lineno;
    return builtInExp;
}

BuiltInExpression* new_Builtin_len(EXP *arg, int lineno) {
    BuiltInExpression *builtInExp = NEW(BuiltInExpression);
    builtInExp->kind = lenK;
    builtInExp->val.len = arg;
    builtInExp->lineno = lineno;
    return builtInExp;
}

BuiltInExpression* new_Builtin_cap(EXP *arg, int lineno) {
    BuiltInExpression *builtInExp = NEW(BuiltInExpression);
    builtInExp->kind = capK;
    builtInExp->val.cap = arg;
    builtInExp->lineno = lineno;
    return builtInExp;
}

EXP_LIST* new_Expression_List(EXP *exp, EXP_LIST *nextExp, int lineno) {
	 EXP_LIST* e = malloc(sizeof(EXP_LIST));
	 e->lineno = lineno;
	 e->exp = exp;
	 e->nextExp = nextExp;
	 return e;
}
