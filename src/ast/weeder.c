#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "pretty.h"
#include "tree.h"
#include "symbol.h"
#include "weeder.h"
#include "../utils/mode.h"

bool modeType;
int isForLoop, isSwitch, isStructBody;

void weed_Symbol(SYMBOL *smbl) {
  if(!smbl) {
    fprintf(stderr, "Error: Tried to weed null smbl");
    exit(1);
  }
  
}

void weed_Identifier_list(IDENTIFIER_LIST *list){
  IDENTIFIER_LIST *crnt = list;

  while (crnt){
    weed_Symbol(crnt->symbol);
    crnt = crnt->nxt;
  }
}


void weed_Program(PROGRAM *root) {

  if(!root) return; // if file is empty 

  weed_Top_Declaration_List(root->top_declaration_list);
}

void weed_Top_Declaration_List(TOP_DECLARATION_LIST *list) {
  TOP_DECLARATION_LIST *crnt = list;

  while (crnt){
    weed_Top_Declaration(crnt->top_declaration);
    crnt = crnt->nxt;
  }
}


void weed_Top_Declaration(TOP_DECLARATION *dclr){
  switch(dclr->kind) {
  case varK:  weed_Var_Declaration(dclr->val.var_dclr); break;
  case typeK: weed_Type_Declaration_list(dclr->val.type_dclr_list); break;
  case funcK: weed_Function_Declaration(dclr->val.function_dclr); break;
  default: fprintf(stderr, "Error: Unrecognised TypeK\n"); exit(1);
  }
}

void weed_Var_Declaration(VAR_DECLARATION *dclr){
  if(dclr->isList){
    weed_Declaration_list(dclr->val.declaration_list);
  }
  else {
    weed_Declaration(dclr->val.declaration);
  }
}

void weed_Type_Declaration_list(TYPE_DECLARATION_LIST *list){
  TYPE_DECLARATION_LIST *crnt = list;
  while (crnt) {
    weed_Type_Declaration(crnt->type_declaration);
    crnt = crnt->nxt;
  }
}


void weed_Type_Declaration(TYPE_DECLARATION *dclr) {
  switch (dclr->kind) {
	case type_structK : 
		{
		isStructBody = 1;
		weed_Symbol(dclr->val.structure.name);
    		weed_Declaration_list(dclr->val.structure.body);
		isStructBody = 0;
		}
		break;
	case type_simpleK :
		{
		weed_Symbol(dclr->val.simple.name);
    		weed_Symbol(dclr->val.simple.type);
		}
		break;
	case type_arrayK :
		{
		weed_Symbol(dclr->val.array.name);
		weed_Symbol(dclr->val.array.type);
		}
		break;
	case type_struct_arrayK :
		{
		weed_Symbol(dclr->val.struct_array.name);
		weed_Declaration_list(dclr->val.struct_array.body);
		}
		break;
	default : fprintf(stderr, "Error: Unrecognised TypeK\n"); exit(1);
  }

}

/* ======================================================= */
void weed_special_function(FUNCTION_DECLARATION *fun_dclr);

void weed_Function_Declaration(FUNCTION_DECLARATION *fun_dclr) {
  
  bool isMain = strcmp(fun_dclr->name->name, "main") == 0;
  bool isInit = strcmp(fun_dclr->name->name, "init") == 0;

  if((isMain || isInit) && (modeSymbol|| modeType)){
    weed_special_function(fun_dclr);
  } 
  else{
    weed_Symbol(fun_dclr->name);
    weed_Signature(fun_dclr->signature);
    weed_STMTS(fun_dclr->body);
  }
}

void weed_special_function(FUNCTION_DECLARATION *fun_dclr){
  if(fun_dclr->signature->parameters || fun_dclr->signature->result){
    fprintf(stderr, "Error: (line %d) %s must have no parameters and no return value\n", fun_dclr->lineno, fun_dclr->name->name);
    exit(1);
  }
}

void weed_Signature(SIGNATURE *signature){
  weed_Parameter_List(signature->parameters);
  if(signature->result) {
    weed_result(signature->result);
  }
}

void weed_result(RESULT *result) {
  if(result->kind == arrayResultK || result->kind == arrayStructResultK) {
    weed_Bracket_list(result->brackets);
  }
  if (result->kind == arrayResultK || result->kind == simpleResultK) {
  	weed_Symbol(result->type);
  }
  else {
	weed_Declaration_list(result->decl_list);
  }
}


void weed_Parameter_List(PARAMETER_LIST *list){
  PARAMETER_LIST *crnt = list;

  while (crnt) {
    weed_Parameter_Unit(crnt->param);
    crnt = crnt->nxt;
  }
}

void weed_Parameter_Unit(PARAMETER_UNIT *unit){
  weed_Identifier_list(unit->ids_list);

  if(unit->isArray){
    weed_Bracket_list(unit->brackets);
  }

  weed_Symbol(unit->type);
}


/* ===================================================== */

void weed_Declaration_list(DECLARATION_LIST *list) {
  DECLARATION_LIST *crnt = list;

  while (crnt) { 
    weed_Declaration(crnt->declaration);
    crnt = crnt->nxt;
  }
}

void weed_Declaration(DECLARATION *declaration) {
  weed_Identifier_list(declaration->lhs);
 
  switch(declaration->kind){
  case Id_list_typek: 
    {
      weed_Symbol(declaration->type);
    }
    break;
  case Id_list_arrayk:
    {
      weed_Bracket_list(declaration->val.indx_list);
      weed_Symbol(declaration->type);
    }
    break;
  case Id_list_eqk:
    {
      if (isStructBody) {
		fprintf(stderr, "Error: assignments are not valid in struct bodies\n");
	       	exit(1);
      }
      weed_expression_list(declaration->val.rhs_list);
    }
    break;
  case Id_list_type_eqk:
    {
      if (isStructBody) {
                fprintf(stderr, "Error: assignments are not valid in struct bodies\n");
                exit(1);
      }
      weed_Symbol(declaration->type);
      weed_expression_list(declaration->val.rhs_list);
    }
    break;
  case Id_list_structK:
    {
	isStructBody = 1;
	weed_Declaration_list(declaration->val.struct_body);
    	isStructBody = 0;
    }
    break;
  case Id_list_array_structK:
    {
	isStructBody = 1;
	weed_Bracket_list(declaration->val.struct_array.indx_list);
	weed_Declaration_list(declaration->val.struct_array.struct_body);
    	isStructBody = 0;
    }
    break;
  case Id_list_array_eqK:
      {
	if (isStructBody) {
                fprintf(stderr, "Error: assignments are not valid in struct bodies\n");
                exit(1);
      	}
        weed_Bracket_list(declaration->val.indx_list);
        weed_Symbol(declaration->type);
        weed_expression_list(declaration->val.rhs_list);
      }
      break;
  default: fprintf(stderr, "Error: Not valid DeclarationK\n"); exit(1);
  }
}

void weed_Bracket(BRACKET *bracket){
	return;
}

void weed_Bracket_list(BRACKET_LIST *list){
	return;
}

/* ============ EXPRESSION ============ */


void weed_expression(EXP *e) {
   if (e == NULL ) {
	  return;
   }

   switch(e->kind) {
        case primaryExpressionK:    weed_Primary_Expression(e->val.primaryExpression); break;
        case unaryOpK:              break;
        case binaryOpK:             break;
        case builtinK:              break;
        default:                    fprintf(stderr, "Error: (line: %d) Not a valid expression kind\n", e->lineno); exit(1);
    }
}

int weed_expression_list(EXP_LIST *exp_list) {
    
    int num_exp = 0;
    while (exp_list->nextExp != NULL) {
        weed_expression(exp_list->exp);
	num_exp++;
        exp_list = exp_list->nextExp;
    }
    weed_expression(exp_list->exp);
    num_exp++;
    return num_exp;
}

void weed_Primary_Expression(PrimaryExpression *primaryExp) {
    switch(primaryExp->kind) {
        case identifierK:           break;
        case parenthesisExpK:       break;

        case intLiteralK:           break;
        case floatLiteralK:         break;
        case runeLiteralK:          break;
        case strLiteralK:           break;
        case blankExpK:             break;

        case primaryExpArgumentK:   weed_Primary_Expression_arguments(primaryExp); break;
        case primaryExpSelectorK:   weed_Primary_Expression_selector(primaryExp); break;
        case primaryExpIndexK:      weed_Primary_Expression_index(primaryExp); break;
        default:                    fprintf(stderr, "Error: (line: %d) Not a valid primaryExp kind\n", primaryExp->lineno); exit(1);
    }
}

void weed_Primary_Expression_arguments(PrimaryExpression *primaryExp) {
    weed_Primary_Expression(primaryExp->val.argument.primaryExp);
    if (primaryExp->val.argument.expList != NULL ) {
    	weed_expression_list(primaryExp->val.argument.expList);
    }
}

void weed_Primary_Expression_selector(PrimaryExpression *primaryExp) {
    weed_Primary_Expression(primaryExp->val.selector.primaryExp);
}

void weed_Primary_Expression_index(PrimaryExpression *primaryExp) {
    weed_Primary_Expression(primaryExp->val.index.primaryExp);
    weed_expression(primaryExp->val.index.indexExp);
}


/* ===================================================== */

void weed_Simple_Stmt(SIMPLE_STMT *simple_stmt, int lineno) {

   if (simple_stmt != NULL ){
	switch( simple_stmt->kind) {
	   	case varBodyK : fprintf(stderr, "Not valid simple statement\n"); exit(1);
                case typeBodyK : fprintf(stderr, "Not valid simple statement\n"); exit(1);
                case ifK : fprintf(stderr, "Not valid simple statement\n"); exit(1);
                case switchK : fprintf(stderr, "Not valid simple statement\n"); exit(1);
                case forK : fprintf(stderr, "Not valid simple statement\n"); exit(1);
                case printK : fprintf(stderr, "Not valid simple statement\n"); exit(1);
                case printlnK : fprintf(stderr, "Not valid simple statement\n"); exit(1);
                case continueK: fprintf(stderr, "Not valid simple statement\n"); exit(1);
                case breakK : fprintf(stderr, "Not valid simple statement\n"); exit(1);
                case returnK : fprintf(stderr, "Not valid simple statement\n"); exit(1);

			// simple statements
   			case assignListK :
                                weed_Assignment_List(simple_stmt, lineno);
                                break;
                        case exprSimpleK :
                                weed_Expression_Statement(simple_stmt, lineno);
                                break;
                        case incK :
                                weed_Inc_Statement(simple_stmt);
                                break;
                        case decK :
                                weed_Dec_Statement(simple_stmt);
                                break;
                        case shortVarDeclK :
                                weed_Short_Var_Decl(simple_stmt, lineno);
                                break;
                        case equalAssignK :
                                weed_Assignment_List(simple_stmt, lineno);
                                break;
                        case plusAssignK :
                                weed_Op_Assignment(simple_stmt);
                                break;
                        case minusAssignK :
                                weed_Op_Assignment(simple_stmt);
                                break;
                        case orAssignK :
                                weed_Op_Assignment(simple_stmt);
                                break;
                        case xorAssignK :
                                weed_Op_Assignment(simple_stmt);
                                break;
                        case mulAssignK :
                                weed_Op_Assignment(simple_stmt);
                                break;
                        case divAssignK :
                                weed_Op_Assignment(simple_stmt);
                                break;
                        case remAssignK :
                                weed_Op_Assignment(simple_stmt);
                                break;
                        case lshiftAssignK :
                                weed_Op_Assignment(simple_stmt);
                                break;
                        case rshiftAssignK :
                                weed_Op_Assignment(simple_stmt);
                                break;
                        case andAssignK :
                                weed_Op_Assignment(simple_stmt);
                                break;
                        case bitclearAssignK:
                                weed_Op_Assignment(simple_stmt);
                                break;
                        case simpleK :
                                weed_Simple_Stmt(simple_stmt, lineno);
                                break;
                        default : fprintf(stderr, "Not valid StatementK"); exit(1);
   	}
   }
}

void weed_If_Statement(STMTS *ifs, int lineno) {

	// optional simple statement
	if (ifs->val.if_stmt.simplestmt != NULL ) {
		weed_Simple_Stmt(ifs->val.if_stmt.simplestmt, lineno);
	}

	// if block
        weed_expression(ifs->val.if_stmt.condition);
        weed_STMTS(ifs->val.if_stmt.block);

	// else if or else or nothing
	if (ifs->val.if_stmt.next != NULL ) {
		weed_STMTS(ifs->val.if_stmt.next);
		
	}

}

void weed_Switch_On(SWITCH_ON *switch_on, int lineno) {

	if (switch_on->simpleStmt != NULL) {
		weed_Simple_Stmt(switch_on->simpleStmt, lineno);
	}

	if (switch_on->condition != NULL) {
		weed_expression(switch_on->condition);
	}
}

void weed_Switch_Case(SWITCH_CASE *switch_case, int num_default, int lineno) {

	isSwitch = 1;

	if (switch_case == NULL ) {
		return;
	}

	if (switch_case->kind == caseK) {
		weed_expression_list(switch_case->caseExp);
	}

	else if (switch_case->kind == defaultK) {
		num_default++;
		if (num_default > 1) {
			fprintf(stderr, "Error: (line %d) only one default case allowed\n", lineno);
			exit(1);
		}
	}

	if (switch_case->caseStmts != NULL) {
		weed_STMTS(switch_case->caseStmts);
	}

	weed_Switch_Case(switch_case->next, num_default, lineno++);

	isSwitch = 0;
}

void weed_Clause(CLAUSE *c, int lineno) {

	if (c->simple_stmt1 != NULL) {
		weed_Simple_Stmt(c->simple_stmt1, lineno);
	}

	if (c->condition != NULL) {
		weed_expression(c->condition);
	}

	if (c->simple_stmt2 != NULL) {
		if (c->simple_stmt2->kind == printK || c->simple_stmt2->kind == printlnK || c->simple_stmt2->kind == continueK || c->simple_stmt2->kind == breakK || c->simple_stmt2->kind == returnK || c->simple_stmt2->kind == shortVarDeclK || c->simple_stmt2->kind == forK || c->simple_stmt2->kind == switchK || c->simple_stmt2->kind == ifK || c->simple_stmt2->kind == typeBodyK || c->simple_stmt2->kind == varBodyK) {
			fprintf(stderr, "Error: (line %d) post condition statements in if clause cannot be short variable declarations\n", lineno);
                        exit(1);
		}

		weed_Simple_Stmt(c->simple_stmt2, lineno);
	}
}

void weed_For_Statement(FOR_STATEMENT *for_stmt, int lineno) {
	
	isForLoop = 1;

	switch (for_stmt->kind) {

		case infiniteK : break;

		case forClauseK :
				weed_Clause(for_stmt->val.clause, lineno);
				break;

		case whileK :
				weed_expression(for_stmt->val.while_condition);
				break;
		default: fprintf(stderr, "Not valid StatementK"); exit(1);
	}

	weed_STMTS(for_stmt->block);

	isForLoop = 0;
}

void weed_Print_Statement(EXP_LIST *print) {

	if (print != NULL) weed_expression_list(print);

}

void weed_Println_Statement(EXP_LIST *println) {

        if (println != NULL) weed_expression_list(println);

}

void weed_Return_Statement(EXP *exp, int num_exp, int lineno) {

	if (exp != NULL) {
		weed_expression(exp);
		num_exp++;
		if (num_exp > 1) {
			fprintf(stderr, "Error: (line %d) no more than 1 value can be returned\n", lineno); 
		        exit(1);
		}
	}

}

void weed_Assignment_List(SIMPLE_STMT *simple_stmt, int lineno) {
	
	int num_lhs = weed_expression_list(simple_stmt->val.assignment_list.lhs);
        int num_rhs = weed_expression_list(simple_stmt->val.assignment_list.rhs);
	if (num_lhs != num_rhs) {
		fprintf(stderr, "Error: (line %d) invalid assignment (lhs(%d), rhs(%d))\n", lineno, num_lhs, num_rhs);
                exit(1);
	}
}

// TODO : make sure this weeds out anything but function calls
void weed_Expression_Statement(SIMPLE_STMT *simple_stmt, int lineno) {
	if ((simple_stmt->val.exp)->kind != primaryExpressionK ) {
		fprintf(stderr, "Error: (line %d) expression statements must be function calls\n", lineno);
                exit(1);
	}	
	else if ((((simple_stmt->val.exp)->val.primaryExpression)->kind != primaryExpArgumentK) && (((simple_stmt->val.exp)->val.primaryExpression)->kind != parenthesisExpK)) {
		fprintf(stderr, "Error: (line %d) expression statements must be function calls\n", lineno);
		exit(1);
	}
	else if (((simple_stmt->val.exp)->val.primaryExpression)->kind == parenthesisExpK) {
	       if (simple_stmt->val.exp->val.primaryExpression->val.parenthesisExp->kind == primaryExpressionK ) {
		       if (simple_stmt->val.exp->val.primaryExpression->val.parenthesisExp->val.primaryExpression->kind != primaryExpArgumentK){
				fprintf(stderr, "Error: (line %d) expression statements must be function calls\n", lineno);
                		exit(1);
			}
		}
	}

	if (simple_stmt->val.exp != NULL ) {
                //weed_expression(simple_stmt->val.exp);
        }	
}

void weed_Inc_Statement(SIMPLE_STMT *simple_stmt) {
    weed_expression(simple_stmt->val.exp);
}

void weed_Dec_Statement(SIMPLE_STMT *simple_stmt) {
    weed_expression(simple_stmt->val.exp);
}

void weed_Short_Var_Decl(SIMPLE_STMT *simple_stmt, int lineno) {

	if (simple_stmt->val.assignment_short_var_decl.identifierList == NULL ) {
		fprintf(stderr, "Error: (line %d) short variable declarations must contain identifier(s) on the lhs\n", lineno);
                exit(1);
	}

    EXP_LIST *exp_list= simple_stmt->val.assignment_short_var_decl.identifierList;

    while (exp_list->nextExp != NULL) {
        if (exp_list->exp->val.primaryExpression->kind != identifierK && exp_list->exp->val.primaryExpression->kind != blankExpK ){
                fprintf(stderr, "Error: (line %d) short variable declarations must only contain identifier on the lhs\n", lineno);
                exit(1);
        }
        exp_list = exp_list->nextExp;
    }
    if (exp_list->exp->kind != primaryExpressionK || (exp_list->exp->val.primaryExpression->kind != identifierK && exp_list->exp->val.primaryExpression->kind != blankExpK)){
                fprintf(stderr, "Error: (line %d) short variable declarations must only contain identifier on the lhs\n", lineno);  
                exit(1);
    }

    int num_lhs = weed_expression_list(simple_stmt->val.assignment_short_var_decl.identifierList);
    int num_rhs = weed_expression_list(simple_stmt->val.assignment_short_var_decl.rhs);

    if (num_lhs != num_rhs) {
                fprintf(stderr, "Error: (line %d) invalid assignment (lhs(%d), rhs(%d))\n", lineno, num_lhs, num_rhs);
                exit(1);
    }
}


void weed_Op_Assignment(SIMPLE_STMT *simple_stmt) {

	weed_expression(simple_stmt->val.assignment.lhs);
	weed_expression(simple_stmt->val.assignment.rhs);
}


void weed_STMTS (STMTS *stmts) {
	STMTS *curr = stmts;

	//printf("weed statements\n");

	while (curr) {

		switch (curr->kind) {
      			case blockK :
                                weed_STMTS(curr->val.block);
                                break;
			case varBodyK :
				weed_Var_Declaration(curr->val.var_dclr);
				break;
			case typeBodyK :
				weed_Type_Declaration_list(curr->val.type_dclr);
				break;
			case ifK :
				weed_If_Statement(curr, curr->lineno);
				break;
			case switchK :
				weed_Switch_On(curr->val.switch_stmt.switch_on, curr->lineno);
				weed_Switch_Case(curr->val.switch_stmt.switch_case, 0, curr->lineno);
				break;
			case forK :
				weed_For_Statement(curr->val.for_stmt, curr->lineno);
				break;
			case printK :
				weed_Print_Statement(curr->val.print);
				break;
			case printlnK :
				weed_Println_Statement(curr->val.print);
				break;
			case continueK :
				if (!isForLoop) {
					fprintf(stderr, "Error: (line %d) continue statements must be inside for loops\n", curr->lineno);
					exit(1);
				}
				break;
			case breakK :
				if (!isForLoop && !isSwitch) {
					fprintf(stderr, "Error: (line %d) break statements must be within for loop or switch\n", curr->lineno);
					exit(1);
				
				}	
				break;
			case returnK :
				weed_Return_Statement(curr->val.exp, 0, curr->lineno);
				break;
			case assignListK :
				weed_Assignment_List(curr->val.simple_stmt, curr->lineno);
				break;
			case exprSimpleK :
				weed_Expression_Statement(curr->val.simple_stmt, curr->lineno);
				break;
			case incK :
				weed_Inc_Statement(curr->val.simple_stmt);
				break;
			case decK :
				weed_Dec_Statement(curr->val.simple_stmt);
				break;
			case shortVarDeclK :
				weed_Short_Var_Decl(curr->val.simple_stmt, curr->lineno);
				break;
			case equalAssignK :
				weed_Assignment_List(curr->val.simple_stmt, curr->lineno);
				break;
			case plusAssignK :
				weed_Op_Assignment(curr->val.simple_stmt);
				break;
			case minusAssignK :
				weed_Op_Assignment(curr->val.simple_stmt);
				break;
        		case orAssignK :
				weed_Op_Assignment(curr->val.simple_stmt);
				break;
        		case xorAssignK :
				weed_Op_Assignment(curr->val.simple_stmt);
				break;
        		case mulAssignK :
				weed_Op_Assignment(curr->val.simple_stmt);
				break;
        		case divAssignK :
				weed_Op_Assignment(curr->val.simple_stmt);
				break;
        		case remAssignK :
				weed_Op_Assignment(curr->val.simple_stmt);
				break;
        		case lshiftAssignK :
				weed_Op_Assignment(curr->val.simple_stmt);
				break;
        		case rshiftAssignK :
				weed_Op_Assignment(curr->val.simple_stmt);
				break;
        		case andAssignK :
				weed_Op_Assignment(curr->val.simple_stmt);
				break;
        		case bitclearAssignK:
				weed_Op_Assignment(curr->val.simple_stmt);
				break;
			case simpleK :
				weed_Simple_Stmt(curr->val.simple_stmt, curr->lineno);
				break;
			default : fprintf(stderr, "Not valid StatementK"); exit(1);
		}
		curr = curr->nextStmt;
		//printf("end statement weed\n");
	}
}
