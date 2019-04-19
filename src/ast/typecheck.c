#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include<stdbool.h>
#include "../utils/memory.h"
#include "../utils/util.h"
#include "symbol.h"
#include "tree.h"
#include "typecheck.h"

/* ============= special functions for typechecking ================*/


int isTermStmt, hasBreak, isFunctionCall;

// function that asserts expression is addresable and produces error if not true
void isAddressable(EXP *e) {

	if (e == NULL ) {
		fprintf(stderr, "Error: (line %d) NULL expression is not addressable\n", e->lineno);
		exit(1);
	}

	// only variable or slice index or array index of addressable array or field selection of addressable struct allowed
	switch(e->kind) {												
		case primaryExpressionK : 
			
			if (e->val.primaryExpression == NULL) {
				fprintf(stderr, "Error: (line %d) NULL primary expression is not addressable\n", e->lineno);
				exit(1);
			}

			switch (e->val.primaryExpression->kind) {
				case identifierK: 	
					
					if (e->val.primaryExpression->val.identifier->category == category_constant) {
						fprintf(stderr, "Error: (line %d) constants are not addressable\n", e->lineno);
						exit(1);
					}

					break;

				case parenthesisExpK : 
					isAddressable(e->val.primaryExpression->val.parenthesisExp);
					break;

				case primaryExpArgumentK : 	
					if (e->val.primaryExpression->base_type) {
							if (e->val.primaryExpression->base_type->category == category_function) {
								fprintf(stderr, "Error: (line %d) function calls are not addressable\n", e->lineno);
								exit(1);
							}			
					}
					break;
				case primaryExpIndexK : 
					// check primaryExp before index is an (addressable?) array or slice
					if (e->val.primaryExpression->val.index.primaryExp != NULL) {
						
						if (e->val.primaryExpression->val.index.primaryExp->kind == primaryExpArgumentK && resolveType(typecheck_Primary_Expression(e->val.primaryExpression->val.index.primaryExp), e->lineno)->kind != sliceK ) {
							
							fprintf(stderr, "Error: (line %d) function calls are not addressable\n", e->lineno);
                                      			exit(1);
						
						}
		
						if (resolveType(typecheck_Primary_Expression(e->val.primaryExpression->val.index.primaryExp), e->lineno)->kind != arrayK && resolveType(typecheck_Primary_Expression(e->val.primaryExpression->val.index.primaryExp), e->lineno)->kind != sliceK ){
							fprintf(stderr, "Error: (line %d) only slice or array indexing is addressable\n", e->lineno);
							exit(1);
						}

						
					}	
					else if (e->val.primaryExpression->val.index.primaryExp == NULL) {
						fprintf(stderr, "Error: (line %d) only slice or array indexing is addressable\n", e->lineno);
						exit(1);
					}

					break;

				case primaryExpSelectorK :
					if (e->val.primaryExpression->val.selector.primaryExp != NULL) {
						if (e->val.primaryExpression->val.index.primaryExp->kind == primaryExpArgumentK) {
                                                        fprintf(stderr, "Error: (line %d) function calls are not addressable\n", e->lineno);
                                                        exit(1);
                                                }
						
						if (resolveType(typecheck_Primary_Expression(e->val.primaryExpression->val.selector.primaryExp), e->lineno)->kind != structK) {
							fprintf(stderr, "Error: (line %d) field selection of anything besides a struct is not accessable\n", e->lineno);
							exit(1);
						}
					}	
					break;

				case intLiteralK :
					fprintf(stderr, "Error: (line %d) int literals are not addressable\n", e->lineno);
					exit(1);
				case floatLiteralK : 
					fprintf(stderr, "Error: (line %d) float literals are not addressable\n", e->lineno);
					exit(1);
				case runeLiteralK : 
					fprintf(stderr, "Error: (line %d) rune literals are not addressable\n", e->lineno);
					exit(1);
				case strLiteralK : 
					fprintf(stderr, "Error: (line %d) string literals are not addressable\n", e->lineno);
					exit(1);
				default : fprintf(stderr, "Error: (line %d) not a valid primaryExpressionK\n", e->lineno); exit(1);
			}
			break;

		case unaryOpK : 
			fprintf(stderr, "Error: (line %d) unary operation expressions are not addressable\n" , e->lineno);
			exit(1);

		case binaryOpK :
			fprintf(stderr, "Error: (line %d) binary operation expressions are not addressable\n" , e->lineno);
			exit(1);

		case builtinK : 
			fprintf(stderr, "Error: (line %d) builtin expressions are not addressable\n" , e->lineno);
			exit(1);

		default : fprintf(stderr, "Error: (line %d) not a valid ExpressionK\n", e->lineno);
	}

}

// function that asserts type is comparable and produces error if not true
bool isComparable(TYPE *t) {
	t = resolveType(t, 0);

	// int, float64, string, bool, rune
	if (t->kind == baseK && (t->val.baseType == intK || 
													 t->val.baseType == float64K || 
													 t->val.baseType == stringK || 
													 t->val.baseType == boolK ||
													 t->val.baseType == runeK)) {
		return true;
	} 
	// array values are comparable if values of array element type are comparable
	else if (t->kind == arrayK) {
		return isComparable(t->val.array.element);
	} 
	// struct values are comparable if all fields comparable
	else if (t->kind == structK) {
		FIELD *cur = t->val.field;
		while (cur) {
			if (!isComparable(cur->type)) {
				return false;
			}
			cur = cur->nxt;
		}
		return true;
	} else if (t->kind == refK) {
		return isComparable(t->val.ref.element);
	} else {
		return false;
	}
}

// function that asserts type is numeric and produces error if not true
bool isNumeric(TYPE *t) {
	// int, float64, rune
	if (t->kind == baseK && (t->val.baseType == intK || 
													 t->val.baseType == float64K || 
													 t->val.baseType == runeK)) {
		return true;
	} else if (t->kind == refK) {
		return isNumeric(t->val.ref.element);
	} else {
		return false;
	}
}

bool isInteger(TYPE *t) {
	// int, rune
	if (t->kind == baseK && (t->val.baseType == intK || 
													 t->val.baseType == runeK)) {
		return true;
	} else if (t->kind == refK) {
		return isInteger(t->val.ref.element);
	} else {
		return false;
	}
}

bool isString(TYPE *t) {
	if (t->kind == baseK && (t->val.baseType == stringK)) {
                return true;
        } else if (t->kind == refK) {
                return isString(t->val.ref.element);
        } else {
                return false;
        }

}

bool isBool(TYPE *t) {
	if (t->kind == baseK && t->val.baseType == boolK) {
		return true;
	} else if (t->kind == refK) {
		return isBool(t->val.ref.element);
	} else {
		return false;
	}
}

bool isOrdered(TYPE *t) {
	// int, float64, rune, string
	if (t->kind == baseK && (t->val.baseType == intK || 
													 t->val.baseType == float64K || 
													 t->val.baseType == runeK ||
													 t->val.baseType == stringK)) {
		return true;
	} else if (t->kind == refK) {
		return isOrdered(t->val.ref.element);
	}
	else {
		return false;
	}
}

// function to find underlying type
TYPE *resolveType(TYPE *type, int lineno) {

	// find underlying type
	TYPE *resolved_type;
	switch(type->kind) {
		case sliceK :
			// do not further resolve (return slice type : e.g.[]natural)
			resolved_type = type;
			break;

		case refK :
			// resolve the underlying type
			if (type->val.ref.element != NULL) {
				resolved_type = resolveType(type->val.ref.element, lineno);
			}
			else {
				resolved_type = type;
			}
			break;

		case arrayK :
			// do not further resolve (return array type : e.g. [3][2]natural)
			resolved_type = type;
			break;

		case structK :
			//do not further resolve (return struct type: e.g. struct{})
			resolved_type = type;
			break;

		case baseK :
			resolved_type = type;
			break;
		default : fprintf(stderr, "Error: (line %d) not a valid typeK\n", lineno); exit(1);
	}	

	return resolved_type;
}

char* convertTypeToString(TYPE *t) {
	switch(t->kind) {
		case sliceK:
			convertTypeToString(t->val.slice);
			break;
		case refK:
			return t->val.ref.id;
			break;
		case arrayK:
			convertTypeToString(t->val.array.element);
			break;
		case structK:
			return "struct";
			break;
		case baseK:
			switch(t->val.baseType) {
				case boolK: 		return "bool";
				case intK:			return "int";
				case float64K:	return "float64";
				case runeK:			return "rune";
				case stringK:		return "string";
				default:				return "<uknown>";
			}
		default: return "<uknown>";
	}
	return "<uknown>";
}

// function throws error if types do no follow equality rules
void assertTypeEquality(TYPE *t1, TYPE *t2, int lineno) {
	switch(t1->kind) {

		// slice types must have identical element types
		case sliceK: 
			
			if (t2->kind != sliceK) {
				fprintf(stderr, "Error: (line: %d) %s type is not identical to %s type\n", lineno,convertTypeToString(t2), convertTypeToString(t1));
				exit(1);								
			}
			// recursively traverse type lists until tails, then tail types are compared
			if (t1->val.slice != NULL && t2->val.slice != NULL) {
				assertTypeEquality(t1->val.slice, t2->val.slice, lineno);
			}
			break;

		// defined types are distinct regardless of identifier or underlying type
		case refK: 
			if (t1 != t2) {
				fprintf(stderr, "Error: (line: %d) defined types must point to the same type specification to be equal [%s type is not equal to %s type]\n", lineno, convertTypeToString(t1), convertTypeToString(t2));
				exit(1);
			}
			break;

		// array types must have identical element types and array length
		case arrayK : 
			if (t2->kind != arrayK) {
				fprintf(stderr, "Error: (line %d)  %s array type is not identical to %s type\n", lineno, convertTypeToString(t2), convertTypeToString(t1));
				exit(1);
			}
                             
			if (t1->val.array.size != t2->val.array.size) {
				fprintf(stderr, "Error: (line %d) arrays are only identical if they are the same length\n", lineno);
				exit(1);
			}
			     
			// recursively traverse type lists until tails, then tail types are compared
			if (t1->val.array.element != NULL && t2->val.array.element != NULL) {
				assertTypeEquality(t1->val.array.element, t2->val.array.element, lineno);
			}
			break;

		// struct types must have same sequence of fields and fields must have same names, types, and tags
		case structK : 
			if (t2->kind != structK) {
				fprintf(stderr, "Error: (line %d) struct types are only identical to other struct types\n", lineno);
				exit(1);			
			}

			// traverse struct fields and assert their type equality
			if (t1->val.field == NULL && t2->val.field == NULL) {
				break;
			}
			else if (t1->val.field == NULL || t2->val.field == NULL) {
				fprintf(stderr, "Error: (line %d) struct types are only identical if all fields have same names (in same order)\n", lineno);
				exit(1);
			}

			FIELD *type1_field = t1->val.field;
			FIELD *type2_field = t2->val.field;
			
			while (type1_field && type2_field) {
	
			if (strcmp(type1_field->name, type2_field->name) != 0) {
				fprintf(stderr, "Error: (line %d) struct types are only identical if all fields have same names (in same order)\n", lineno);
				exit(1);
			}

			assertTypeEquality(type1_field->type, type2_field->type, lineno);

			type1_field = type1_field->nxt;
			type2_field = type2_field->nxt;
			}
			break;

		// base types must be identical
		case baseK :
			if (t1->val.baseType != t2->val.baseType) {
				fprintf(stderr, "Error: (line %d) %s type is not identical to  %s type\n", lineno, convertTypeToString(t1), convertTypeToString(t2));
				exit(1);
			}
			break;	       
	}
}


/* ================= AST traversal ================= */

void typecheck_Program(PROGRAM *root){
    typecheck_Top_Declaration_List(root->top_declaration_list);
}

void typecheck_Top_Declaration_List(TOP_DECLARATION_LIST *crnt){
  while (crnt){
    typecheck_Top_Declaration(crnt->top_declaration);
    crnt = crnt->nxt;
  }
}

void typecheck_Top_Declaration(TOP_DECLARATION *declaration){
 switch(declaration->kind){
  case varK:
    typecheck_Var_Declaration(declaration->val.var_dclr);
    break;
  case typeK: 
    break; // done in symbol.c
  case funcK:
    typecheck_Function_Declaration(declaration->val.function_dclr);
    break;
  default:
    fprintf(stderr, "Error: (line: %d) Cannot put this typecheck kind\n", declaration->lineno);
  }
}									

void typecheck_Var_Declaration(VAR_DECLARATION *dclr){
  if (dclr->isList) {
    typecheck_Declaration_list(dclr->val.declaration_list);
  }
  else {
    typecheck_Declaration(dclr->val.declaration);
  }
}

void typecheck_Declaration_list(DECLARATION_LIST *list){
  while(list){
    typecheck_Declaration(list->declaration);
    list = list->nxt;
  }
}

void typecheck_Declaration(DECLARATION *declaration){
  IDENTIFIER_LIST *id;
  EXP_LIST        *exp;

  switch(declaration->kind){
  case Id_list_type_eqk:
  case Id_list_eqk: // var x, y, z = 1, 2, 3
    {
      id  = declaration->lhs;
      exp = declaration->val.rhs_list;
      
      if(declaration->kind == Id_list_eqk){
        for(;id && exp; exp = exp->nextExp, id = id->nxt) {
          	id->symbol->val.type = typecheck_expression(exp->exp);
        }
      }
      else{
        for(;id && exp; exp = exp->nextExp, id = id->nxt) {
          assertTypeEquality(id->symbol->val.type, typecheck_expression(exp->exp), declaration->lineno);
        }
      }
    }
    break;
  case Id_list_array_eqK:
    {
	id  = declaration->lhs;
      	exp = declaration->val.array_eq.rhs_list;

	for(;id && exp; exp = exp->nextExp, id = id->nxt) {
          assertTypeEquality(id->symbol->val.type, typecheck_expression(exp->exp), declaration->lineno);
        }
    }
    break;
  case Id_list_typek:
  case Id_list_arrayk:
  case Id_list_structK:
  case Id_list_array_structK: 
    break; // Types of these expressions are resolved in symbol.c
  default:
    fprintf(stderr, "Error: (line: %d) This DeclarationK is not recognised\n", declaration->lineno);
    exit(1);
  }
}


/* ============= FUNCTION DECLARATION ============ */

void typecheck_Function_Declaration(FUNCTION_DECLARATION *fun_dclr){
  
	typecheck_STMTS(fun_dclr, fun_dclr->body);
	if (fun_dclr->body && fun_dclr->signature->result) {
		
		// typecheck result if it's a struct (declarations must type check)
		if (fun_dclr->signature->result->kind == structResultK || fun_dclr->signature->result->kind == arrayStructResultK) {
			typecheck_Declaration_list(fun_dclr->signature->result->decl_list);
		}

		// check function has terminating statement if it has a return type specified
		if (fun_dclr->signature->result->type) {

			STMTS *body = fun_dclr->body;
			while ( body->nextStmt && body) {
				body = body->nextStmt;
			}

			// error if needs a terminating stmt but there isn't one
			if (body) {
				if (body->isTerminating ==  0){
					fprintf(stderr, "Error: (line %d) function must end in a terminating statement\n", fun_dclr->lineno);
					exit(1);
				}
			}
			else {
				fprintf(stderr, "Error: (line %d) function must end in a terminating statement\n", fun_dclr->lineno);
				exit(1);
			}
		}
	}
	else if (fun_dclr->signature->result && fun_dclr->body == NULL) {
		if (fun_dclr->signature->result->type) {
			fprintf(stderr, "Error: (line %d) function must end in a terminating statement\n", fun_dclr->lineno);                   
                	exit(1);
		}
	}
}

/* ============= BRACKETS ============ */

//void typecheck_Bracket(BRACKET *bracket);
//void typecheck_Bracket_list(BRACKET_LIST *list);


/* ============ IDENTIFIERS ============ */

void typecheck_Identifier_list(IDENTIFIER_LIST *list, BRACKET_LIST *brackets, char *type);

/* ============ STATEMENTS ============ */

void typecheck_STMTS(FUNCTION_DECLARATION *fun_dclr, STMTS *stmts) {
	STMTS *curStmt = stmts;
	
	// for terminating statements evaluation
	isTermStmt = 0;
	int hasElse = 0;
	// for switch statements
	TYPE *cond_type;

	while (curStmt) {
		switch (curStmt->kind) {
			/* Empty statement */
       			case blockK:
                                typecheck_STMTS(fun_dclr, curStmt->val.block);
                                break;
			case breakK:
				hasBreak = 1;
				break;
			case continueK:
				break;
			case exprSimpleK:
				typecheck_Expression_Statement(curStmt->val.simple_stmt);
				break;
			case returnK:
				typecheck_Return_Statement(fun_dclr, curStmt->val.exp, curStmt->lineno);
				curStmt->isTerminating = 1;
				break;
			case shortVarDeclK:
				typecheck_Short_Var_Decl(curStmt->val.simple_stmt);
				break;
			case varBodyK:
				typecheck_Var_Declaration(curStmt->val.var_dclr);
				break;
			case equalAssignK :
				typecheck_Assignment_List(curStmt->val.simple_stmt);
				break;
			case assignListK:
				typecheck_Assignment_List(curStmt->val.simple_stmt);
				break;
			/* Op-Assignment (add_op_eq) */
			case plusAssignK:
				typecheck_Op_Assignment(curStmt->val.simple_stmt);
				break;
			case minusAssignK:
				typecheck_Op_Assignment(curStmt->val.simple_stmt);
				break;
			case orAssignK:
				typecheck_Op_Assignment(curStmt->val.simple_stmt);
				break;
			case xorAssignK:
				typecheck_Op_Assignment(curStmt->val.simple_stmt);
				break;
			/* Op-Assignment (mul_op_eq) */
			case mulAssignK :
				typecheck_Op_Assignment(curStmt->val.simple_stmt);
				break;
			case divAssignK :
				typecheck_Op_Assignment(curStmt->val.simple_stmt);
				break;
			case remAssignK :
				typecheck_Op_Assignment(curStmt->val.simple_stmt);
				break;
			case lshiftAssignK :
				typecheck_Op_Assignment(curStmt->val.simple_stmt);
				break;
			case rshiftAssignK :
				typecheck_Op_Assignment(curStmt->val.simple_stmt);
				break;
			case andAssignK :
				typecheck_Op_Assignment(curStmt->val.simple_stmt);
				break;
			case bitclearAssignK:
				typecheck_Op_Assignment(curStmt->val.simple_stmt);
				break;
			case printK:
				if (curStmt->val.print != NULL) {
					typecheck_Println_Statement(curStmt->val.print);
				}
				break;
			case printlnK:
				if (curStmt->val.print != NULL) {
					typecheck_Println_Statement(curStmt->val.print);
				}
				break;
			case forK:
				typecheck_For_Statement(fun_dclr, curStmt->val.for_stmt, curStmt->lineno);
				if (isTermStmt) curStmt->isTerminating = 1;
				break;
			case ifK:
				if (curStmt->val.if_stmt.next) {
					hasElse = 1;
				}
				typecheck_If_Statement(fun_dclr, curStmt);
				isTermStmt = curStmt->isTerminating && hasElse;
				if (isTermStmt) curStmt->isTerminating = 1;
				else curStmt->isTerminating = 0;
				hasBreak = 0;
				break;
			case switchK:
				{
				cond_type = typecheck_Switch_On(curStmt->val.switch_stmt.switch_on);
				typecheck_Switch_Case(fun_dclr, curStmt->val.switch_stmt.switch_case, cond_type, curStmt->lineno);
				
				int hasDefault = 0;
				SWITCH_CASE *curr = curStmt->val.switch_stmt.switch_case;
				if (curr) {
					if (curr->caseStmts != NULL) {
                                                isTermStmt = curr->caseStmts->isTerminating;
                                	}
					else isTermStmt = 0;
					if (curr->kind == defaultK ) hasDefault = 1;
                                	if (curr->next) curr = curr->next;
				}
				while (curr && curr->next) {
					if (curr->caseStmts != NULL) {
						isTermStmt = isTermStmt && curr->caseStmts->isTerminating;
					}
					else isTermStmt = 0;
					if (curr->kind == defaultK ) hasDefault = 1;
					curr = curr->next;
				}		
				curStmt->isTerminating = isTermStmt && hasDefault;
				hasBreak = 0;
				}
				break;
			case incK:
				typecheck_Inc_Statement(curStmt->val.simple_stmt);
				break;
			case decK:
				typecheck_Dec_Statement(curStmt->val.simple_stmt);
				break;
			case typeBodyK :
				break; // done in symbol.c
			case simpleK :
				typecheck_Simple_Stmt(curStmt->val.simple_stmt);
				break;
			default : fprintf(stderr, "Error: (line: %d) Not valid StatementK in symbol.c file\n", curStmt->lineno); exit(1);
		}

   	curStmt = curStmt->nextStmt;
  	}

}

void typecheck_Expression_Statement(SIMPLE_STMT *simple_stmt) {

	if (simple_stmt->val.exp != NULL ) {
		
		if (simple_stmt->val.exp->val.primaryExpression != NULL) {
			if (simple_stmt->val.exp->val.primaryExpression->base_type->category != category_function) {
				fprintf(stderr, "Error: (line %d) expression statement must be a function call\n", simple_stmt->lineno);
				exit(1);
			}	
			typecheck_Primary_Expression_arguments(simple_stmt->val.exp->val.primaryExpression);
		}
	}
}

void typecheck_Return_Statement(FUNCTION_DECLARATION *fun_dclr, EXP *exp, int lineno) {

	// check that function declaration had no return type
	if (exp == NULL) {
		if (fun_dclr->signature->result != NULL) {
			fprintf(stderr, "Error: (line %d) return type does not match function declaration\n", lineno);
			exit(1);
		}	
	}

	// check that function declaration return type is the same as expression type
	else {
		if (fun_dclr->signature->result == NULL) {
			fprintf(stderr, "Error: (line %d) return type does not match function declaration\n", exp->lineno);
			exit(1);
		}
	
		// typecheck and get return expression type
		TYPE *exp_type = typecheck_expression(exp);
	
		// check types are the same
	 	assertTypeEquality(exp_type, fun_dclr->signature->result->type->val.type, exp->lineno);	
	}
}

void typecheck_Short_Var_Decl(SIMPLE_STMT *simple_stmt) {
	
	// Check variables already declared in the current scope are assigned expressions of the same type
	EXP_LIST *lhs_exps = simple_stmt->val.assignment_short_var_decl.identifierList;
	EXP_LIST *rhs_exps = simple_stmt->val.assignment_short_var_decl.rhs;

	while (lhs_exps && lhs_exps->exp && rhs_exps && rhs_exps->exp) {
		SYMBOL *curr_id = lhs_exps->exp->val.primaryExpression->val.identifier;
		EXP *curr_exp = rhs_exps->exp;

		// ensure all rhs expressions are well typed
		TYPE *exp_type = typecheck_expression(curr_exp);

		if (curr_id->category == category_type) {
			fprintf(stderr, "Error: (line %d) lhs of short var declaration cannot be a type\n", curr_exp->lineno);
			exit(1);
		}

		// check predeclared variables with types are assigned expressions of the same type (no resolving)
		if (curr_id->val.type != NULL && !isBlankIdentifier(curr_id->name)) {
			assertTypeEquality(curr_id->val.type, exp_type, curr_exp->lineno);	
		}

		// add new mappings of id -> exp_type
		if (!isBlankIdentifier(curr_id->name)) {	
			curr_id->val.type = exp_type;
		}

		lhs_exps = lhs_exps->nextExp;
		rhs_exps = rhs_exps->nextExp;
	}
}

void typecheck_Assignment_List(SIMPLE_STMT *simple_stmt) {	

	EXP_LIST *lhs_exps = simple_stmt->val.assignment_list.lhs;
	EXP_LIST *rhs_exps = simple_stmt->val.assignment_list.rhs;

	// typecheck all expressions on left and right-hand sides
	EXP_TYPE_LIST *lhs_types = typecheck_expression_list(lhs_exps, true);
	EXP_TYPE_LIST *rhs_types = typecheck_expression_list(rhs_exps, false);

	// For every pair of lvalue/expression, typeof(vi) = typeof(ei) (no resolving)
	while (lhs_types != NULL && rhs_types != NULL && lhs_exps != NULL && rhs_exps != NULL && 
				 lhs_exps->exp != NULL && rhs_exps->exp != NULL && lhs_types->type != NULL && rhs_types->type != NULL) {
		
		// do not assert type equality when lhs expression is a blank identifier
		if (lhs_exps->exp->kind == primaryExpressionK) {
			if (lhs_exps->exp->val.primaryExpression->kind == identifierK) {

				if (!isBlankIdentifier(lhs_exps->exp->val.primaryExpression->val.identifier->name)) {
					assertTypeEquality(rhs_types->type, lhs_types->type, simple_stmt->lineno);
				}
			}
			else {
				assertTypeEquality(rhs_types->type, lhs_types->type, simple_stmt->lineno);
			}
		}
		else assertTypeEquality(rhs_types->type, lhs_types->type, simple_stmt->lineno);

		isAddressable(lhs_exps->exp);
		lhs_types = lhs_types->next;
		rhs_types = rhs_types->next;
		lhs_exps = lhs_exps->nextExp;
		rhs_exps = rhs_exps->nextExp;
	}
}

TYPE *typecheck_Op_Assignment(SIMPLE_STMT *simple_stmt) {

	// 1. Expression on the left-hand side is well-typed
	TYPE *lhs_type = typecheck_expression(simple_stmt->val.assignment.lhs);


	// 2. Expressions on the right-hand side is well-typed
	TYPE *rhs_type = typecheck_expression(simple_stmt->val.assignment.rhs);

	// 3. Expresssion on lhs must be lvalue
	isAddressable(simple_stmt->val.assignment.lhs);

	// 4. check types of lhs and rhs are accepted by the operator
	
	TYPE *lhs_resolve_type = resolveType(lhs_type, simple_stmt->lineno);
	TYPE *rhs_resolve_type = resolveType(rhs_type, simple_stmt->lineno);

	if (lhs_resolve_type->kind != baseK || rhs_resolve_type->kind != baseK) {
		fprintf(stderr, "Error: (line %d) op assignements must have expressions that resolve to base types\n", simple_stmt->lineno);
		exit(1);
	}

	BaseK lhs_base_type = lhs_resolve_type->val.baseType;
	BaseK rhs_base_type = rhs_resolve_type->val.baseType;

	switch(simple_stmt->kind) {
		case varBodyK: break;
		case typeBodyK: break;
		case ifK : break;
		case switchK: break;
		case forK : break;
		case printK : break;
		case printlnK : break;
		case continueK : break;
		case breakK : break;
		case returnK : break;
		case blockK : break;
		case assignListK : break;
		case exprSimpleK : break;
		case incK : break;
		case decK : break;
		case shortVarDeclK : break;

		case equalAssignK : break;
		case plusAssignK : 
			// check types of lhs and rhs match and are either ints, floats, or strings
			if ((lhs_base_type != intK && lhs_base_type != float64K && lhs_base_type != runeK && lhs_base_type != stringK) || (rhs_base_type != intK && rhs_base_type != float64K && rhs_base_type != runeK && rhs_base_type != stringK)) {
				fprintf(stderr, "Error: (line: %d) plus-assignment requires both lhs and rhs are eiter int, float64, rune, or string\n", simple_stmt->lineno); exit(1);
			}
			assertTypeEquality(lhs_type, rhs_type, simple_stmt->lineno);
			break;
		case minusAssignK :
			if ((lhs_base_type != intK && lhs_base_type != float64K && lhs_base_type != runeK) || (rhs_base_type != intK && rhs_base_type != float64K && rhs_base_type != runeK)) {
				fprintf(stderr, "Error: (line: %d) minus-assignment requires both lhs and rhs are eiter int, float64, or rune\n", simple_stmt->lineno); exit(1);
			}
			assertTypeEquality(lhs_type, rhs_type, simple_stmt->lineno);
			break;
		case orAssignK : 
			if ((lhs_base_type != intK && lhs_base_type != runeK) || (rhs_base_type != intK && rhs_base_type != runeK)) {
				fprintf(stderr, "Error: (line: %d) or-assignment requires both lhs and rhs are int or rune\n", simple_stmt->lineno); exit(1);
			} 
			assertTypeEquality(lhs_type, rhs_type, simple_stmt->lineno);
			break;
		case xorAssignK :
			if ((lhs_base_type != intK && lhs_base_type != runeK) || (rhs_base_type != intK && rhs_base_type != runeK)) {
					fprintf(stderr, "Error: (line: %d) xor-assignment requires both lhs and rhs are int or rune\n", simple_stmt->lineno); exit(1);
			}
			assertTypeEquality(lhs_type, rhs_type, simple_stmt->lineno);
			break;
		case mulAssignK : 
			if ((lhs_base_type != intK && lhs_base_type != float64K && lhs_base_type != runeK) || (rhs_base_type != intK && rhs_base_type != float64K && rhs_base_type != runeK)) {
					fprintf(stderr, "Error: (line: %d) mul-assignment requires both lhs and rhs are eiter int, float64, or rune\n", simple_stmt->lineno); exit(1);
			}
			assertTypeEquality(lhs_type, rhs_type, simple_stmt->lineno);
			break;
		case divAssignK :
			if ((lhs_base_type != intK && lhs_base_type != float64K && lhs_base_type != runeK) || (rhs_base_type != intK && rhs_base_type != float64K && rhs_base_type != runeK)) {
					fprintf(stderr, "Error: (line: %d) div-assignment requires both lhs and rhs are eiter int, float64, or rune\n", simple_stmt->lineno); exit(1);
			}
			assertTypeEquality(lhs_type, rhs_type, simple_stmt->lineno);
			break;
		case remAssignK :
			if ((lhs_base_type != intK && lhs_base_type != runeK) || (rhs_base_type != intK && rhs_base_type != runeK)) {
					fprintf(stderr, "Error: (line: %d) rem-assignment requires both lhs and rhs are int or rune\n", simple_stmt->lineno); exit(1);
			}
			assertTypeEquality(lhs_type, rhs_type, simple_stmt->lineno);
			break;
		case lshiftAssignK :
			if ((lhs_base_type != intK && lhs_base_type != runeK) || (rhs_base_type != intK && rhs_base_type != runeK)) {
					fprintf(stderr, "Error: (line: %d) left-shift-assignment requires both lhs and rhs are int or rune\n", simple_stmt->lineno); exit(1);
			}
			assertTypeEquality(lhs_type, rhs_type, simple_stmt->lineno);
			break;
		case rshiftAssignK :
			if ((lhs_base_type != intK && lhs_base_type != runeK) || (rhs_base_type != intK && rhs_base_type != runeK)) {
					fprintf(stderr, "Error: (line: %d) right-shift-assignment requires both lhs and rhs are int or rune\n", simple_stmt->lineno); exit(1);
			}
			assertTypeEquality(lhs_type, rhs_type, simple_stmt->lineno);
			break;
		case andAssignK :
			if ((lhs_base_type != intK && lhs_base_type != runeK) || (rhs_base_type != intK && rhs_base_type != runeK)) {
					fprintf(stderr, "Error: (line: %d) and-assignment requires both lhs and rhs are int or rune\n", simple_stmt->lineno); exit(1);
			}
			assertTypeEquality(lhs_type, rhs_type, simple_stmt->lineno);
			break;
		case bitclearAssignK :
			if ((lhs_base_type != intK && lhs_base_type != runeK) || (rhs_base_type != intK && rhs_base_type != runeK)) {
					fprintf(stderr, "Error: (line: %d) bitwise-clear-assignment requires both lhs and rhs are int or rune\n", simple_stmt->lineno); exit(1);
			}
			assertTypeEquality(lhs_type, rhs_type, simple_stmt->lineno);
			break;
		case simpleK : break;
	}

	return lhs_type;
}

// TODO: implement block as a separate type of STMTS
void typecheck_Block_Statment(STMTS *block);

void typecheck_Print_Statement(EXP_LIST *print) {
	
	// typecheck all expressions in list
	EXP_TYPE_LIST *list = typecheck_expression_list(print, false);
	TYPE *base_type;

	// each expression must resolve to a base type
	while (list) {
		base_type = resolveType(list->type, print->lineno);

		// Returned base_type can be baseK or stuctK, so resolve again for structK
		while (base_type->kind == structK) {
			base_type = resolveType(base_type->val.field->type, print->lineno);
		}

		if (base_type->kind != baseK){
			fprintf(stderr, "Error: (line: %d) expressions in print statement must resolve to a base type\n", print->lineno);
			exit(1);
		}
		list = list->next;
	}
}

void typecheck_Println_Statement(EXP_LIST *println) {

	// typecheck all expressions in list
	EXP_TYPE_LIST *list = typecheck_expression_list(println, false);

	// each expression must resolve to a base type
	while (list) {
		TYPE *base_type = resolveType(list->type, println->lineno);
		
		if (base_type->kind != baseK) {
			fprintf(stderr, "Error: (line %d) expressions in println statement must resolve to a base type\n", println->lineno);
			exit(1);
		}
		list = list->next;
	}
}

void typecheck_For_Statement(FUNCTION_DECLARATION *fun_dclr, FOR_STATEMENT *for_stmt, int lineno) {
	
	// infinite : only check stmts

	// while
	if (for_stmt->kind == whileK) {
		
		if (for_stmt->val.while_condition != NULL) {

			TYPE *cond_type = typecheck_expression(for_stmt->val.while_condition);
			TYPE *base_type = resolveType(cond_type, lineno);
			if (base_type->kind != baseK || base_type->val.baseType != boolK) {
				fprintf(stderr, "Error: (line %d) while condition must resolve to a boolean\n", lineno);
				exit(1);
			}
		}
		
	}

	// three-part
	else if (for_stmt->kind == forClauseK) {

		if (for_stmt->val.clause->simple_stmt1 != NULL ) {
			typecheck_Simple_Stmt(for_stmt->val.clause->simple_stmt1);
		}

		if (for_stmt->val.clause->condition != NULL ) {
			TYPE *cond_type = typecheck_expression(for_stmt->val.clause->condition);
			TYPE *base_type = resolveType(cond_type, lineno);
			if (base_type->kind != baseK) {
				fprintf(stderr, "Error: (line %d) for condition must resolve to a boolean\n", lineno);
                                exit(1);
			}
			else if (base_type->val.baseType != boolK) {
				fprintf(stderr, "Error: (line %d) for condition must resolve to a boolean\n", lineno);
                                exit(1);
			}
		}

		if (for_stmt->val.clause->simple_stmt2 != NULL ) {
			typecheck_Simple_Stmt(for_stmt->val.clause->simple_stmt2);
		}
	}	
	
	hasBreak = 0;
	if (for_stmt->block != NULL) {
		typecheck_STMTS(fun_dclr, for_stmt->block);
	}

	// terminating statement
	switch (for_stmt->kind) {
		case whileK : 
			{
				if (!hasBreak && for_stmt->val.while_condition == NULL) isTermStmt = 1;
			}
			break;
		case forClauseK :
			{
				if (!hasBreak && for_stmt->val.clause->condition == NULL) isTermStmt = 1;
			}
			break;
		case infiniteK :
			{
				if (!hasBreak) isTermStmt = 1;
			}
			break;
		default: fprintf(stderr, "Error: (line %d) not a valid for loop kind\n", lineno); exit(1);
	}
	hasBreak = 0;

}

void typecheck_If_Statement(FUNCTION_DECLARATION *fun_dclr, STMTS *stmt) {
	
	if (stmt->val.if_stmt.simplestmt != NULL) {
		typecheck_Simple_Stmt(stmt->val.if_stmt.simplestmt);
	}

	if (stmt->val.if_stmt.condition != NULL ) {
		TYPE *cond_type = typecheck_expression(stmt->val.if_stmt.condition);
		TYPE *base_type = resolveType(cond_type, stmt->lineno);
		if (base_type->kind != baseK) {
			fprintf(stderr, "Error: (line %d) if condition must resolve to a boolean\n", stmt->lineno);
                        exit(1);
		}
		else if (base_type->val.baseType != boolK) {
			fprintf(stderr, "Error: (line %d) if condition must resolve to a boolean\n", stmt->lineno);                     
                        exit(1);
		}
	}

	// record if if-block has terminating statement
	int ifIsTermStmt = 0;
	if (stmt->val.if_stmt.block != NULL ) {
		typecheck_STMTS(fun_dclr, stmt->val.if_stmt.block);
		
		STMTS *body = stmt->val.if_stmt.block;
                while ( body->nextStmt && body) {
                        body = body->nextStmt;
                }

                // whether last stmt is a terminating stmt
                if (body) {
                	ifIsTermStmt = body->isTerminating;
			stmt->isTerminating = ifIsTermStmt;
		}
	}

	// record if all else-if/else blocks have terminating stmts
	if (stmt->val.if_stmt.next != NULL ) {
		typecheck_STMTS(fun_dclr, stmt->val.if_stmt.next);
		stmt->isTerminating = ifIsTermStmt && stmt->val.if_stmt.next->isTerminating;		
	}

}

TYPE *typecheck_Switch_On(SWITCH_ON *switch_on) {

	if (switch_on->simpleStmt != NULL ) {
		typecheck_Simple_Stmt(switch_on->simpleStmt);
	}

	if (switch_on->condition != NULL) {
		TYPE *cond_type = typecheck_expression(switch_on->condition);

		// check cond_type is a comparable type
		if (!isComparable(cond_type)) {
			fprintf(stderr, "Error: (line %d) not a valid comparable type\n", switch_on->condition->lineno);
                        exit(1);
		}

		return cond_type; 
	}

	return NULL;
}

void typecheck_Switch_Case(FUNCTION_DECLARATION *fun_dclr, SWITCH_CASE *switch_case, TYPE *cond_type, int lineno) {

	if (switch_case == NULL) {
		return;
	}

	typecheck_Switch_Case(fun_dclr, switch_case->next, cond_type, lineno);

	if (switch_case->caseExp != NULL ) {
			EXP_TYPE_LIST *list = typecheck_expression_list(switch_case->caseExp, false);
		
			// all types must be same as condition
			if ( cond_type != NULL ) {
				while (list) {
					assertTypeEquality(cond_type, list->type, lineno);
					list = list->next;
				}
			}
			else {
				while (list) {
					if (list->type->kind != baseK || list->type->val.baseType != boolK) {
						fprintf(stderr, "Error: (line %d) switch statement expresion type is incompatible with case type [%s != bool]\n", lineno, convertTypeToString(list->type));
						exit(1);							
					}
					list = list->next;
				}
			}
	}

	hasBreak = 0;
	if (switch_case->caseStmts != NULL) {
			typecheck_STMTS(fun_dclr, switch_case->caseStmts);
			
			STMTS *body = switch_case->caseStmts;
               		while ( body->nextStmt && body) {
                        		body = body->nextStmt;
                	}

                	// whether last stmt is a terminating stmt
                	if (body) {
                	        switch_case->caseStmts->isTerminating = body->isTerminating && !hasBreak;
                	}
	}
	hasBreak = 0;

}

void typecheck_Inc_Statement(SIMPLE_STMT *simple_stmt) {

	// assert expression is addressable
	isAddressable(simple_stmt->val.exp);

	TYPE *expr_type = typecheck_expression(simple_stmt->val.exp);

	// check if expression resolves to a numeric type
	TYPE *base_type = resolveType(expr_type, simple_stmt->lineno);
	if (base_type->kind != baseK) {
		fprintf(stderr, "Error: (line %d) incompatible type in increment [expected numeric (int, rune, float64)]\n", simple_stmt->lineno);
		exit(1);
	}
	if (base_type->val.baseType != intK && base_type->val.baseType != float64K && base_type->val.baseType != runeK) {
		fprintf(stderr, "Error: (line %d) incompatible type in increment [expected numeric (int, rune, float64)]\n", simple_stmt->lineno);
		exit(1);
	}	
}
void typecheck_Dec_Statement(SIMPLE_STMT *simple_stmt) {

	// assert expression is addressable
        isAddressable(simple_stmt->val.exp);

	TYPE *expr_type = typecheck_expression(simple_stmt->val.exp);

	// check if expression resolves to a numeric type
	TYPE *base_type = resolveType(expr_type, simple_stmt->lineno);
	if (base_type->kind != baseK) {
		fprintf(stderr, "Error: (line %d) incompatible type in decrement [expected numeric (int, rune, float64)]\n", simple_stmt->lineno);
		exit(1);
	}
	if (base_type->val.baseType != intK && base_type->val.baseType != float64K && base_type->val.baseType != runeK) {
		fprintf(stderr, "Error: (line %d) incompatible type in decrement [expected numeric (int, rune, float64)]\n", simple_stmt->lineno);
		exit(1);
	}
}

void typecheck_Simple_Stmt(SIMPLE_STMT *simple_stmt) {

	switch(simple_stmt->kind) {
      		case varBodyK : break;
      		case typeBodyK : break;
      		case ifK : break;
      		case switchK : break;
      		case forK : break;
      		case printK : break;
      		case printlnK : break;
      		case continueK: break;
      		case breakK : break;
      		case returnK : break;

      		// simple statements
      		case assignListK :
        		typecheck_Assignment_List(simple_stmt);
        		break;
      		case exprSimpleK :
        		typecheck_Expression_Statement(simple_stmt);
        		break;
      		case incK :
        		typecheck_Inc_Statement(simple_stmt);
        		break;
      		case decK :
        		typecheck_Dec_Statement(simple_stmt);
        		break;
      		case shortVarDeclK :
        		typecheck_Short_Var_Decl(simple_stmt);
        		break;
      		case equalAssignK :
        		typecheck_Assignment_List(simple_stmt);
        		break;
      		case plusAssignK :
        		typecheck_Op_Assignment(simple_stmt);
        		break;
      		case minusAssignK :
        		typecheck_Op_Assignment(simple_stmt);
        		break;
      		case orAssignK :
        		typecheck_Op_Assignment(simple_stmt);
        		break;
      		case xorAssignK :
        		typecheck_Op_Assignment(simple_stmt);
        		break;
      		case mulAssignK :
        		typecheck_Op_Assignment(simple_stmt);
        		break;
      		case divAssignK :
        		typecheck_Op_Assignment(simple_stmt);
        		break;
      		case remAssignK :
        		typecheck_Op_Assignment(simple_stmt);
        		break;
      		case lshiftAssignK :
        		typecheck_Op_Assignment(simple_stmt);
        		break;
      		case rshiftAssignK :
        		typecheck_Op_Assignment(simple_stmt);
        		break;
      		case andAssignK :
        		typecheck_Op_Assignment(simple_stmt);
        		break;
      		case bitclearAssignK:
        		typecheck_Op_Assignment(simple_stmt);
        		break;
      		case simpleK :
        		typecheck_Simple_Stmt(simple_stmt);
        		break;
      		default : fprintf(stderr, "Error: (line: %d) Not valid StatementK (simple stmt)\n", simple_stmt->lineno); exit(1);
   	}

}

// void symbol_Clause(CLAUSE *c);


/* ============ EXPRESSION ============ */

TYPE *typecheck_expression(EXP *e) {
	if (e == NULL) {
		return NULL;
	}
	
	TYPE *expr_type;

	switch(e->kind) {
		case primaryExpressionK:    expr_type = typecheck_Primary_Expression(e->val.primaryExpression); break;
		case unaryOpK:              expr_type = typecheck_Unary_Op(e->val.unaryOpExpression); break;
		case binaryOpK:             expr_type = typecheck_Binary_Op(e->val.binaryOpExpression); break;
		case builtinK:              expr_type = typecheck_Builtin(e->val.builtinExpression); break;
		default:                    fprintf(stderr, "Error: (line: %d) Not a valid expression kind in typecheck \n", e->lineno); exit(1);
	}

        if(expr_type == NULL){
          fprintf(stderr, "Error: (line %d) Type has to be resolved\n", e->lineno);
	  exit(1);
        }

	e->type = expr_type;
	return expr_type;
}

EXP_TYPE_LIST *typecheck_expression_list(EXP_LIST *exp_list, bool isLHS) {

	EXP_TYPE_LIST *list = (EXP_TYPE_LIST *)malloc(sizeof(EXP_TYPE_LIST));
	EXP_TYPE_LIST **list_ptr = &list;
	
	while (exp_list && exp_list->exp) {

		if (!list->type) {
			if (isLHS && exp_list->exp->kind == primaryExpressionK && exp_list->exp->val.primaryExpression->kind == blankExpK) {
				// Do not typcheck if blank statement
			} else {
				list->type = typecheck_expression(exp_list->exp);
			}
		}
		else {
			EXP_TYPE_LIST *next_node = (EXP_TYPE_LIST *)malloc(sizeof(EXP_TYPE_LIST));
			next_node->type = typecheck_expression(exp_list->exp);
			(*list_ptr)->next = next_node;
			list_ptr = &((*list_ptr)->next);
		}

		exp_list = exp_list->nextExp;
  	}
	return list;
}																
TYPE *typecheck_Primary_Expression(PrimaryExpression *primaryExp) {

	TYPE *expr_type = NULL;

	switch(primaryExp->kind) {
		case identifierK :	    
					    if (primaryExp->val.identifier->category == category_function && !isFunctionCall) {
							fprintf(stderr, "Error: (line %d) function name %s cannot be used without being called\n", primaryExp->lineno, primaryExp->val.identifier->name);
							exit(1);
					    }
					    // covers type cast case
					    if (primaryExp->val.identifier->category == category_type && !isFunctionCall) {
							fprintf(stderr, "Error: (line %d) type %s cannot be used as a variable\n", primaryExp->lineno, primaryExp->val.identifier->name);
							exit(1);
					    }

					    if (isBlankIdentifier(primaryExp->val.identifier->name)) {
						break;
					    }

					    if (primaryExp->val.identifier->val.type != NULL) {
					    	expr_type = primaryExp->val.identifier->val.type;
					    }
					    break;
		case parenthesisExpK : 	    
					    expr_type = typecheck_expression(primaryExp->val.parenthesisExp);
					    break;

		case intLiteralK: 	   
					    expr_type = primaryExp->base_type->val.type;
					    break;

		case floatLiteralK:         
					    expr_type = primaryExp->base_type->val.type;
					    break;

		case runeLiteralK:          
					    expr_type = primaryExp->base_type->val.type;
					    break;

		case strLiteralK:           
					    expr_type = primaryExp->base_type->val.type;
					    break;
		case blankExpK:
			fprintf(stderr, "Error: (line: %d) Invalid use of blank identifier\n", primaryExp->lineno); 
			exit(1);

		case primaryExpArgumentK:   expr_type = typecheck_Primary_Expression_arguments(primaryExp); break;
		case primaryExpSelectorK:   expr_type = typecheck_Primary_Expression_selector(primaryExp); break;
		case primaryExpIndexK:      expr_type = typecheck_Primary_Expression_index(primaryExp); break;
		default:                    fprintf(stderr, "Error: (line: %d) Not a valid primaryExp kind\n", primaryExp->lineno); exit(1);
	}

	return expr_type;
}

TYPE *typecheck_Primary_Expression_arguments(PrimaryExpression *primaryExp) {
	
	isFunctionCall = 1;
	typecheck_Primary_Expression(primaryExp->val.argument.primaryExp);
	isFunctionCall = 0;

	// function calls: loop through each argument/expression and check if type matches that of declaration
	if (primaryExp->base_type->category == category_function) {	
		if(primaryExp->val.argument.expList){
		 	EXP_TYPE_LIST *type_list = typecheck_expression_list(primaryExp->val.argument.expList, false);
			PARAMETER *param_list = primaryExp->base_type->val.function->parameter;
		
			while (type_list && type_list->type && param_list && param_list->type) {
				assertTypeEquality(type_list->type, param_list->type, primaryExp->lineno);
				type_list = type_list->next;
				param_list = param_list->nxt;
			}
			if (param_list != NULL || type_list != NULL){
				fprintf(stderr, "Error: (line %d) function not called with appropriate number of arguments\n", primaryExp->lineno);
				exit(1);
	
			}
		}
		return primaryExp->base_type->val.function->result;
	}

	// type casts
	else {
		if (resolveType(primaryExp->base_type->val.type, primaryExp->lineno)->kind != baseK) {
			fprintf(stderr, "Error: (line %d) conversion expects base types [received %s]\n", primaryExp->lineno, primaryExp->base_type->name);
			exit(1);
		}
		
		// check expression is well typed
		EXP_TYPE_LIST *type_list = typecheck_expression_list(primaryExp->val.argument.expList, false);
		if (type_list && type_list->type) {
			
			// type and expr must resolve to numeric types or type resolves to string and expr to int or resolve to identical types
			if (!(isNumeric(primaryExp->base_type->val.type) && isNumeric(type_list->type))) {
				if (!(isString(primaryExp->base_type->val.type) && isInteger(type_list->type))) {
					assertTypeEquality(resolveType(primaryExp->base_type->val.type, primaryExp->lineno), resolveType(type_list->type, primaryExp->lineno), primaryExp->lineno);
				}
			}
		}		
		else {
			fprintf(stderr, "Error: (line %d) type cast expects one argument\n", primaryExp->lineno);
			exit(1);
		}	

		if (type_list->next) {
			fprintf(stderr, "Error: (line %d) conversion only takes one expression argument\n", primaryExp->lineno);
			exit(1);
		}

		return primaryExp->base_type->val.type;
	}
}

TYPE *get_Field_Type(FIELD *field, char *name, int lineno){

  while(field){
    if(strcmp(field->name, name) == 0) return field->type;

    field = field->nxt;
  }

  fprintf(stderr, "Error: (line %d) target struct does not have field %s\n", lineno, name);
  exit(1);
}

TYPE *typecheck_Primary_Expression_selector(PrimaryExpression *primaryExp) {

  if(primaryExp->val.selector.type) return primaryExp->val.selector.type;

  TYPE * type = typecheck_Primary_Expression(primaryExp->val.selector.primaryExp);

  if(isTypeNodeKindOf(type, structK)){
    type = getType(type, structK);

    primaryExp->val.selector.type = get_Field_Type(type->val.field, primaryExp->val.selector.identifier, primaryExp->lineno);

    return primaryExp->val.selector.type;
  }
  else{
    fprintf(stderr, "Error: (line %d) selector target must be a struct type [received %s]\n", primaryExp->lineno, convertTypeToString(type));
    exit(1);
  }
}

TYPE *typecheck_Primary_Expression_index(PrimaryExpression *primaryExp) {
  TYPE *type_expr = typecheck_Primary_Expression(primaryExp->val.index.primaryExp);
 
  if (!(isTypeNodeKindOf(type_expr, arrayK) || isTypeNodeKindOf(type_expr, sliceK)) ) {
    fprintf(stderr, "Error: (line: %d) expression of indexing must be of array or slice type\n", primaryExp->lineno);
    exit(1);
  }

  if(isTypeNodeKindOf(type_expr, arrayK)){    
    TYPE *type_index = typecheck_expression(primaryExp->val.index.indexExp);
    if (!isBaseKindOf(type_index, intK)) {
      fprintf(stderr, "Error: (line: %d) index of indexing must be of type int \n", primaryExp->lineno);
      exit(1);
    }
  }

  if (type_expr->kind == arrayK) {
    return type_expr->val.array.element;
  } else {
    return type_expr->val.slice;
  }
}

TYPE *typecheck_Unary_Op(UnaryOpExpression *unaryOpExp) {
	TYPE *type = typecheck_expression(unaryOpExp->unaryRHS);

	switch(unaryOpExp->kind) {
		// Unary plus: expr must resolve to a numeric type (int, float64, rune)
		case unaryPlusK:
			if (isNumeric(type)) {
				return type;
			} else {
				fprintf(stderr, "Error: (line: %d) incomatible type in unary op + [received %s, expected numeric (int, rune, float64)] \n", unaryOpExp->lineno, convertTypeToString(type));
				exit(1);
			}
		// Negation: expr must resolve to a numeric type (int, float64, rune)
		case unaryMinusK:
			if (isNumeric(type)) {
				return type;
			} else {
				fprintf(stderr, "Error: (line: %d) incomatible type in unary op - [received %s, expected numeric (int, rune, float64)] \n", unaryOpExp->lineno, convertTypeToString(type));
				exit(1);
			}
		// Logical negation: expr must resolve to a bool
		case notK:
			if (isBool(type)){
				return type;
			} else {
				fprintf(stderr, "Error: (line: %d) incomatible type in unary op ! [received %s, expected bool] \n", unaryOpExp->lineno, convertTypeToString(type));
				exit(1);
			}
		// Bitwise negation: expr must resolve to an integer type (int, rune)
		case bitwiseComplementK:
			if (isInteger(type)) {
				return type;
			} else {
				fprintf(stderr, "Error: (line: %d) incomatible type in unary op ^ [received %s, expected integer (int, rune)] \n", unaryOpExp->lineno, convertTypeToString(type));
				exit(1);
			}
		default:
			fprintf(stderr, "Error: (line: %d) Not a valid unary operation kind\n", unaryOpExp->lineno);
			exit(1);
	}
}

TYPE *typecheck_Binary_Op(BinaryOpExpression *binaryOpExp) {
	TYPE *type1 = typecheck_expression(binaryOpExp->lhs);
	TYPE *type2 = typecheck_expression(binaryOpExp->rhs);

	// binary op expression must both have same types
	assertTypeEquality(type1, type2, binaryOpExp->lineno);	// Now we know type1 and type2 has same type

	switch(binaryOpExp->kind) {
		case plusK:
			if (isNumeric(type1) && isNumeric(type2)) {
				return type1;
			} else if (type1->kind == baseK && type1->val.baseType == stringK) {
				return type1;
			} else {
				fprintf(stderr, "Error: (line: %d) incomatible type in arithmetic op + [received %s, expected numeric (int, rune, float64) or string] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case subK:
			if (isNumeric(type1) && isNumeric(type2)) {
				return type1;
			} else {
				fprintf(stderr, "Error: (line: %d) incomatible type in arithmetic op - [received %s, expected numeric (int, rune, float64)] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case multK:
			if (isNumeric(type1) && isNumeric(type2)) {
				return type1;
			} else {
				fprintf(stderr, "Error: (line: %d) incomatible type in arithmetic op * [received %s, expected numeric (int, rune, float64)] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case divK:
			if (isNumeric(type1) && isNumeric(type2)) {
				return type1;
			} else {
				fprintf(stderr, "Error: (line: %d) incomatible type in arithmetic op / [received %s, expected numeric (int, rune, float64)] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case remK:
			if (isNumeric(type1) && isNumeric(type2)) {
				return type1;
			} else {
				fprintf(stderr, "Error: (line: %d) incomatible type in arithmetic op %% [received %s, expected numeric (int, rune, float64)] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case bitOrK:
			if (isNumeric(type1) && isNumeric(type2)) {
				return type1;
			} else {
				fprintf(stderr, "Error: (line: %d) incomatible type in arithmetic op | [received %s, expected numeric (int, rune, float64)] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case bitAnd:
			if (isNumeric(type1) && isNumeric(type2)) {
				return type1;
			} else {
				fprintf(stderr, "Error: (line: %d) incomatible type in arithmetic op & [received %s, expected numeric (int, rune, float64)] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case bitwiseXorK:
			if (isNumeric(type1) && isNumeric(type2)) {
				return type1;
			} else {
				fprintf(stderr, "Error: (line: %d) incomatible type in arithmetic op ^ [received %s, expected numeric (int, rune, float64)] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case bitClearK:
			if (isNumeric(type1) && isNumeric(type2)) {
				return type1;
			} else {
				fprintf(stderr, "Error: (line: %d) incomatible type in arithmetic op &^ [received %s, expected numeric (int, rune, float64)] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case andK:
			if (isBool(type1) && isBool(type2)) {
				return type1;
			} else {
				fprintf(stderr, "Error: (line: %d) incompatible type in logical op && [received %s, expected bool] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case orK:
			if (isBool(type1) && isBool(type2)) {
				return type1;
			} else {
				fprintf(stderr, "Error: (line: %d) incompatible type in logical op || [received %s, expected bool] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case equalK:
			if (isComparable(type1) && isComparable(type2)) {
				TYPE *t = NEW(TYPE);
				t->kind = baseK;
				t->val.baseType = boolK;
				return t;
			} else {
				fprintf(stderr, "Error: (line: %d) incompatible type in relational op == [received %s, expected comparable] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case notEqualK:
			if (isComparable(type1) && isComparable(type2)) {
				TYPE *t = NEW(TYPE);
				t->kind = baseK;
				t->val.baseType = boolK;
				return t;
			} else {
				fprintf(stderr, "Error: (line: %d) incompatible type in relational op != [received %s, expected comparable] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case lesserK: 
			if (isOrdered(type1) && isOrdered(type2)) {
				TYPE *t = NEW(TYPE);
				t->kind = baseK;
				t->val.baseType = boolK;
				return t;
			} else {
				fprintf(stderr, "Error: (line: %d) incompatible type in relational op < [received %s, expected ordered (int, rune, float64, string)] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case ltEqualsK: 
			if (isOrdered(type1) && isOrdered(type2)) {
				TYPE *t = NEW(TYPE);
				t->kind = baseK;
				t->val.baseType = boolK;
				return t;
			} else {
				fprintf(stderr, "Error: (line: %d) incompatible type in relational op <= [received %s, expected ordered (int, rune, float64, string)] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case greaterK: 
			if (isOrdered(type1) && isOrdered(type2)) {
				TYPE *t = NEW(TYPE);
				t->kind = baseK;
				t->val.baseType = boolK;
				return t;
			} else {
				fprintf(stderr, "Error: (line: %d) incompatible type in relational op > [received %s, expected ordered (int, rune, float64, string)] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case gtEqualsK: 
			if (isOrdered(type1) && isOrdered(type2)) {
				TYPE *t = NEW(TYPE);
				t->kind = baseK;
				t->val.baseType = boolK;
				return t;
			} else {
				fprintf(stderr, "Error: (line: %d) incompatible type in relational op >= [received %s, expected ordered (int, rune, float64, string)] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case leftShiftK:
			if (isInteger(type1) && isInteger(type2))  {
				return type1;
			} else {
				fprintf(stderr, "Error: (line: %d) incomatible type in arithmetic op << [received %s, expected integer (int, rune)] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		case rightShiftK:
			if (isInteger(type1) && isInteger(type2))  {
				return type1;
			} else {
				fprintf(stderr, "Error: (line: %d) incomatible type in arithmetic op >> [received %s, expected integer (int, rune)] \n", binaryOpExp->lineno, convertTypeToString(type1));
				exit(1);
			}
		default: 
			fprintf(stderr, "Error: (line: %d) Not a valid binary operation kind\n", binaryOpExp->lineno);
			exit(1);
	}
}

bool isTypeNodeKindOf(TYPE *type, TypeK kind){
  if(type == NULL) return false;
    switch(type->kind){
    case sliceK:
      if(sliceK == kind) return true;
      return isTypeNodeKindOf(type->val.slice, kind);
    case refK:
      if(refK == kind) return true;
      return isTypeNodeKindOf(type->val.ref.element, kind);
    case arrayK:
      if(arrayK == kind) return true;
      return isTypeNodeKindOf(type->val.array.element, kind);
    case structK: return structK == kind;
    case baseK:   return baseK   == kind;
    default:
      fprintf(stderr, "Error: This kind is not in TypeK\n");
      exit(1);
    }
}

bool isBaseKindOf(TYPE *type, BaseK kind){
  if(type == NULL) return false;
    switch(type->kind){
    case sliceK:  return isBaseKindOf(type->val.slice, kind);
    case refK:    return isBaseKindOf(type->val.ref.element, kind);
    case arrayK:  return isBaseKindOf(type->val.array.element, kind);
    case structK: return false;
    case baseK:   return type->val.baseType  == kind;
    default:
      fprintf(stderr, "Error: This kind is not in TypeK\n");
      exit(1);
    }
}

TYPE * getType(TYPE *type, TypeK kind){
  if(type == NULL) return false;
    switch(type->kind){
    case sliceK:
      if(sliceK == kind) return type->val.slice;
      return getType(type->val.slice, kind);
    case refK:
      if(refK == kind) return type->val.ref.element;
      return getType(type->val.ref.element, kind);
    case arrayK:
      if(arrayK == kind) return type->val.array.element;
      return getType(type->val.array.element, kind);
    case structK: return type;
    case baseK:   return type;
    default:
      fprintf(stderr, "Error: This kind is not in TypeK\n");
      exit(1);
    }
}

TYPE *typecheck_Builtin(BuiltInExpression *builtInExp) {
  TYPE *arg_expr_type = NULL;

  switch(builtInExp->kind) {
  case appendK:
    {
      TYPE *arg1_type = typecheck_expression(builtInExp->val.append.arg1);
      TYPE *arg2_type = typecheck_expression(builtInExp->val.append.arg2);

      if (isTypeNodeKindOf(arg1_type, sliceK)) {
        TYPE *arg1_resolved_type = getType(arg1_type, sliceK);

        if (arg1_resolved_type->kind == arg2_type->kind) {
          assertTypeEquality(arg1_resolved_type, arg2_type, builtInExp->lineno);

          arg_expr_type = NEW(TYPE);
          arg_expr_type = arg1_type;
        } 
        else {
          fprintf(stderr, "Error: (line: %d) append expression slice type is incompatible with element type\n", builtInExp->lineno); // TODO: get types later
          exit(1);                      
        }

      } 
      else {
        fprintf(stderr, "Error: (line: %d) append builtin expects slice type as first argument [received %s]\n", builtInExp->lineno, convertTypeToString(arg1_type));
        exit(1);
      }
                                
    }
    break;
  case lenK:
    {
      TYPE *arg_type = typecheck_expression(builtInExp->val.len);

      if( isTypeNodeKindOf(arg_type, sliceK) 
          || (isTypeNodeKindOf(arg_type, arrayK)
          || ( isTypeNodeKindOf(arg_type, baseK) && isBaseKindOf(arg_type, stringK) ) )) {

        arg_expr_type = builtInExp->return_type->val.type;
      }
      else {
        fprintf(stderr, "Error: (line: %d) arg of len is not of type array\n", builtInExp->lineno);
        exit(1);
      }
    } 
    break;
  case capK: 
    {
      TYPE *arg_type = typecheck_expression(builtInExp->val.cap);
      if (isTypeNodeKindOf(arg_type, sliceK) || isTypeNodeKindOf(arg_type, arrayK)) {
        arg_expr_type = builtInExp->return_type->val.type;;
      } else {
        fprintf(stderr, "Error: (line: %d) arg of cap must resolve to type slice or array\n", builtInExp->lineno);
        exit(1);
      }
    }
    break;
  }

  return arg_expr_type;
}

