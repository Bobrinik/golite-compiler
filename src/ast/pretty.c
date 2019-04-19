#include <stdio.h>
#include <stdlib.h>
#include "pretty.h"
#include "tree.h"
#include "symbol.h"
#include <stdbool.h>

int tabNum = 0;
int isStructResult = 0;

void printTab() {
    for (int i = 0; i < tabNum; i++) {
        printf("    ");
    }
}

void pprint_Symbol(SYMBOL *smbl) {
  if(!smbl) {
    fprintf(stderr, "Error: Tried to ppirnt null smbl\n");
    exit(1);
  }

  printf("%s", smbl->name);
}

void pprint_Identifier_list(IDENTIFIER_LIST *list){
  IDENTIFIER_LIST *crnt = list;

  while (crnt){
    pprint_Symbol(crnt->symbol);
    crnt = crnt->nxt;
    if(crnt) printf(", ");
  }
}

/* ==================================================== */

void pprint_Program(PROGRAM *root) {

  if(!root) return; // if file is empty

  printf("package %s\n", root->package->name);
  pprint_Top_Declaration_List(root->top_declaration_list);
}

void pprint_Top_Declaration_List(TOP_DECLARATION_LIST *list) {
  TOP_DECLARATION_LIST *crnt = list;

  while (crnt){
    pprint_Top_Declaration(crnt->top_declaration);
    crnt = crnt->nxt;
  }
}


void pprint_Top_Declaration(TOP_DECLARATION *dclr){
  switch(dclr->kind) {
  case varK:  pprint_Var_Declaration(dclr->val.var_dclr); break;
  case typeK: pprint_Type_Declaration_list(dclr->val.type_dclr_list); break;
  case funcK: pprint_Function_Declaration(dclr->val.function_dclr); break;
  default: fprintf(stderr, "Error: Unrecognised TypeK\n"); exit(1);
  }
}

void pprint_Var_Declaration(VAR_DECLARATION *dclr){
  printf("var ");

  if(dclr->isList){
    printf("(\n");
    tabNum++;
    pprint_Declaration_list(dclr->val.declaration_list);
    tabNum--;
    printTab();
    printf(")\n");
  }
  else {
    pprint_Declaration(dclr->val.declaration);
  }
}

void pprint_Type_Declaration_list(TYPE_DECLARATION_LIST *list){
  TYPE_DECLARATION_LIST *crnt = list;
  while (crnt) { 
    pprint_Type_Declaration(crnt->type_declaration);
    crnt = crnt->nxt;
  }
}

void pprint_Type_Declaration(TYPE_DECLARATION *dclr) {
  printTab();
  printf("type ");
  switch (dclr->kind) {
	case type_structK :
		pprint_Symbol(dclr->val.structure.name);
    		printf(" struct ");
    		printf("{\n");
    		tabNum++;
    		pprint_Declaration_list(dclr->val.structure.body);
    		tabNum--;
    		printTab();
    		printf("}\n");
		break;
	case type_simpleK :
		pprint_Symbol(dclr->val.simple.name);
    		printf(" ");
    		pprint_Symbol(dclr->val.simple.type);
    		printf("\n");
		break;
	case type_arrayK :
		pprint_Symbol(dclr->val.array.name);
                printf(" ");
		pprint_Bracket_list(dclr->val.array.brackets);
                pprint_Symbol(dclr->val.array.type);
                printf("\n");	  
		break;
	case type_struct_arrayK :
		pprint_Symbol(dclr->val.array.name);
                printf(" ");
                pprint_Bracket_list(dclr->val.array.brackets);
		printf("struct ");
                printf("{\n");
                tabNum++;
                pprint_Declaration_list(dclr->val.structure.body);
                tabNum--;
                printTab();
                printf("}\n");
		break;
	default : fprintf(stderr, "Error: Unrecognised TypeK\n"); exit(1);
  }
}

/* ======================================================= */

void pprint_Function_Declaration(FUNCTION_DECLARATION *fun_dclr) {
  printTab();
  printf("func ");
  pprint_Symbol(fun_dclr->name);
  pprint_Signature(fun_dclr->signature);
  printf(" {\n");
  tabNum++;
  pprint_STMTS(fun_dclr->body);
  tabNum--;
  printTab();
  printf("}\n");
}

void pprint_Signature(SIGNATURE *signature){
  printf("(");
  pprint_Parameter_List(signature->parameters); 
  printf(")");
  if(signature->result) {
    printf(" ");
    pprint_result(signature->result);
  }
}

void pprint_result(RESULT *result) {
  if(result->kind == arrayResultK || result->kind == arrayStructResultK) {
    pprint_Bracket_list(result->brackets);
  }
  if (result->type) {
  	pprint_Symbol(result->type);
  }
  else if(result->decl_list) {
	printf("struct { ");

	isStructResult = 1;	
	DECLARATION_LIST *crnt = result->decl_list;
	while(crnt) {
		pprint_Declaration(crnt->declaration);
		printf("; ");
    		crnt = crnt->nxt;
	}	
	printf("} ");
	isStructResult = 0;
  }
}


void pprint_Parameter_List(PARAMETER_LIST *list){
  PARAMETER_LIST *crnt = list;

  while (crnt) { 
    pprint_Parameter_Unit(crnt->param);
    crnt = crnt->nxt;
    if(crnt) printf(", "); // reversing golite.y
  }
}

void pprint_Parameter_Unit(PARAMETER_UNIT *unit){
  pprint_Identifier_list(unit->ids_list);
  printf(" ");

  if(unit->isArray){
    pprint_Bracket_list(unit->brackets);
  }

  pprint_Symbol(unit->type);
}

/* ===================================================== */

void pprint_Declaration_list(DECLARATION_LIST *list) {
  DECLARATION_LIST *crnt = list;

  while (crnt) { 
    printTab();
    pprint_Declaration(crnt->declaration);
    crnt = crnt->nxt;
  }
}

void pprint_Declaration(DECLARATION *declaration) {
  pprint_Identifier_list(declaration->lhs);
 
  switch(declaration->kind){
    case Id_list_typek: 
      {
        printf(" ");
        pprint_Symbol(declaration->type);
      }
      break;
    case Id_list_arrayk:
      {
        printf(" ");
        pprint_Bracket_list(declaration->val.indx_list);
        pprint_Symbol(declaration->type);
      }
      break;
    case Id_list_eqk:
      {
        printf(" = ");
        pprint_expression_list(declaration->val.rhs_list);
      }
      break;
    case Id_list_type_eqk:
      {
        printf(" ");
        pprint_Symbol(declaration->type);
        printf(" = ");
        pprint_expression_list(declaration->val.rhs_list);
      }
      break;
    case Id_list_structK:
      {
	printf(" ");
      	printf(" struct ");
    	printf("{\n");
    	tabNum++;
    	pprint_Declaration_list(declaration->val.struct_body);
    	tabNum--;
    	printTab();
    	printf("}\n");
      }
      break;
    case Id_list_array_structK:
      {
	printf(" ");
        pprint_Bracket_list(declaration->val.indx_list);
	printf("struct ");
        printf("{\n");
        tabNum++;
        pprint_Declaration_list(declaration->val.struct_body);
        tabNum--;
        printTab();
        printf("}\n");
      }
      break;
    case Id_list_array_eqK:
      {
	printf(" ");
        pprint_Bracket_list(declaration->val.array_eq.indx_list);
        pprint_Symbol(declaration->type);
	printf(" = ");
        pprint_expression_list(declaration->val.array_eq.rhs_list);
      }
      break;
    default: fprintf(stderr, "Error: Not valid DeclarationK\n"); exit(1);
  }
  if (isStructResult) return;
  	
  printf("\n");
 
}

void pprint_Bracket(BRACKET *bracket){
  if(bracket->isEmpty) printf("[]");
  else printf("[%d]", bracket->idx);
}

void pprint_Bracket_list(BRACKET_LIST *list){
  BRACKET_LIST *crnt = list;

  while (crnt){
    pprint_Bracket(crnt->bracket);
    crnt = crnt->nxt;
  }
}

/* ============ EXPRESSION ============ */

void pprint_expression(EXP *e) {  
  if (e == NULL) {
    return;
  }

  switch(e->kind) {
      case primaryExpressionK:    pprint_Primary_Expression(e->val.primaryExpression); break;
      case unaryOpK:              pprint_Unary_Op(e->val.unaryOpExpression); break;
      case binaryOpK:             pprint_Binary_Op(e->val.binaryOpExpression); break;
      case builtinK:              pprint_Builtin(e->val.builtinExpression); break;
      default:                    fprintf(stderr, "Error: (line: %d) Not a valid expression kind\n", e->lineno); exit(1);
  }
}

void pprint_expression_list(EXP_LIST *exp_list) {
    while (exp_list->nextExp != NULL) {
        pprint_expression(exp_list->exp);
        printf(", ");
        exp_list = exp_list->nextExp;
    }
    pprint_expression(exp_list->exp);
}

void pprint_Primary_Expression(PrimaryExpression *primaryExp) {
    switch(primaryExp->kind) {
        case identifierK:           printf("%s", primaryExp->val.identifier->name); break;
        case parenthesisExpK:       pprint_expression(primaryExp->val.parenthesisExp);  break;

        case intLiteralK:           printf("%d", primaryExp->val.intLiteral); break;
        case floatLiteralK:         printf("%f", primaryExp->val.floatLiteral); break;
        case runeLiteralK:          primaryExp->val.runeLiteral.isRuneLiteralEscape ? printf("'\\%c'", primaryExp->val.runeLiteral.val) : printf("'%c'", primaryExp->val.runeLiteral.val); break;
        case strLiteralK:           printf("%s", primaryExp->val.strLiteral); break;

        case primaryExpArgumentK:   pprint_Primary_Expression_arguments(primaryExp); break;
        case primaryExpSelectorK:   pprint_Primary_Expression_selector(primaryExp); break;
        case primaryExpIndexK:      pprint_Primary_Expression_index(primaryExp); break;
        default:                    fprintf(stderr, "Error: (line: %d) Not a valid primaryExp kind\n", primaryExp->lineno); exit(1);
    }
}

void pprint_Primary_Expression_arguments(PrimaryExpression *primaryExp) {
    pprint_Primary_Expression(primaryExp->val.argument.primaryExp);
    printf("(");

    if(primaryExp->val.argument.expList){
      pprint_expression_list(primaryExp->val.argument.expList);
    }

    printf(")");
}

void pprint_Primary_Expression_selector(PrimaryExpression *primaryExp) {
  
    pprint_Primary_Expression(primaryExp->val.selector.primaryExp);
    printf(".");
    printf("%s", primaryExp->val.selector.identifier);
}

void pprint_Primary_Expression_index(PrimaryExpression *primaryExp) {
    pprint_Primary_Expression(primaryExp->val.index.primaryExp);
    printf("[");
    pprint_expression(primaryExp->val.index.indexExp);
    printf("]");
}


void pprint_Unary_Op(UnaryOpExpression *unaryOpExp) {
    switch(unaryOpExp->kind) {
        case unaryPlusK:            printf("+"); break;
        case unaryMinusK:           printf("-"); break;
        case notK:                  printf("!"); break;
        case bitwiseComplementK:    printf("^"); break;
        default:                    fprintf(stderr, "Error: (line: %d) Not a valid unaryOpExp kind\n", unaryOpExp->lineno); exit(1);
    }
    printf("(");
    pprint_expression(unaryOpExp->unaryRHS);
    printf(")");
}

void pprint_Binary_Op(BinaryOpExpression *binaryOpExp) {
    printf("(");
    pprint_expression(binaryOpExp->lhs);

    switch(binaryOpExp->kind) {
        case orK:           printf(" || "); break;
        case andK:          printf(" && "); break;
        case equalK:        printf(" == "); break;
        case notEqualK:     printf(" != "); break;
        case ltEqualsK:     printf(" <= "); break;
        case gtEqualsK:     printf(" >= "); break;
        case lesserK:       printf(" < "); break;
        case greaterK:      printf(" > "); break;
        case plusK:         printf(" + "); break;
        case subK:          printf(" - "); break;
        case bitOrK:        printf(" | "); break;
        case bitwiseXorK:   printf(" ^ "); break;
        case multK:         printf(" * "); break;
        case divK:          printf(" / "); break;
        case remK:          printf(" %c ", '%'); break;
        case rightShiftK:   printf(" >> "); break;
        case leftShiftK:    printf(" << "); break;
        case bitAnd:        printf(" & "); break;
        case bitClearK:     printf(" &^ "); break;
        default:            fprintf(stderr, "Error: (line: %d) Not a valid binaryOpExp kind\n", binaryOpExp->lineno); exit(1);
    }

    pprint_expression(binaryOpExp->rhs);
    printf(")");
}

void pprint_Builtin(BuiltInExpression *builtInExp) {
    switch(builtInExp->kind) {
        case appendK:
            printf("append(");
            pprint_expression(builtInExp->val.append.arg1);
            printf(", ");
            pprint_expression(builtInExp->val.append.arg2);
            printf(")");
            break;
        case lenK:
            printf("len(");
            pprint_expression(builtInExp->val.len);
            printf(")");
            break;
        case capK:
            printf("cap(");
            pprint_expression(builtInExp->val.cap);
            printf(")");
            break;
        default: fprintf(stderr, "Error: (line: %d) Not a valid builtInExp kind\n", builtInExp->lineno); exit(1);
    }
}

/* ============ STATEMENTS ============ */

void pprint_Simple_Stmt(SIMPLE_STMT *simple_stmt) {
	
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
      case blockK:
        break;
      // simple statements
      case assignListK :
        pprint_Assignment_List(simple_stmt);
        break;
      case exprSimpleK :
        pprint_Expression_Statement(simple_stmt);
        break;
      case incK :
        pprint_Inc_Statement(simple_stmt);
        break;
      case decK :
        pprint_Dec_Statement(simple_stmt);
        break;
      case shortVarDeclK :
        pprint_Short_Var_Decl(simple_stmt);
        break;
      case equalAssignK :
        pprint_Assignment_List(simple_stmt);
        break;
      case plusAssignK :
        pprint_Op_Assignment(simple_stmt);
        break;
      case minusAssignK :
        pprint_Op_Assignment(simple_stmt);
        break;
      case orAssignK :
        pprint_Op_Assignment(simple_stmt);
        break;
      case xorAssignK :
        pprint_Op_Assignment(simple_stmt);
        break;
      case mulAssignK :
        pprint_Op_Assignment(simple_stmt);
        break;
      case divAssignK :
        pprint_Op_Assignment(simple_stmt);
        break;
      case remAssignK :
        pprint_Op_Assignment(simple_stmt);
        break;
      case lshiftAssignK :
        pprint_Op_Assignment(simple_stmt);
        break;
      case rshiftAssignK :
        pprint_Op_Assignment(simple_stmt);
        break;
      case andAssignK :
        pprint_Op_Assignment(simple_stmt);
        break;
      case bitclearAssignK:
        pprint_Op_Assignment(simple_stmt);
        break;
      case simpleK :
        pprint_Simple_Stmt(simple_stmt);
        break;
      default : fprintf(stderr, "Error: Not valid StatementK in pprint_Simple_Stmt\n"); exit(1);
   }
}

void pprint_If_Statement(STMTS *stmt) {
	printf("if ");
        
	// optional simple statement
	if (stmt->val.if_stmt.simplestmt != NULL ) {
		pprint_Simple_Stmt(stmt->val.if_stmt.simplestmt);
    printf("; ");
	}
	
	// if block
  pprint_expression(stmt->val.if_stmt.condition);
	printf(" {\n");
  tabNum++;
  pprint_STMTS(stmt->val.if_stmt.block);
  tabNum--;
        
  if (stmt->val.if_stmt.next != NULL) { 
    printTab();
    printf("} "); 
  } else {
    printTab();
    printf("}\n"); 
  }
  
  if (stmt->val.if_stmt.next != NULL) {
    printf("else {\n");
    tabNum++;
    pprint_STMTS(stmt->val.if_stmt.next);
    tabNum--;
    printTab();
    printf("}");
  }

}

void pprint_Switch_On(SWITCH_ON *switch_on) {

	if (switch_on->simpleStmt != NULL) {
		pprint_Simple_Stmt(switch_on->simpleStmt);
		printf("; ");
	}	

	if (switch_on->condition != NULL) {
		pprint_expression(switch_on->condition);
	}
}

void pprint_Switch_Case(SWITCH_CASE *switch_case) {
  if (switch_case == NULL) {
    return;
  }
  pprint_Switch_Case(switch_case->next);

  switch(switch_case->kind) {
    case caseK: 
      printTab();
      printf("case ");
      pprint_expression_list(switch_case->caseExp);
      break;
    case defaultK:
      printTab();
      printf("default");
      break;
    default: 
      fprintf(stderr, "Error: Not valid switchK\n"); exit(1);
  }

	printf(":\n");
  tabNum++;
  pprint_STMTS(switch_case->caseStmts);
  tabNum--;
}

void pprint_Clause(CLAUSE *c) {
	
	pprint_Simple_Stmt(c->simple_stmt1);
  printf("; ");

	if (c->condition != NULL) {
		pprint_expression(c->condition);
	}
  printf("; ");

	pprint_Simple_Stmt(c->simple_stmt2);
}

void pprint_For_Statement(FOR_STATEMENT *for_stmt) {
	printf("for ");

	switch (for_stmt->kind) {
		
		case infiniteK : break;

		case forClauseK :
				pprint_Clause(for_stmt->val.clause);
				break;

		case whileK : 
				pprint_expression(for_stmt->val.while_condition);
				break;
		default: fprintf(stderr, "Error: Not valid StatementK in pprint_for_Statement\n"); exit(1);
	}

	printf(" {\n");
  tabNum++;
	pprint_STMTS(for_stmt->block);
  tabNum--;
  printTab();
	printf("}");

}

void pprint_Print_Statement(EXP_LIST *print) {
	printf("print(");
  if (print != NULL) pprint_expression_list(print);
	printf(")");

}

void pprint_Println_Statement(EXP_LIST *println) {
	printf("println(");
  if (println != NULL) pprint_expression_list(println);
  printf(")");

}

void pprint_Return_Statement(EXP *exp) {
	printf("return ");
	if (exp != NULL) {
		pprint_expression(exp);
	}
}

void pprint_Assignment_List(SIMPLE_STMT *simple_stmt) {
  pprint_expression_list(simple_stmt->val.assignment_list.lhs);
  printf(" = ");
  pprint_expression_list(simple_stmt->val.assignment_list.rhs);
}

void pprint_Expression_Statement(SIMPLE_STMT *simple_stmt) {
    if (simple_stmt->val.exp != NULL) {
		  pprint_expression(simple_stmt->val.exp);
	}
}

void pprint_Inc_Statement(SIMPLE_STMT *simple_stmt) {
    pprint_expression(simple_stmt->val.exp);
    printf("++");
}

void pprint_Dec_Statement(SIMPLE_STMT *simple_stmt) {
    pprint_expression(simple_stmt->val.exp);
    printf("--");
}

void pprint_Short_Var_Decl(SIMPLE_STMT *simple_stmt) {
    pprint_expression_list(simple_stmt->val.assignment_short_var_decl.identifierList);
    printf(" := ");
    pprint_expression_list(simple_stmt->val.assignment_short_var_decl.rhs);
}

void pprint_Op_Assignment(SIMPLE_STMT *simple_stmt) {

	pprint_expression(simple_stmt->val.assignment.lhs);

  switch (simple_stmt->kind) {
    case blockK:
      break;
		case plusAssignK : 
			printf(" += ");
			break;
    case minusAssignK : 
			printf(" -= ");
			break;
    case orAssignK : 
			printf(" |= ");
			break;
    case xorAssignK : 
			printf(" ^= ");
			break;
    case mulAssignK :
			printf(" *= ");       
			break;
    case divAssignK : 
			printf(" /= ");
			break;
    case remAssignK : 
			printf(" %%= ");
			break;
    case lshiftAssignK : 
			printf(" <<= ");
			break;
    case rshiftAssignK : 
			printf(" >>= ");
			break;
    case andAssignK : 
			printf(" &= ");
			break;
    case bitclearAssignK :
			printf(" &^= ");       
			break;
		
		// will not actually be possible (already tested in pprint_STMTS)
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
    case assignListK : break;
    case exprSimpleK : break;
    case incK : break;
    case decK : break;
    case shortVarDeclK : break;
    default : fprintf(stderr, "Error: Not valid StatementK in pprint_op_assignment\n"); exit(1);
  }

	pprint_expression(simple_stmt->val.assignment.rhs);
}

void pprint_Block(STMTS *stmts){
  printTab();
  printf("{\n");
  tabNum++;
  pprint_STMTS(stmts);
  tabNum--;
  printTab();
  printf("}");
}

void pprint_STMTS(STMTS *stmts) {
  STMTS *curr = stmts;

  while (curr) {
    if (!(curr->kind == typeBodyK || curr->kind == blockK)) {
      printTab();
    }

    switch (curr->kind) {
    case blockK :
      pprint_Block(curr->val.block);
      break;
    case varBodyK : 
      pprint_Var_Declaration(curr->val.var_dclr);
      break;
    case typeBodyK : 
      pprint_Type_Declaration_list(curr->val.type_dclr);
      break;
    case ifK : 
      pprint_If_Statement(curr);
      break;
    case switchK : 
      printf("switch ");
      pprint_Switch_On(curr->val.switch_stmt.switch_on);
      printf(" {\n");
      tabNum++;
      pprint_Switch_Case(curr->val.switch_stmt.switch_case);
      tabNum--;
      printTab();
      printf("}");
      break;
    case forK : 
      pprint_For_Statement(curr->val.for_stmt);
      break;
    case printK : 
      pprint_Print_Statement(curr->val.print);
      break;
    case printlnK : 
      pprint_Println_Statement(curr->val.print);
      break;
    case continueK : 
      printf("continue");
      break;
    case breakK :
      printf("break");
      break;
    case returnK : 
      pprint_Return_Statement(curr->val.exp);
      break;
    case assignListK : 
      pprint_Assignment_List(curr->val.simple_stmt);
      break;
    case exprSimpleK : 
      pprint_Expression_Statement(curr->val.simple_stmt);
      break;
    case incK : 
      pprint_Inc_Statement(curr->val.simple_stmt);
      break;
    case decK : 
      pprint_Dec_Statement(curr->val.simple_stmt);
      break;
    case shortVarDeclK : 
      pprint_Short_Var_Decl(curr->val.simple_stmt);
      break;
    case equalAssignK : 
      pprint_Assignment_List(curr->val.simple_stmt);
      break;
    case plusAssignK : 
      pprint_Op_Assignment(curr->val.simple_stmt);
      break;
    case minusAssignK : 
      pprint_Op_Assignment(curr->val.simple_stmt);
      break;
    case orAssignK : 
      pprint_Op_Assignment(curr->val.simple_stmt);
      break;
    case xorAssignK :
      pprint_Op_Assignment(curr->val.simple_stmt);       
      break;
    case mulAssignK : 
      pprint_Op_Assignment(curr->val.simple_stmt);
      break;
    case divAssignK : 
      pprint_Op_Assignment(curr->val.simple_stmt);
      break;
    case remAssignK : 
      pprint_Op_Assignment(curr->val.simple_stmt);	
      break;
    case lshiftAssignK : 
      pprint_Op_Assignment(curr->val.simple_stmt);
      break;
    case rshiftAssignK : 
      pprint_Op_Assignment(curr->val.simple_stmt);
      break;
    case andAssignK : 
      pprint_Op_Assignment(curr->val.simple_stmt);
      break;
    case bitclearAssignK: 
      pprint_Op_Assignment(curr->val.simple_stmt);
      break;
    case simpleK : 
      pprint_Simple_Stmt(curr->val.simple_stmt);
      break;
    default : fprintf(stderr, "Error: Not valid StatementK in pprint_STMTS\n"); exit(1);
    }

    if (curr->kind != varBodyK) {
      printf("\n"); /* At the end of each statement, we have a new line. */
    }

    curr = curr->nextStmt;
  }
	
}






