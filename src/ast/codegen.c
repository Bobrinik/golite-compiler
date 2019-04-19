#include <stdio.h>
#include <stdlib.h>
#include "codegen.h"
#include "tree.h"
#include "symbol.h"
#include <stdbool.h>
#include "../utils/util.h"
#include "../utils/memory.h"
#include <string.h>

extern FILE* fp;

char *bitwise_clear = "golite_op_and_not";
char *golite_len = "golite_op_len";

int depth = 0;
int tmpNum = 0;
bool hasMainFunc = false;

// ============================================
char *  get_name(SYMBOL *symbol, ENVIRONMENT *t);
char *  new_name(char *name);
bool is_js_var_declared_cur_scope(SYMBOL *sym, ENVIRONMENT *t);

ENVIRONMENT *new_Environment() {
  ENVIRONMENT *table = NEW(ENVIRONMENT);
  for(int i  = 0; i < HashSize; i++)
    {
      table->table[i] = NULL;
    }

  table->parent = NULL;

  return table;
}

void assertNameDoesNoeExistInEnvironment(char *name, ENVIRONMENT *t){
  int idx = hash(name);
  for(RECORD *s = t->table[idx]; s; s = s->next){
    if(strcmp(name, s->name) == 0) {
      fprintf(stderr, "CODEGEN-Error: identifier \"%s\" already declared\n", name);
      exit(1);
    }
  }
}

ENVIRONMENT *enter_environment(ENVIRONMENT *parent) {
  ENVIRONMENT *table = new_Environment();

  table->depth  = parent->depth + 1;
  table->parent = parent;

  return table;
}

RECORD * put_record_in_environment(RECORD *record, ENVIRONMENT *t){
  int i = hash(record->name);
  assertNameDoesNoeExistInEnvironment(record->name, t);
  record->next = t->table[i]; // link to previous symbol in the same table entry
  t->table[i]  = record;

  return record;
}

RECORD * get_Record_by_name(char *name, ENVIRONMENT *t){
  if (!t) return NULL; // it means that symbol is not in the table
  int i = hash(name);
  
  for(RECORD *s = t->table[i]; s; s = s->next){
    if(strcmp(s->name, name) == 0) return s;
  }
    
   return get_Record_by_name(name, t->parent);
}

RECORD * get_Record(SYMBOL *symbol, ENVIRONMENT *t){
  if (!t) return NULL; // it means that symbol is not in the table
  char *name = symbol->name;
  int i = hash(name);
  
  for(RECORD *s = t->table[i]; s; s = s->next){
    if(strcmp(s->name, name) == 0) return s;
  }
    
   // go to parent if nothing is in current scope
  return get_Record(symbol, t->parent);
}

RECORD * get_Record_from_Record(RECORD *record, ENVIRONMENT *t){
  if (!t) return NULL; // it means that symbol is not in the table
  char *name = record->name;
  int i = hash(name);
  
  for(RECORD *s = t->table[i]; s; s = s->next){
    if(strcmp(s->name, name) == 0) return s;
  }
    
   // go to parent if nothing is in current scope
  return get_Record_from_Record(record, t->parent);
}

RECORD * get_function_Record(SYMBOL *symbol, ENVIRONMENT *t){
  if (!t) return NULL; // it means that symbol is not in the table
  char *name = symbol->name;
  int i = hash(name);
  
  for(RECORD *s = t->table[i]; s; s = s->next){
    if(strcmp(s->name, name) == 0 && s->isFunction) return s;
  }
    
   // go to parent if nothing is in current scope
  return get_Record(symbol, t->parent);
}

char * get_js_function(SYMBOL *symbol, ENVIRONMENT *env) {
  RECORD *rec =  get_function_Record(symbol, env);
  if(!rec) {
    fprintf(stderr, "Function with name %s not found\n", symbol->name);
  }
  return rec->codegen_name;
}

char * get_js_var(SYMBOL *symbol, ENVIRONMENT *t){
  if( strcmp(symbol->name, "true") == 0 || 
      strcmp(symbol->name, "false") == 0) {

      return symbol->name;
  }

  RECORD * rec = get_Record(symbol, t);
  if(!rec){
    fprintf(stderr, "CODEGEN-Error: JS var %s not found\n", symbol->name);
    exit(1);
  }
  // resolving hidden variable declarations
  if(rec->isHiding) return rec->hidden_record->codegen_name;

  return rec->codegen_name;
}


char * generate_JS_Name(char *name){
  return  concat("golite_gen_js_", name);
}

char * get_Scope_Prefix(ENVIRONMENT *t){
  char * result = "";
  for(int i = 0; i < t->depth; i++) {
    result = concat("js_", result);
  }
  return result;
}


char * create_JS_var(SYMBOL *sym, ENVIRONMENT *t){
  RECORD *record       = NEW(RECORD);
  record->isHiding     = false;
  record->name         = sym->name;
  record->codegen_name = concat(get_Scope_Prefix(t), generate_JS_Name(sym->name));
  record->next         = NULL;
  
  RECORD * hidden = get_Record(sym, t);

  if(hidden){
    record->isHiding      = true;
    record->hidden_record = hidden;
  }

  put_record_in_environment(record, t);
  return record->codegen_name;
}

char * create_JS_var_soft(SYMBOL *sym, ENVIRONMENT *t){
  RECORD * existing = get_Record(sym, t);
  if(existing) return existing->codegen_name;
  return create_JS_var(sym, t);
}

char * get_temp_name(int n){
  char num[10];
  sprintf(num, "%d", n);
  return concat("tmp__", num);
}

char * get_JS_temp(int n, ENVIRONMENT *t){
  char *temp_name = get_temp_name(n);
  RECORD *record = get_Record_by_name(temp_name, t);
  if(!record) {
    fprintf(stderr, "CODEGEN-Error: temp variable %d is not found\n", n);
    exit(1);
  }
  return record->codegen_name;
}

char * create_JS_temp_var(int n, ENVIRONMENT *t){
  RECORD *record       = NEW(RECORD);
  record->isHiding     = false;
  record->isFunction   = false;
  record->name         = get_temp_name(n);
  record->codegen_name = get_temp_name(n);
  record->next         = NULL;
  //     result = fmt_String( concat(result, "%s : %s"), (char *[]) { field->name, get_JS_Default(field->type)});
  RECORD * hidden = get_Record_from_Record(record, t);

  if(hidden){
    record->isHiding      = true;
    record->hidden_record = hidden;
  }

  put_record_in_environment(record, t);
  return record->codegen_name;
}

char * create_JS_Function_Name(SYMBOL *sym, ENVIRONMENT *t){
  RECORD *record       = NEW(RECORD);
  record->isFunction   = true;
  record->isHiding     = false;
  record->name         = sym->name;
  record->codegen_name = concat("js_function_", generate_JS_Name(sym->name));
  record->next         = NULL;

  put_record_in_environment(record, t);
  return record->codegen_name;
}

bool is_js_var_declared(SYMBOL *sym, ENVIRONMENT *t){
  RECORD *record = get_Record(sym, t);
  return record != NULL;
}

bool is_js_var_declared_cur_scope(SYMBOL *sym, ENVIRONMENT *t) {
  	if (!t) return false; // it means that symbol is not in the table
  	char *name = sym->name;
  	int i = hash(name);

  	for(RECORD *s = t->table[i]; s; s = s->next){
    		if(strcmp(s->name, name) == 0) return true;
  	}
	return false;
}

// ============================================
// used for incrementing init function declaration name
int init_funct_count = -1;

char * js_Header(){
  char *l1 = "\"use strict\";\n";
  char *l2 = fmt_String("function %s(l, r){return l & ~r;}\n", (char *[]) {bitwise_clear});
  char *l3 = fmt_String("function %s(l){ return l.length; }\n", (char *[]) {golite_len});
  return concat(concat(l1, l2), l3);
}

void init_table() {
  for(int i  = 0; i < HashSize; i++)
    {
      table[i] = NULL;
    }
}

/* char * get_New_Simple_Name(char *name){ */
/*   if (!isDeclared(concat("js_", name))) { */
/*   	return concat("js_", name); */
/*   } */
/*   else { */
/* 	return  get_New_Simple_Name(concat("js_", name)); */
/*   } */
/* } */

/* char * get_JS_Simple_Name(char *name) { */
/* 	if (strcmp(name, "true") == 0 || strcmp(name, "false") == 0) { */
/*     		return name; */
/*   	} */

/* 	if (isDeclared(name)) return name;	 */
/* 	return concat("js_", name); */
/* } */

// check if variable with that name has been declared
/* int isDeclared(char *n) { */
/* 	int i = hash(n); */
	
/* 	for(NAME *s = table[i]; s; s = s->next){ */
/*    	 	if(strcmp(s->name, n) == 0) return 1; */
/*   	} */
/* 	return 0; */
/* } */

void add_to_table(char *name) {
  RECORD *n = (RECORD *) malloc(sizeof(RECORD));
  n->name = name;

  int i = hash(name);
  n->next = table[i]; 
	
  table[i] = n;
  return;
}

/* char * get_JS_Name(SYMBOL *sym, int isDeclaration){ */
 
/*   if (isDeclaration) { */
/*     char *name = get_JS_Simple_Name(sym->name); */
/*     add_to_table(name); */
/*     sym->name = name; */
/*   } */
/*   return get_JS_Simple_Name(sym->name); */
/* } */


char * get_JS_From_BaseK(TYPE *type);
char * get_JS_Default(TYPE *type, bool isSlice);
char * get_JS_From_StructK(FIELD *field);

char * get_JS_Name(char *name){
  return concat("js_", name);
}

char * get_JS_From_StructK(FIELD *field){
  char *result = "{ ";
  while(field){
    field->name = get_JS_Name(field->name);
    result = fmt_String( concat(result, "%s : %s"), (char *[]) { field->name, get_JS_Default(field->type, false)});
    result = field->nxt ? concat(result, ", ") : concat(result, " ");

    field = field->nxt;
  }
  result = concat(result, "}");
  return result;
}

char * get_JS_From_BaseK(TYPE *type){
  if(type->kind != baseK){
    fprintf(stderr, "CODEGEN ERROR: get_JS_From_BaseK\n");
    exit(1);
  }

  switch(type->val.baseType){
  case intK:     return "0";
  case boolK:    return "false";
  case float64K: return "0.0";
  case runeK:    return "\"\\0\"";
  case stringK:  return "\"\"";
  default:
    fprintf(stderr,"CODEGEN ERROR: No such basetype \n");
    exit(1);
  }
}

char * get_JS_Default(TYPE *type, bool isSlice){
  if(type == NULL) return "not resolved";

  switch(type->kind){
  case sliceK:
    {
      char *res = get_JS_Default(type->val.slice, true);
      res = concat("[", res);
      res = concat(res, "]");
      return res;
    }
  case refK: // colalpsing it
    return get_JS_Default(type->val.ref.element, isSlice);
  case arrayK:
    {
      char *element = get_JS_Default(type->val.array.element, false);
      char *res = concat("[", element);
      for (int i = 0; i< type->val.array.size - 1; i++) {
	res = concat(res, ", ");
	res = concat(res, element);
      }
      res = concat(res, "]");
      return res;
    }
  case structK: return get_JS_From_StructK(type->val.field);
  case baseK:   
    if(!isSlice){
      return get_JS_From_BaseK(type);
    }
    return "";
  default:
    fprintf(stderr, "Error: This kind is not in TypeK\n");
    exit(1);
  }
}

void outputTabs() {
  for (int i = 0; i < depth; i++) {
    fprintf(fp, "    ");
  }
}

/* void codegen_Symbol(SYMBOL *smbl) { */
/*   if(!smbl) { */
/*     fprintf(stderr, "Error: Tried to ppirnt null smbl\n"); */
/*     exit(1); */
/*   } */

/*   fprintf(fp, "%s", get_JS_Name(smbl, 0)); */
/* } */


/* ==================================================== */

void codegen_Program(PROGRAM *root) {

  if(!root) return; // if file is empty
  fprintf(fp, "%s", js_Header());
  init_table();
  ENVIRONMENT *prog_env = new_Environment();
  prog_env->depth = 0;
  codegen_Top_Declaration_List(root->top_declaration_list, prog_env);
  if (hasMainFunc) fprintf(fp, "\nmain()\n");
}

void codegen_Top_Declaration_List(TOP_DECLARATION_LIST *crnt, ENVIRONMENT *env) {
  while (crnt){
    codegen_Top_Declaration(crnt->top_declaration, env);
    crnt = crnt->nxt;
  }
}

void codegen_Top_Declaration(TOP_DECLARATION *dclr, ENVIRONMENT *env){
  switch(dclr->kind) {
  case varK:  codegen_Var_Declaration(dclr->val.var_dclr, env); break;
  case typeK: break; // not generating type defs in JS
  case funcK: codegen_Function_Declaration(dclr->val.function_dclr, env); break; // disable for now
	      
  default: fprintf(stderr, "Error: Unrecognised TypeK\n"); exit(1);
  }
}

void codegen_Var_Declaration(VAR_DECLARATION *dclr, ENVIRONMENT *env){
  if(dclr->isList){
    codegen_Declaration_list(dclr->val.declaration_list, env);
  }
  else {
    codegen_Declaration(dclr->val.declaration, env);
  }
}

/* ======================================================= */

void codegen_Function_Declaration(FUNCTION_DECLARATION *fun_dclr, ENVIRONMENT *env) {
  if(!env) printf("EMPTY IN FUNCTION\n");

  outputTabs();
  ENVIRONMENT *new_env = enter_environment(env);
  if(strcmp(fun_dclr->name->name, "init") == 0){
    init_funct_count++;
    fprintf(fp, "function %s_%d", get_JS_Name(fun_dclr->name->name), init_funct_count);
    codegen_Signature(fun_dclr->signature, new_env);
    fprintf(fp, " {\n");
    depth++;
    fprintf(fp, "let js__\n");
    codegen_STMTS(fun_dclr->body, new_env);
    depth--;
    outputTabs();
    fprintf(fp, "}\n");
    fprintf(fp, "%s_%d()\n", get_JS_Name(fun_dclr->name->name), init_funct_count);
  } else if(strcmp(fun_dclr->name->name, "main") == 0){

    fprintf(fp, "function main");
    codegen_Signature(fun_dclr->signature, new_env);
    fprintf(fp, " {\n");
    depth++;
    fprintf(fp, "let js__\n");
    codegen_STMTS(fun_dclr->body, new_env);
    depth--;
    outputTabs();
    fprintf(fp, "}\n");
    fprintf(fp, "main()\n");
  } else {
    fprintf(fp, "function %s", create_JS_Function_Name(fun_dclr->name, env));
    codegen_Signature(fun_dclr->signature, new_env);
    fprintf(fp, " {\n");
    depth++;
    fprintf(fp, "let js__\n");
    codegen_STMTS(fun_dclr->body, new_env);
    depth--;
    outputTabs();
    fprintf(fp, "}\n");
  }
}

void codegen_Signature(SIGNATURE *signature, ENVIRONMENT *env){
  fprintf(fp, "(");
  codegen_Parameter_List(signature->parameters, env); 
  fprintf(fp, ")");
}

void codegen_result(RESULT *result, ENVIRONMENT *env) {
  if(result->kind == arrayResultK || result->kind == arrayStructResultK) {
    codegen_Bracket_list(result->brackets);
  }
  if (result->kind == simpleResultK || result->kind == arrayResultK) {
    // TODO:
    //codegen_Symbol(result->type);
  }
  if (result->kind == arrayStructResultK || result->kind == structResultK) {
    codegen_Declaration_list(result->decl_list, env);
  }
}


void codegen_Parameter_List(PARAMETER_LIST *list, ENVIRONMENT *env){
  PARAMETER_LIST *crnt = list;

  while (crnt) { 
    codegen_Parameter_Unit(crnt->param, env);
    crnt = crnt->nxt;
    if(crnt) fprintf(fp, ", "); // reversing golite.y
  }
}

void codegen_Parameter_Unit(PARAMETER_UNIT *unit, ENVIRONMENT *env){
  IDENTIFIER_LIST *crnt = unit->ids_list;

  while (crnt){
    char * var = create_JS_var(crnt->symbol, env);
    fprintf(fp, "%s", var);
    crnt = crnt->nxt;
    if(crnt) fprintf(fp, ", ");
  }
}

/* ===================================================== */

void codegen_Declaration_list(DECLARATION_LIST *list, ENVIRONMENT *env) {
  DECLARATION_LIST *crnt = list;

  while (crnt) { 
    outputTabs();
    codegen_Declaration(crnt->declaration, env);
    crnt = crnt->nxt;
  }
}

void codegen_Declaration(DECLARATION *declaration, ENVIRONMENT *env) {
  IDENTIFIER_LIST *crnt = declaration->lhs;
  int rightExpCounter = 0;

  while(crnt){
    switch(declaration->kind){
    case Id_list_typek: 
    case Id_list_arrayk:
    case Id_list_structK:
    case Id_list_array_structK:
      {
        char * struct_name = create_JS_var(crnt->symbol, env);
        fprintf(fp, "let %s = %s", struct_name, get_JS_Default(crnt->symbol->val.type, false));
      }
      break;
    case Id_list_eqk:
    case Id_list_type_eqk: 
      {
        // at this point we need to be able to grab the previous stuff
        // I can check if simillar name already exists and have a resolving reference
        char * var_name = create_JS_var(crnt->symbol, env);
        fprintf(fp, "let %s = ", var_name);

        EXP_LIST *crnt_exp = declaration->val.rhs_list;

        for (int i = 0; i < rightExpCounter; i++) {
          crnt_exp = crnt_exp->nextExp;
        }

        codegen_expression(crnt_exp->exp, env);
        
        // Should go after first codegen of expression, weird scope resolution
        RECORD * record = get_Record(crnt->symbol, env);
        if(record->isHiding){
          record->isHiding = false;
        }
        break;
      }
    default: fprintf(stderr, "Error: Not valid DeclarationK\n"); exit(1);
    }
    fprintf(fp, "\n");
    rightExpCounter++;
    if (crnt->nxt) outputTabs();
    crnt = crnt->nxt;
  }
}

void codegen_Bracket(BRACKET *bracket){
  if(bracket->isEmpty) fprintf(fp, "[]");
  else fprintf(fp, "[%d]", bracket->idx);
}

void codegen_Bracket_list(BRACKET_LIST *list){
  BRACKET_LIST *crnt = list;

  while (crnt){
    codegen_Bracket(crnt->bracket);
    crnt = crnt->nxt;
  }
}

/* ============ EXPRESSION ============ */

void codegen_expression(EXP *e, ENVIRONMENT *env) {  
  if (e == NULL) {
    return;
  }

  switch(e->kind) {
      case primaryExpressionK:    codegen_Primary_Expression(e->val.primaryExpression, env); break;
      case unaryOpK:              codegen_Unary_Op(e->val.unaryOpExpression, env); break;
      case binaryOpK:             codegen_Binary_Op(e->val.binaryOpExpression, env); break;
      case builtinK:              codegen_Builtin(e->val.builtinExpression, env); break;
      default:                    fprintf(stderr, "Error: (line: %d) Not a valid expression kind\n", e->lineno); exit(1);
  }
}

void codegen_expression_list(EXP_LIST *exp_list, ENVIRONMENT *env) {
    while (exp_list->nextExp != NULL) {
      codegen_expression(exp_list->exp, env);
      fprintf(fp, ", ");
      exp_list = exp_list->nextExp;
    }
    codegen_expression(exp_list->exp, env);
}

void codegen_Primary_Expression(PrimaryExpression *primaryExp, ENVIRONMENT *env) {
  switch(primaryExp->kind) {
  case identifierK:        
    fprintf(fp, "%s", get_js_var(primaryExp->val.identifier, env));
    break;
  case parenthesisExpK:       codegen_expression(primaryExp->val.parenthesisExp, env);  break;

  case intLiteralK:           fprintf(fp, "%d", primaryExp->val.intLiteral); break;
  case floatLiteralK:         fprintf(fp, "%.9g", primaryExp->val.floatLiteral); break;
  case runeLiteralK:          primaryExp->val.runeLiteral.isRuneLiteralEscape ? fprintf(fp, "('\\%c').charCodeAt(0)", primaryExp->val.runeLiteral.val) : fprintf(fp, "('%c').charCodeAt(0)", primaryExp->val.runeLiteral.val); break;
  case strLiteralK:
    if (strncmp(primaryExp->val.strLiteral, "`", 1) == 0) {		// raw strings include escape chars
      fprintf(fp, "String.raw%s", primaryExp->val.strLiteral);
    }
    else fprintf(fp, "%s", primaryExp->val.strLiteral); 
    break;
  case primaryExpArgumentK:   codegen_Primary_Expression_arguments(primaryExp, env); break;
  case primaryExpSelectorK:   codegen_Primary_Expression_selector(primaryExp, env); break;
  case primaryExpIndexK:      codegen_Primary_Expression_index(primaryExp, env); break;
  case blankExpK:		    fprintf(fp, "js__"); break;
  default:                    fprintf(stderr, "Error: (line: %d) Not a valid primaryExp kind\n", primaryExp->lineno); exit(1);
  }
}

void codegen_Primary_Expression_arguments(PrimaryExpression *primaryExp, ENVIRONMENT *env) {
    codegen_Primary_Expression(primaryExp->val.argument.primaryExp, env);
    fprintf(fp, "(");

    if(primaryExp->val.argument.expList){
      codegen_expression_list(primaryExp->val.argument.expList, env);
    }

    fprintf(fp, ")");
}

void codegen_Primary_Expression_selector(PrimaryExpression *primaryExp, ENVIRONMENT *env) {
  
    codegen_Primary_Expression(primaryExp->val.selector.primaryExp, env);
    fprintf(fp, ".");
    fprintf(fp, "%s", concat("js_", primaryExp->val.selector.identifier));
}

void codegen_Primary_Expression_index(PrimaryExpression *primaryExp, ENVIRONMENT *env) {
    codegen_Primary_Expression(primaryExp->val.index.primaryExp, env);
    fprintf(fp, "[");
    codegen_expression(primaryExp->val.index.indexExp, env);
    fprintf(fp, "]");
}


void codegen_Unary_Op(UnaryOpExpression *unaryOpExp, ENVIRONMENT *env) {
    switch(unaryOpExp->kind) {
        case unaryPlusK:            fprintf(fp, "+"); break;
        case unaryMinusK:           fprintf(fp, "-"); break;
        case notK:                  fprintf(fp, "!"); break;
        case bitwiseComplementK:    fprintf(fp, "~"); break;
        default:                    fprintf(stderr, "Error: (line: %d) Not a valid unaryOpExp kind\n", unaryOpExp->lineno); exit(1);
    }
    // fprintf(fp, "(");
    codegen_expression(unaryOpExp->unaryRHS, env);
    // fprintf(fp, ")");
}

void codegen_Binary_Op(BinaryOpExpression *binaryOpExp, ENVIRONMENT *env) {
    fprintf(fp, "(");
    if(binaryOpExp->kind == bitClearK) fprintf(fp, "%s(", bitwise_clear);

    codegen_expression(binaryOpExp->lhs, env);
    switch(binaryOpExp->kind) {
        case orK:           fprintf(fp, " || "); break;
        case andK:          fprintf(fp, " && "); break;
        case equalK:        fprintf(fp, " === "); break;
        case notEqualK:     fprintf(fp, " != "); break;
        case ltEqualsK:     fprintf(fp, " <= "); break;
        case gtEqualsK:     fprintf(fp, " >= "); break;
        case lesserK:       fprintf(fp, " < "); break;
        case greaterK:      fprintf(fp, " > "); break;
        case plusK:         fprintf(fp, " + "); break;
        case subK:          fprintf(fp, " - "); break;
        case bitOrK:        fprintf(fp, " | "); break;
        case bitwiseXorK:   fprintf(fp, " ^ "); break;
        case multK:         fprintf(fp, " * "); break;
	case divK:          fprintf(fp, " / "); break;
        case remK:          fprintf(fp, " %c ", '%'); break;
        case rightShiftK:   fprintf(fp, " >> "); break;
        case leftShiftK:    fprintf(fp, " << "); break;
        case bitAnd:        fprintf(fp, " & "); break;
        case bitClearK:     fprintf(fp, ","); break;
        default:            fprintf(stderr, "Error: (line: %d) Not a valid binaryOpExp kind\n", binaryOpExp->lineno); exit(1);
    }

    codegen_expression(binaryOpExp->rhs, env);

    if(binaryOpExp->kind == bitClearK) fprintf(fp, ")");
    fprintf(fp, ")");
}

void codegen_Builtin(BuiltInExpression *builtInExp, ENVIRONMENT *env) {
    switch(builtInExp->kind) {
        case appendK:
            codegen_expression(builtInExp->val.append.arg1, env);
            fprintf(fp, ".concat( ");
            codegen_expression(builtInExp->val.append.arg2, env);
            fprintf(fp, ")");
            break;
        case lenK:
            fprintf(fp, "%s(", golite_len);
            codegen_expression(builtInExp->val.len, env);
            fprintf(fp, ")");
            break;
        case capK:
            fprintf(fp, "%s(", golite_len);
            codegen_expression(builtInExp->val.cap, env);
            fprintf(fp, ")");
            break;
        default: fprintf(stderr, "Error: (line: %d) Not a valid builtInExp kind\n", builtInExp->lineno); exit(1);
    }
}

/* ============ STATEMENTS ============ */

void codegen_Simple_Stmt(SIMPLE_STMT *simple_stmt, ENVIRONMENT *env) {
	
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
        codegen_Assignment_List(simple_stmt, env);
        break;
      case exprSimpleK :
        codegen_Expression_Statement(simple_stmt, env);
        break;
      case incK :
        codegen_Inc_Statement(simple_stmt, env);
        break;
      case decK :
        codegen_Dec_Statement(simple_stmt, env);
        break;
      case shortVarDeclK :
        codegen_Short_Var_Decl(simple_stmt, env);
        break;
      case equalAssignK :
        codegen_Assignment_List(simple_stmt, env);
        break;
      case plusAssignK :
        codegen_Op_Assignment(simple_stmt, env);
        break;
      case minusAssignK :
        codegen_Op_Assignment(simple_stmt, env);
        break;
      case orAssignK :
        codegen_Op_Assignment(simple_stmt, env);
        break;
      case xorAssignK :
        codegen_Op_Assignment(simple_stmt, env);
        break;
      case mulAssignK :
        codegen_Op_Assignment(simple_stmt, env);
        break;
      case divAssignK :
        codegen_Op_Assignment(simple_stmt, env);
        break;
      case remAssignK :
        codegen_Op_Assignment(simple_stmt, env);
        break;
      case lshiftAssignK :
        codegen_Op_Assignment(simple_stmt, env);
        break;
      case rshiftAssignK :
        codegen_Op_Assignment(simple_stmt, env);
        break;
      case andAssignK :
        codegen_Op_Assignment(simple_stmt, env);
        break;
      case bitclearAssignK:
        codegen_Op_Assignment(simple_stmt, env);
        break;
      case simpleK :
        codegen_Simple_Stmt(simple_stmt, env);
        break;
      default : fprintf(stderr, "Error: Not valid StatementK\n"); exit(1);
   }
}

void codegen_If_Statement(STMTS *stmt, ENVIRONMENT *env) {
  fprintf(fp, "if (true) {\n");
  depth++;
    ENVIRONMENT *new_env1 = enter_environment(env);
  if (stmt->val.if_stmt.simplestmt != NULL ) {
    outputTabs();
    codegen_Simple_Stmt(stmt->val.if_stmt.simplestmt, new_env1);
    fprintf(fp, ";\n");
  }

  outputTabs();
  fprintf(fp, "if ");
  // optional simple statement
	
  // if block
  fprintf(fp, "(");
  codegen_expression(stmt->val.if_stmt.condition, new_env1);
  fprintf(fp, ") {\n");
  depth++;
  ENVIRONMENT *new_env2 = enter_environment(new_env1);
  codegen_STMTS(stmt->val.if_stmt.block, new_env2);
  depth--;
        
  if (stmt->val.if_stmt.next != NULL) { 
    outputTabs();
    fprintf(fp, "} "); 
  } else {
    outputTabs();
    fprintf(fp, "}\n"); 
  }

  if (stmt->val.if_stmt.next != NULL) {
    ENVIRONMENT *new_env3 = enter_environment(new_env1);
    fprintf(fp, "else {\n");
    depth++;
    codegen_STMTS(stmt->val.if_stmt.next, new_env3);
    depth--;
    outputTabs();
    fprintf(fp, "}");
  }

  fprintf(fp, "\n");
  depth--;
  outputTabs();
  fprintf(fp, "}\n");

}

void codegen_Switch_On(SWITCH_ON *switch_on, ENVIRONMENT *env) {

	if (switch_on->simpleStmt != NULL) {
		fprintf(fp, "if (true) {\n");
        	depth++;
        	outputTabs();
		codegen_Simple_Stmt(switch_on->simpleStmt, env);
		fprintf(fp,";\n");
                outputTabs();
	}	

	fprintf(fp, "switch (");
	if (switch_on->condition != NULL) {
		codegen_expression(switch_on->condition, env);
	}
	else {
		fprintf(fp, "true");
	}
	fprintf(fp, ")");
}

void codegen_Switch_Case(SWITCH_CASE *switch_case, ENVIRONMENT *env) {
  if (switch_case == NULL) {
    return;
  }
  codegen_Switch_Case(switch_case->next, env);
  ENVIRONMENT *new_env1 = enter_environment(env) ;
  switch(switch_case->kind) {
  case caseK: 
    {      
      EXP_LIST *caseExp = switch_case->caseExp;
      while (caseExp && caseExp->exp){
        outputTabs();
        fprintf(fp, "case ");
        codegen_expression(caseExp->exp, new_env1);
        fprintf(fp, ":\n");
        caseExp = caseExp->nextExp;
      }
      depth++;
      codegen_STMTS(switch_case->caseStmts, new_env1);
      fprintf(fp, "break;\n");
      depth--;
    }
    break;
  case defaultK:
    outputTabs();
    fprintf(fp, "default");
    fprintf(fp, ":\n");
    depth++;
    codegen_STMTS(switch_case->caseStmts, new_env1);
    fprintf(fp, "break;\n");
    depth--;
    break;
  default: 
    fprintf(stderr, "Error: Not valid switchK\n"); exit(1);
  }
}

void codegen_Clause(CLAUSE *c, ENVIRONMENT *env) {

	if (c == NULL) return;

	if (c->simple_stmt1) {
		
		if (c->simple_stmt1->kind == shortVarDeclK) { 
			fprintf(fp, "if (true) {\n");
                	depth++;
                	outputTabs();
			codegen_Simple_Stmt(c->simple_stmt1, env);
			fprintf(fp,";\n");
        		outputTabs();
        		fprintf(fp, "for ( ; ");
		}

		else {
			fprintf(fp, "for ( ");
			codegen_Simple_Stmt(c->simple_stmt1, env);
        		fprintf(fp," ; ");
		}
	}
	else {
		fprintf(fp, "for ( ; ");
	}

        if (c->condition != NULL) {
                codegen_expression(c->condition, env);
        }
        fprintf(fp, " ; ");

	if (c->simple_stmt2) {
        	codegen_Simple_Stmt(c->simple_stmt2, env);
	}
	fprintf(fp, ")"); 	

}

void codegen_For_Statement(FOR_STATEMENT *for_stmt, ENVIRONMENT *env) {
        ENVIRONMENT *new_env1 = enter_environment(env);

        switch (for_stmt->kind) {

                case infiniteK :
                                fprintf(fp, "while (true) ");
                                break;

                case forClauseK :
				codegen_Clause(for_stmt->val.clause, new_env1);
				
				break;

                case whileK :
                                fprintf(fp, "while (");
                                codegen_expression(for_stmt->val.while_condition, new_env1);
                                fprintf(fp, ")");
                                break;
                default: fprintf(stderr, "Error: Not valid StatementK\n"); exit(1);
        }

        fprintf(fp, " {\n");
        depth++;
        ENVIRONMENT *new_env2 = enter_environment(new_env1);
        codegen_STMTS(for_stmt->block, new_env2);
        depth--;
        outputTabs();
        fprintf(fp, "}");

        // end if(true) scope when short var decl in init
        if (for_stmt->kind == forClauseK && for_stmt->val.clause->simple_stmt1) {
		if (for_stmt->val.clause->simple_stmt1->kind == shortVarDeclK) {
                	fprintf(fp, "\n");
			depth--;
                	outputTabs();
                	fprintf(fp, "}");
		}
        }
        fprintf(fp, "\n");

}

void print_expression(EXP *e, ENVIRONMENT *env) {
	switch (e->type->kind) {
                        case sliceK:
                                break;
                        case refK :
                                break;
			case arrayK :
				break;
			case structK :
				break;
			case baseK :
				if (e->type->val.baseType == float64K) {
					fprintf(fp, "Number.parseFloat(");
					codegen_expression(e, env);
					fprintf(fp, ").toExponential(6)");
				}
				else if (e->type->val.baseType == runeK) {
					fprintf(fp, "parseInt(");
					codegen_expression(e,env);
					fprintf(fp, ")");
				}
				else {
					codegen_expression(e, env);
				}
				break;
			default: fprintf(stderr, "Error: (line %d) invalid typeK\n", e->lineno);
	}
}

void codegen_Print_Statement(EXP_LIST *print, ENVIRONMENT *env) {
  	while (print->exp != NULL) {
		
		// print float with sign
		if (print->exp->type->kind == baseK && print->exp->type->val.baseType == float64K) {
			fprintf(fp, "if (Math.sign(");
			codegen_expression(print->exp, env);
			fprintf(fp, ") == 1) process.stdout.write(\"+\")\n");
			outputTabs();
		}

		fprintf(fp, "process.stdout.write(String(");
		print_expression(print->exp, env);
		fprintf(fp, "))\n");
		if (print->nextExp == NULL) break;
		print = print->nextExp;
		outputTabs();
	}

}

void codegen_Println_Statement(EXP_LIST *println, ENVIRONMENT *env) {
	while (println && println->exp) {

		// print float with sign
                if (println->exp->type->kind == baseK && println->exp->type->val.baseType == float64K) {
                        fprintf(fp, "if (Math.sign(");
                        codegen_expression(println->exp, env);
                        fprintf(fp, ") == 1) process.stdout.write(\"+\")\n");
                        outputTabs();
                }

		fprintf(fp, "process.stdout.write(String(");
		print_expression(println->exp, env);
		if (println->nextExp == NULL){
		       	fprintf(fp, "))\n");
                	outputTabs();	
			break;
		}
		fprintf(fp, "+\" \"))\n");
                outputTabs();
		println = println->nextExp;
	}

	fprintf(fp, "process.stdout.write(String(\"\\n\"))");
}

// if exp is an array or struct, duplicate, and return duplicate (return-by-value)
void return_by_value(EXP *exp, ENVIRONMENT *env) {
	if (exp == NULL) {
		return;
	}

	if (exp->type->kind == arrayK) {
		fprintf(fp, "let %s = [0]\n", create_JS_temp_var(tmpNum, env));				
		
		outputTabs();
		codegen_expression(exp, env);
		fprintf(fp, ".forEach(function(item, index) { ");
                fprintf(fp, "%s[index] = item; })\n", get_JS_temp(tmpNum, env));
		
		outputTabs();
		fprintf(fp, "return %s", get_JS_temp(tmpNum, env));
		tmpNum++;
	}
	else if (exp->type->kind == structK) {
		fprintf(fp, "let %s = JSON.parse(JSON.stringify(", create_JS_temp_var(tmpNum, env));
		codegen_expression(exp, env);
		fprintf(fp, "))\n");

		outputTabs();
		fprintf(fp, "return %s",  get_JS_temp(tmpNum, env));
                tmpNum++;
	}
	else {
		fprintf(fp, "return ");
		codegen_expression(exp, env);
	}
}							
							

void codegen_Return_Statement(EXP *exp, ENVIRONMENT *env) {
	
	if (exp != NULL) {
		return_by_value(exp, env);
	}
	else fprintf(fp, "return\n");
}

void assignment_helper(EXP_LIST *lhs, EXP_LIST *rhs, int isShortDecl, ENVIRONMENT *env) {

	EXP_LIST *lhs_exp = lhs;
	EXP_LIST *rhs_exp = rhs;
        
	int numTmp = 0;
        // create temporary variables for all rhs expressions (help with swaps)
        if (lhs_exp->nextExp) {
                while (lhs_exp && lhs_exp->exp && rhs_exp && rhs_exp->exp) {
                        // create temporary variables
                        fprintf(fp, "let %s = ",  create_JS_temp_var(tmpNum, env));
                        
                        tmpNum++;
                        numTmp++;
                        codegen_expression(rhs_exp->exp, env);

                        lhs_exp = lhs_exp->nextExp;
                        rhs_exp = rhs_exp->nextExp;
                        fprintf(fp, "\n");
                        outputTabs();
                }
        }

        lhs_exp = lhs;
        rhs_exp = rhs;

        // assignments temporaries to lhs expressions
        while (lhs_exp && lhs_exp->exp && rhs_exp && rhs_exp->exp) {

		// declare any new vars before assignment
		if (isShortDecl) {

				if (!is_js_var_declared_cur_scope(lhs_exp->exp->val.primaryExpression->val.identifier, env)) {	
                                        fprintf(fp, "let %s;\n", create_JS_var(lhs_exp->exp->val.primaryExpression->val.identifier, env));
                                	outputTabs();
					RECORD * record = get_Record(lhs_exp->exp->val.primaryExpression->val.identifier, env);
        				if(record->isHiding){
          					record->isHiding = false;
        				}
				}
                }

		int isCompoundType = 0; 
                if (rhs_exp->exp->kind == primaryExpressionK) {
                        if (rhs_exp->exp->val.primaryExpression->kind == identifierK) {
                                if (rhs_exp->exp->val.primaryExpression->val.identifier->category == category_variable) {
																
					// rhs array: make array copy to lhs exp	
					if (rhs_exp->exp->val.primaryExpression->val.identifier->val.type) {
                                                if (rhs_exp->exp->val.primaryExpression->val.identifier->val.type->kind == arrayK) {
                                                        codegen_expression(rhs_exp->exp, env);

                                                        if (numTmp > 0) {
                                                                fprintf(fp, ".forEach(function(item, index) {%s[index] = item; })", get_JS_temp(tmpNum-numTmp, env));
                                                        }
                                                        else {
                                                                fprintf(fp, ".forEach(function(item, index) { ");
                                                                codegen_expression(lhs_exp->exp, env);
                                                                fprintf(fp, "[index] = item; })");
                                                        }
                                                        isCompoundType = 1;
                                                }
						else if (rhs_exp->exp->val.primaryExpression->val.identifier->val.type->kind == structK) {
							
							if (numTmp > 0) {
								fprintf(fp, "let %s = JSON.parse(JSON.stringify(",create_JS_temp_var((tmpNum - numTmp), env));
							}
							else {
								codegen_expression(lhs_exp->exp, env);
								fprintf(fp, " = JSON.parse(JSON.stringify(");
							}
							codegen_expression(rhs_exp->exp, env);
                					fprintf(fp, "))\n");

							isCompoundType = 1;
						}			
						
                                        }
				
                                }
                        }
                }

                if(!isCompoundType) {

			if (isShortDecl) {
					fprintf(fp, "%s", create_JS_var_soft(lhs_exp->exp->val.primaryExpression->val.identifier, env));
			}
			else codegen_expression(lhs_exp->exp, env);
                        fprintf(fp, " = ");

                        if (numTmp > 0) {
                                fprintf(fp, "%s", get_JS_temp(tmpNum-numTmp, env));
                        }
                        else {
                                codegen_expression(rhs_exp->exp, env);
                        }
                }

                numTmp--;

                lhs_exp = lhs_exp->nextExp;
                rhs_exp = rhs_exp->nextExp;

		if (numTmp > 0) {
                        fprintf(fp, "\n");
                        outputTabs();
                }
        }
}

void codegen_Assignment_List(SIMPLE_STMT *simple_stmt, ENVIRONMENT *env) {

	EXP_LIST *lhs_exp = simple_stmt->val.assignment_list.lhs;
	EXP_LIST *rhs_exp = simple_stmt->val.assignment_list.rhs;

	assignment_helper(lhs_exp, rhs_exp, 0, env);
}

void codegen_Expression_Statement(SIMPLE_STMT *simple_stmt, ENVIRONMENT *env) {
    if (simple_stmt->val.exp != NULL) {
		  codegen_expression(simple_stmt->val.exp, env);
	}
}

void codegen_Inc_Statement(SIMPLE_STMT *simple_stmt, ENVIRONMENT *env) {
    codegen_expression(simple_stmt->val.exp, env);

    // Rune increment - not working properly
    /*if (simple_stmt->val.exp->kind == primaryExpressionK && 
        simple_stmt->val.exp->val.primaryExpression->kind == identifierK && 
        simple_stmt->val.exp->type->kind == baseK && 
        simple_stmt->val.exp->type->val.baseType == runeK) {
      
      fprintf(fp, " = String.fromCharCode(");
      codegen_expression(simple_stmt->val.exp);
      fprintf(fp, " + 1).charAt(0);");
    }
    else {*/
      fprintf(fp, "++");
   // }
}

void codegen_Dec_Statement(SIMPLE_STMT *simple_stmt, ENVIRONMENT *env) {
    codegen_expression(simple_stmt->val.exp, env);

    /* Rune decrement
    if (simple_stmt->val.exp->kind == primaryExpressionK && 
        simple_stmt->val.exp->val.primaryExpression->kind == identifierK && 
        simple_stmt->val.exp->type->kind == baseK && 
        simple_stmt->val.exp->type->val.baseType == runeK) {
      
      fprintf(fp, " = String.fromCharCode(");
      codegen_expression(simple_stmt->val.exp, env);
      fprintf(fp, ".charCodeAt(0) - 1).charAt(0);");
    }
    else {*/
      fprintf(fp, "--");
    //}
}

void codegen_Short_Var_Decl(SIMPLE_STMT *simple_stmt, ENVIRONMENT *env) {
  
	EXP_LIST *lhs_exp = simple_stmt->val.assignment_short_var_decl.identifierList;
  	EXP_LIST *rhs_exp = simple_stmt->val.assignment_short_var_decl.rhs;
	
	assignment_helper(lhs_exp, rhs_exp, 1, env);
}

void codegen_Op_Assignment(SIMPLE_STMT *simple_stmt, ENVIRONMENT *env) {
  if(simple_stmt->kind == bitclearAssignK){
    codegen_expression(simple_stmt->val.assignment.lhs, env);
    fprintf(fp, " = ");
    fprintf(fp, "%s(", bitwise_clear);
  }

  codegen_expression(simple_stmt->val.assignment.lhs, env);

  switch (simple_stmt->kind) {

  case plusAssignK : 
    fprintf(fp, " += ");
    break;
  case minusAssignK : 
    fprintf(fp, " -= ");
    break;
  case orAssignK : 
    fprintf(fp, " |= ");
    break;
  case xorAssignK : 
    fprintf(fp, " ^= ");
    break;
  case mulAssignK :
    fprintf(fp, " *= ");       
    break;
  case divAssignK : 
    fprintf(fp, " /= ");
    break;
  case remAssignK : 
    fprintf(fp, " %%= ");
    break;
  case lshiftAssignK : 
    fprintf(fp, " <<= ");
    break;
  case rshiftAssignK : 
    fprintf(fp, " >>= ");
    break;
  case andAssignK : 
    fprintf(fp, " &= ");
    break;
  case bitclearAssignK :
    fprintf(fp, ",");
    break;
    // will not actually be possible (already tested in codegen_STMTS)
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
  default : fprintf(stderr, "Error: Not valid StatementK\n"); exit(1);
  }

  codegen_expression(simple_stmt->val.assignment.rhs, env);

  if(simple_stmt->kind == bitclearAssignK) fprintf(fp, ")");
}


void codegen_STMTS(STMTS *stmts, ENVIRONMENT *env) {
  STMTS *curr = stmts;

  while (curr) {
    if (curr->kind != typeBodyK) {
      outputTabs();
    }

    switch (curr->kind) {
    case blockK:
      fprintf(fp,"{\n");
      depth++;
      codegen_STMTS(curr->val.block, enter_environment(env));
      depth--;
      outputTabs();
      fprintf(fp,"}\n");
      break;
    case varBodyK : 
      codegen_Var_Declaration(curr->val.var_dclr, env);
      break;
    case typeBodyK : 
      // types not supported in JS
      break;
    case ifK : 
      codegen_If_Statement(curr, env);
      break;
    case switchK : 
      {
        ENVIRONMENT *new_env = enter_environment(env);
        codegen_Switch_On(curr->val.switch_stmt.switch_on, new_env);
        fprintf(fp, " {\n");
        depth++;
        codegen_Switch_Case(curr->val.switch_stmt.switch_case, new_env);
        depth--;
        outputTabs();
        fprintf(fp, "}");

        if (curr->val.switch_stmt.switch_on->simpleStmt) {
          fprintf(fp, "\n");
          depth--;
          outputTabs();
          fprintf(fp, "}");
        }
      }
      break;
    case forK : 
      codegen_For_Statement(curr->val.for_stmt, env);
      break;
    case printK : 
      codegen_Print_Statement(curr->val.print, env);
      break;
    case printlnK : 
      codegen_Println_Statement(curr->val.print, env);
      break;
    case continueK : 
      fprintf(fp, "continue");
      break;
    case breakK :
      fprintf(fp, "break");
      break;
    case returnK : 
      codegen_Return_Statement(curr->val.exp, env);
      break;
    case assignListK : 
      codegen_Assignment_List(curr->val.simple_stmt, env);
      break;
    case exprSimpleK : 
      codegen_Expression_Statement(curr->val.simple_stmt, env);
      break;
    case incK : 
      codegen_Inc_Statement(curr->val.simple_stmt, env);
      break;
    case decK : 
      codegen_Dec_Statement(curr->val.simple_stmt, env);
      break;
    case shortVarDeclK : 
      codegen_Short_Var_Decl(curr->val.simple_stmt, env);
      break;
    case equalAssignK : 
      codegen_Assignment_List(curr->val.simple_stmt, env);
      break;
    case plusAssignK : 
      codegen_Op_Assignment(curr->val.simple_stmt, env);
      break;
    case minusAssignK : 
      codegen_Op_Assignment(curr->val.simple_stmt, env);
      break;
    case orAssignK : 
      codegen_Op_Assignment(curr->val.simple_stmt, env);
      break;
    case xorAssignK :
      codegen_Op_Assignment(curr->val.simple_stmt, env);       
      break;
    case mulAssignK : 
      codegen_Op_Assignment(curr->val.simple_stmt, env);
      break;
    case divAssignK : 
      codegen_Op_Assignment(curr->val.simple_stmt, env);
      break;
    case remAssignK : 
      codegen_Op_Assignment(curr->val.simple_stmt, env);	
      break;
    case lshiftAssignK : 
      codegen_Op_Assignment(curr->val.simple_stmt, env);
      break;
    case rshiftAssignK : 
      codegen_Op_Assignment(curr->val.simple_stmt, env);
      break;
    case andAssignK : 
      codegen_Op_Assignment(curr->val.simple_stmt, env);
      break;
    case bitclearAssignK: 
      codegen_Op_Assignment(curr->val.simple_stmt, env);
      break;
    case simpleK : 
      codegen_Simple_Stmt(curr->val.simple_stmt, env);
      break;
    default : fprintf(stderr, "Error: Not valid StatementK\n"); exit(1);
    }

    if (curr->kind != varBodyK) {
      fprintf(fp, "\n"); /* At the end of each statement, we have a new line. */
    }

    curr = curr->nextStmt;
  }	
}
