#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "symbol.h"
#include "tree.h"
#include "../utils/memory.h"
#include "../utils/util.h"
#include "typecheck.h"

typedef struct TYPE_LIST   TYPE_LIST;
typedef struct FIELD_LIST  FIELD_LIST;

struct TYPE_LIST {
  TYPE *head;
  TYPE **tail;
};

struct FIELD_LIST {
  FIELD *head;
  FIELD *tail;
};


bool modeSymbol;
SYMBOL_TABLE *root_table;

void init_Root_SymbolTable();
SYMBOL_TABLE *enter_scope_Symboltable(SYMBOL_TABLE *parent);

SYMBOL *put_Symbol(SYMBOL *symbol, SYMBOL_TABLE *t, int lineno);
SYMBOL *put_Symbol_Silent(SYMBOL *symbol, SYMBOL_TABLE *t, int lineno); // regardless mode symbol is saved without outputing to stdout
SYMBOL *get_Symbol_By_Name(char *name, SYMBOL_TABLE *t); 
SYMBOL *get_Symbol(SYMBOL *symbol, SYMBOL_TABLE *t); 
SYMBOL *get_Symbol_With_Category(SYMBOL *symbol, CategoryK category, SYMBOL_TABLE *t); // only returns type symbols

TYPE_LIST *brackets_To_Type_List(BRACKET_LIST *list);

SYMBOL_TABLE *new_Symbol_Table();

SYMBOL *new_Base_Type(char *name, CategoryK category, BaseK typeK);



/* == Assertion code == */
void assertSymbolDoesNotExistInTable(SYMBOL *sym, SYMBOL_TABLE *t, int lineno);
void assertType(SYMBOL *sym, char *name);

void assertNotSpecialFunctionName(char *name, int lineno){
  if( strcmp(name, "init") == 0 || strcmp(name, "main") == 0){
    fprintf(stderr, "Error: (line %d) %s must be a function",lineno, name);
    exit(1);
  }
}

void assertSymbolDoesNotExistInTable(SYMBOL *sym, SYMBOL_TABLE *t, int lineno){
  // don't allow same declarations in the same table or scope
  int idx = hash(sym->name);
  for(SYMBOL *s = t->table[idx]; s; s = s->next){
    if(strcmp(sym->name, s->name) == 0) {
      fprintf(stderr, "Error: (line: %d) identifier \"%s\" already declared\n", lineno, sym->name);
      exit(1);
    }
  }
}

void assertType(SYMBOL *sym, char *name){
  if(!sym) {
    fprintf(stderr, "Error: type %s is not declared", name);
    exit(1);
  }

  if(sym->category != category_type) {
    fprintf(stderr, "Error: %s is not a type\n", sym->name);
    exit(1);
  }
}

/* ==== Utils for Typing === */
FIELD * symbol_Type_Struct_Body(DECLARATION_LIST *list, SYMBOL_TABLE *t);

/*
  - Convert list of brackets into a list of types.
  - Elements of this list can be slice or an array.
*/
TYPE_LIST *brackets_To_Type_List(BRACKET_LIST *list){
  TYPE **previous = NULL;  // use this to connect types
  TYPE  *head = NULL;


  for(BRACKET_LIST *node = list; node; node = node->nxt) {
    TYPE *type = NEW(TYPE);
    if(!head) head = type;
    
    if(node->bracket->isEmpty){
      
      type->kind = sliceK;

      if(!previous)
        previous    = &(type->val.slice);
      else {
        *previous   = type;
        previous    = &(type->val.slice);
      }
    }
    else{
      type->kind = arrayK;
      type->val.array.size = node->bracket->idx;

      if(!previous)
        previous    = &(type->val.array.element);
      else {
        *previous   = type;
        previous    = &(type->val.array.element);
      }
    }    
  }

  TYPE_LIST *result = NEW(TYPE_LIST);

  result->head = head;

  result->tail = previous;
  return result;
}

/*
  - Convert declaration into a list of struct fields.
 */
FIELD_LIST* symbol_Type_Struct_Declaration(DECLARATION *dclr, SYMBOL_TABLE *t){
  switch(dclr->kind){
  case Id_list_arrayk:
  case Id_list_typek:
    {
      // need to get a type to register
      SYMBOL *type_symbol = get_Symbol_With_Category(dclr->type, category_type, t);

      TYPE *type;

      if(dclr->kind == Id_list_arrayk) {
        TYPE_LIST *type_list = brackets_To_Type_List(dclr->val.indx_list);

        type             = type_list->head;
        *type_list->tail = type_symbol->val.type;
      }
      else {
        type = type_symbol->val.type;
      }
      
      FIELD  *head = NULL;
      FIELD **tail = &head;     // without it points to NULL

      IDENTIFIER_LIST *crnt = dclr->lhs;

      while(crnt){
        crnt->symbol->category = category_field_name;
        crnt->symbol->val.type = type;
      
        put_Symbol_Silent(crnt->symbol, t, crnt->lineno);

        FIELD *crnt_field = NEW(FIELD);
        crnt_field->name  = crnt->symbol->name;
        crnt_field->type  = type;
        
        if(!head){
          head = crnt_field;
        }
        else {
          (*tail)->nxt = crnt_field;
          tail = &(*tail)->nxt;
        }
        crnt = crnt->nxt;
      }
              
      FIELD_LIST *list = NEW(FIELD_LIST);
      
      list->head = head;
      list->tail = (*tail);
      return list;
    }
    break;
  case Id_list_structK:
    {
      FIELD  *head = NULL;
      FIELD **tail = &head;

      TYPE *type = NEW(TYPE);

      type->kind = structK;
      type->val.field =  symbol_Type_Struct_Body(dclr->val.struct_body, enter_scope_Symboltable(t));

      IDENTIFIER_LIST *crnt = dclr->lhs;      

      while(crnt){
        crnt->symbol->category = category_variable;
        crnt->symbol->val.type = type;

        put_Symbol_Silent(crnt->symbol, t, crnt->lineno);

        FIELD *crnt_field = NEW(FIELD);
        crnt_field->name = crnt->symbol->name;
        crnt_field->type = crnt->symbol->val.type;
        
        if(!head){
          head = crnt_field;
        }
        else {
          (*tail)->nxt = crnt_field;
          tail = &(*tail)->nxt;
        }

        crnt = crnt->nxt;
      }
      FIELD_LIST *list = NEW(FIELD_LIST);
      list->head = head;
      list->tail = (*tail);
      return list; 
    }
    break;
  case Id_list_array_structK:
    {
      FIELD  *head = NULL;
      FIELD **tail = &head;

      TYPE_LIST *lst  = brackets_To_Type_List(dclr->val.struct_array.indx_list);
      TYPE      *type = NEW(TYPE);

      type->kind      = structK;
      type->val.field = symbol_Type_Struct_Body(dclr->val.struct_array.struct_body, enter_scope_Symboltable(t));

      *lst->tail  = type;

      IDENTIFIER_LIST *crnt = dclr->lhs;      

      while(crnt){
        crnt->symbol->category = category_variable;
        crnt->symbol->val.type = lst->head;

        put_Symbol_Silent(crnt->symbol, t, crnt->lineno);

        FIELD *crnt_field = NEW(FIELD);
        crnt_field->name = crnt->symbol->name;
        crnt_field->type = crnt->symbol->val.type;
        
        if(!head){
          head = crnt_field;
        }
        else {
          (*tail)->nxt = crnt_field;
          tail = &(*tail)->nxt;
        }

        crnt = crnt->nxt;
      }
      FIELD_LIST *list = NEW(FIELD_LIST);
      list->head = head;
      list->tail = (*tail);
      return list; 
    }
    break;
  case Id_list_eqk:
  case Id_list_type_eqk:
    fprintf(stderr, "Error: (line: %d) Cannot do assignment inside of a struct\n", dclr->lineno);
    exit(1);
  default:
     fprintf(stderr, "Error: (line %d) This kind is not in DeclarationK\n", dclr->lineno);
     exit(1);
  }
  return NULL;
}


/*
  - Converts declartions inside of a struct body into a list of struct fields.
 */
FIELD * symbol_Type_Struct_Body(DECLARATION_LIST *list, SYMBOL_TABLE *t){

  if(!list) return NULL; // Body can be empty
    
  FIELD      *head = NULL; 
  FIELD      *tail = head; 
  FIELD_LIST *field_list; 

  DECLARATION_LIST *crnt = list;  

  while(crnt){
    field_list = symbol_Type_Struct_Declaration(crnt->declaration, t);
    
    if(!head){
      head = field_list->head; 
      tail = field_list->tail;
    } 
    else{ 
      tail->nxt = field_list->head;
      tail = field_list->tail;
    } 
    
    crnt = crnt->nxt; 
  }

  tail->nxt = NULL;
  
  return head; 
}


/* ============ Utils for printing ============= */
char * get_Category_String(CategoryK kind){
  switch(kind){ 
  case category_constant:       return "constant";
  case category_type:           return "type";
  case category_variable:       return "variable";
  case category_function:       return "function";
  case category_field_name:     return "field_name";
  default:
    fprintf(stderr, "Error: This kind is not in CategoryK\n");
    exit(1);
  }
}

char * get_BaseK_String(BaseK kind){
  switch(kind) { 
  case boolK:    return "bool";
  case intK:     return "int";
  case float64K: return "float64";
  case runeK:    return "rune";
  case stringK:  return "string";
  default: 
    fprintf(stderr, "Error: This kind is not in BaseK\n");
    exit(1);
  }
}

// recursively print struct types
void print_Struct_Field_Type(TYPE *type){

    switch(type->kind){
    case sliceK:
      printf("[]");
      print_Struct_Field_Type(type->val.slice);
      break;
    case refK:
      printf("%s;", type->val.ref.id);
      break;
    case arrayK:
      printf("[%d]", type->val.array.size);
      print_Struct_Field_Type(type->val.array.element);
      break;
    case structK:
      print_Type(type, false);
      break;
    case baseK:
      printf("%s;", get_BaseK_String(type->val.baseType));
      break;
    default:
      fprintf(stderr, "Error: This kind is not in TypeK\n");
      exit(1);
    }
}

void print_Type(TYPE *type, bool isTypeDeclaration){
  if(!type){
    fprintf(stderr, "Error: TYPE cannot be null\n");
    exit(1);
  }
    
  switch(type->kind){
  case refK:
    {
      if(isTypeDeclaration){
        printf("%s -> ",type->val.ref.id);
        print_Type(type->val.ref.element, isTypeDeclaration);
      }
      else{
        printf("%s", type->val.ref.id);
      }
    }
    break;
  case arrayK:
    printf("[%d]", type->val.array.size);
    print_Type(type->val.array.element, isTypeDeclaration);
    break;
  case sliceK:
    
    printf("[]");
    print_Type(type->val.slice, isTypeDeclaration);
    break;
  case structK:
    {
      printf("struct {");

      FIELD *field = type->val.field;
      while(field){
        printf(" %s ", field->name);
        print_Struct_Field_Type(field->type);
        field = field->nxt;
      }
      printf(" }");
    }
    break; // TODO: Need to add this later
  case baseK:
    printf("%s", get_BaseK_String(type->val.baseType));
    break;
  default: 
    fprintf(stderr, "Error: This kind is not in TypeK\n");
    exit(1);
  }
}

/* === For printing function declarations === */
void print_Param_Type(TYPE *type){
    switch(type->kind){
    case sliceK:
      printf("[]");
      print_Param_Type(type->val.slice);
      break;
    case refK:
      printf("%s", type->val.ref.id);
      break;
    case arrayK:
      printf("[%d]", type->val.array.size);
      print_Param_Type(type->val.array.element);
      break;
    case structK:
      fprintf(stderr,"Error: Cannot have structs in paramters\n");
      exit(1);
      break;
    case baseK:
      printf("%s", get_BaseK_String(type->val.baseType));
      break;
    default:
      fprintf(stderr, "Error: This kind is not in TypeK\n");
      exit(1);
    }
}

void print_Result_Type(TYPE *type){
    switch(type->kind){
    case sliceK:
      printf("[]");
      print_Param_Type(type->val.slice);
      break;
    case refK:
      printf("%s", type->val.ref.id);
      break;
    case arrayK:
      printf("[%d]", type->val.array.size);
      print_Param_Type(type->val.array.element);
      break;
    case structK:
      printf("struct { ");

      FIELD *crnt = type->val.field;
      while (crnt) {
	printf(" %s ", crnt->name);
	print_Param_Type(crnt->type);
	printf(";");
	crnt = crnt->nxt;
      }
      printf(" }");
      break;
    case baseK:
      printf("%s", get_BaseK_String(type->val.baseType));
      break;
    default:
      fprintf(stderr, "Error: This kind is not in TypeK\n");
      exit(1);
    }
}

void print_Parameters(PARAMETER *parameter) {
  PARAMETER *crnt = parameter;

  if(!parameter){
    return;
  }
  while (crnt) { 
    print_Param_Type(crnt->type);

    crnt = crnt->nxt;
    if (crnt) {
      printf(", ");
    }
  }
}

void print_Function_returnType(TYPE *result) {
  if (result == NULL) {
    printf("void");
    return;
  }
  print_Result_Type(result);
}


// Root function for printing
void print_Symbol_Type(SYMBOL *symbol) {
  switch(symbol->category){
  case category_constant:
    printf("%s", get_BaseK_String(symbol->val.type->val.baseType));
    break;
  case category_type:
    print_Type(symbol->val.type, true);
    break;
  case category_variable:
    if(symbol->val.type) print_Type(symbol->val.type, false);
    else printf("<infer>");
    break;
  case category_function:
    printf("(");
    print_Parameters(symbol->val.function->parameter);
    printf(") -> ");
    print_Function_returnType(symbol->val.function->result);
    break;
  case category_field_name:
    printf("%s", symbol->name);
    break;
  default:
    fprintf(stderr, "Error: This kind is not in CategoryK\n");
    exit(1);
  }
}


/* ============ SYMBOL TABLE / SYMBOL UTILITIES ============= */
SYMBOL_TABLE *new_Symbol_Table() {
  SYMBOL_TABLE *table = NEW(SYMBOL_TABLE);

  for(int i  = 0; i < HashSize; i++)
    {
      table->table[i] = NULL;
    }

  table->parent = NULL;

  return table;
}

SYMBOL_TABLE *enter_scope_Symboltable(SYMBOL_TABLE *parent) {
  SYMBOL_TABLE *table = new_Symbol_Table();

  table->depth  = parent->depth + 1;
  table->parent = parent;

  return table;
}

SYMBOL * new_Symbol(char *name) {
  SYMBOL *smbl = NEW(SYMBOL);
  smbl->name     = name;
  smbl->val.type = NULL;
  return smbl;
}

SYMBOL *new_Base_Type(char *name, CategoryK category, BaseK typeK){
  SYMBOL *smb  = NEW(SYMBOL);

  smb->category = category;
  smb->name = name;

  if(category == category_type){
    TYPE *type = NEW(TYPE);
    type->kind = baseK;
    type->val.baseType = typeK;

    smb->val.type = type;
    return smb;
  }
  else {
    fprintf(stderr, "Error: Not a valid category for base type\n");
    exit(1);
  }
}

SYMBOL *new_Constant(char *name, BaseK typeK, SYMBOL_TABLE *t) {
	SYMBOL *smb  = NEW(SYMBOL);

	smb->category = category_constant;
	smb->name = name;
	if (typeK == boolK) {
		SYMBOL *type_sym = get_Symbol(new_Symbol("bool"), t);
		smb->val.type = type_sym->val.type;
	}
	else {
		fprintf(stderr, "Error: Not a valid constant type\n");
		exit(1);
	}
	return smb;
}

void init_Root_SymbolTable() {
  root_table = new_Symbol_Table();
  root_table->depth = 1;

  put_Symbol(new_Base_Type("int",      category_type,     intK),    root_table, 0); 
  put_Symbol(new_Base_Type("float64",  category_type,     float64K),root_table, 0);
  put_Symbol(new_Base_Type("bool",     category_type,     boolK),   root_table, 0);
  put_Symbol(new_Base_Type("rune",     category_type,     runeK),   root_table, 0);
  put_Symbol(new_Base_Type("string",   category_type,     stringK), root_table, 0);
  put_Symbol(new_Constant("true",   boolK, root_table),   root_table, 0);
  put_Symbol(new_Constant("false",  boolK, root_table),   root_table, 0);
}


SYMBOL *put_Symbol_Silent(SYMBOL *symbol, SYMBOL_TABLE *t, int lineno){
  char *name = symbol->name;
  if (isBlankIdentifier(name)) {
    return NULL;
  }

  int i = hash(name);
  assertSymbolDoesNotExistInTable(symbol, t, lineno);

  symbol->next = t->table[i]; // link to previous symbol in the same table entry
  t->table[i]  = symbol;

  return symbol;
}

void print_symbol(SYMBOL *symbol, SYMBOL_TABLE *t){
    for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t');
    printf("%s [%s] = ", symbol->name, get_Category_String(symbol->category));
    print_Symbol_Type(symbol);
    printf("\n");
}

SYMBOL *put_Symbol(SYMBOL *symbol, SYMBOL_TABLE *t, int lineno){
  char *name = symbol->name;
  if (isBlankIdentifier(name)) {
    if (modeSymbol && symbol->category == category_function) {
      for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t');
      printf("%s [%s] = <unmapped>\n", name, get_Category_String(symbol->category));
    }
    return NULL;
  }

  int i = hash(name);
  assertSymbolDoesNotExistInTable(symbol, t, lineno);

  symbol->next = t->table[i]; // link to previous symbol in the same table entry
  t->table[i] = symbol;

  if(modeSymbol) {
    // indent
    for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t');

    printf("%s [%s] = ", name, get_Category_String(symbol->category));
    print_Symbol_Type(symbol);
    printf("\n");
  }

  return symbol;
}

SYMBOL *get_Symbol(SYMBOL *symbol, SYMBOL_TABLE *t){
  if (!t) return NULL; // it means that symbol is not in the table

  char *name = symbol->name;
  int i = hash(name);
  
  for(SYMBOL *s = t->table[i]; s; s = s->next){
    if(strcmp(s->name, name) == 0) return s;
  }
    
  return get_Symbol(symbol, t->parent); // go to parent if nothing is in current scope
}

SYMBOL *get_Symbol_ignore_category(SYMBOL *symbol, SYMBOL_TABLE *t, CategoryK category){
  if (!t) return NULL; // it means that symbol is not in the table

  char *name = symbol->name;
  int i = hash(name);
  
  for(SYMBOL *s = t->table[i]; s; s = s->next){
    if(strcmp(s->name, name) == 0 && s->category != category) return s;
  }
    
  return get_Symbol(symbol, t->parent); // go to parent if nothing is in current scope
}

SYMBOL *get_Symbol_By_Name(char *name, SYMBOL_TABLE *t){
  if (!t) return NULL;

  int i = hash(name);
  
  for(SYMBOL *s = t->table[i]; s; s = s->next){
    if(strcmp(s->name, name) == 0) return s;
  }
    
  return get_Symbol_By_Name(name, t->parent); // go to parent if nothing is in current scope
}

SYMBOL *get_Symbol_With_Category(SYMBOL *symbol, CategoryK category, SYMBOL_TABLE *t){
  SYMBOL *sym = get_Symbol_ignore_category(symbol, t, category_field_name);
  if(!sym) {
    fprintf(stderr, "Error: Symbol %s is not found\n", symbol->name);
    exit(1);
  }
  
  if(sym->category != category) {
    fprintf(stderr, "Error: %s [%s] != %s [%s]\n", sym->name, get_Category_String(sym->category), sym->name, get_Category_String(category));
    exit(1);
  }
  return sym;
}

// function to determine whether symbol has been declared in the CURRENT scope
int in_Current_Scope(SYMBOL *symbol, SYMBOL_TABLE *t) {
  if(!t) return 0;
  char *name = symbol->name;
  int i = hash(name);

  for(SYMBOL *s = t->table[i]; s; s = s->next){
    if(strcmp(s->name, name) == 0) return 1;
  }

  return 0;
}


/*============ CODE FOR WALKING AST =============*/

void symbol_Program(PROGRAM *root) {

  if(!root) return; // if file is empty

  if (modeSymbol) printf("{\n");
  init_Root_SymbolTable();

  symbol_Top_Declaration_List(root->top_declaration_list, root_table);
  if (modeSymbol) printf("}\n");
}

void symbol_Top_Declaration_List(TOP_DECLARATION_LIST *list, SYMBOL_TABLE *t) {
  TOP_DECLARATION_LIST *crnt = list;

  if(modeSymbol){
    for (int i = 0; i < t->depth; i++) putchar('\t');
    printf("{\n");
  }
  
  SYMBOL_TABLE *new_table = enter_scope_Symboltable(t);
  while (crnt){
    symbol_Top_Declaration(crnt->top_declaration, new_table);
    crnt = crnt->nxt;
  }

  if(modeSymbol){
    for (int i = 0; i < t->depth; i++) putchar('\t');
    printf("}\n");
  }
}


void symbol_Top_Declaration(TOP_DECLARATION *declaration, SYMBOL_TABLE *t)
{
  switch(declaration->kind){
  case varK:
    symbol_Var_Declaration(true, declaration->val.var_dclr, t);
    break;
  case typeK: 
    symbol_Type_Declaration_List(true, declaration->val.type_dclr_list, t);
    break;
  case funcK:
    symbol_Function_Declaration(declaration->val.function_dclr, t);
    break;
  default:
    fprintf(stderr, "Error: (line: %d) Cannot put this symbol kind\n", declaration->lineno);
  }
}

void check_link(TYPE *tp){
  for(TYPE *crnt = tp; crnt; crnt= tp->val.slice){
    printf("ok\n");
  }
}


void symbol_Declaration(bool isTop, DECLARATION *dclr, SYMBOL_TABLE *t){
  switch(dclr->kind){
  case Id_list_type_eqk:
  case Id_list_typek:
    {
      IDENTIFIER_LIST *crnt = dclr->lhs;

      // need to get a type to register
      SYMBOL *type_symbol = get_Symbol_With_Category(dclr->type, category_type, t);

      while(crnt){
        crnt->symbol->category = category_variable;
        crnt->symbol->val.type = type_symbol->val.type;

        if(isTop)  assertNotSpecialFunctionName(crnt->symbol->name, dclr->lineno);

        put_Symbol(crnt->symbol, t, crnt->lineno);

        crnt = crnt->nxt;
      }

      // go over all expressions
      if (dclr->kind == Id_list_type_eqk) {
		EXP_LIST *exps = dclr->val.rhs_list;
		while(exps) {
			symbol_expression(exps->exp, t);
			exps = exps->nextExp;
		}
	}
    }
    break;
  case Id_list_arrayk: 
    {
      SYMBOL *type_symbol = get_Symbol_With_Category(dclr->type, category_type, t);

      TYPE_LIST *list = brackets_To_Type_List(dclr->val.indx_list);

      *list->tail = type_symbol->val.type;

      IDENTIFIER_LIST *crnt = dclr->lhs;      
      while(crnt){
        crnt->symbol->category = category_variable;
        crnt->symbol->val.type = list->head;
        if(isTop)  assertNotSpecialFunctionName(crnt->symbol->name, dclr->lineno);
        put_Symbol(crnt->symbol, t, crnt->lineno);

        crnt = crnt->nxt;
      }
    }
    break;
  case Id_list_eqk:
    {
      IDENTIFIER_LIST *crnt = dclr->lhs;
      while(crnt){
        crnt->symbol->category = category_variable;
        crnt->symbol->val.type = NULL;
        
        if(isTop)  assertNotSpecialFunctionName(crnt->symbol->name, dclr->lineno);
        
        put_Symbol(crnt->symbol, t, crnt->lineno);
        crnt= crnt->nxt;
      }

      // go over all expressions
      EXP_LIST *exps = dclr->val.rhs_list;
      while(exps) {
	      symbol_expression(exps->exp, t);
              exps = exps->nextExp;
      }
    }
    break;
  case Id_list_structK:
    {
      TYPE *type = NEW(TYPE);

      type->kind = structK;
      type->val.field =  symbol_Type_Struct_Body(dclr->val.struct_body, enter_scope_Symboltable(t));

      IDENTIFIER_LIST *crnt = dclr->lhs;      

      while(crnt){
        crnt->symbol->category = category_variable;
        crnt->symbol->val.type = type;
        
        if(isTop)  assertNotSpecialFunctionName(crnt->symbol->name, dclr->lineno);
        
        put_Symbol(crnt->symbol, t, crnt->lineno);

        crnt = crnt->nxt;
      }
    }
    break;
  case Id_list_array_structK:
    {
      TYPE_LIST *lst  = brackets_To_Type_List(dclr->val.struct_array.indx_list);
      TYPE      *type = NEW(TYPE);

      type->kind      = structK;
      type->val.field = symbol_Type_Struct_Body(dclr->val.struct_array.struct_body, enter_scope_Symboltable(t));

      *lst->tail  = type;

      IDENTIFIER_LIST *crnt = dclr->lhs;      

      while(crnt){
        crnt->symbol->category = category_variable;
        crnt->symbol->val.type = lst->head;
        
        if(isTop)  assertNotSpecialFunctionName(crnt->symbol->name, dclr->lineno);
        
        put_Symbol(crnt->symbol, t, crnt->lineno);

        crnt = crnt->nxt;
      }
    }
    break;
   case Id_list_array_eqK:
      {
        SYMBOL *type_symbol = get_Symbol_With_Category(dclr->type, category_type, t);

        TYPE_LIST *list = brackets_To_Type_List(dclr->val.array_eq.indx_list);

        *list->tail = type_symbol->val.type;

        IDENTIFIER_LIST *crnt = dclr->lhs;
        while(crnt){
        	crnt->symbol->category = category_variable;
        	crnt->symbol->val.type = list->head;
        	if(isTop)  assertNotSpecialFunctionName(crnt->symbol->name, dclr->lineno);
        	put_Symbol(crnt->symbol, t, crnt->lineno);

        	crnt = crnt->nxt;
        }

	// go over all expressions
        EXP_LIST *exps = dclr->val.array_eq.rhs_list;
        while(exps) {
              symbol_expression(exps->exp, t);
              exps = exps->nextExp;
        }
      }
      break;
  default:
    fprintf(stderr, "Error: This DeclarationK is not recognised\n");
    exit(1);
  }
}

void symbol_Type_Declaration_List(bool isTop, TYPE_DECLARATION_LIST *list, SYMBOL_TABLE *t){
  TYPE_DECLARATION_LIST *crnt = list;
  while (crnt) { 
    symbol_Type_Declaration(isTop, crnt->type_declaration, t);
    crnt = crnt->nxt;
  }
}

void symbol_Type_Declaration(bool isTop, TYPE_DECLARATION *dclr, SYMBOL_TABLE *t){
  switch(dclr->kind){
  case type_simpleK:
    {
      SYMBOL *type_symbol = get_Symbol_With_Category(dclr->val.simple.type, category_type, t);
      
      dclr->val.simple.name->category = category_type;

      TYPE *type = NEW(TYPE);

      type->kind            = refK;
      type->val.ref.id      = dclr->val.simple.name->name;
      type->val.ref.element = type_symbol->val.type;

      dclr->val.simple.name->val.type = type;
      
      if(isTop)  assertNotSpecialFunctionName(dclr->val.simple.name->name, dclr->lineno);
      
      put_Symbol(dclr->val.simple.name, t, dclr->lineno);
    }
    break;
  case type_arrayK:
    {
      SYMBOL *type_symbol = get_Symbol_With_Category(dclr->val.array.type, category_type, t);

      dclr->val.array.name->category = category_type;

      TYPE_LIST *list = brackets_To_Type_List(dclr->val.array.brackets);

      TYPE *type            = NEW(TYPE);
      type->kind            = refK;
      type->val.ref.id      = dclr->val.array.name->name;
      type->val.ref.element = list->head;
      *list->tail           = type_symbol->val.type;
      
      dclr->val.array.name->val.type = type;
      
      if(isTop)  assertNotSpecialFunctionName(dclr->val.array.name->name, dclr->lineno);
      put_Symbol(dclr->val.array.name, t, dclr->lineno);      
    }
    break;
  case type_structK:
    {
      
      TYPE *type = NEW(TYPE);
      dclr->val.structure.name->category = category_type;
      type->kind                         = refK;
      type->val.ref.id                   = dclr->val.structure.name->name;

      
      TYPE *struct_type = NEW(TYPE);
      struct_type->kind      = structK;

      type->val.ref.element = struct_type;

      dclr->val.structure.name->val.type = type;

      if(isTop)  assertNotSpecialFunctionName(dclr->val.structure.name->name, dclr->lineno);
                
      put_Symbol_Silent(dclr->val.structure.name, t, dclr->lineno);
      struct_type->val.field = symbol_Type_Struct_Body(dclr->val.structure.body, enter_scope_Symboltable(t));      

      if(modeSymbol) print_symbol(dclr->val.structure.name, t);
    }
    break;
  case type_struct_arrayK:
    {      
      TYPE *type = NEW(TYPE);
      dclr->val.struct_array.name->category = category_type;
      type->kind                            = refK;
      type->val.ref.id                      = dclr->val.structure.name->name;

      TYPE_LIST *list       = brackets_To_Type_List(dclr->val.struct_array.brackets);
      type->val.ref.element = list->head;

      if(isTop)  assertNotSpecialFunctionName(dclr->val.struct_array.name->name, dclr->lineno);

      put_Symbol_Silent(dclr->val.struct_array.name, t, dclr->lineno);
      
      TYPE *struct_type = NEW(TYPE);
      struct_type->kind      = structK;
      struct_type->val.field = symbol_Type_Struct_Body(dclr->val.struct_array.body, enter_scope_Symboltable(t));

      *list->tail = struct_type;
      
      dclr->val.struct_array.name->val.type = type;

      if(modeSymbol) print_symbol(dclr->val.struct_array.name, t);
    }
    break;
  default:
    fprintf(stderr, "Error: Not in TypeDeclarationK\n");
    exit(1);
  }
}

void symbol_Declaration_list(bool isTop, DECLARATION_LIST *list, SYMBOL_TABLE *t) {
	DECLARATION_LIST *crnt = list;

  	while (crnt) {
    		symbol_Declaration(isTop, crnt->declaration, t);
    		crnt = crnt->nxt;
  	}
}


/* ============ FUNCTION DECLARATION ============ */

typedef struct PARAMETER_PAYLOAD PARAMETER_PAYLOAD;
typedef struct DELAYED_SYMBOL   DELAYED_SYMBOL;

struct DELAYED_SYMBOL {
  SYMBOL *value;
  DELAYED_SYMBOL *nxt;
};

struct PARAMETER_PAYLOAD {
  PARAMETER *head;
  PARAMETER **tail;
  DELAYED_SYMBOL *head_sym;
  DELAYED_SYMBOL **tail_sym;
};

PARAMETER_PAYLOAD * symbol_Parameter_Unit(PARAMETER_UNIT *unit, SYMBOL_TABLE *t);
TYPE *symbol_Function_Return(RESULT *result, SYMBOL_TABLE *t);

TYPE *symbol_Function_Return(RESULT *result, SYMBOL_TABLE *t) {
  if(!result) return NULL;

  if (result->kind == simpleResultK || result->kind == arrayResultK) {
  	SYMBOL *type_symbol = get_Symbol_With_Category(result->type, category_type, t);
 

  	if(result->kind == arrayResultK){
    		TYPE_LIST *lst         = brackets_To_Type_List(result->brackets);
    		result->type->val.type = lst->head;
    		*lst->tail             = type_symbol->val.type;
    		return result->type->val.type;
  	}

  	result->type->val.type = type_symbol->val.type;
  	return result->type->val.type;
  }

  if (result->kind == structResultK) {
	  TYPE *struct_type = NEW(TYPE);
      	  struct_type->kind      = structK;
      	  struct_type->val.field = symbol_Type_Struct_Body(result->decl_list, enter_scope_Symboltable(t));
  	  result->type = new_Symbol("");
	  result->type->val.type = struct_type;
	  return result->type->val.type;
  }

  if (result->kind == arrayStructResultK) {
      TYPE_LIST *list       = brackets_To_Type_List(result->brackets);
      result->type = new_Symbol("");
      result->type->val.type = list->head;

      TYPE *struct_type = NEW(TYPE);
      struct_type->kind      = structK;
      struct_type->val.field = symbol_Type_Struct_Body(result->decl_list, enter_scope_Symboltable(t));
      *list->tail = struct_type;
      return result->type->val.type;
  }

  return NULL;
}

void symbol_Function_Declaration(FUNCTION_DECLARATION *fun_dclr, SYMBOL_TABLE *t) {
  bool isInitFunction = strcmp(fun_dclr->name->name, "init") == 0;

  fun_dclr->name->category = category_function;

  FUNCTION *function = NEW(FUNCTION);

  PARAMETER  *param_head = NULL;
  PARAMETER **param_tail = NULL;

  DELAYED_SYMBOL  *symbol_head = NULL;
  DELAYED_SYMBOL **symbol_tail = NULL;

  if (!isInitFunction) {
          put_Symbol_Silent(fun_dclr->name, t, fun_dclr->lineno);
  }

  // Put a symbol for each parameter to the function
  PARAMETER_LIST *crnt = fun_dclr->signature->parameters;
 
  while (crnt) {
    PARAMETER_PAYLOAD *pair = symbol_Parameter_Unit(crnt->param, t);

    if(!symbol_head){
      symbol_head = pair->head_sym;
      symbol_tail = pair->tail_sym;
    }
    else{
      *symbol_tail = pair->head_sym;
      symbol_tail  = pair->tail_sym;
    }
    
    if(!param_head){
      param_head = pair->head;
      param_tail = pair->tail;
    }
    else{
      *param_tail = pair->head;
      param_tail  = pair->tail;
    }
    crnt = crnt->nxt;
  }

  if(symbol_tail) *symbol_tail = NULL;

  function->parameter          = param_head;
  fun_dclr->name->val.function = function;

  function->result =  symbol_Function_Return(fun_dclr->signature->result, t);

  if(isInitFunction){
    if (modeSymbol) {
      for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t');
      printf("init [function] = <unmapped>\n");
    }
  }
  else {
  	if(modeSymbol) {
    		// indent
    		for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t');

    		printf("%s [%s] = ", fun_dclr->name->name, get_Category_String(fun_dclr->name->category));
    		print_Symbol_Type(fun_dclr->name);
    		printf("\n");
  	}
  }

  if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("{\n"); }

  // Function has new scope
  SYMBOL_TABLE *function_scope = enter_scope_Symboltable(t);
  DELAYED_SYMBOL *sym = symbol_head;
  while(sym){
    put_Symbol(sym->value, function_scope, fun_dclr->lineno);
    sym = sym->nxt;
  }
  
  symbol_STMTS(fun_dclr->body, function_scope);

  if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("}\n"); }
}




PARAMETER_PAYLOAD * symbol_Parameter_Unit(PARAMETER_UNIT *unit,  SYMBOL_TABLE *t){
  PARAMETER *head = NULL;
  PARAMETER **param = &head;
  PARAMETER_PAYLOAD *lst = NEW(PARAMETER_PAYLOAD);

  lst->head_sym = NULL;
  lst->tail_sym = &lst->head_sym;
   
  SYMBOL *type_symbol = get_Symbol_With_Category(unit->type, category_type, t);

  TYPE *type = type_symbol->val.type;  

  if(unit->isArray){
    TYPE_LIST *list = brackets_To_Type_List(unit->brackets);

    *list->tail = type_symbol->val.type;
  
    type        = list->head;
  }  

  IDENTIFIER_LIST *crnt = unit->ids_list;
  while(crnt){
    crnt->symbol->category = category_variable;
    crnt->symbol->val.type = type;
    
    *lst->tail_sym = NEW(DELAYED_SYMBOL);
    (*lst->tail_sym)->value = crnt->symbol;
    
    lst->tail_sym = &(*lst->tail_sym)->nxt;

    *param         = NEW(PARAMETER);
    (*param)->type = type;
    param          = &(*param)->nxt;

    crnt = crnt->nxt;
  }

  lst->head = head;
  lst->tail = param;

  return lst;
}


/* ============ BRACKETS ============ */

void print_Bracket(BRACKET *bracket){
  if(bracket->isEmpty) printf("[]");
  else printf("[%d]", bracket->idx);
}

void print_Bracket_list(BRACKET_LIST *list){
  BRACKET_LIST *crnt = list;

  while (crnt){
    print_Bracket(crnt->bracket);
    crnt = crnt->nxt;
  }
}

/* ============ STATEMENTS ============ */

void symbol_STMTS(STMTS *stmts, SYMBOL_TABLE *t) {
  STMTS *curStmt = stmts;

  // new symbol table in case in scope is opened
  SYMBOL_TABLE *new_t1;

  while (curStmt) {
    switch (curStmt->kind) {
      /* Empty statement */
    case breakK:
      break;
    case continueK: 
      break;
    case blockK:
      {
        if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("{\n"); }
        symbol_STMTS(curStmt->val.block,enter_scope_Symboltable(t));
        if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("}\n"); }
      }
      break;
    case exprSimpleK: 
      symbol_Expression_Statement(curStmt->val.simple_stmt, t);
      break;
    case returnK: 
      symbol_Return_Statement(curStmt->val.exp, t);
      break;
    case shortVarDeclK: 
      symbol_Short_Var_Decl(curStmt->val.simple_stmt, t);
      break;
    case varBodyK: 
      symbol_Var_Declaration(false, curStmt->val.var_dclr, t);
      break;
    case equalAssignK : 
      symbol_Assignment_List(curStmt->val.simple_stmt, t);
      break;
    case assignListK: 
      symbol_Assignment_List(curStmt->val.simple_stmt, t);
      break;
      /* Op-Assignment (add_op_eq) */
    case plusAssignK: 
      symbol_Op_Assignment(curStmt->val.simple_stmt, t);
      break;
    case minusAssignK: 
      symbol_Op_Assignment(curStmt->val.simple_stmt, t);
      break;
    case orAssignK: 
      symbol_Op_Assignment(curStmt->val.simple_stmt, t);
      break;
    case xorAssignK:
      symbol_Op_Assignment(curStmt->val.simple_stmt, t);       
      break;
      /* Op-Assignment (mul_op_eq) */
    case mulAssignK : 
      symbol_Op_Assignment(curStmt->val.simple_stmt, t);
      break;
    case divAssignK : 
      symbol_Op_Assignment(curStmt->val.simple_stmt, t);
      break;
    case remAssignK : 
      symbol_Op_Assignment(curStmt->val.simple_stmt, t);	
      break;
    case lshiftAssignK : 
      symbol_Op_Assignment(curStmt->val.simple_stmt, t);
      break;
    case rshiftAssignK : 
      symbol_Op_Assignment(curStmt->val.simple_stmt, t);
      break;
    case andAssignK : 
      symbol_Op_Assignment(curStmt->val.simple_stmt, t);
      break;
    case bitclearAssignK: 
      symbol_Op_Assignment(curStmt->val.simple_stmt, t);
      break;
      /* Block statement -> Taken care by opening new symbol table scope in Function declaration, If, and For */
    case printK:
      if (curStmt->val.print != NULL) {
        symbol_expression_list(curStmt->val.print, t);	
      }
      break;
    case printlnK: 
      if (curStmt->val.print != NULL) {
        symbol_expression_list(curStmt->val.print, t);
      }
      break;
    case forK: 
      symbol_For_Statement(curStmt->val.for_stmt, t);
      break;
    case ifK: 
      symbol_If_Statement(curStmt, t);
      break;
    case switchK: 
	
      // new scope for switch statement (including switch_on)
      if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("{\n"); }
      new_t1 = enter_scope_Symboltable(t);				
      t->depth++;

      symbol_Switch_On(curStmt->val.switch_stmt.switch_on, new_t1);
      symbol_Switch_Case(curStmt->val.switch_stmt.switch_case, new_t1);

      // close switch statement scope
      t->depth--;
      if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("}\n"); }
      break;
    case incK: 
      symbol_Inc_Statement(curStmt->val.simple_stmt, t);
      break;
    case decK: 
      symbol_Dec_Statement(curStmt->val.simple_stmt, t);
      break;		
    case typeBodyK : 
      symbol_Type_Declaration_List(false, curStmt->val.type_dclr, t);
      break;
    case simpleK : 
      symbol_Simple_Stmt(curStmt->val.simple_stmt, t);
      break;
    default : fprintf(stderr, "Error: (line: %d) Not valid StatementK in symbol.c file\n", curStmt->lineno); exit(1);
    }

    curStmt = curStmt->nextStmt;
  }

}

void symbol_Expression_Statement(SIMPLE_STMT *simple_stmt, SYMBOL_TABLE *t) {
	if (simple_stmt->val.exp != NULL) {
		symbol_expression(simple_stmt->val.exp, t);
	}
}

void symbol_Return_Statement(EXP *exp, SYMBOL_TABLE *t) {
	if (exp != NULL) {
		symbol_expression(exp, t);
	}
}

void symbol_Short_Var_Decl(SIMPLE_STMT *simple_stmt, SYMBOL_TABLE *t) {
	
	// lhs identifier list : check if any of the identifiers have not been declared in the current scope
	EXP_LIST *lhs = simple_stmt->val.assignment_short_var_decl.identifierList;
	EXP_LIST *rhs = simple_stmt->val.assignment_short_var_decl.rhs;
	int newVariable = 0;
  	int countAllVariable = 0;

	while (lhs != NULL && lhs->exp != NULL && rhs != NULL && rhs->exp != NULL) {
    		countAllVariable++;
    
		if (!in_Current_Scope(lhs->exp->val.primaryExpression->val.identifier, t)){
			if (!isBlankIdentifier(lhs->exp->val.primaryExpression->val.identifier->name)){
			
				// add any newly declared variables to the symbol table
                        	newVariable++;		
			}
		
			lhs->exp->val.primaryExpression->val.identifier->category = category_variable;

			put_Symbol(lhs->exp->val.primaryExpression->val.identifier, t, simple_stmt->lineno);
		}

		// tie each id previously declared to their declared symbol
		else {
			symbol_Primary_Expression(lhs->exp->val.primaryExpression, t);
		}

		rhs = rhs->nextExp;
		lhs = lhs->nextExp;
	}

	if (newVariable == 0) {
		fprintf(stderr, "Error: (line %d) short declaration contains no new variables\n", simple_stmt->lineno);
		exit(1);
	}

	// rhs expression list : check if expression identifiers exist in symbol tables
	symbol_expression_list(simple_stmt->val.assignment_short_var_decl.rhs, t);

}

void symbol_Var_Declaration(bool isTop, VAR_DECLARATION *dclr, SYMBOL_TABLE *t) {
	if (dclr->isList) {
		symbol_Declaration_list(isTop, dclr->val.declaration_list, t);
	}
	else {
		symbol_Declaration(isTop, dclr->val.declaration, t);
	}
}

void symbol_Assignment_List(SIMPLE_STMT *simple_stmt, SYMBOL_TABLE *t) {
	
	// lhs expression list : check all expression identifiers exist in symbol table
	symbol_expression_list(simple_stmt->val.assignment_list.lhs, t);

	// rhs expression list : check all expression identifiers exist in symbol table
	symbol_expression_list(simple_stmt->val.assignment_list.rhs, t);

}

void symbol_Op_Assignment(SIMPLE_STMT *simple_stmt, SYMBOL_TABLE *t) {

	// lhs expression : check any expression identifiers exist in symbol table
	symbol_expression(simple_stmt->val.assignment.lhs, t);

	// rhs expression : check any expression identifiers exist in symbol table
	symbol_expression(simple_stmt->val.assignment.rhs, t);
}

void symbol_Print_Statement(EXP_LIST *print, SYMBOL_TABLE *t) {

	// argument expression list : check all expression identifiers exist in symbol table
	symbol_expression_list(print, t);
}

void symbol_Println_Statement(EXP_LIST *println, SYMBOL_TABLE *t) {

	// argument expression list : check all expression identifiers exist in symbol table
	symbol_expression_list(println, t);
}

void symbol_For_Statement(FOR_STATEMENT *for_stmt, SYMBOL_TABLE *t) {

	// new scope for clause in for statement
	if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("{\n"); }

	SYMBOL_TABLE *new_t1 = enter_scope_Symboltable(t);

	switch (for_stmt->kind) {
		case whileK : 
			symbol_expression(for_stmt->val.while_condition, new_t1);
			break;

		case forClauseK :
			if (for_stmt->val.clause->simple_stmt1 != NULL) {
				symbol_Simple_Stmt(for_stmt->val.clause->simple_stmt1, new_t1);
			}
			if (for_stmt->val.clause->condition != NULL) {
				symbol_expression(for_stmt->val.clause->condition, new_t1);
			}
			if (for_stmt->val.clause->simple_stmt2 != NULL) {
				symbol_Simple_Stmt(for_stmt->val.clause->simple_stmt2, new_t1);
			}
			break;

		case infiniteK :
			break;
	}

	t->depth++;
	
	if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("{\n"); }

	// new scope for body of for statement within for statement scope
	SYMBOL_TABLE *new_t2 = enter_scope_Symboltable(new_t1);
	new_t1->depth++;
	symbol_STMTS(for_stmt->block, new_t2);

	// close body scope
	new_t1->depth--;
  if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("}\n"); }
	
	t->depth--;
	// close for clause scope
  if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("}\n"); }
}

void symbol_If_Statement(STMTS *stmt, SYMBOL_TABLE *t) {

	// new scope for if statement
  if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("{\n"); }

	SYMBOL_TABLE *new_t1 = enter_scope_Symboltable(t);

	// optional simple statement
	if (stmt->val.if_stmt.simplestmt != NULL ) {
		symbol_Simple_Stmt(stmt->val.if_stmt.simplestmt, new_t1);
	}

	// condition
	symbol_expression(stmt->val.if_stmt.condition, new_t1);

	t->depth++;

	// new scope for if block within if statement scope
	if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("{\n"); }

	SYMBOL_TABLE *new_t2 = enter_scope_Symboltable(new_t1);
	new_t1->depth++;
	symbol_STMTS(stmt->val.if_stmt.block, new_t2);
	// close if block scope
  new_t1->depth--;
  if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("}\n"); }

	// optional else if/else blocks
	if (stmt->val.if_stmt.next != NULL) {
    SYMBOL_TABLE *new_t3 = enter_scope_Symboltable(new_t1);

    if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("{\n"); }
		symbol_STMTS(stmt->val.if_stmt.next, new_t3);    
    if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("}\n"); }
	}

  // close if simple stmt/condition scope
  t->depth--;
	if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("}\n"); }
}

void symbol_Switch_On(SWITCH_ON *switch_on, SYMBOL_TABLE *t) {

	// optional simple statement
	if (switch_on->simpleStmt != NULL) {
		symbol_Simple_Stmt(switch_on->simpleStmt, t);
	}

	// optional condition : check an identifiers in condition exist in symbol table
	if (switch_on->condition != NULL) {
		symbol_expression(switch_on->condition, t);
	}

}

void symbol_Switch_Case(SWITCH_CASE *switch_case, SYMBOL_TABLE *t) {

	if (switch_case == NULL) {
		return;
	}

	symbol_Switch_Case(switch_case->next, t);

	// new scope for each switch case
	if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("{\n"); }
	SYMBOL_TABLE *new_t1 = enter_scope_Symboltable(t);
	t->depth++;

	// check any identifiers in expression exist in symbol table
	if (switch_case->caseExp != NULL ){
		symbol_expression_list(switch_case->caseExp, new_t1);
	}

	if (switch_case->caseStmts != NULL) {
		symbol_STMTS(switch_case->caseStmts, new_t1);
	}

	t->depth--;
	if (modeSymbol) { for (int lvl = 0; lvl < t->depth; lvl++) putchar('\t'); printf("}\n"); }
		
}

void symbol_Inc_Statement(SIMPLE_STMT *simple_stmt, SYMBOL_TABLE *t) {
	
	symbol_expression(simple_stmt->val.exp, t);

}

void symbol_Dec_Statement(SIMPLE_STMT *simple_stmt, SYMBOL_TABLE *t) {

	symbol_expression(simple_stmt->val.exp, t);
}

void symbol_Simple_Stmt(SIMPLE_STMT *simple_stmt, SYMBOL_TABLE *t) {
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
        symbol_Assignment_List(simple_stmt,t);
        break;
      case exprSimpleK :
        symbol_Expression_Statement(simple_stmt,t);
        break;
      case incK :
        symbol_Inc_Statement(simple_stmt,t);
        break;
      case decK :
        symbol_Dec_Statement(simple_stmt,t);
        break;
      case shortVarDeclK :
        symbol_Short_Var_Decl(simple_stmt,t);
        break;
      case equalAssignK :
        symbol_Assignment_List(simple_stmt,t);
        break;
      case plusAssignK :
        symbol_Op_Assignment(simple_stmt, t);
        break;
      case minusAssignK :
        symbol_Op_Assignment(simple_stmt, t);
        break;
      case orAssignK :
        symbol_Op_Assignment(simple_stmt, t);
        break;
      case xorAssignK :
        symbol_Op_Assignment(simple_stmt, t);
        break;
      case mulAssignK :
        symbol_Op_Assignment(simple_stmt, t);
        break;
      case divAssignK :
        symbol_Op_Assignment(simple_stmt, t);
        break;
      case remAssignK :
        symbol_Op_Assignment(simple_stmt, t);
        break;
      case lshiftAssignK :
        symbol_Op_Assignment(simple_stmt, t);
        break;
      case rshiftAssignK :
        symbol_Op_Assignment(simple_stmt, t);
        break;
      case andAssignK :
        symbol_Op_Assignment(simple_stmt, t);
        break;
      case bitclearAssignK:
        symbol_Op_Assignment(simple_stmt, t);
        break;
      case simpleK :
        symbol_Simple_Stmt(simple_stmt, t);
        break;
      default : fprintf(stderr, "Error: (line: %d) Not valid StatementK\n", simple_stmt->lineno); exit(1);
   }

}

/* ============ EXPRESSION ============ */

void symbol_expression(EXP *e, SYMBOL_TABLE *t) {  
  if (e == NULL) {
    return;
  }
  
  switch(e->kind) {
    case primaryExpressionK:    symbol_Primary_Expression(e->val.primaryExpression, t); break;
    case unaryOpK:              symbol_Unary_Op(e->val.unaryOpExpression, t); break;
    case binaryOpK:             symbol_Binary_Op(e->val.binaryOpExpression, t); break;
    case builtinK:              symbol_Builtin(e->val.builtinExpression, t); break;
    default:                    fprintf(stderr, "Error: (line: %d) Not a valid expression kind in symbol \n", e->lineno); exit(1);
  }
}

void symbol_expression_list(EXP_LIST *exp_list, SYMBOL_TABLE *t) {
  while (exp_list->nextExp != NULL) {
    symbol_expression(exp_list->exp, t);
    exp_list = exp_list->nextExp;
  }
  symbol_expression(exp_list->exp, t);
}

void symbol_Primary_Expression(PrimaryExpression *primaryExp, SYMBOL_TABLE *t) {
  switch(primaryExp->kind) {
    case identifierK: {

      if (isBlankIdentifier(primaryExp->val.identifier->name)) {
        break;
      }
     
      SYMBOL *retrieved = get_Symbol(primaryExp->val.identifier, t);
      
      if (retrieved == NULL) {
	      fprintf(stderr, "Error: (line %d) %s has not been declared\n", primaryExp->lineno, primaryExp->val.identifier->name);
	      exit(1);
      }
      primaryExp->val.identifier = retrieved;

      break;
    }

    case parenthesisExpK:       symbol_expression(primaryExp->val.parenthesisExp, t);  break;

    case intLiteralK: 	{          
				primaryExp->base_type = get_Symbol(new_Symbol("int"), t);
				break;
			}
    case floatLiteralK: {        
				primaryExp->base_type = get_Symbol(new_Symbol("float64"), t);
				break;
			}
    case runeLiteralK:  {       
				primaryExp->base_type = get_Symbol(new_Symbol("rune"), t);
				break;
			}
    case strLiteralK:   {        
				primaryExp->base_type = get_Symbol(new_Symbol("string"), t);
				break;
			}
    case blankExpK: {
        primaryExp->base_type = get_Symbol(new_Symbol("blank"), t);
        break;
    }  
    case primaryExpArgumentK:   symbol_Primary_Expression_arguments(primaryExp, t); break;
    case primaryExpSelectorK:   symbol_Primary_Expression_selector(primaryExp, t); break;
    case primaryExpIndexK:      symbol_Primary_Expression_index(primaryExp, t); break;
    default:                    fprintf(stderr, "Error: (line: %d) Not a valid primaryExp kind\n", primaryExp->lineno); exit(1);
  }
}

void symbol_Primary_Expression_arguments(PrimaryExpression *primaryExp, SYMBOL_TABLE *t) {
 
 symbol_Primary_Expression(primaryExp->val.argument.primaryExp, t); 

 if (primaryExp->val.argument.primaryExp->kind == identifierK) {
	SYMBOL *func_name = get_Symbol(primaryExp->val.argument.primaryExp->val.identifier, t);	
	
	if (func_name->category != category_function && func_name->category != category_type) {
		fprintf(stderr, "Error: (line %d) attempt to call primary expression that's not a function\n", primaryExp->lineno);
		exit(1);
	}
	primaryExp->base_type = func_name;		
  }
  
 else if (primaryExp->val.argument.primaryExp->kind == primaryExpSelectorK) {
	SYMBOL *func_name = get_Symbol(new_Symbol(primaryExp->val.argument.primaryExp->val.selector.identifier), t);
  	
	if (func_name->category != category_function) {
                fprintf(stderr, "Error: (line %d) attempt to call primary expression that's not a function\n", primaryExp->lineno);
                exit(1);
        }
	primaryExp->base_type = func_name;
  }
  else if (primaryExp->val.argument.primaryExp->kind == parenthesisExpK) {
	 	if (primaryExp->val.argument.primaryExp->val.parenthesisExp->kind == primaryExpressionK){
			if (primaryExp->val.argument.primaryExp->val.parenthesisExp->val.primaryExpression->kind == identifierK) {
	 			SYMBOL *func_name = get_Symbol(primaryExp->val.argument.primaryExp->val.parenthesisExp->val.primaryExpression->val.identifier, t);

        			if (func_name->category != category_function && func_name->category != category_type) {
                			fprintf(stderr, "Error: (line %d) attempt to call primary expression that's not a function\n", primaryExp->lineno);
                			exit(1);
        			}
        			primaryExp->base_type = func_name;
			}
			else {
				fprintf(stderr, "Error: (line %d) not a valid function call\n", primaryExp->lineno);
          			exit(1);
			}
			
	 	}
		else {
			fprintf(stderr, "Error: (line %d) not a valid function call\n", primaryExp->lineno);
          		exit(1);
		}
  }
  else{
	  fprintf(stderr, "Error: (line %d) not a valid function call\n", primaryExp->lineno);
	  exit(1);
  }

  if(primaryExp->val.argument.expList){
    symbol_expression_list(primaryExp->val.argument.expList, t);
  }
}

void symbol_Primary_Expression_selector(PrimaryExpression *primaryExp, SYMBOL_TABLE *t) {
  symbol_Primary_Expression(primaryExp->val.selector.primaryExp, t);
}

void symbol_Primary_Expression_index(PrimaryExpression *primaryExp, SYMBOL_TABLE *t) {
  symbol_Primary_Expression(primaryExp->val.index.primaryExp, t);
  symbol_expression(primaryExp->val.index.indexExp, t);
}

void symbol_Unary_Op(UnaryOpExpression *unaryOpExp, SYMBOL_TABLE *t) {
  symbol_expression(unaryOpExp->unaryRHS, t);
}

void symbol_Binary_Op(BinaryOpExpression *binaryOpExp, SYMBOL_TABLE *t) {
  symbol_expression(binaryOpExp->lhs, t);
  symbol_expression(binaryOpExp->rhs, t);
}

void symbol_Builtin(BuiltInExpression *builtInExp, SYMBOL_TABLE *t) {
  switch(builtInExp->kind) {
    case appendK:
      symbol_expression(builtInExp->val.append.arg1, t);
      symbol_expression(builtInExp->val.append.arg2, t);
      break;
    case lenK:
      symbol_expression(builtInExp->val.len, t);
      builtInExp->return_type = get_Symbol(new_Symbol("int"), t);
      break;
    case capK:
      symbol_expression(builtInExp->val.cap, t);
      builtInExp->return_type = get_Symbol(new_Symbol("int"), t);
      break;
    default: fprintf(stderr, "Error: (line: %d) Not a valid builtInExp kind in symbol\n", builtInExp->lineno); exit(1);
  }
}
