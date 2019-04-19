#include "symbol.h"
#ifndef __TREE_H
#define __TREE_H
#include <stdbool.h>


// put all typedefs on top so it's easier to move stuff down bellow
typedef struct PARAMETER PARAMETER;
typedef struct TYPE TYPE;
typedef struct PACKAGE PACKAGE;

typedef struct PROGRAM PROGRAM;

// root of a parse tree
extern PROGRAM *root;

typedef struct SYMBOL SYMBOL;

// https://www.cs.mcgill.ca/~cs520/2019/project/Milestone1_Specifications.pdf#subsection.2.3q
typedef struct TOP_DECLARATION TOP_DECLARATION;
typedef struct TOP_DECLARATION_LIST TOP_DECLARATION_LIST;

typedef struct TYPE_DECLARATION TYPE_DECLARATION;
typedef struct TYPE_DECLARATION_LIST TYPE_DECLARATION_LIST;
typedef struct VAR_DECLARATION VAR_DECLARATION;
typedef struct FUNCTION_DECLARATION FUNCTION_DECLARATION;
typedef enum   TopDeclarationK TopDeclarationK;

typedef struct DECLARATION DECLARATION;
typedef struct DECLARATION_LIST DECLARATION_LIST;
typedef struct BRACKET BRACKET;
typedef struct BRACKET_LIST BRACKET_LIST;
typedef enum   DeclarationK DeclarationK;
typedef enum   TypeDeclarationK TypeDeclarationK;

typedef struct PARAMETER_UNIT PARAMETER_UNIT;
typedef struct PARAMETER_LIST PARAMETER_LIST;
typedef struct RESULT RESULT;
typedef struct SIGNATURE SIGNATURE;

typedef struct SIMPLE_STMT SIMPLE_STMT;
typedef struct SWITCH_STATEMENT SWITCH_STATEMENT;
typedef struct SWITCH_CASE SWITCH_CASE;
typedef struct SWITCH_CASE_LIST SWITCH_CASE_LIST;
typedef struct SWITCH_ON SWITCH_ON;
typedef struct CLAUSE CLAUSE;
typedef struct FOR_STATEMENT FOR_STATEMENT;
typedef struct STMTS STMTS;
typedef enum   StatementK StatementK;

typedef struct EXP_LIST EXP_LIST;
typedef struct EXP EXP;

typedef struct PrimaryExpression PrimaryExpression;
typedef struct BuiltInExpression BuiltInExpression;
typedef struct UnaryOpExpression UnaryOpExpression;
typedef struct BinaryOpExpression BinaryOpExpression;

typedef enum ExpressionKind ExpressionKind;
typedef enum PrimaryExpressionKind PrimaryExpressionKind;
typedef enum UnaryOpKind UnaryOpKind;
typedef enum BinaryOpKind BinaryOpKind;
typedef enum BuiltinKind BuiltinKind;

typedef struct IDENTIFIER_LIST IDENTIFIER_LIST;



struct PROGRAM {
  PACKAGE *package;
  TOP_DECLARATION_LIST *top_declaration_list;
};

PROGRAM * new_Program(PACKAGE *package, TOP_DECLARATION_LIST *top_declr_list);



struct PACKAGE {
  int lineno;
  char *name;
};

PACKAGE * new_Package(char *name, int lineno);

/* =========================== */

enum TopDeclarationK {
  varK,
  typeK,
  funcK
};

struct TOP_DECLARATION {
  int lineno;
  TopDeclarationK kind;
  union {
    VAR_DECLARATION *var_dclr;
    TYPE_DECLARATION_LIST *type_dclr_list;
    FUNCTION_DECLARATION *function_dclr;
  } val;
};


TOP_DECLARATION * new_Top_Declaration(TopDeclarationK kind, DECLARATION * declaration, DECLARATION_LIST *lst);
TOP_DECLARATION * new_Top_Declaration_for_var(VAR_DECLARATION *declaration);

TOP_DECLARATION * new_Top_Declaration_for_type(TYPE_DECLARATION_LIST *declaration);

TOP_DECLARATION * new_Top_Declaration_for_func(FUNCTION_DECLARATION *func);

struct TOP_DECLARATION_LIST {
  TOP_DECLARATION *top_declaration;
  TOP_DECLARATION_LIST *nxt;
};

TOP_DECLARATION_LIST * new_Top_Declaration_List( TOP_DECLARATION *decl, TOP_DECLARATION_LIST *nxt);

/* =========================== */

enum DeclarationK {
  Id_list_typek,
  Id_list_arrayk,
  Id_list_eqk,
  Id_list_type_eqk,
  Id_list_structK,
  Id_list_array_structK,
  Id_list_array_eqK
};

struct DECLARATION {
  int lineno;
  IDENTIFIER_LIST *lhs;
  SYMBOL *type;
  DeclarationK kind;
  union {
    BRACKET_LIST *indx_list;
    EXP_LIST *rhs_list;
    struct { BRACKET_LIST *indx_list; EXP_LIST *rhs_list; } array_eq;
    DECLARATION_LIST *struct_body;
    struct { BRACKET_LIST *indx_list; DECLARATION_LIST *struct_body;} struct_array;
  } val;
};

//  identifier_list tIDENTIFIER 
DECLARATION * new_Declaration_id_list_type(IDENTIFIER_LIST *ids, SYMBOL *type, int lineno);

DECLARATION * new_Declaration_id_list_array(IDENTIFIER_LIST *ids, BRACKET_LIST *brackets, SYMBOL *type ,int lineno);

// TODO: in tree make sure that there is same number on lhs and rhs
DECLARATION * new_Declaration_id_list_exp_list(IDENTIFIER_LIST *ids, EXP_LIST *exps, int lineno);

// identifier_list tIDENTIFIER '=' expression_list
// TODO: in tree make sure that there is same number on lhs and rhs
DECLARATION * new_Declaration_id_list_type_exp_list(IDENTIFIER_LIST *ids, SYMBOL *type,  EXP_LIST *exps, int lineno);

DECLARATION * new_Declaration_id_list_struct(IDENTIFIER_LIST *ids, DECLARATION_LIST *body, int lineno);

DECLARATION * new_Declaration_id_list_array_struct(IDENTIFIER_LIST *ids, BRACKET_LIST *brackets, DECLARATION_LIST *body, int lineno);

DECLARATION * new_Declaration_id_list_array_exp_list(IDENTIFIER_LIST *ids, BRACKET_LIST *brackets, SYMBOL *type, EXP_LIST *exps, int lineno);

struct DECLARATION_LIST {
  DECLARATION *declaration;
  DECLARATION_LIST *nxt;
};

DECLARATION_LIST * new_Declaration_list(DECLARATION *declaration, DECLARATION_LIST *list);


struct BRACKET{
  bool isEmpty;
  int idx;
};

BRACKET * new_Empty_Bracket();
BRACKET * new_Bracket(char *idx);

struct BRACKET_LIST {
  BRACKET *bracket;
  BRACKET_LIST *nxt;
};

BRACKET_LIST * new_Bracket_list(BRACKET *bracket, BRACKET_LIST *nxt);


struct TYPE_DECLARATION_LIST {
  TYPE_DECLARATION *type_declaration;
  TYPE_DECLARATION_LIST *nxt;
};


enum TypeDeclarationK {
	type_simpleK,
	type_structK,
	type_arrayK,
	type_struct_arrayK
};

TYPE_DECLARATION_LIST * new_Type_Declaration_list(TYPE_DECLARATION *bracket, TYPE_DECLARATION_LIST *nxt);

struct TYPE_DECLARATION {
  int lineno;
  TypeDeclarationK kind;
  union {
    struct {SYMBOL *name; SYMBOL *type; } simple;
    struct {SYMBOL *name; DECLARATION_LIST *body; } structure;
    struct {SYMBOL *name; BRACKET_LIST *brackets; SYMBOL *type; } array;
    struct {SYMBOL *name; BRACKET_LIST *brackets; DECLARATION_LIST *body;} struct_array;
  } val;
};


TYPE_DECLARATION * new_Type_Declaration(SYMBOL *name, SYMBOL *type, int lineno);
TYPE_DECLARATION * new_Type_Struct_Declaration(SYMBOL *name, DECLARATION_LIST *list, int lineno);
TYPE_DECLARATION * new_Type_Bracket_Declaration(SYMBOL *name, BRACKET_LIST *list, SYMBOL *type, int lineno);
TYPE_DECLARATION * new_Type_Bracket_Struct_Declaration(SYMBOL *name, BRACKET_LIST *list, DECLARATION_LIST *body, int lineno);

struct VAR_DECLARATION {
  int lineno;
  bool isList;
  union {
    DECLARATION *declaration;
    DECLARATION_LIST *declaration_list;
  } val;
};

VAR_DECLARATION * new_Var_Declaration_list(DECLARATION_LIST *list, int lineno);
VAR_DECLARATION * new_Var_Declaration(DECLARATION *decl, int lineno);

/* ================ Statements ==================== */

// types of statements
enum StatementK {
  varBodyK,
  typeBodyK,
  ifK,
  switchK,
  forK,
  printK,
  printlnK,
  continueK,
  breakK,
  returnK,
  blockK,
  assignListK,
  exprSimpleK,
  incK,
  decK,
  shortVarDeclK,

  equalAssignK,
  plusAssignK,
  minusAssignK,
  orAssignK,
  xorAssignK,
  mulAssignK,
  divAssignK,
  remAssignK,
  lshiftAssignK,
  rshiftAssignK,
  andAssignK,
  bitclearAssignK,
	simpleK
};

struct SIMPLE_STMT {
	int lineno;
	StatementK kind;
	union {
		EXP *exp;
		  struct { EXP *lhs; StatementK kind; EXP *rhs; } assignment;
      struct { EXP_LIST *lhs; EXP_LIST *rhs; } assignment_list;
      struct { EXP_LIST *identifierList; EXP_LIST *rhs; } assignment_short_var_decl;
	}val;
};

typedef enum {
  caseK,
  defaultK,
} SwitchCaseK;

struct SWITCH_CASE {
	SwitchCaseK kind; 
	EXP_LIST *caseExp; 
	STMTS *caseStmts;
	SWITCH_CASE *next;
};

struct SWITCH_ON {
	SIMPLE_STMT *simpleStmt;
	EXP *condition;
};

typedef enum {
	whileK,
	forClauseK,
	infiniteK,
} ForStmtK;

struct CLAUSE {
	SIMPLE_STMT *simple_stmt1;
	SIMPLE_STMT *simple_stmt2;
	EXP *condition;
};

struct FOR_STATEMENT {
	ForStmtK kind;
	union {
		EXP *while_condition;
		CLAUSE *clause;
	} val;
	STMTS *block;
};


struct STMTS {
  int lineno;
  StatementK kind;
  int isTerminating;

  union {
    VAR_DECLARATION *var_dclr;
    TYPE_DECLARATION_LIST *type_dclr;
    STMTS *block;
    struct { SIMPLE_STMT *simplestmt; EXP *condition; STMTS *block; STMTS *next; } if_stmt;
    struct { SWITCH_ON *switch_on; SWITCH_CASE *switch_case; } switch_stmt;
    FOR_STATEMENT *for_stmt;
    EXP_LIST *print;
    EXP *exp;
    SIMPLE_STMT *simple_stmt;
  } val;
  
  STMTS *nextStmt;
};

STMTS * new_Block_Stmt(STMTS *stmts, bool isTop);
STMTS * new_Var_Decl_Statement(VAR_DECLARATION *decl, int lineno);
STMTS * new_Type_Decl_Statement(TYPE_DECLARATION_LIST *decl, int lineno);
STMTS * new_If_Statement(SIMPLE_STMT *simplestmt, EXP *condition, STMTS *block, STMTS *next, int lineno);
STMTS * new_Switch_Statement(SWITCH_ON *switch_on, SWITCH_CASE *switch_case, int lineno);
STMTS * new_Print_Statement(EXP_LIST *printArgs, int lineno);
STMTS * new_For_Statement(FOR_STATEMENT *for_stmt, int lineno);
STMTS * new_Println_Statement(EXP_LIST *printArgs, int lineno);
STMTS * new_Continue_Statement(int lineno);
STMTS * new_Break_Statement(int lineno);
STMTS * new_Return_Statement(EXP *returnExp, int lineno);
STMTS * new_Simple_Statement(SIMPLE_STMT *simple_stmt, int lineno);

STMTS * new_Block(STMTS *stmts);
SIMPLE_STMT * new_Expr_Simple_Statement(EXP *exp, int lineno);
SIMPLE_STMT * new_Inc_Simple_Statement(EXP *incExp, int lineno);
SIMPLE_STMT * new_Dec_Simple_Statement(EXP *decExp, int lineno);
SIMPLE_STMT * new_Assign_ExpList_Simple_Statement(EXP_LIST *lhs, EXP_LIST *rhs, int lineno);
SIMPLE_STMT * new_Assign_Exp_Simple_Statement(EXP *lhs, StatementK assignOpKind, EXP *rhs, int lineno);
SIMPLE_STMT * new_Short_Var_Decl_Simple_Statement(EXP_LIST *identifierList, EXP_LIST *rhs, int lineno);
SWITCH_CASE * new_Switch(SwitchCaseK kind, EXP_LIST *caseExpr, STMTS *case_stmts);

SWITCH_ON *     new_Switch_On(SIMPLE_STMT *simple, EXP *condition);
FOR_STATEMENT * new_While_For(EXP *while_condition, STMTS *block);
FOR_STATEMENT * new_Clause_For(CLAUSE *clause, STMTS *block);
CLAUSE *        new_Clause(SIMPLE_STMT *simple1, EXP *condition, SIMPLE_STMT *simple2);
FOR_STATEMENT * new_Infinite_For(STMTS *block);

/* ================ Function Declaration ==================== */

struct FUNCTION_DECLARATION {
  int lineno;
  SYMBOL *name;
  SIGNATURE *signature;
  STMTS *body;
};

FUNCTION_DECLARATION * new_Function_Declaration(SYMBOL *name, SIGNATURE *signature, STMTS *body,int lineno);

struct SIGNATURE {
  PARAMETER_LIST *parameters;
  RESULT *result;
};

SIGNATURE * new_Signature(PARAMETER_LIST *parameters, RESULT *result);

struct PARAMETER_UNIT {
  bool isArray;
  IDENTIFIER_LIST *ids_list;
  BRACKET_LIST *brackets;
  SYMBOL *type;
};

PARAMETER_UNIT * new_Parameter_Unit(IDENTIFIER_LIST *ids_list, SYMBOL *type);
PARAMETER_UNIT * new_Parameter_Unit_Array(IDENTIFIER_LIST *ids_list, BRACKET_LIST *brackets, SYMBOL *type);

struct PARAMETER_LIST {
  PARAMETER_UNIT *param;
  PARAMETER_LIST *nxt;
};

PARAMETER_LIST * new_Parameter_List(PARAMETER_UNIT *unit, PARAMETER_LIST *nxt);

typedef enum {
	simpleResultK,
	arrayResultK,
	structResultK,
	arrayStructResultK
}ResultKind;

struct RESULT {
  ResultKind kind;
  SYMBOL *type;
  BRACKET_LIST *brackets;
  DECLARATION_LIST *decl_list;
};

RESULT *new_simple_result(SYMBOL *type);
RESULT *new_array_result(BRACKET_LIST *list, SYMBOL *type);
RESULT *new_array_struct_result(BRACKET_LIST *b_list, DECLARATION_LIST *decl_list);
RESULT *new_struct_result(DECLARATION_LIST *decl_list);
/* ================== Expressions ====================== */

enum ExpressionKind {
  primaryExpressionK,
  unaryOpK,
  binaryOpK,
  builtinK
};

enum PrimaryExpressionKind {
  identifierK,
  parenthesisExpK,
  primaryExpArgumentK,
  primaryExpIndexK,
  primaryExpSelectorK,
  blankExpK,

  intLiteralK,
	floatLiteralK,
	runeLiteralK,
	strLiteralK,
};

enum UnaryOpKind {
  unaryPlusK,		      // + (exp)
	unaryMinusK,		    // - (exp)
	bitwiseComplementK,	// ^ (exp)
	notK,			          // ! (exp)
};

enum BinaryOpKind {
	plusK,			        // exp + exp
	subK,			          // exp - exp
	multK,			        // exp * exp
	divK,			          // exp / exp
	remK,			          // exp % exp
	andK,			          // exp && exp
	orK,			          // exp || exp
	bitwiseXorK,		    // exp ^ exp 
	bitClearK,		      // exp &^ exp
	leftShiftK,		      // exp << exp
	rightShiftK,		    // exp >> exp
	equalK,			        // exp == exp
	notEqualK,		      // exp != exp
	ltEqualsK,		      // exp <= exp
	gtEqualsK,		      // exp >= exp
	lesserK,		        // exp < exp
	greaterK,		        // exp > exp
	bitOrK,			        // exp | exp
	bitAnd,			        // exp & exp
};

enum BuiltinKind {
  appendK,
	lenK,
	capK
	// functionCallK, // also used for type casts (not recognized yet)
};

// defining expression list (function call and type cast arguments)
struct EXP_LIST {
	int lineno;
	EXP *exp;
	EXP_LIST *nextExp;
};

/* Expression Node */
struct EXP {
  ExpressionKind kind;
  int lineno;
  TYPE *type;

  union {
    /* Expression */
    struct { EXP *exp; } expression; 

    PrimaryExpression *primaryExpression;
    UnaryOpExpression *unaryOpExpression;
    BinaryOpExpression *binaryOpExpression;
    BuiltInExpression *builtinExpression;

  } val;
};

struct PrimaryExpression {
  PrimaryExpressionKind kind;
  SYMBOL *base_type;
  int lineno;

  union {
    SYMBOL* identifier;
    EXP *parenthesisExp;
    struct { PrimaryExpression *primaryExp; EXP_LIST *expList; } argument; 
    struct { PrimaryExpression *primaryExp; char *identifier; TYPE *type;} selector;
    struct { PrimaryExpression *primaryExp; EXP *indexExp; } index; 
    
    /* literal  */
    int   intLiteral;
    float floatLiteral;
    struct { char val; bool isRuneLiteralEscape; } runeLiteral;
    char  *strLiteral;
  } val;
};

struct UnaryOpExpression {
  UnaryOpKind kind;
  int lineno;

  EXP *unaryRHS; 
};

struct BinaryOpExpression {
  BinaryOpKind kind;
  int lineno;

  EXP *lhs; 
  EXP *rhs; 
};

struct BuiltInExpression {
  BuiltinKind kind;
  int lineno;
  SYMBOL *return_type;

  union {
    EXP *len;
    EXP *cap;
    struct { EXP *arg1; EXP *arg2; } append;
  } val;
};

EXP* new_Expression_primary(PrimaryExpression *primaryExp, ExpressionKind kind, int lineno);
EXP* new_Expression_unaryOp(UnaryOpExpression *unaryOpExp, ExpressionKind kind, int lineno);
EXP* new_Expression_binaryOp(BinaryOpExpression *binaryOpExp, ExpressionKind kind, int lineno);
EXP* new_Expression_builtinOp(BuiltInExpression *builtinExp, ExpressionKind kind, int lineno);

PrimaryExpression* new_Primary_Expression_identifier(SYMBOL *identifier, int lineno);
PrimaryExpression* new_Primary_Expression_parenthesis(EXP *exp, int lineno);
PrimaryExpression* new_Primary_Expression_arguments(PrimaryExpression *primaryExp, EXP_LIST *expList, int lineno);
PrimaryExpression* new_Primary_Expression_index(PrimaryExpression *primaryExp, EXP *indexExp, int lineno);
PrimaryExpression* new_Primary_Expression_selector(PrimaryExpression *primaryExp, char *identifier, int lineno);

PrimaryExpression* new_Literal_intval_decimal(char *val, int lineno);
PrimaryExpression* new_Literal_intval_octal(char *val, int lineno);
PrimaryExpression* new_Literal_intval_hex(char *val, int lineno);
PrimaryExpression* new_Literal_floatval(float val, int lineno);
PrimaryExpression* new_Literal_runeval(char val, int lineno);
PrimaryExpression* new_Literal_runeval_escape(char val, int lineno);
PrimaryExpression* new_Literal_stringval(char *val, int lineno);

UnaryOpExpression* new_Unary_Op(EXP *unaryRHS, UnaryOpKind unaryOperatorKind, int lineno);
BinaryOpExpression* new_Binary_Op(EXP *lhs, EXP *rhs, BinaryOpKind binaryOperatorKind, int lineno);

BuiltInExpression* new_Builtin_append(EXP *arg1, EXP *arg2, int lineno);
BuiltInExpression* new_Builtin_len(EXP *arg, int lineno);
BuiltInExpression* new_Builtin_cap(EXP *arg, int lineno);

EXP_LIST* new_Expression_List(EXP *exp, EXP_LIST *nextExp, int lineno);


/* ==================================================== */


struct IDENTIFIER_LIST {
  SYMBOL *symbol;
  IDENTIFIER_LIST *nxt;
  int lineno;
};

IDENTIFIER_LIST * new_Identifier_list(char *current, IDENTIFIER_LIST *nxt, int lineno);

#endif
