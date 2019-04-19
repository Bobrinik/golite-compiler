%{
    #include <stdio.h>
    #include <stdlib.h>
    #include "utils/mode.h"
    #include "ast/tree.h"
    #include "ast/symbol.h"

    int yylex(void);

    extern int lineno;
    extern int yylineno; // comes from flex file

    PROGRAM *root;

    void yyerror(const char *s){
        if(!modeToken && !modeScan){
            fprintf(stderr, "Error: (line %d) %s\n", yylineno, s);
            isParseError = true;
        } else {
            while(yylex());
        }
    }

%}
%locations
%error-verbose

// Copy  the code ahead of the default versions and also into the header file and before YYLTYPE
%code requires
{
	#include "ast/tree.h"
    #include "ast/symbol.h"
}

%union {
    char                  *intval_str;
    float                  floatval;
    char                   runeval;
    char                   runeval_escape;
    char                  *str;
    PROGRAM               *program;
 
    TOP_DECLARATION       *top_declaration;
    TOP_DECLARATION_LIST  *top_declaration_list;
    VAR_DECLARATION       *var_declaration;
    TYPE_DECLARATION      *type_declaration;
    TYPE_DECLARATION_LIST *type_declaration_list;
 
    DECLARATION           *declaration;
    IDENTIFIER_LIST       *identifier_list;
    DECLARATION_LIST      *declaration_list;
    BRACKET               *bracket;
    BRACKET_LIST          *bracket_list;

    FUNCTION_DECLARATION  *function_declaration;
    SIGNATURE             *signature;
    PARAMETER_UNIT        *parameter_unit;
    PARAMETER_LIST        *parameters;
    RESULT                *result;
    SYMBOL                *symbol;

    EXP_LIST              *exp_list;
    EXP                   *exp;
    PrimaryExpression     *primary_exp;
    UnaryOpExpression     *unary_exp;
    BinaryOpExpression    *binary_exp;
    BuiltInExpression     *builtin_exp;             

    STMTS 		          *stmts;

    SWITCH_ON             *switch_on;
    SWITCH_CASE           *switch_case;
    CLAUSE                *clause;
    
    SIMPLE_STMT		      *simple_stmt;

    int                   operation_kind;    /* enum can be represented as integer */
}

/* ============ TOKENS ============ */

%token tBREAK tCASE tCHAN tCONST tCONTINUE tDEFAULT tDEFER tELSE tFALLTHROUGH tFOR tFUNC tGO tGOTO tIF tIMPORT tINTERFACE tMAP tPACKAGE tRANGE tRETURN tSELECT tSTRUCT tSWITCH tTYPE tVAR
%token tPRINT tPRINTLN tAPPEND tLEN tCAP

%token <intval_str>     tDECIMALVAL tOCTALVAL tHEXVAL
%token <floatval>       tFLOATVAL
%token <runeval>        tRUNEVAL
%token <runeval_escape> tRUNEVAL_ESCAPE
%token <str>            tSTRINGVAL
%token <str>            tIDENTIFIER

%token tBITCLEAR
%token tLEFTSHIFT tRIGHTSHIFT
%token tAND tOR

%token tSHORTVARDECLARATION
%token tSUM_ASSIGNMENT tDIFFERENCE_ASSIGNMENT tBITWISEOR_ASSIGNMENT tBITWISEXOR_ASSIGNMENT
%token tPRODUCT_ASSIGNMENT tQUOTIENT_ASSIGNMENT tREMAINDER_ASSIGNMENT  tLEFTSHIFT_ASSIGNMENT tRIGHTSHIFT_ASSIGNMENT tBITWISEAND_ASSIGNMENT tBITCLEAR_ASSIGNMENT

%token tVARIADICPARAM

%token tINC tDEC

%token tLTEQUALS tGTEQUALS tEQUAL tNOTEQUAL

%token tRECEIVE


/* ============ TYPES FOR AST ============ */

%type  <program>               program

/* DECLARATIONS */
%type  <top_declaration_list>  top_level_declarations_list
%type  <top_declaration>       top_level_declaration  

%type  <type_declaration>      type_declaration
%type  <type_declaration_list> type_declarations type_declaration_list

%type  <var_declaration>       var_declarations

%type  <bracket>               bracket
%type  <bracket_list>          bracket_list

%type  <identifier_list>       identifier_list

%type  <declaration>           declaration
%type  <declaration_list>      declaration_list


/* EXPRESSIONS */
%type  <function_declaration>  function_declaration
%type  <signature>             signature

%type  <parameter_unit>        parameter_unit
%type  <parameters>            parameters
%type  <result>                result

%type  <exp_list>              expression_list
%type  <exp>                   expression
%type  <primary_exp>           primary_expression literal
%type  <unary_exp>             unary_op
%type  <binary_exp>            binary_op
%type  <builtin_exp>           builtin


/* STATEMENT */
%type <stmts>			statement
%type <stmts>           declaration_stmt return_stmt break_stmt continue_stmt 
%type <stmts> 			block_stmt if_stmt switch_stmt for_stmt print_stmt println_stmt
%type <stmts>			statement_list expression_switch_stmt 

%type <simple_stmt>		simple_stmt inc_dec_stmt assignment short_variable_declaration expression_stmt

%type <operation_kind>  assign_op mul_op_eq add_op_eq

%type <switch_on>       switch_on
%type <switch_case>     expression_case_clause expression_case_clause_list
%type <clause>          for_clause

/* ============ PRECEDENCE ============ */

%left tOR
%left tAND
%left tEQUAL tNOTEQUAL '<' tLTEQUALS '>' tGTEQUALS
%left '+' '-' '|' '^'
%left '*' '/' '%' tLEFTSHIFT tRIGHTSHIFT '&' tBITCLEAR
%right UPLUS UMINUS '!' UCARET


/* ============ ROOT NODE ============ */

%start program


%% 

program : tPACKAGE tIDENTIFIER ';' top_level_declarations_list  { root = new_Program(new_Package($2, @1.first_line), $4);   }
        | tPACKAGE tIDENTIFIER ';'                              { root = new_Program(new_Package($2, @1.first_line), NULL); }
        ;

top_level_declarations_list: top_level_declaration ';'                             {$$ =  new_Top_Declaration_List( $1, NULL); }
                           | top_level_declaration ';' top_level_declarations_list {$$ =  new_Top_Declaration_List( $1, $3);   }
                           ;


top_level_declaration : var_declarations               { $$ = new_Top_Declaration_for_var($1);  }
                      | type_declarations              { $$ = new_Top_Declaration_for_type($1); } 
                      | function_declaration           { $$ = new_Top_Declaration_for_func($1); }
                      ;


/*============*/
var_declarations : tVAR declaration                 { $$ = new_Var_Declaration($2, @1.first_line); }
                 | tVAR '(' declaration_list ')'    { $$ = new_Var_Declaration_list($3, @1.first_line); }
                 ;

declaration_list : 				                    { $$ = NULL; }
                 | declaration ';' declaration_list { $$ = new_Declaration_list($1, $3); }
                 ;

/*there is no keywords for types in golite such as: float, string, or int */
declaration : identifier_list tIDENTIFIER                                       { $$ = new_Declaration_id_list_type($1, new_Symbol($2), @1.first_line); }
	        | identifier_list '(' tIDENTIFIER ')'                           { $$ = new_Declaration_id_list_type($1, new_Symbol($3), @1.first_line); }
	        | identifier_list bracket_list tIDENTIFIER                      { $$ = new_Declaration_id_list_array($1, $2, new_Symbol($3) ,@1.first_line); }
                | identifier_list '=' expression_list                           { $$ = new_Declaration_id_list_exp_list($1, $3, @1.first_line); }
                | identifier_list tIDENTIFIER '=' expression_list               { $$ = new_Declaration_id_list_type_exp_list($1, new_Symbol($2), $4, @1.first_line); }
	        | identifier_list '(' tIDENTIFIER ')' '=' expression_list       { $$ = new_Declaration_id_list_type_exp_list($1, new_Symbol($3), $6, @1.first_line); }
                | identifier_list bracket_list tIDENTIFIER '=' expression_list  { $$ = new_Declaration_id_list_array_exp_list($1, $2, new_Symbol($3), $5, @1.first_line); }
		| identifier_list tSTRUCT '{' declaration_list '}' 		{ $$ = new_Declaration_id_list_struct($1, $4, @1.first_line); }	
	        | identifier_list bracket_list tSTRUCT '{' declaration_list '}'	{ $$ = new_Declaration_id_list_array_struct($1, $2, $5, @1.first_line); }
	        ;


identifier_list :  tIDENTIFIER                      { $$ = new_Identifier_list($1, NULL, @1.first_line); }
                |  tIDENTIFIER ',' identifier_list  { $$ = new_Identifier_list($1 , $3, @1.first_line);  }
                ;

/*============*/
type_declarations : tTYPE type_declaration               { $$ = new_Type_Declaration_list($2, NULL); }
                  | tTYPE '(' type_declaration_list ')'  { $$ = $3; }
                  ;

type_declaration : tIDENTIFIER tIDENTIFIER                                      { $$ = new_Type_Declaration(new_Symbol($1), new_Symbol($2), @1.first_line); }
		 | tIDENTIFIER '(' tIDENTIFIER ')'                              { $$ = new_Type_Declaration(new_Symbol($1), new_Symbol($3), @1.first_line); }
		 | tIDENTIFIER tSTRUCT '{' declaration_list '}'                 { $$ = new_Type_Struct_Declaration(new_Symbol($1), $4, @1.first_line); }
		 | tIDENTIFIER bracket_list tIDENTIFIER		                { $$ = new_Type_Bracket_Declaration(new_Symbol($1), $2, new_Symbol($3), @1.first_line); }
		 | tIDENTIFIER bracket_list tSTRUCT '{' declaration_list '}'    { $$ = new_Type_Bracket_Struct_Declaration(new_Symbol($1), $2, $5, @1.first_line); }
		 ;

type_declaration_list : 					                        { $$ = NULL; }
                      | type_declaration ';' type_declaration_list  { $$ = new_Type_Declaration_list($1, $3);   }
                      ;

/*============*/

function_declaration : tFUNC tIDENTIFIER signature block_stmt   { $$ = new_Function_Declaration(new_Symbol($2), $3, $4, @1.first_line);}
                     ;

signature : '(' parameters ')' result 	{ $$ = new_Signature($2, $4); }
          | '(' ')' result 		        { $$ = new_Signature(NULL, $3); }
	      ;

result : 				{$$ = NULL; }
       | tIDENTIFIER                   { $$ = new_simple_result(new_Symbol($1)); }
       | bracket_list tIDENTIFIER      { $$ = new_array_result($1, new_Symbol($2)); }
       | tSTRUCT '{' declaration_list '}' { $$ = new_struct_result($3); }
       | bracket_list tSTRUCT '{' declaration_list '}' { $$ = new_array_struct_result($1, $4); }
       ;

parameters : parameter_unit			          { $$ = new_Parameter_List($1, NULL); }
	       | parameter_unit ',' parameters            { $$ = new_Parameter_List($1, $3); }
	       ;

parameter_unit : identifier_list tIDENTIFIER               { $$ = new_Parameter_Unit( $1, new_Symbol($2)); }
               | identifier_list '(' tIDENTIFIER ')'       { $$ = new_Parameter_Unit( $1, new_Symbol($3)); }
               | identifier_list bracket_list tIDENTIFIER  { $$ = new_Parameter_Unit_Array($1, $2, new_Symbol($3)); }
               ;

/*TYPES ============*/

/*https://golang.org/ref/spec#_literals*/

bracket : '[' tDECIMALVAL ']'   { $$ = new_Bracket($2); }
        | '[' ']'               { $$ = new_Empty_Bracket(); }
        ;

bracket_list : bracket               { $$ = new_Bracket_list($1, NULL); }
             | bracket bracket_list  { $$ = new_Bracket_list($1, $2);   }
             ;


/* ============ EXPRESSION ============ */

expression_list : expression                        { $$ = new_Expression_List($1, NULL, @1.first_line);}
                | expression ',' expression_list    { $$ = new_Expression_List($1, $3, @1.first_line); }
                ;

expression : primary_expression     { $$ = new_Expression_primary($1, primaryExpressionK, @1.first_line); }
           | unary_op               { $$ = new_Expression_unaryOp($1, unaryOpK, @1.first_line); }
           | binary_op              { $$ = new_Expression_binaryOp($1, binaryOpK, @1.first_line); }
           | builtin                { $$ = new_Expression_builtinOp($1, builtinK, @1.first_line); }
           ;

primary_expression : tIDENTIFIER                                { $$ = new_Primary_Expression_identifier(new_Symbol($1), @1.first_line); }
                   | literal                                    { $$ = $1; }
                   | '(' expression ')'                         { $$ = new_Primary_Expression_parenthesis($2, @1.first_line); }
                   | primary_expression '(' expression_list ')' { $$ = new_Primary_Expression_arguments($1, $3, @1.first_line); } 
                   | primary_expression '(' ')'                 { $$ = new_Primary_Expression_arguments($1, NULL, @1.first_line); } 
                   | primary_expression '.' tIDENTIFIER         { $$ = new_Primary_Expression_selector($1, $3, @1.first_line); }
                   | primary_expression '[' expression ']'      { $$ = new_Primary_Expression_index($1, $3, @1.first_line); }
                   ;

literal : tDECIMALVAL                           { $$ = new_Literal_intval_decimal($1, @1.first_line); }
        | tOCTALVAL                             { $$ = new_Literal_intval_octal($1, @1.first_line); }
        | tHEXVAL                               { $$ = new_Literal_intval_hex($1, @1.first_line); }
        | tFLOATVAL                             { $$ = new_Literal_floatval($1, @1.first_line); }
        | tRUNEVAL                              { $$ = new_Literal_runeval($1, @1.first_line); }
        | tRUNEVAL_ESCAPE                       { $$ = new_Literal_runeval_escape($1, @1.first_line);}
        | tSTRINGVAL                            { $$ = new_Literal_stringval($1, @1.first_line); }
        ;

unary_op : '+' expression %prec UPLUS           { $$ = new_Unary_Op($2, unaryPlusK, @1.first_line); }
         | '-' expression %prec UMINUS          { $$ = new_Unary_Op($2, unaryMinusK, @1.first_line); }
         | '!' expression                       { $$ = new_Unary_Op($2, notK, @1.first_line); }
         | '^' expression %prec UCARET          { $$ = new_Unary_Op($2, bitwiseComplementK, @1.first_line); }
         ;

binary_op : expression tOR expression           { $$ = new_Binary_Op($1, $3, orK, @1.first_line); }
         | expression tAND expression           { $$ = new_Binary_Op($1, $3, andK, @1.first_line); }
         | expression tEQUAL expression         { $$ = new_Binary_Op($1, $3, equalK, @1.first_line); }
         | expression tNOTEQUAL expression      { $$ = new_Binary_Op($1, $3, notEqualK, @1.first_line); }
         | expression tLTEQUALS expression      { $$ = new_Binary_Op($1, $3, ltEqualsK, @1.first_line); }
         | expression tGTEQUALS expression      { $$ = new_Binary_Op($1, $3, gtEqualsK, @1.first_line); }
         | expression '<' expression            { $$ = new_Binary_Op($1, $3, lesserK, @1.first_line); }
         | expression '>' expression            { $$ = new_Binary_Op($1, $3, greaterK, @1.first_line); }
         | expression '+' expression            { $$ = new_Binary_Op($1, $3, plusK, @1.first_line); }
         | expression '-' expression            { $$ = new_Binary_Op($1, $3, subK, @1.first_line); }
         | expression '|' expression            { $$ = new_Binary_Op($1, $3, bitOrK, @1.first_line); }
         | expression '^' expression            { $$ = new_Binary_Op($1, $3, bitwiseXorK, @1.first_line); }
         | expression '*' expression            { $$ = new_Binary_Op($1, $3, multK, @1.first_line); }
         | expression '/' expression            { $$ = new_Binary_Op($1, $3, divK, @1.first_line); }
         | expression '%' expression            { $$ = new_Binary_Op($1, $3, remK, @1.first_line); }
         | expression tRIGHTSHIFT expression    { $$ = new_Binary_Op($1, $3, rightShiftK, @1.first_line); }
         | expression tLEFTSHIFT expression     { $$ = new_Binary_Op($1, $3, leftShiftK, @1.first_line); }
         | expression '&' expression            { $$ = new_Binary_Op($1, $3, bitAnd, @1.first_line); }
         | expression tBITCLEAR expression      { $$ = new_Binary_Op($1, $3, bitClearK, @1.first_line); }
         ;

builtin : tAPPEND '(' expression ',' expression ')'     { $$ = new_Builtin_append($3, $5, @1.first_line); }
        | tLEN '(' expression ')'                       { $$ = new_Builtin_len($3, @1.first_line); }
        | tCAP '(' expression ')'                       { $$ = new_Builtin_cap($3, @1.first_line); }
        ;


/* ============ STATEMENT ============ */

statement : declaration_stmt		        { $$ = $1; }
          | simple_stmt				{ $$ = new_Simple_Statement($1, @1.first_line); }
          | return_stmt 			{ $$ = $1; }
          | break_stmt				{ $$ = $1; }
          | continue_stmt			{ $$ = $1; }
          | block_stmt				{ $$ = new_Block_Stmt($1, false); }
          | if_stmt				{ $$ = $1; }
          | switch_stmt 			{ $$ = $1; }
          | for_stmt				{ $$ = $1; }
          | print_stmt				{ $$ = $1; }
          | println_stmt			{ $$ = $1; }
          ;

declaration_stmt : var_declarations		{ $$ = new_Var_Decl_Statement($1, @1.first_line); }
                 | type_declarations	        { $$ = new_Type_Decl_Statement($1, @1.first_line); }
                 ;

simple_stmt : /* Empty statement */         { $$ = NULL; }
            | expression_stmt		    { $$ = $1;}
            | inc_dec_stmt		    { $$ = $1;}
            | assignment		    { $$ = $1;}
            | short_variable_declaration    { $$ = $1;}
            ;

expression_stmt : expression	/* weed to make sure its a function call */ { $$ = new_Expr_Simple_Statement($1, @1.first_line); }	
                ;

inc_dec_stmt : expression tINC 			{ $$ = new_Inc_Simple_Statement($1, @1.first_line); } 
             | expression tDEC			{ $$ = new_Dec_Simple_Statement($1, @1.first_line); } 
             ;

/* https://golang.org/ref/spec#Operands
 The blank identifier may appear as an operand only on the left-hand side of an assignment. */ 
assignment : expression_list '=' expression_list	{ $$ = new_Assign_ExpList_Simple_Statement($1, $3, @1.first_line); }
           | expression assign_op expression		{ $$ = new_Assign_Exp_Simple_Statement($1, $2, $3, @1.first_line); }
           ;

assign_op : add_op_eq				{ $$ = $1; }
          | mul_op_eq				{ $$ = $1; }
          ;

add_op_eq : tSUM_ASSIGNMENT             /* '+=' */  { $$ = plusAssignK; }
          | tDIFFERENCE_ASSIGNMENT      /* '-=' */  { $$ = minusAssignK; }
          | tBITWISEOR_ASSIGNMENT       /* '|=' */  { $$ = orAssignK; }
          | tBITWISEXOR_ASSIGNMENT      /* '^=' */  { $$ = xorAssignK; }
          ;

mul_op_eq : tPRODUCT_ASSIGNMENT         /* '*=' */  { $$ = mulAssignK; }
          | tQUOTIENT_ASSIGNMENT        /* '/=' */  { $$ = divAssignK; }
          | tREMAINDER_ASSIGNMENT       /* '%=' */  { $$ = remAssignK; }
          | tLEFTSHIFT_ASSIGNMENT       /* '<<=' */ { $$ = lshiftAssignK; }
          | tRIGHTSHIFT_ASSIGNMENT      /* '>>=' */ { $$ = rshiftAssignK; }
          | tBITWISEAND_ASSIGNMENT      /* '&=' */  { $$ = andAssignK; }
          | tBITCLEAR_ASSIGNMENT        /* '&^=' */ { $$ = bitclearAssignK; }
          ;

// TODO: in tree check that all expressions on LHS are identities
short_variable_declaration : expression_list tSHORTVARDECLARATION expression_list 	{ $$ = new_Short_Var_Decl_Simple_Statement($1, $3, @1.first_line); }
                           ;

return_stmt : tRETURN				{ $$ = new_Return_Statement(NULL, @1.first_line); }
            | tRETURN expression                { $$ = new_Return_Statement($2, @1.first_line); }
            ;

break_stmt : tBREAK                             { $$ = new_Break_Statement(@1.first_line); }
           ;

continue_stmt : tCONTINUE			{ $$ = new_Continue_Statement(@1.first_line); }
              ;

block_stmt : '{' statement_list '}'		{  $$ = new_Block_Stmt($2, true); }
           | '{' '}' 				{  $$ = NULL; }
           ; 

statement_list : statement ';'			{ $$ = $1;}
               | statement ';' statement_list	{ $$ = $1; $$->nextStmt = $3; }
               ;

if_stmt : tIF expression block_stmt				                        { $$ = new_If_Statement(NULL, $2, $3, NULL, @1.first_line); }
        | tIF simple_stmt ';' expression block_stmt		                        { $$ = new_If_Statement($2, $4, $5, NULL, @1.first_line); }
	| tIF simple_stmt ';' expression block_stmt tELSE if_stmt                       { $$ = new_If_Statement($2, $4, $5, $7, @1.first_line); }
	| tIF simple_stmt ';' expression block_stmt tELSE block_stmt                    { $$ = new_If_Statement($2, $4, $5, $7, @1.first_line); }
        | tIF expression block_stmt tELSE if_stmt		                        { $$ = new_If_Statement(NULL, $2, $3, $5, @1.first_line); }
        | tIF expression block_stmt tELSE block_stmt 	                                { $$ = new_If_Statement(NULL, $2, $3, $5, @1.first_line); }
        ;

switch_stmt : expression_switch_stmt 		{$$ = $1; }
            ;               

expression_switch_stmt : tSWITCH switch_on '{' expression_case_clause_list '}'		{ $$ = new_Switch_Statement($2, $4, @1.first_line); }
                       ;

switch_on : /* empty */                 { $$ = new_Switch_On(NULL, NULL); }
          | simple_stmt ';' expression	{ $$ = new_Switch_On($1, $3); }
          | simple_stmt ';'		{ $$ = new_Switch_On($1, NULL); }
          | expression			{ $$ = new_Switch_On(NULL, $1); }
          ;

expression_case_clause : tCASE expression_list  ':' statement_list 	{ $$ = new_Switch(caseK, $2, $4); }
                       | tCASE expression_list  ':'			{ $$ = new_Switch(caseK, $2, NULL); }
                       | tDEFAULT ':' statement_list 			{ $$ = new_Switch(defaultK, NULL, $3); }
                       | tDEFAULT					{ $$ = new_Switch(defaultK, NULL, NULL); }
	               | tDEFAULT ':'					{ $$ = new_Switch(defaultK, NULL, NULL); }
	               ;

/* weed to check maximum 1 default in a switch statement */

expression_case_clause_list : /* empty */						{ $$ = NULL; }
                            | expression_case_clause_list expression_case_clause	{ $$ = $2; $$->next = $1; }
                            ;

for_stmt : tFOR block_stmt		{ $$ = new_For_Statement(new_Infinite_For($2), @1.first_line); }
         | tFOR expression block_stmt	{ $$ = new_For_Statement(new_While_For($2, $3), @1.first_line); }
         | tFOR for_clause block_stmt	{ $$ = new_For_Statement(new_Clause_For($2, $3), @1.first_line); }
         ;

for_clause : simple_stmt ';' ';' simple_stmt 		    { $$ = new_Clause($1, NULL, $4); }
           | simple_stmt ';' expression ';' simple_stmt	    { $$ = new_Clause($1, $3, $5); }
           ;

print_stmt : tPRINT '(' expression_list ')' 	    { $$ = new_Print_Statement($3, @1.first_line); }
           | tPRINT '(' ')'			    { $$ = new_Print_Statement(NULL, @1.first_line); }

println_stmt : tPRINTLN '(' expression_list ')'	    { $$ = new_Println_Statement($3, @1.first_line); }
             | tPRINTLN '(' ')'			    { $$ = new_Println_Statement(NULL, @1.first_line); }


%%
