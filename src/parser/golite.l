/* Code Section */
%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <stdbool.h>
    #include "y.tab.h"      /* Include token types from golite.y */

    // YY_USER_ACTION will execute before every match and set yyloc
    #define YY_USER_ACTION yylloc.first_line = yylloc.last_line = yylineno;

    extern bool modeToken;
    extern bool modeParse;
    int lastToken = -1;
    // https://github.com/comp520/Examples/blob/4626ad0cf9c947fc65c4f94e8b8f12f3a054bea4/flex%2Bbison/optional-semicolon/tiny.l
    #define RETURN(token) lastToken = token; return token;

    int needsSemiColon()
    {
        return  lastToken == tIDENTIFIER ||
                lastToken == tDECIMALVAL ||
                lastToken == tOCTALVAL ||
                lastToken == tHEXVAL ||
                lastToken == tFLOATVAL ||
                lastToken == tRUNEVAL ||
                lastToken == tRUNEVAL_ESCAPE ||
                lastToken == tSTRINGVAL ||
                lastToken == tBREAK ||
                lastToken == tCONTINUE ||
                lastToken == tFALLTHROUGH ||
                lastToken == tRETURN ||
                lastToken == tINC ||
                lastToken == tDEC ||
                lastToken == ')' ||
                lastToken == ']' ||
                lastToken == '}';
    }
%}

/* Helper Definition Section */

%option yylineno

/* exclude   ('"', \x22) and  ('\', \x5c) */
/* http://www.defindit.com/ascii.html */
STRING \"(\\a|\\b|\\f|\\n|\\r|\\t|\\v|\\\"|\\\\|\\\'|[\x00-\x09]|[\x0b-\x21]|[\x23-\x5b]|[\x5d-\x7f])*\"

 /* Token Parsing Section */
%% 

"//".*                         /* Ignore line for a comment */
"/*"([^*]|\*+[^*/])*\*+"/"     /* Ignore multi-line comment */
[ \t\r]                        /* Ignore space and tab */
\n                             { if(needsSemiColon()) { if(modeToken) printf(";\n");  RETURN(';'); } }
<<EOF>>                        { if(needsSemiColon()) { if(modeToken) printf(";\n");  RETURN(';'); } return 0; }

"+" |
"&" |
"(" |
")" |
"-" |
"|" |
"<" |
"[" |
"]" |
"*" |
"^" |
">" |
"{" |
"}" |
"/" |
"=" |
"," |
";" |
"%" |
"!" |
"." |
":"             { if (modeToken) printf("%s\n", yytext);  RETURN(yytext[0]); }
"&^"            { if (modeToken) printf("tBITCLEAR\n");   RETURN(tBITCLEAR); }
">>"            { if (modeToken) printf("tRIGHTSHIFT\n"); RETURN(tRIGHTSHIFT); }
"<<"            { if (modeToken) printf("tLEFTSHIFT\n");  RETURN(tLEFTSHIFT); }
"||"            { if (modeToken) printf("tOR\n");         RETURN(tOR); }
"&&"            { if (modeToken) printf("tAND\n");        RETURN(tAND); }

"*="            { if (modeToken) printf("tPRODUCT_ASSIGNMENT\n");    RETURN(tPRODUCT_ASSIGNMENT); }
"/="            { if (modeToken) printf("tQUOTIENT_ASSIGNMENT\n");   RETURN(tQUOTIENT_ASSIGNMENT); }
"%="            { if (modeToken) printf("tREMAINDER_ASSIGNMENT\n");  RETURN(tREMAINDER_ASSIGNMENT); }
"<<="           { if (modeToken) printf("tLEFTSHIFT_ASSIGNMENT\n");  RETURN(tLEFTSHIFT_ASSIGNMENT); }
">>="           { if (modeToken) printf("tRIGHTSHIFT_ASSIGNMENT\n"); RETURN(tRIGHTSHIFT_ASSIGNMENT); }
"&="            { if (modeToken) printf("tBITWISEAND_ASSIGNMENT\n"); RETURN(tBITWISEAND_ASSIGNMENT); }
"&^="           { if (modeToken) printf("tBITCLEAR_ASSIGNMENT\n");   RETURN(tBITCLEAR_ASSIGNMENT); }

"+="            { if (modeToken) printf("tSUM_ASSIGNMENT\n");        RETURN(tSUM_ASSIGNMENT); }
"-="            { if (modeToken) printf("tDIFFERENCE_ASSIGNMENT\n"); RETURN(tDIFFERENCE_ASSIGNMENT); }
"|="            { if (modeToken) printf("tBITWISEOR_ASSIGNMENT\n");  RETURN(tBITWISEOR_ASSIGNMENT); }
"^="            { if (modeToken) printf("tBITWISEXOR_ASSIGNMENT\n"); RETURN(tBITWISEXOR_ASSIGNMENT); }

":="            { if (modeToken) printf("tSHORTVARDECLARATION\n");   RETURN(tSHORTVARDECLARATION); }

"..."           { if (modeToken) printf("tVARIADICPARAM\n");         RETURN(tVARIADICPARAM); }

"++"            { if (modeToken) printf("tINC\n");           RETURN(tINC); }
"--"            { if (modeToken) printf("tDEC\n");           RETURN(tDEC); }
"=="            { if (modeToken) printf("tEQUAL\n");         RETURN(tEQUAL); }
"!="            { if (modeToken) printf("tNOTEQUAL\n");      RETURN(tNOTEQUAL); }
"<="            { if (modeToken) printf("tLTEQUALS\n");      RETURN(tLTEQUALS); }
">="            { if (modeToken) printf("tGTEQUALS\n");      RETURN(tGTEQUALS); }

"<-"            { if (modeToken) printf("tRECEIVE\n");       RETURN(tRECEIVE); }
 
"break"         { if (modeToken) printf("tBREAK\n");         RETURN(tBREAK); }
"case"          { if (modeToken) printf("tCASE\n");          RETURN(tCASE); }
"chan"          { if (modeToken) printf("tCHAN\n");          RETURN(tCHAN); }
"const"         { if (modeToken) printf("tCONST\n");         RETURN(tCONST); }
"continue"      { if (modeToken) printf("tCONTINUE\n");      RETURN(tCONTINUE); }
"default"       { if (modeToken) printf("tDEFAULT\n");       RETURN(tDEFAULT); }
"defer"         { if (modeToken) printf("tDEFER\n");         RETURN(tDEFER); }
"else"          { if (modeToken) printf("tELSE\n");          RETURN(tELSE); }
"fallthrough"   { if (modeToken) printf("tFALLTHROUGH\n");   RETURN(tFALLTHROUGH); }
"for"           { if (modeToken) printf("tFOR\n");           RETURN(tFOR); }
"func"          { if (modeToken) printf("tFUNC\n");          RETURN(tFUNC); }
"go"            { if (modeToken) printf("tGO\n");            RETURN(tGO); }
"goto"          { if (modeToken) printf("tGOTO\n");          RETURN(tGOTO); }
"if"            { if (modeToken) printf("tIF\n");            RETURN(tIF); }
"import"        { if (modeToken) printf("tIMPORT\n");        RETURN(tIMPORT); }
"interface"     { if (modeToken) printf("tINTERFACE\n");     RETURN(tINTERFACE); }
"map"           { if (modeToken) printf("tMAP\n");           RETURN(tMAP); }
"package"       { if (modeToken) printf("tPACKAGE\n");       RETURN(tPACKAGE);}
"range"         { if (modeToken) printf("tRANGE\n");         RETURN(tRANGE); }
"return"        { if (modeToken) printf("tRETURN\n");        RETURN(tRETURN); }
"select"        { if (modeToken) printf("tSELECT\n");        RETURN(tSELECT); }
"struct"        { if (modeToken) printf("tSTRUCT\n");        RETURN(tSTRUCT); }
"switch"        { if (modeToken) printf("tSWITCH\n");        RETURN(tSWITCH); }
"type"          { if (modeToken) printf("tTYPE\n");          RETURN(tTYPE); }
"var"           { if (modeToken) printf("tVAR\n");           RETURN(tVAR); }

"print"         { if (modeToken) printf("tPRINT\n");         RETURN(tPRINT); }
"println"       { if (modeToken) printf("tPRINTLN\n");       RETURN(tPRINTLN); }
"append"        { if (modeToken) printf("tAPPEND\n");        RETURN(tAPPEND); }
"len"           { if (modeToken) printf("tLEN\n");           RETURN(tLEN); }
"cap"           { if (modeToken) printf("tCAP\n");           RETURN(tCAP); }

[a-zA-Z_][a-zA-Z0-9_]*      { if (modeToken) printf("tIDENTIFIER(%s)\n",      yytext);   yylval.str = strdup(yytext);   RETURN(tIDENTIFIER); }

[1-9][0-9]*                 { if (modeToken) printf("tINTVAL(%s)\n", yytext);   yylval.intval_str = strdup(yytext);     RETURN(tDECIMALVAL); } /* ORDER MATTERS HERE! */

"0"([0-7]*)                { if (modeToken) printf("tINTVAL(%s)\n", yytext);    yylval.intval_str = strdup(yytext);     RETURN(tOCTALVAL); }

([8-9]*)                    { fprintf(stderr, "Error: (line %d) malformed octal constant: ‘%s’\n", yylineno, yytext); exit(1); }

"0"("x"|"X")([0-9a-fA-F])+  { if (modeToken) printf("tINTVAL(%s)\n",   yytext);    yylval.intval_str = strdup(yytext);  RETURN(tHEXVAL); }

([0-9]*)"."[0-9]+           { if (modeToken) printf("tFLOATVAL(%s)\n", yytext);    yylval.floatval = atof(yytext); RETURN(tFLOATVAL); }
([0-9]+)"."[0-9]*           { if (modeToken) printf("tFLOATVAL(%s)\n", yytext);    yylval.floatval = atof(yytext); RETURN(tFLOATVAL); }

'[\x00-\x26\x28-\x7f]'  { if (modeToken) { printf("tRUNEVAL(%s)\n", yytext); }  yylval.runeval = yytext[1]; RETURN(tRUNEVAL); }  /* Support all ASCII characters for rune */

'\\a'       { if (modeToken) { printf("tRUNEVAL(%s)\n", yytext); }  yylval.runeval_escape = 'a'; RETURN(tRUNEVAL_ESCAPE); }
'\\b'       { if (modeToken) { printf("tRUNEVAL(%s)\n", yytext); }  yylval.runeval_escape = 'b'; RETURN(tRUNEVAL_ESCAPE); }
'\\f'       { if (modeToken) { printf("tRUNEVAL(%s)\n", yytext); }  yylval.runeval_escape = 'f'; RETURN(tRUNEVAL_ESCAPE); }
'\\n'       { if (modeToken) { printf("tRUNEVAL(%s)\n", yytext); }  yylval.runeval_escape = 'n'; RETURN(tRUNEVAL_ESCAPE); }
'\\r'       { if (modeToken) { printf("tRUNEVAL(%s)\n", yytext); }  yylval.runeval_escape = 'r'; RETURN(tRUNEVAL_ESCAPE); }
'\\t'       { if (modeToken) { printf("tRUNEVAL(%s)\n", yytext); }  yylval.runeval_escape = 't'; RETURN(tRUNEVAL_ESCAPE); }
'\\v'       { if (modeToken) { printf("tRUNEVAL(%s)\n", yytext); }  yylval.runeval_escape = 'v'; RETURN(tRUNEVAL_ESCAPE); }
'\\\\'      { if (modeToken) { printf("tRUNEVAL(%s)\n", yytext); }  yylval.runeval_escape = '\\'; RETURN(tRUNEVAL_ESCAPE); }
'\\''       { if (modeToken) { printf("tRUNEVAL(%s)\n", yytext); }  yylval.runeval_escape = 27;   RETURN(tRUNEVAL_ESCAPE); }

`[\x00-\x5f\x61-\x7f]*`             { if (modeToken) printf("tSTRINGVAL(%s)\n", yytext);    yylval.str = strdup(yytext); RETURN(tSTRINGVAL); }    /* Note, this is a back-tick! Exclude ASCII x60, which is the back-tick */
{STRING}                            { if (modeToken) printf("tSTRINGVAL(%s)\n", yytext);    yylval.str = strdup(yytext); RETURN(tSTRINGVAL); }    /* Note, exclude ASCII x22, which is the double-quote */

[0-9]+[a-zA-Z_][a-zA-Z0-9_]*    { fprintf(stderr, "Error: (line %d) unexpected identifier ‘%s’\n", yylineno, yytext); exit(1); }    /* After scanning identifier and numbers, throw error as this identifier is invalid. */


.                               { fprintf(stderr, "Error: (line %d) unexpected character ‘%s’\n", yylineno, yytext); exit(1); }

%%
