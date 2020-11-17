%{
#include "stdio.h"
#include "string.h"
#include "parser.tab.h"

extern int curr_line;
extern int curr_pos;
extern const char* yytext;
void yyerror(const char *msg);
int yylex();
%}

%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY INTEGER ARRAY OF IF THEN ENDIF ELSE WHILE DO FOR BEGIN_LOOP END_LOOP CONTINUE READ WRITE AND OR NOT TRUE FALSE RETURN IDENT NUMBER SEMICOLON COLON COMMA

%right ASSIGN
%left  OR
%left  AND
%right NOT
%left  LT LTE GT GTE EQ NEQ
%left  ADD SUB
%left  MULT DIV MOD
%left  L_SQUARE_BRACKET R_SQUARE_BRACKET
%left  L_PAREN R_PAREN

%start program

%%

program: /* epsilon */ {printf("program -> epsilon\n");}
       | program function {printf("program -> program function\n");}   
       ;

function: FUNCTION ident SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY {printf("function -> FUNCTION ident SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY\n");}
        ;

declarations: /* epsilon */ {printf("declarations -> epsilon\n");}
            | declaration SEMICOLON declarations {printf("declarations -> declaration SEMICOLON declarations\n");}
            ;

statements: /* epsilon */ {printf("statements -> epsilon\n");}
          | statement SEMICOLON statements {printf("statements -> statement SEMICOLON statements\n");}
          ;

declaration: identifiers COLON INTEGER {printf("declaration -> identifiers COLON INTEGER\n");}
           | identifiers COLON ARRAY L_SQUARE_BRACKET number R_SQUARE_BRACKET OF INTEGER {printf("declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET number R_SQUARE_BRACKET OF INTEGER\n");}
           | identifiers COLON ARRAY L_SQUARE_BRACKET number R_SQUARE_BRACKET L_SQUARE_BRACKET number R_SQUARE_BRACKET OF INTEGER {printf("declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET number R_SQUARE_BRACKET L_SQUARE_BRACKET number R_SQUARE_BRACKET OF INTEGER\n");}
           | error {printf("Syntax error at line %d: invalid declaration\n", curr_line);}
           ;
        
identifiers: ident {printf("identifiers -> ident\n");}
           | identifiers COMMA ident {printf("identifiers -> identifiers COMMA ident\n");}
           | identifiers error ident {printf("Syntax error at line %d: missing \",\" between identifiers", curr_line);}
           ;

statement: var ASSIGN expression {printf("statement -> var ASSIGN expression\n");}
         | IF bool_exp THEN statements ENDIF {printf("statement -> IF bool_exp THEN statements ENDIF\n");}
         | IF bool_exp THEN statements ELSE statements ENDIF {printf("statement -> IF bool_exp THEN statements ENDIF\n");}
         | WHILE bool_exp BEGIN_LOOP statements END_LOOP {printf("statement -> WHILE bool_exp BEGIN_LOOP statements END_LOOP\n");}
         | DO BEGIN_LOOP statements END_LOOP WHILE bool_exp {printf("statement -> DO BEGIN_LOOP statements END_LOOP WHILE bool_exp\n");}
         | FOR var ASSIGN number SEMICOLON bool_exp SEMICOLON var ASSIGN expression BEGIN_LOOP statements END_LOOP {printf("statement -> FOR var ASSIGN number SEMICOLON bool_exp SEMICOLON var ASSIGN expression BEGIN_LOOP statements END_LOOP\n");}
         | READ vars {printf("statement -> READ vars\n");}
         | WRITE vars {printf("statement -> WRITE vars\n");}
         | CONTINUE {printf("statement -> CONTINUE\n");}
         | RETURN expression {printf("statement -> RETURN expression\n");}
         | var error expression {printf("Syntax error at line %d: \":=\" expected\n", curr_line);}
         ;

vars: var {printf("vars -> var\n");}
    | vars COMMA var {printf("vars -> vars COMMA var\n");}
    | vars error var {printf("Syntax error at line %d: missing \",\" between vars", curr_line);}
    ;

bool_exp: relation_and_expr {printf("bool_exp -> relation_and_expr\n");}
        | bool_exp OR relation_and_expr {printf("bool_exp -> relation_and_expr OR relation_and_expr\n");}
        ;
    
relation_and_expr: relation_expr {printf("relation_and_expr -> relation_expr\n");}
                 | relation_and_expr AND relation_expr  {printf("relation_and_expr -> relation_and_expr AND relation _expr\n");}
                 ;

relation_expr: expression comp expression {printf("relation_exp -> expression comp expression\n");}
             | NOT expression comp expression {printf("relation_exp -> NOT expression comp expression\n");}
             | TRUE {printf("relation_exp -> TRUE\n");}
             | NOT TRUE {printf("relation_exp -> NOT TRUE\n");}
             | FALSE {printf("relation_exp -> FALSE\n");}
             | NOT FALSE {printf("relation_exp -> NOT FALSE\n");}
             | L_PAREN bool_exp R_PAREN {printf("L_PAREN bool_exp R_PAREN\n");}
             | NOT L_PAREN bool_exp R_PAREN {printf("NOT L_PAREN bool_exp R_PAREN\n");}
             ;

 comp: EQ {printf("comp -> EQ\n");}
     | NEQ {printf("comp -> NEQ\n");}
     | LT {printf("comp -> LT\n");}
     | GT {printf("comp -> GT\n");}
     | LTE {printf("comp -> LTE\n");}
     | GTE {printf("comp -> GTE\n");}
     ;

expression: multiplicative_expr {printf("expression -> multiplicative_expr\n");}
          | expression ADD multiplicative_expr {printf("expression -> expression ADD multiplicative_expr\n");}
          | expression SUB multiplicative_expr {printf("expression -> expression SUB multiplicative_expr\n");}
          ;
multiplicative_expr: term {printf("multiplicative_expr -> term\n");}
                   | multiplicative_expr MULT term {printf("multiplicative_expr -> multiplicative_expr MULT term\n");}
                   | multiplicative_expr DIV term {printf("multiplicative_expr -> multiplicative_expr DIV term\n");}
                   | multiplicative_expr MOD term {printf("multiplicative_expr -> multiplicative_expr MOD term\n");}
                   ;

term: var {printf("term -> var\n");}
    | SUB var {printf("term -> SUB var\n");}
    | number {printf("term -> number\n");}
    | SUB number {printf("term -> SUB number\n");}
    | L_PAREN expression R_PAREN {printf("term -> L_PAREN expression R_PAREN\n");}
    | SUB L_PAREN expression R_PAREN {printf("term -> SUB L_PAREN expression R_PAREN\n");}
    | ident L_PAREN expressions R_PAREN {printf("term -> ident L_PAREN expressions R_PAREN\n");}
    ;

expressions: /* epsilon */ {printf("expressions -> epsilon\n");}
           | expression {printf("expressions -> expression\n");}
           | expressions COMMA expression {printf("expressions -> expressions COMMA expression\n");}
           | expressions error expression {printf("Syntax error at line %d: missing \",\" between expressions", curr_line);}
           ;

var: ident {printf("var -> ident\n");}
   | ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET {printf("var -> ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");}
   | ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET L_SQUARE_BRACKET expression R_SQUARE_BRACKET {printf("ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");}
   ;

ident: IDENT {printf("ident -> IDENT %s\n", yytext);}
     ;

number: NUMBER {printf("number -> NUMBER %s\n", yytext);}  

%%

int main(int argc, char** argv)
{
    yyparse();
    return 0;
}

void yyerror(const char* msg) 
{
    printf("Syntax error in line %d\n", curr_line);
}