%{
#include "parser.tab.h"
    int curr_line = 1;
    int curr_pos = 0;
%}
    
DIGIT   [0-9]

%%

    /* RESERVED WORDS */
"function"              {curr_pos += yyleng; return FUNCTION;}
"beginparams"           {curr_pos += yyleng; return BEGIN_PARAMS;}
"endparams"             {curr_pos += yyleng; return END_PARAMS;}
"beginlocals"           {curr_pos += yyleng; return BEGIN_LOCALS;}
"endlocals"             {curr_pos += yyleng; return END_LOCALS;}
"beginbody"             {curr_pos += yyleng; return BEGIN_BODY;}
"endbody"               {curr_pos += yyleng; return END_BODY;}
"integer"               {curr_pos += yyleng; return INTEGER;}
"array"                 {curr_pos += yyleng; return ARRAY;}
"of"                    {curr_pos += yyleng; return OF;}
"if"                    {curr_pos += yyleng; return IF;}
"then"                  {curr_pos += yyleng; return THEN;}
"endif"                 {curr_pos += yyleng; return ENDIF;}
"else"                  {curr_pos += yyleng; return ELSE;}
"while"                 {curr_pos += yyleng; return WHILE;}
"do"                    {curr_pos += yyleng; return DO;}
"for"                   {curr_pos += yyleng; return FOR;}
"beginloop"             {curr_pos += yyleng; return BEGIN_LOOP;}
"endloop"               {curr_pos += yyleng; return END_LOOP;}
"continue"              {curr_pos += yyleng; return CONTINUE;}
"read"                  {curr_pos += yyleng; return READ;}
"write"                 {curr_pos += yyleng; return WRITE;}
"and"                   {curr_pos += yyleng; return AND;}
"or"                    {curr_pos += yyleng; return OR;}
"not"                   {curr_pos += yyleng; return NOT;}
"true"                  {curr_pos += yyleng; return TRUE;}
"false"                 {curr_pos += yyleng; return FALSE;}
"return"                {curr_pos += yyleng; return RETURN;}

    /* ARITHMETIC OPERATORS */
"-"                     {curr_pos += yyleng; return SUB;}
"+"                     {curr_pos += yyleng; return ADD;}
"*"                     {curr_pos += yyleng; return MULT;}
"/"                     {curr_pos += yyleng; return DIV;}
"%"                     {curr_pos += yyleng; return MOD;}

    /* COMPARISON OPERATORS */
"=="                    {curr_pos += yyleng; return EQ;}
"<>"                    {curr_pos += yyleng; return NEQ;}
"<"                     {curr_pos += yyleng; return LT;}
">"                     {curr_pos += yyleng; return GT;}
"<="                    {curr_pos += yyleng; return LTE;}
">="                    {curr_pos += yyleng; return GTE;}

    /* IDENTIFIERS AND NUMBERS */
[a-zA-Z_]([a-zA-Z0-9_]*[a-zA-Z0-9])?  {curr_pos += yyleng; return IDENT;}
{DIGIT}+                {curr_pos += yyleng; return NUMBER;}

    /* OTHER SPECIAL SYMBOLS */
";"                     {curr_pos += yyleng; return SEMICOLON;}
":"                     {curr_pos += yyleng; return COLON;}
","                     {curr_pos += yyleng; return COMMA;}
"("                     {curr_pos += yyleng; return L_PAREN;}
")"                     {curr_pos += yyleng; return R_PAREN;}
"["                     {curr_pos += yyleng; return L_SQUARE_BRACKET;}
"]"                     {curr_pos += yyleng; return R_SQUARE_BRACKET;}
":="                    {curr_pos += yyleng; return ASSIGN;}

##.*                    {/* ignore comment */ curr_line++; curr_pos = 1;}
[ \t]                   {/* ignore whitespace */ curr_pos += yyleng;}
\n                      {curr_line++; curr_pos = 1;}

{DIGIT}+[0-9a-zA-Z_]*   {printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", curr_line, curr_pos, yytext);}

.                       {printf("Error at line %d, column %d: unrecognized symbol \"%s\"\n", curr_line, curr_pos, yytext);}

%%