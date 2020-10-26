
    int curr_line = 1;
    int curr_pos = 1;

DIGIT   [0-9]

%%

    /* RESERVED WORDS */
"function"              {printf("FUNCTION\n"); curr_pos += yyleng;}
"beginparams"           {printf("BEGIN_PARAMS\n"); curr_pos += yyleng;}
"endparams"             {printf("END_PARAMS\n"); curr_pos += yyleng;}
"beginlocals"           {printf("BEGIN_LOCALS\n"); curr_pos += yyleng;}
"endlocals"             {printf("END_LOCALS\n"); curr_pos += yyleng;}
"beginbody"             {printf("BEGIN_BODY\n"); curr_pos += yyleng;}
"endbody"               {printf("END_BODY\n"); curr_pos += yyleng;}
"integer"               {printf("INTEGER\n"); curr_pos += yyleng;}
"array"                 {printf("ARRAY\n"); curr_pos += yyleng;}
"of"                    {printf("OF\n"); curr_pos += yyleng;}
"if"                    {printf("IF\n"); curr_pos += yyleng;}
"then"                  {printf("THEN\n"); curr_pos += yyleng;}
"endif"                 {printf("ENDIF\n"); curr_pos += yyleng;}
"else"                  {printf("ELSE\n"); curr_pos += yyleng;}
"while"                 {printf("WHILE\n"); curr_pos += yyleng;}
"do"                    {printf("DO\n"); curr_pos += yyleng;}
"for"                   {printf("FOR\n"); curr_pos += yyleng;}
"beginloop"             {printf("BEGIN_LOOP\n"); curr_pos += yyleng;}
"endloop"               {printf("END_LOOP\n"); curr_pos += yyleng;}
"continue"              {printf("CONTINUE\n"); curr_pos += yyleng;}
"read"                  {printf("READ\n"); curr_pos += yyleng;}
"write"                 {printf("WRITE\n"); curr_pos += yyleng;}
"and"                   {printf("AND\n"); curr_pos += yyleng;}
"or"                    {printf("OR\n"); curr_pos += yyleng;}
"not"                   {printf("NOT\n"); curr_pos += yyleng;}
"true"                  {printf("TRUE\n"); curr_pos += yyleng;}
"false"                 {printf("FALSE\n"); curr_pos += yyleng;}
"return"                {printf("RETURN\n"); curr_pos += yyleng;}

    /* ARITHMETIC OPERATORS */
"-"                     {printf("SUB\n"); curr_pos += yyleng;}
"+"                     {printf("ADD\n"); curr_pos += yyleng;}
"*"                     {printf("MULT\n"); curr_pos += yyleng;}
"/"                     {printf("DIV\n"); curr_pos += yyleng;}
"%"                     {printf("MOD\n"); curr_pos += yyleng;}

    /* COMPARISON OPERATORS */
"=="                    {printf("EQ\n"); curr_pos += yyleng;}
"<>"                    {printf("NEQ\n"); curr_pos += yyleng;}
"<"                     {printf("LT\n"); curr_pos += yyleng;}
">"                     {printf("GT\n"); curr_pos += yyleng;}
"<="                    {printf("LTE\n"); curr_pos += yyleng;}
">="                    {printf("GTE\n"); curr_pos += yyleng;}

    /* IDENTIFIERS AND NUMBERS */
[a-zA-Z_]([a-zA-Z0-9_]*[a-zA-Z0-9])?  {printf("IDENT %s\n", yytext); curr_pos += yyleng;}
{DIGIT}+                {printf("NUMBER %s\n", yytext); curr_pos += yyleng;}

    /* OTHER SPECIAL SYMBOLS */
";"                     {printf("SEMICOLON\n"); curr_pos += yyleng;}
":"                     {printf("COLON\n"); curr_pos += yyleng;}
","                     {printf("COMMA\n"); curr_pos += yyleng;}
"("                     {printf("L_PAREN\n"); curr_pos += yyleng;}
")"                     {printf("R_PAREN\n"); curr_pos += yyleng;}
"["                     {printf("L_SQUARE_BRACKET\n"); curr_pos += yyleng;}
"]"                     {printf("R_SQUARE_BRACKET\n"); curr_pos += yyleng;}
":="                    {printf("ASSIGN\n"); curr_pos += yyleng;}

##.*                    {/* ignore comment */ curr_line++; curr_pos = 1;}
[ \t]                   {/* ignore whitespace */ curr_pos += yyleng;}
\n                      {curr_line++; curr_pos = 1;}

{DIGIT}+[0-9a-zA-Z_]*   {printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", curr_line, curr_pos, yytext); exit(0);}

.                       {printf("Error at line %d, column %d: unrecognized symbol \"%s\"\n", curr_line, curr_pos, yytext); exit(0);}

%%