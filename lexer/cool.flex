/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;


/*
 *  Add Your own definitions here
 */

char* error_msg;
int comment_deep;

int lineno = 0;

%}

/*
 * Define names for regular expressions here.
 */

DARROW          =>
ASSIGN		<-
LE              <=

 /*
  * case insensitive 
  */ 
a [aA]
b [bB]
c [cC]               
d [dD]
e [eE]
f [fF]
g [gG]
h [hH]
i [iI]
j [jJ]
k [kK]
l [lL]
m [mM]
n [nN]
o [oO]
p [pP]
q [qQ]
r [rR]
s [sS]
t [tT]
u [uU]
v [vV]
w [wW]
x [xX]
y [yY]
z [zZ]

%x COMMENT
%x STRING

%option yylineno

%%

^.*\n {
  lineno++;
  REJECT;
}
<COMMENT>^.*\n {
  lineno++;
  REJECT;
}
<STRING>^.*\n {
  lineno++;
  REJECT;
}

 /*
  *  Nested comments
  */

"(*" {
  BEGIN(COMMENT);
  comment_deep = 1;
}

<COMMENT>"(*" {
  comment_deep++;
}

"*)" {
  cool_yylval.error_msg = "Unmatched *)";
   curr_lineno=lineno; return (ERROR);
}

<COMMENT>"*)" {
  if (comment_deep == 1) {
    BEGIN(INITIAL);
  } else if (comment_deep < 1) {
    BEGIN(INITIAL);
    cool_yylval.error_msg = "Unmatched *)";
     curr_lineno=lineno; return (ERROR);
  } else {
    comment_deep--;
  }
}

<COMMENT>\\\* 

<COMMENT>\\\(

<COMMENT>\\\)

<COMMENT>. 

<COMMENT><<EOF>> {
  BEGIN(INITIAL);
  cool_yylval.error_msg = "EOF in comment";
   curr_lineno=lineno; return (ERROR);
}

"--".*\n?


 /*
  *  The multiple-character operators.
  */
{DARROW}		{  curr_lineno=lineno; return (DARROW); }
{ASSIGN}                {  curr_lineno=lineno; return (ASSIGN); }
{LE}                    {  curr_lineno=lineno; return (LE); }
"+" {  curr_lineno=lineno; return '+'; }
"/" {  curr_lineno=lineno; return '/'; }
"-" {  curr_lineno=lineno; return '-'; }
"*" {  curr_lineno=lineno; return '*'; }
"=" {  curr_lineno=lineno; return '='; }
"<" {  curr_lineno=lineno; return '<'; }
"." {  curr_lineno=lineno; return '.'; }
"~" {  curr_lineno=lineno; return '~'; }
"," {  curr_lineno=lineno; return ','; }
";" {  curr_lineno=lineno; return ';'; }
":" {  curr_lineno=lineno; return ':'; }
"(" {  curr_lineno=lineno; return '('; }
")" {  curr_lineno=lineno; return ')'; }
"@" {  curr_lineno=lineno; return '@'; }
"{" {  curr_lineno=lineno; return '{'; }
"}" {  curr_lineno=lineno; return '}'; }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

{c}{l}{a}{s}{s} {
   curr_lineno=lineno; return (CLASS);
}

{e}{l}{s}{e} {
   curr_lineno=lineno; return (ELSE);
}

f{a}{l}{s}{e} {
  cool_yylval.boolean = 0;
   curr_lineno=lineno; return (BOOL_CONST);
}

{f}{i} {
   curr_lineno=lineno; return (FI);
}

{i}{f} {
   curr_lineno=lineno; return (IF);
}

{i}{n} {
   curr_lineno=lineno; return (IN);
}

{i}{n}{h}{e}{r}{i}{t}{s} {
   curr_lineno=lineno; return (INHERITS);
}

{i}{s}{v}{o}{i}{d} {
   curr_lineno=lineno; return (ISVOID);
}

{l}{e}{t} {
   curr_lineno=lineno; return (LET);
}

{l}{o}{o}{p} {
   curr_lineno=lineno; return (LOOP);
}

{p}{o}{o}{l} {
   curr_lineno=lineno; return (POOL);
}

{t}{h}{e}{n} {
   curr_lineno=lineno; return (THEN);
}

{w}{h}{i}{l}{e} {
   curr_lineno=lineno; return (WHILE);
}

{c}{a}{s}{e} {
   curr_lineno=lineno; return (CASE);
}

{e}{s}{a}{c} {
   curr_lineno=lineno; return (ESAC);
}

{n}{e}{w} {
   curr_lineno=lineno; return (NEW);
}

{o}{f} {
   curr_lineno=lineno; return (OF);
}

{n}{o}{t} {
   curr_lineno=lineno; return (NOT);
}

t{r}{u}{e} {
  cool_yylval.boolean = 1;
   curr_lineno=lineno; return (BOOL_CONST);
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

\" {
  BEGIN STRING;
  string_buf_ptr = string_buf;
  error_msg = NULL;
}

<STRING>\" {
  BEGIN 0;
  if (error_msg) {
    cool_yylval.error_msg = error_msg;
     curr_lineno=lineno; return (ERROR);
  }
  *string_buf_ptr++ = 0;
  cool_yylval.symbol = stringtable.add_string(string_buf);
   curr_lineno=lineno; return STR_CONST;
}

<STRING>\n {
  BEGIN 0;
  if (!error_msg) {
    error_msg = "Unterminated string constant";
  }
  cool_yylval.error_msg = error_msg;
   curr_lineno=lineno; return (ERROR);
}

<STRING><<EOF>> {
  BEGIN 0;
  if (!error_msg) {
    error_msg = "EOF in string constant";
  }
  cool_yylval.error_msg = error_msg;
   curr_lineno=lineno; return (ERROR);
}

<STRING>\\\n {
  if (string_buf_ptr - string_buf >= MAX_STR_CONST - 1) {
    error_msg = "String constant too long";
  } else {
    *string_buf_ptr++ = '\n';
  }
}

<STRING>\\[^btnf] {
  if (!error_msg) {
  
  if (string_buf_ptr - string_buf >= MAX_STR_CONST - 1) {
    error_msg = "String constant too long";
  } else {
    if (yytext[1] == 0) { 
      error_msg = "String contains null character";
    } else {
    *string_buf_ptr++ = yytext[1];
    }
  }
  }
}

<STRING>\\[btnf] {
  if (!error_msg) {

  if (string_buf_ptr - string_buf >= MAX_STR_CONST - 1) {
    error_msg = "String constant too long";
  } else {
    switch(yytext[1]) {
      case 'b': *string_buf_ptr++ = '\b'; break;
      case 't': *string_buf_ptr++ = '\t'; break;
      case 'n': *string_buf_ptr++ = '\n'; break;
      case 'f': *string_buf_ptr++ = '\f'; break;
    }
  }
  }
}

<STRING>\0 {
  if (!error_msg) {
  error_msg = "String contains null character";
  }
}

<STRING>. {
  if (!error_msg) {
  if (string_buf_ptr - string_buf >= MAX_STR_CONST - 1) {
    error_msg = "String constant too long";
  } else {
    *string_buf_ptr++ = yytext[0];
  }
  
  }
}


 /*
  *  Integers, Identifiers
  */

[0-9]+ {
  cool_yylval.symbol = inttable.add_string(yytext);
   curr_lineno=lineno; return (INT_CONST);
}

[A-Z][a-zA-Z0-9_]* {
  cool_yylval.symbol = idtable.add_string(yytext);
   curr_lineno=lineno; return (TYPEID);
}

[a-z][a-zA-Z0-9_]* {
  cool_yylval.symbol = idtable.add_string(yytext);
   curr_lineno=lineno; return (OBJECTID);
}

 /*
  * White Space
  */

[ \n\f\r\t\v] 


. {
  cool_yylval.error_msg = strdup(yytext);
   curr_lineno=lineno; return (ERROR);
}

%%


