//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#define yylineno curr_lineno;
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

#define Program_EXTRAS                          \
virtual void semant() = 0;			\
virtual void dump_with_types(ostream&, int) = 0; 



#define program_EXTRAS                          \
void semant();     				\
void dump_with_types(ostream&, int);            

#define Class__EXTRAS                   \
virtual Symbol get_filename() = 0;      \
virtual void dump_with_types(ostream&,int) = 0;  \
virtual Symbol get_name() = 0;                    \
virtual Symbol get_parent() = 0;               \
virtual Features get_features() = 0;         


#define class__EXTRAS                                 \
Symbol get_filename() { return filename; }             \
void dump_with_types(ostream&,int);                   \
Symbol get_name() { return name; }                    \
Symbol get_parent() { return parent; }                \
Features get_features() { return features; }         



#define Feature_EXTRAS                                        \
virtual void dump_with_types(ostream&,int) = 0;               \
virtual int get_type() = 0;                                    \
virtual Symbol get_name() = 0;                                 \
static const int ATTR = 0;                                     \
static const int METHOD = 1;


#define Feature_SHARED_EXTRAS                                       \
void dump_with_types(ostream&,int);    

#define attr_EXTRAS \
int get_type() { return Feature_class::ATTR; } \
Symbol get_name() { return name; } \
Symbol get_type_decl() { return type_decl; } \
Expression get_init() { return init; }


#define method_EXTRAS \
int get_type() { return Feature_class::METHOD; } \
Symbol get_name() { return name; } \
Formals get_formals() { return formals; } \
Symbol get_return_type() { return return_type; } \
Expression get_expr() { return expr; }




#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0; \
virtual Symbol get_name() = 0; \
virtual Symbol get_type_decl() = 0;


#define formal_EXTRAS                           \
void dump_with_types(ostream&,int); \
Symbol get_name() { return name; } \
Symbol get_type_decl() { return type_decl; }



#define Case_EXTRAS                             \
virtual void dump_with_types(ostream& ,int) = 0;


#define branch_EXTRAS                                   \
void dump_with_types(ostream& ,int); \
Symbol get_name() { return name; } \
Symbol get_type_decl() { return type_decl; } \
Expression get_expr() { return expr; }



#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; } \
virtual int get_category() = 0; \
static const int INT_CONST = 0; \
static const int BOOL_CONST = 1; \
static const int STRING_CONST = 2; \
static const int OBJECT = 3; \
static const int ASSIGN = 4; \
static const int NEW_ = 5; \
static const int DISPATCH = 6; \
static const int STATIC_DISPATCH = 7; \
static const int COND = 8; \
static const int BLOCK = 9; \
static const int LET = 10; \
static const int TYPCASE = 11; \
static const int LOOP = 12; \
static const int ISVOID = 13; \
static const int COMP = 14; \
static const int LT = 15; \
static const int LEQ = 16; \
static const int NEG = 17; \
static const int PLUS = 18; \
static const int SUB = 19; \
static const int MUL = 20; \
static const int DIVIDE = 21; \
static const int EQ = 22; \
static const int NO_EXPR = 23;

#define int_const_EXTRAS \
int get_category() { return INT_CONST; }

#define bool_const_EXTRAS \
int get_category() { return BOOL_CONST; }

#define string_const_EXTRAS \
int get_category() { return STRING_CONST; }

#define object_EXTRAS \
int get_category() { return OBJECT; } \
Symbol get_name() { return name; }

#define assign_EXTRAS \
int get_category() { return ASSIGN; } \
Symbol get_name() { return name; } \
Expression get_expr() { return expr; }

#define new__EXTRAS \
int get_category() { return NEW_; } \
Symbol get_type_name() { return type_name; }

#define dispatch_EXTRAS \
int get_category() { return DISPATCH; } \
Expression get_expr()  { return expr; } \
Symbol get_name() { return name; } \
Expressions get_actual() { return actual; }

#define static_dispatch_EXTRAS \
int get_category() { return STATIC_DISPATCH; } \
Expression get_expr() { return expr; } \
Symbol get_type_name() { return type_name; } \
Symbol get_name() { return name; } \
Expressions get_actual() { return actual; }

#define cond_EXTRAS \
int get_category() { return COND; } \
Expression get_pred() { return pred; } \
Expression get_then_exp() { return then_exp; } \
Expression get_else_exp() { return else_exp; }

#define block_EXTRAS \
int get_category() { return BLOCK; } \
Expressions get_body() { return body; }

#define let_EXTRAS \
int get_category() { return LET; } \
Symbol get_identifier() { return identifier; } \
Symbol get_type_decl() { return type_decl; } \
Expression get_init() { return init; } \
Expression get_body() { return body; }

#define typcase_EXTRAS \
int get_category() { return TYPCASE; } \
Expression get_expr() { return expr; } \
Cases get_cases() { return cases; }

#define loop_EXTRAS \
int get_category() { return LOOP; } \
Expression get_pred() { return pred; } \
Expression get_body() { return body; }

#define isvoid_EXTRAS \
int get_category() { return ISVOID; } \
Expression get_e1() { return e1; }

#define comp_EXTRAS \
int get_category() { return COMP; } \
Expression get_e1() { return e1; }

#define lt_EXTRAS \
int get_category() { return LT; } \
Expression get_e1() { return e1; } \
Expression get_e2() { return e2; } 

#define leq_EXTRAS \
int get_category() { return LEQ; } \
Expression get_e1() { return e1; } \
Expression get_e2() { return e2; }

#define neg_EXTRAS \
int get_category() { return NEG; } \
Expression get_e1() { return e1; } 

#define plus_EXTRAS \
int get_category() { return PLUS; } \
Expression get_e1() { return e1; } \
Expression get_e2() { return e2; }


#define sub_EXTRAS \
int get_category() { return SUB; } \
Expression get_e1() { return e1; } \
Expression get_e2() { return e2; }

#define mul_EXTRAS \
int get_category() { return MUL; } \
Expression get_e1() { return e1; } \
Expression get_e2() { return e2; }

#define divide_EXTRAS \
int get_category() { return DIVIDE; } \
Expression get_e1() { return e1; } \
Expression get_e2() { return e2; }

#define eq_EXTRAS \
int get_category() { return EQ; } \
Expression get_e1() { return e1; } \
Expression get_e2() { return e2; }

#define no_expr_EXTRAS \
int get_category() { return NO_EXPR; }


#define Expression_SHARED_EXTRAS           \
void dump_with_types(ostream&,int); 

#endif
