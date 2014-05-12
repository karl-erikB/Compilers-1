#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include <map>
#include <set>
#include <vector>
#include <utility>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

struct ClassTreeNode {
	bool inited;
	Symbol parent;
	Class_ class_;
	std::vector<Symbol> children;

	ClassTreeNode() 
	{
		inited = false;
	}
};

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

  bool HasCycle(Symbol, std::map<Symbol, int> &);
  int CheckTypeClass(Symbol);
  void TraversalClass(Symbol);
  void CreateMethodTable(Symbol, const std::vector<method_class *> &);
  bool TypeCheck(Symbol, Symbol);
  Symbol Lub(Symbol, Symbol);
  Symbol RegularType(Symbol, bool &);

  Symbol ExprType(Expression);

  Symbol ObjectExpr(object_class *);
  Symbol AssignExpr(assign_class *);
  Symbol New_Expr(new__class *);
  Symbol DispatchExpr(dispatch_class *);
  Symbol StaticDispatchExpr(static_dispatch_class *);
  Symbol CommonDispatchExpr(Symbol, Symbol, Symbol, Expressions);
  Symbol CondExpr(cond_class *);
  Symbol BlockExpr(block_class *);
  Symbol LetExpr(let_class *);
  Symbol TypcaseExpr(typcase_class *);
  Symbol LoopExpr(loop_class *);
  Symbol IsvoidExpr(isvoid_class *);
  Symbol UnaryExpr(Expression, Symbol);
  Symbol BinaryExpr(Expression, Expression, Symbol);
  Symbol EqExpr(Expression, Expression, std::set<Symbol>, Symbol);

  // own code
  typedef std::map<Symbol, ClassTreeNode> ClassTree;

  typedef std::pair<Symbol, Symbol> MethodTableKey;
  typedef std::pair<Formals, Symbol> MethodTableValue;
  typedef SymbolTable<MethodTableKey, MethodTableValue> MethodTable;

  typedef SymbolTable<Symbol, Symbol> ObjectTable;

  Classes classes_;
  ClassTree class_tree_;
  MethodTable *method_table;
  ObjectTable *object_table;
  Symbol current_class;


public:
  ClassTable(Classes);
  ~ClassTable();
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  // own code
  bool CheckAndBuildTypeTree();
  void ScopeCheckingAndTypeChecking();

};


#endif

