

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string>
#include <map>
#include <set>
#include <utility>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr), classes_(classes) {

    /* Fill this in */
	install_basic_classes();
	method_table = new MethodTable();
	object_table = new ObjectTable();
}

ClassTable::~ClassTable() {
	delete method_table;
	delete object_table;
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

	// add basic class to classes
	Class_ basic_classes[] = {Object_class, IO_class, Int_class, Bool_class, Str_class};	
	int basic_classes_size = sizeof(basic_classes) / sizeof(Class_);
	for (int i = basic_classes_size - 1; i >= 0; --i) {
		classes_ = append_Classes(single_Classes(basic_classes[i]), classes_);
	}

}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

	if (classtable->CheckAndBuildTypeTree()) {
		classtable->ScopeCheckingAndTypeChecking();
	}


    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}

bool ClassTable::CheckAndBuildTypeTree()
{
	bool has_main = false;
	// check class redefined
	for (int i = classes_->first(); classes_->more(i); i = classes_->next(i)) {
		Class_ class_ = classes_->nth(i);

		if (class_->get_parent() == Int || class_->get_parent() == Bool || class_->get_parent() == Str) {
			semant_error(class_) << "class " << class_->get_name()->get_string() << " inherit from " << class_->get_parent()->get_string()
				<< " is not allowed" << endl;
			return false;
		}
		ClassTreeNode &node = class_tree_[class_->get_name()];
		if (node.inited) {
			semant_error(class_) << "class " << class_->get_name()->get_string() << " redefined" << endl;
			return false;
		} 

		if (class_->get_name() == Main) {
			has_main = true;
		}

		if (class_->get_name() == SELF_TYPE) {
			semant_error(class_) << "Redefinition of basic class SELF_TYPE." << endl;
			return false;
		}

		node.inited = true;
		node.parent = class_->get_parent();
		node.class_ = class_;

		if (class_->get_parent() != No_class) {
			class_tree_[class_->get_parent()].children.push_back(class_->get_name());
		}


	}

	if (!has_main) {
		semant_error() << "Class Main is not defined." << endl;
		return false;
	}


	// check inherited class defined

	for (int i = classes_->first(); classes_->more(i); i = classes_->next(i)) {
		Class_ class_ = classes_->nth(i);
		if (class_->get_parent() != No_class && !class_tree_[class_->get_parent()].inited) {
			semant_error(class_) << "the parent " << class_->get_parent()->get_string() << " of class " << class_->get_name()->get_string() << " is not defined" << endl;
			return false;
		}
	}


	// check cycle
	std::map<Symbol, int> visit_state;
	for (ClassTree::const_iterator it = class_tree_.begin(); it != class_tree_.end(); ++it) {
		if (0 == visit_state[it->first] && HasCycle(it->first, visit_state)) {
			semant_error(it->second.class_) << "cycle inheritance" << endl;
			return false;
		}
	}

	return true;
}

bool ClassTable::HasCycle(Symbol s, std::map<Symbol, int> &visit_state)
{
	visit_state[s] = -1;

	const std::vector<Symbol> &children = class_tree_[s].children;
	for (int i = 0; i < static_cast<int>(children.size()); i++) {
		Symbol t = children[i];
		if (-1 == visit_state[t]) {
			return true;
		} else if (0 == visit_state[t] && HasCycle(t, visit_state)) {
			return true;
		}
	}
	visit_state[s] = 1;

	return false;
}

int ClassTable::CheckTypeClass(Symbol type)
{
	if (class_tree_.find(type) != class_tree_.end()) {
		return 1;
	}
   	if (type == SELF_TYPE) {
		return 2;
	}
	if ('_' == type->get_string()[0]) {
		return 3;
	}
	return 0;

}

void ClassTable::ScopeCheckingAndTypeChecking()
{
	method_table->enterscope();
	CreateMethodTable(Object, std::vector<method_class *>());

	TraversalClass(Object);

	method_table->exitscope();
}

void ClassTable::TraversalClass(Symbol curr_class) 
{
	//cout << "parse class " << curr_class->get_string() << endl;
	current_class = curr_class;
	object_table->enterscope();
	const ClassTreeNode &node = class_tree_[curr_class];
	Class_ class_ = node.class_;

	Features features = class_->get_features();
	// add attr to object_table and check type
	for (int i = features->first(); features->more(i); i = features->next(i)) {
		Feature feature = features->nth(i);
		if (feature->get_type() == Feature_class::ATTR) {
			attr_class *attr = static_cast<attr_class *>(feature);
			Symbol attr_type, attr_type_decl = attr->get_type_decl();
			int check_ret = CheckTypeClass(attr_type_decl);
			if (check_ret) {
				if (2 == check_ret) {
					attr_type = curr_class;
				} else {
					attr_type = attr_type_decl;
				}
			} else {
				attr_type = Object;
				semant_error(class_) << "the type " << attr_type_decl->get_string() << " of attribute " << attr->get_name()->get_string() << " in class " << curr_class->get_string() << "is not defined" << endl;
			}

			if (attr->get_name() != self) {
				if (NULL == object_table->lookup(attr->get_name())) {
					object_table->addid(attr->get_name(), new Symbol(attr_type));
				} else {
					if (NULL != object_table->probe(attr->get_name())) {
						semant_error(class_) << "the attribute " << attr->get_name()->get_string() << " in class " 
							<< curr_class->get_string() << " is redefined" << endl;
					} else {
						semant_error(class_) << "the attribute " << attr->get_name()->get_string() << " in class "
							<< curr_class->get_string() << " is inherited attributes, not redefined" << endl;
					}
				}
			} else {
				semant_error(class_) << "the attribute cannot be self in class " << curr_class->get_string() << endl;
			}
		}
	}

	// redundancy with check self in object expr
	object_table->addid(self, new Symbol(SELF_TYPE));

	// type check attr expression
	for (int i = features->first(); features->more(i); i = features->next(i)) {
		Feature feature = features->nth(i);
		if (feature->get_type() == Feature_class::ATTR) {
			attr_class *attr = static_cast<attr_class *>(feature);
			int check_ret = CheckTypeClass(attr->get_type_decl());
			Symbol attr_type = Object;
			if (2 == check_ret) {
				attr_type = curr_class;
			} else if (0 != check_ret) {
				attr_type = attr->get_type_decl();
			}
			if (!TypeCheck(attr_type, ExprType(attr->get_init()))) {
				semant_error(class_) << "the init of the attribute " << attr->get_name()->get_string() << " is not cast to " <<  attr_type->get_string() << endl;
			}
		}
	}

	for (int i = features->first(); features->more(i); i = features->next(i)) {
		Feature feature = features->nth(i);
		if (feature->get_type() == Feature_class::METHOD) {
			object_table->enterscope();

			method_class *method = static_cast<method_class *>(feature);
			Formals formals = method->get_formals();

			for (int j = formals->first(); formals->more(j); j = formals->next(j)) {
				formal_class *formal = static_cast<formal_class*>(formals->nth(j));

				if (NULL == object_table->probe(formal->get_name())) {
					Symbol formal_type = Object;
					int ret = CheckTypeClass(formal->get_type_decl());
					if (0 != ret && 2 != ret) {
						formal_type = formal->get_type_decl();
					}
					if (formal->get_name() != self) {
						object_table->addid(formal->get_name(), new Symbol(formal_type));
					} else {
						semant_error(class_) << "formal param of method " << method->get_name()->get_string() << " cannot be self" << endl;
					}
				} else {
					semant_error(class_) << "the formal param " << formal->get_name()->get_string() << " of the method " 
						<< method->get_name()->get_string() << " is redefined" << endl;
				}

			}

			Symbol body_type = ExprType(method->get_expr());

			Symbol return_type = Object;

			int ret = CheckTypeClass(method->get_return_type());
			if (ret) {
				if (ret == 2) {
					return_type = SELF_TYPE;
				} else {
					return_type = method->get_return_type();
				}
			}

			//cout << "return type " << return_type -> get_string() <<  " body_type " << body_type->get_string() << endl;

			if (!TypeCheck(return_type, body_type)) {
				semant_error(class_) << "the expr type of the method " << method->get_name()->get_string() << " is not cast to " << return_type->get_string() << endl;
			}


			object_table->exitscope();
		}
	}



	for (int i = 0; i < static_cast<int>(node.children.size()); i++) {
		TraversalClass(node.children[i]);
	}


	object_table->exitscope();
}

Symbol ClassTable::ExprType(Expression expr)
{
	int ret = expr->get_category();
	Symbol type = Object;

	std::set<Symbol> all_sets;
	all_sets.insert(Int);
	all_sets.insert(Str);
	all_sets.insert(Bool);
	switch(ret) {
		case Expression_class::INT_CONST:
			type = Int;
			break;
		case Expression_class::BOOL_CONST:
			type = Bool;
			break;
		case Expression_class::STRING_CONST:
			type = Str;
			break;
		case Expression_class::OBJECT:
			type = ObjectExpr(static_cast<object_class*>(expr));
			break;
		case Expression_class::ASSIGN:
			type = AssignExpr(static_cast<assign_class*>(expr));
			break;
		case Expression_class::NEW_: 
			type = New_Expr(static_cast<new__class*>(expr));
			break;
		case Expression_class::DISPATCH:
			type = DispatchExpr(static_cast<dispatch_class*>(expr));
			break;
		case Expression_class::STATIC_DISPATCH:
			type = StaticDispatchExpr(static_cast<static_dispatch_class*>(expr));
			break;
		case Expression_class::COND:
			type = CondExpr(static_cast<cond_class*>(expr));
			break;
		case Expression_class::BLOCK:
			type = BlockExpr(static_cast<block_class*>(expr));
			//cout << "Block " << type->get_string() << endl;
			break;
		case Expression_class::LET:
			type = LetExpr(static_cast<let_class*>(expr));
			break;
		case Expression_class::TYPCASE:
			type = TypcaseExpr(static_cast<typcase_class*>(expr));
			break;
		case Expression_class::LOOP:
			type = LoopExpr(static_cast<loop_class*>(expr));
			break;
		case Expression_class::ISVOID:
			type = IsvoidExpr(static_cast<isvoid_class*>(expr));
			break;
		case Expression_class::COMP:
			type = UnaryExpr(static_cast<comp_class*>(expr)->get_e1(), Bool);
			break;
		case Expression_class::NEG:
			type = UnaryExpr(static_cast<neg_class*>(expr)->get_e1(), Int);
			break;
		case Expression_class::LT: 
			{
				lt_class *lt = static_cast<lt_class*>(expr);
				type = BinaryExpr(lt->get_e1(), lt->get_e2(), Bool);
			}
			break;
		case Expression_class::LEQ:
			{
				leq_class *leq = static_cast<leq_class*>(expr);
				type = BinaryExpr(leq->get_e1(), leq->get_e2(), Bool);
			}
			break;
		case Expression_class::PLUS:
			{
				plus_class *plus = static_cast<plus_class*>(expr);
				type = BinaryExpr(plus->get_e1(), plus->get_e2(), Int);
			}
			break;
		case Expression_class::SUB:
			{
				sub_class *sub = static_cast<sub_class*>(expr);
				type = BinaryExpr(sub->get_e1(), sub->get_e2(), Int);
			}
			break;
		case Expression_class::MUL:
			{
				mul_class *mul = static_cast<mul_class*>(expr);
				type = BinaryExpr(mul->get_e1(), mul->get_e2(), Int);
			}
			break;
		case Expression_class::DIVIDE:
			{
				divide_class *divide = static_cast<divide_class*>(expr);
				type = BinaryExpr(divide->get_e1(), divide->get_e2(), Int);
			}
			break;
		case Expression_class::EQ:
			{
				eq_class *eq = static_cast<eq_class*>(expr);
				type = EqExpr(eq->get_e1(), eq->get_e2(), all_sets, Bool);
			}
			break;
		case Expression_class::NO_EXPR:
			type = No_type;
			break;
		default:
			break;
	}

	expr->set_type(type);

	return type;

}	

Symbol ClassTable::ObjectExpr(object_class *object_expr)
{
	Symbol type, *object_type;
	if (self == object_expr->get_name()) {
		type = SELF_TYPE;
	} else if (NULL != (object_type = object_table->lookup(object_expr->get_name()))) {
		type = *object_type;
	} else {
		semant_error(class_tree_[current_class].class_) << "identify " << object_expr->get_name()->get_string() << " in class " << current_class->get_string() << " is not defined" << endl;
		type = Object;
	}
	return type;
}

Symbol ClassTable::AssignExpr(assign_class *assign_expr)
{
	Symbol type = ExprType(assign_expr->get_expr()), *object_type;
	if (self == assign_expr->get_name()) {
		semant_error(class_tree_[current_class].class_) << "assign to self " << 
			assign_expr->get_name()->get_string() << " in class " << current_class->get_string() << " is not legal" << endl;
	} else if (NULL != (object_type = object_table->lookup(assign_expr->get_name()))) {
		if (!TypeCheck(*object_type, type)) {
			semant_error(class_tree_[current_class].class_) << "assign to idenftify " 
				<< assign_expr->get_name()->get_string() << " type " << type->get_string() 
				<< " cannot cast to " << (*object_type)->get_string() << endl;
		} 	
	} else {
		semant_error(class_tree_[current_class].class_) << "identify " << assign_expr->get_name()->get_string() << " in class " << current_class->get_string() << " is not defined" << endl;
	}	
	return type;
}

Symbol ClassTable::New_Expr(new__class *new_expr)
{
	Symbol type;
	if (SELF_TYPE == new_expr->get_type_name()) {
		type = SELF_TYPE;
	} else if (class_tree_.find(new_expr->get_type_name()) != class_tree_.end()) {
		type = new_expr->get_type_name();
	} else {
		semant_error(class_tree_[current_class].class_) << "new type " 
			<< new_expr->get_type_name()->get_string() << " in class " << current_class->get_string() 
			<< " is not defined" << endl;
		type = Object;
	}
	return type;
}

Symbol ClassTable::DispatchExpr(dispatch_class *dispatch_expr)
{
	Symbol type = ExprType(dispatch_expr->get_expr());
	return CommonDispatchExpr(type, type, dispatch_expr->get_name(), dispatch_expr->get_actual());
}

Symbol ClassTable::StaticDispatchExpr(static_dispatch_class *static_dispatch_expr)
{
	return CommonDispatchExpr(ExprType(static_dispatch_expr->get_expr()), 
			static_dispatch_expr->get_type_name(), static_dispatch_expr->get_name(), 
			static_dispatch_expr->get_actual());
}

Symbol ClassTable::CommonDispatchExpr(Symbol t0, Symbol t1, Symbol name, Expressions actual)
{
	Symbol type = Object;

	std::vector<Symbol> actual_types;
	for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
		Expression expr = actual->nth(i);
		actual_types.push_back(ExprType(expr));
	}

	if (CheckTypeClass(t1)) {
		if (TypeCheck(t1, t0)) {
			Symbol tt1; 
			if (t1 == SELF_TYPE) {
				tt1 = current_class;
			} else {
				tt1 = t1;
			}
			MethodTableKey key = std::make_pair(tt1, name);
			MethodTableValue *val = method_table->lookup(key);
			if (NULL == val) {
				semant_error(class_tree_[current_class].class_) << "dispatch method " << name->get_string() << " in class " << current_class->get_string() << " is not defined" << endl;
			} else {
				Formals formals = val->first;
				//cout << "method " << name->get_string() <<  " " << formals->len() << " " << actual_types.size() << endl;
				if (formals->len() == static_cast<int>(actual_types.size())) {

					//cout << name->get_string() << endl;
					int i = 0;
					int j = formals->first();

					while (i < static_cast<int>(actual_types.size()) && formals->more(j)) {
						//cout << "formal param "<<  formals->nth(j)->get_type_decl() << " actual param " << actual_types[i] << endl;
						if (!TypeCheck(formals->nth(j)->get_type_decl(), actual_types[i])) {
							semant_error(class_tree_[current_class].class_) << "the actual param " << actual_types[i]->get_string() 
								<< " of dispatch method " << name->get_string() << " in class " << current_class->get_string() 
								<< " cannot cast to " << formals->nth(j)->get_type_decl()->get_string() << endl;

						}
						i++;
						j = formals->next(j);
					}

				} else {
					semant_error(class_tree_[current_class].class_) << "length of dispatch method " << name->get_string() 
						<< " actual param in class " << current_class->get_string() << " is not same with formal param" << endl;
				}
				Symbol return_type = val->second;
				if (SELF_TYPE != return_type) {
					type = return_type;
				} else {
					type = t0;
				}
			}
		} else {
			semant_error(class_tree_[current_class].class_) << "static dispatch method " << name->get_string() 
				<< " in type "  << t1->get_string() << " is not legal in class " << current_class->get_string() << endl;
		}
	} else {
		semant_error(class_tree_[current_class].class_) << "the type " << t1->get_string() << " in static dispatch method " << name->get_string()
			<< " is not legal in class " << current_class->get_string() << endl;
	}

	return type;

}

Symbol ClassTable::CondExpr(cond_class *cond_expr)
{
	Symbol pred_type = ExprType(cond_expr->get_pred());
	if (Bool != pred_type) {
		semant_error(class_tree_[current_class].class_) << "cond pred type is wrong" << endl;
	}	

	Symbol then_exp_type = ExprType(cond_expr->get_then_exp()),
		   else_exp_type = ExprType(cond_expr->get_else_exp());

	return Lub(then_exp_type, else_exp_type);
}

Symbol ClassTable::BlockExpr(block_class *block_expr)
{
	Symbol type;
	Expressions exprs = block_expr->get_body();
	for (int i = exprs->first(); exprs->more(i); i = exprs->next(i)) {
		type = ExprType(exprs->nth(i));
	}
	return type;
}

Symbol ClassTable::LetExpr(let_class *let_expr)
{
	Symbol type;

	Symbol type_decl;

	int ret = CheckTypeClass(let_expr->get_type_decl());

	if (2 == ret) {
		type_decl = SELF_TYPE;
	} else if (0 != ret) {
		type_decl = let_expr->get_type_decl();
	} else {
		semant_error(class_tree_[current_class].class_) << "let expr the type " << let_expr->get_type_decl()->get_string() 
			<< " is not defined in class " << current_class->get_string() << endl;
		type_decl = Object;
	}
	Symbol init_type = ExprType(let_expr->get_init());
	if (!TypeCheck(type_decl, init_type)) {
		semant_error(class_tree_[current_class].class_) << "let expr the init type " 
			<< init_type->get_string() << " cannot cast to " << type_decl->get_string() << endl;
	}

	object_table->enterscope();

	if (self != let_expr->get_identifier()) {  
		object_table->addid(let_expr->get_identifier(), new Symbol(type_decl));
	} else {
		semant_error(class_tree_[current_class].class_) << "let expr bind self" << endl;
	}

	type = ExprType(let_expr->get_body());

	object_table->exitscope();
	return type;
}

Symbol ClassTable::TypcaseExpr(typcase_class *typcase_expr)
{
	Symbol type = Object;
	bool inited = false;

	ExprType(typcase_expr->get_expr());

	Cases cases = typcase_expr->get_cases();

	std::set<Symbol> type_decls;

	for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
		branch_class *branch = static_cast<branch_class*>(cases->nth(i));
		object_table->enterscope();
		Symbol type_decl;

		if (1 == CheckTypeClass(branch->get_type_decl())) {
			type_decl = branch->get_type_decl();
			if (type_decls.find(type_decl) == type_decls.end()) {
				type_decls.insert(type_decl);
			} else {
				semant_error(class_tree_[current_class].class_) << "case expr the type decl " << branch->get_type_decl()->get_string() 
					<< " is conflict" << endl; 
			}
		} else {
			semant_error(class_tree_[current_class].class_) << "case expr the type " << branch->get_type_decl()->get_string() 
				<< " is not defined" << endl; 
			type_decl = Object;
		}
		object_table->addid(branch->get_name(), new Symbol(type_decl));

		if (!inited)  {
			inited = true;
			type = ExprType(branch->get_expr());
		} else {
			type = Lub(type, ExprType(branch->get_expr())); 
		}

		object_table->exitscope();
		
	}
	return type;
}

Symbol ClassTable::LoopExpr(loop_class *loop_expr)
{
	Symbol type = Object;

	Symbol pred_type = ExprType(loop_expr->get_pred());

	if (pred_type != Bool) {
		semant_error(class_tree_[current_class].class_) << "the type of pred in while is not bool" << endl;
	}

	ExprType(loop_expr->get_body());


	return type;
}

Symbol ClassTable::IsvoidExpr(isvoid_class *isvoid_expr)
{
	Symbol type = Bool;

	ExprType(isvoid_expr->get_e1());

	return type;
}

Symbol ClassTable::UnaryExpr(Expression e1, Symbol target_type)
{
	Symbol type = target_type;

	Symbol real_type = ExprType(e1);
	if (real_type != target_type) {
		semant_error(class_tree_[current_class].class_) << "the type of unary expr is not " <<  target_type->get_string() << endl;
	}

	return type;
}

Symbol ClassTable::BinaryExpr(Expression e1, Expression e2, Symbol target_type)
{
	Symbol type = target_type;

	Symbol e1_type = ExprType(e1),
		   e2_type = ExprType(e2);

	if (e1_type != Int) {
		semant_error(class_tree_[current_class].class_) << "the type of binary expr e1 is not allowed" << endl;
	} 
	if (e2_type != Int) {
		semant_error(class_tree_[current_class].class_) << "the type of binary expr e2 is not allowed" << endl;
	} 
	return type;
}

Symbol ClassTable::EqExpr(Expression e1, Expression e2, std::set<Symbol> limits_type, Symbol target_type)
{
	Symbol type = target_type;

	Symbol e1_type = ExprType(e1),
		   e2_type = ExprType(e2);

	if (limits_type.find(e1_type) != limits_type.end() 
			|| limits_type.find(e2_type) != limits_type.end()) {
		if (e1_type != e2_type) {
			semant_error(class_tree_[current_class].class_) << "sq expr type of e1 must be same with type of e2, when some one type is Int, String, Bool" << endl;
		}
	}

	return type;
}




Symbol ClassTable::Lub(Symbol t1, Symbol t2)
{
	bool least_r1 = false, least_r2 = false;
	t1 = RegularType(t1, least_r1);
	t2 = RegularType(t2, least_r2);
	if (least_r1) {
		return t2;
	} else {
		if (least_r2) {
			return t1;
		} else {
			if (t1 == SELF_TYPE && t2 == SELF_TYPE) {
				return SELF_TYPE;
			} else {
				if (t1 == SELF_TYPE) {
					t1 = current_class;
				}

				if (t2 == SELF_TYPE) {
					t2 = current_class;
				}


				Symbol s1 = t1;

				while (s1 != No_class) {
					Symbol s2 = t2;

					while (s2 != No_class) {
						if (s1 == s2) {
							return s1;
						}
						s2 = class_tree_[s2].parent;
					}

					s1 = class_tree_[s1].parent;

				}
			}

		}
	}

	// never reach this
	return Object;
}


bool ClassTable::TypeCheck(Symbol t1, Symbol t2)
{
	bool least_r1 = false, least_r2 = false;
	t1 = RegularType(t1, least_r1);
	t2 = RegularType(t2, least_r2);

	//cout << t1->get_string() << " " << least_r1 << endl;
	//cout << t2->get_string() << " " << least_r2 << endl;

	if (least_r1) {
		return least_r2;
	} else {
		if (least_r2) {
			return true;
		} else {

			if (t1 == SELF_TYPE) {
				if (t2 == SELF_TYPE) {
					return true;
				} else {
					return false;
				}
			} else {
				if (t2 == SELF_TYPE) {
					t2 = current_class;
				}
				Symbol s = t2;
				while (s != No_class) {
					if (s == t1) {
						return true;
					}
					s = class_tree_[s].parent;
				}
			}

		}
	}
	return false;
}

Symbol ClassTable::RegularType(Symbol t, bool &least)
{
	int ret = CheckTypeClass(t);
	if (0 == ret) {
		return Object;
	} else if (2 == ret) {
		return SELF_TYPE;
	} else if (3 == ret) {
		least = true;
		return t;
	} else {
		return t;
	}
}


void ClassTable::CreateMethodTable(Symbol curr_class, const std::vector<method_class *> &ancestor_method)
{
	const ClassTreeNode &node = class_tree_[curr_class];

	std::vector<method_class *> new_method;

	Class_ class_ = node.class_;
	Features features = class_->get_features();
	for (int i = features->first(); features->more(i); i = features->next(i)) {
		Feature feature = features->nth(i);
		if (feature->get_type() == Feature_class::METHOD) {
			method_class *method = static_cast<method_class *>(feature);
			MethodTableKey key = std::make_pair(curr_class, method->get_name());
			if (method_table->probe(key) == NULL) {
				if (!CheckTypeClass(method->get_return_type())) {
					semant_error(class_) << "return type " << method->get_return_type()->get_string() << " of method " << method->get_name()->get_string() << " in class " << curr_class->get_string() << " is not defined" << endl;
				}
				Formals formals = method->get_formals();
				for (int j = formals->first(); formals->more(j); j = formals->next(j)) {
					Formal formal = formals->nth(j);
					int ret = CheckTypeClass(formal->get_type_decl());
					if (0 == ret || 2 == ret) {
						std::string msg;
						if (0 == ret) {
							msg = " is not defined";
						} else if (2 == ret) {
							msg = " is SELF_TYPE, not allowed";
						}
						semant_error(class_) << "formal parameter type " << formal->get_type_decl()->get_string() << " of method " 
							<< method->get_name()->get_string() << " in class " << curr_class->get_string() 
							<< msg << endl;
					} 				
				}

				method_table->addid(key, new MethodTableValue(formals, method->get_return_type()));
				new_method.push_back(method);
			} else {
				semant_error(class_) << "method " << method->get_name()->get_string() << " redefined in class " << curr_class->get_string() << endl;
			}
		}
	}

	std::vector<method_class *> next_ancestor_method;

	for (int i = 0; i < static_cast<int>(ancestor_method.size()); i++) {
		method_class *method = ancestor_method[i];
		MethodTableKey key = std::make_pair(curr_class, method->get_name());
		MethodTableValue *val = method_table->probe(key);
		if (NULL == val) {
			method_table->addid(key, new MethodTableValue(method->get_formals(), method->get_return_type()));
			next_ancestor_method.push_back(method);
		} else {
			int is_valid = 0;
			if (method->get_return_type() == val->second) {
				Formals formals = method->get_formals(), now_formals = val->first;
				if (formals->len() == now_formals->len()) {
					int j = formals->first();
					int k = now_formals->first();
					while (formals->more(j) && now_formals->more(k)) {
						if (formals->nth(j)->get_type_decl() != now_formals->nth(k)->get_type_decl()) {
							is_valid = -1;
							break;
						}
						j = formals->next(j);
						k = now_formals->next(k);
					}

				} else {
					is_valid = -2;
				}
			} else {
				is_valid = -3;
			}
			if (0 != is_valid) {
				std::string msg;
				if (-1 == is_valid) {
					msg = "the type of formal parameter is not same";
				} else if (-2 == is_valid) {
					msg = "the number of formal parameters is not same";
				} else if (-3 == is_valid) {
					msg = "return type is not same";
				}
				semant_error(class_) << "the redefinition of method " << method->get_name()->get_string() << " in class " 
					<< curr_class->get_string() << " is invalid: " << msg << endl;
			}
		}
	}
	for (int i = 0; i < static_cast<int>(new_method.size()); i++) {
		next_ancestor_method.push_back(new_method[i]);
	}

	for (int i = 0; i < static_cast<int>(node.children.size()); i++) {
		CreateMethodTable(node.children[i], next_ancestor_method);
	}
}




