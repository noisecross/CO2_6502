/**
* |------------------------------------------|
* | CO2 6502, COMPILER OPTIMIZER TO 6502     |
* | File: semanticAnalyzer.cpp               |
* | v1.0, September 2012                     |
* | Author: Emilio Arango Delgado de Mendoza |
* |------------------------------------------|
*/

#include "semanticAnalyzer.h"
#include "../CommonFiles/errManager.h"
#include <algorithm>
#include <iomanip>
#include <sstream>

#include <numeric>



extern SymbolsTable            symbolsTable;
extern ErrManager              errManager;
extern SyntaxTree              syntaxTree;



static bool                    constShortcut;
static int                     breakable      = 0;
static int                     continuable    = 0;
static list<QualifiedType>     currCaseType   = list<QualifiedType>();
static list<pair<string, int>> labelsExpected = list<pair<string, int>>();



/**
* addError
* 
* Auxiliar method. Takes an error message, a lex token and a line argument and perform a call to the
* error manager with that data.
*
* @param errMessage: The generic error message to this error.
* @param lex: The token which have produced the error.
* @param line: Number line where the error has been produced.
*/
static void addError(string errMessage, string lex, int line){
	ostringstream errOutput;

	errOutput << "\t" << lex << " (" << line << ")" << endl;
	errOutput << "\t^" << errMessage;
	errManager.addError(errOutput.str());
}



/**
* addWarning
* 
* Auxiliar method. Takes a warning message, a lex token and a line argument and perform a call to the
* error manager with that data.
*
* @param warMessage: The generic warning message to this event.
* @param lex: The token which have produced the event.
* @param line: Number line where the event has been produced.
*/
static void addWarning(string warMessage, string lex, int line){
	ostringstream warOutput;

	warOutput << "\t" << lex << " (" << line << ")" << endl;
	warOutput << "\t^" << warMessage;
	errManager.addWarning(warOutput.str());
}



/**
* promoteExpressionType
* 
* Auxiliar method. Takes two QualifiedTypes and return the better type to be inherited.
*
* @param operand1: The first type.
* @param operand2: The second type.
* @param line: The line where the operation is taken place.
*
* @return the better mix of the two qualifiers.
*/
static QualifiedType promoteExpressionType(const QualifiedType &operand1, const QualifiedType &operand2, const int &line){
	QualifiedType output;

	const string auxMsg = operand1.toString() + " <- " + operand2.toString();
	
	/* FUTURE <strings> STRING_LITERAL */
	if (operand1.type == TYPE_LITERAL){
		output = QualifiedType(operand2);
	}else if(operand2.type == TYPE_LITERAL){
		output = QualifiedType(operand1);
	}else{
		if (operand1.b_unsigned != operand1.b_unsigned)
			addWarning(WAR_SEM_MSG_01, "", line);

		int pointerDepth1 = operand1.pointerDepth;
		int pointerDepth2 = operand2.pointerDepth;

		if (pointerDepth1 < 0) pointerDepth1 = 0;
		if (pointerDepth2 < 0) pointerDepth2 = 0;

		if ((pointerDepth1 | pointerDepth2) == 0){
			/* Both types are not pointers return greater type */
			/* If both types are equally sized unsigned is preferred */
			if(symbolsTable.getTypeSize(operand1.type) > symbolsTable.getTypeSize(operand2.type)){
				output = QualifiedType(operand1);
			}else if(symbolsTable.getTypeSize(operand1.type) < symbolsTable.getTypeSize(operand2.type)){
				output = QualifiedType(operand2);
			}else{
				output = QualifiedType(operand1);
				output.b_unsigned = operand1.b_unsigned || operand2.b_unsigned;
			}			
		}else if (pointerDepth1 == 0){
			/* First operand type is not a pointer */
			output = QualifiedType(operand2);
		}else if (pointerDepth2 == 0){
			/* Second operand type is not a pointer */
			output = QualifiedType(operand1);
		}else{
			/* Both types are pointers */
			/* If both are the same type and depth is ok */
			/* If one of them is void* is ok */
			/* Else error */
			if (operand1.type == "void"){
				output = QualifiedType(operand2);
			}else if (operand2.type == "void"){
				output = QualifiedType(operand1);
			}else if (pointerDepth1 == pointerDepth2){
				if (operand1.type == operand2.type){
					output = QualifiedType(operand1);
				}else{
					addError(ERR_SEM_MSG_06, auxMsg, line);
				}
			}else{
				addError(ERR_SEM_MSG_06, auxMsg, line);
			}
		}
	}

	return output;
}



/**
* promoteTypeLiteral
* 
* Auxiliar method. Takes two SyntaxTreeNode* and return the better type to be inherited.
*
* @param childNode0: The first node.
* @param childNode1: The second node.
* @param parentNode: The parent node or NULL if not relevant.
*/
static void promoteTypeLiteral(SyntaxTreeNode* childNode0, SyntaxTreeNode* childNode1, SyntaxTreeNode* parentNode){
	/* Always try to promote to unsigned */
	if (childNode0->type.type == TYPE_LITERAL && childNode1->type.type == TYPE_LITERAL){
		/* Always try to promote to unsigned */
		if(childNode0->value >= 0 && childNode1->value >= 0){
			childNode0->type.b_unsigned = childNode1->type.b_unsigned = true;
			childNode0->type.type = (childNode0->value > 255 || childNode1->value > 255) ? "int" : "char";
			childNode1->type.type = childNode0->type.type;
		}else{
			childNode0->type.b_unsigned = childNode1->type.b_unsigned = false;
			childNode0->type.type = (childNode0->value > 127 || childNode0->value < -128 ||
				childNode1->value > 127 || childNode1->value < -128) ? "int" : "char";
			childNode1->type.type = childNode0->type.type;
		}
	}else if(childNode0->type.type == TYPE_LITERAL){
		/* if value [1] is known and doesn't fit into its type-signation, modify it */
		/* Try to fit [0] type to [1] and promote otherwise */
		/* Signation is promoted to signed if possible and size is recalc */
		if(childNode0->value < 0 || !childNode1->type.b_unsigned){
			childNode0->type.b_unsigned = false;
			if (childNode1->lex == ""){
				childNode1->type.b_unsigned = false;
				if(childNode1->valueIsUsed && (childNode1->value < -128 || childNode1->value > 127)){
					childNode1->type.type = "int";
				}
			}
		}
		if(childNode1->type.type == "int"){
			childNode0->type.type == "int";
		}else{
			if(childNode0->type.b_unsigned){
				childNode0->type.type = (childNode0->value > 255) ? "int" : "char";
			}else{
				childNode0->type.type = (childNode0->value > 127 || childNode0->value < -128) ? "int" : "char";
			}
		}
	}else if(childNode1->type.type == TYPE_LITERAL){
		/* if value [0] is known and doesn't fit into its type-signation, modify it */
		/* Try to fit [1] type to [0] and promote otherwise */
		/* Signation is promoted to signed if possible and size is recalc */
		if(childNode1->value < 0 || !childNode0->type.b_unsigned){
			childNode1->type.b_unsigned = false;
			if (childNode0->lex == ""){
				childNode0->type.b_unsigned = false;
				if(childNode0->valueIsUsed && (childNode0->value < -128 || childNode0->value > 127)){
					childNode0->type.type = "int";
				}
			}
		}
		if(childNode0->type.type == "int"){
			childNode1->type.type == "int";
		}else{
			if(childNode1->type.b_unsigned){
				childNode1->type.type = (childNode1->value > 255) ? "int" : "char";
			}else{
				childNode1->type.type = (childNode1->value > 127 || childNode1->value < -128) ? "int" : "char";
			}
		}
	}

	if (parentNode != NULL){
		parentNode->type = promoteExpressionType(childNode0->type, childNode1->type, parentNode->line);
	}
}



/**
* checkLabelsDefinition
* 
* Auxiliar method. After the definition of a function check if all the required labels have been
* defined. Notify the error manager every undefined label.
*/
static void checkLabelsDefinition(){

	auto f_checkExpectedLabel = [](const pair<string, int> &x){
		if (!symbolsTable.existsLabel(symbolsTable.currentFramework, x.first))
			addError(ERR_SEM_MSG_12, x.first, x.second);
	};


	for_each (labelsExpected.begin(), labelsExpected.end(), f_checkExpectedLabel);
}



/**
* checkConstantProperty
* 
* Auxiliar method. Before an assignment operation check if is been doing over a const variable
* defined. Notify the error manager every undefined label.
* Also performs a check to ensure that an argument is not beign accessed in a way that would spoil the overlap
* That means, only can be l-value if the operation is MVA or STP
*
* @param l_value: The memory place where the assignation will go.
* @param line: The line where the operation is taken place.
* @param bypass: The overlappable value to store in a modified argument.
*/
static void checkConstantProperty(const string &l_value, int line, bool bypass){
	if (l_value != ""){
		if (symbolsTable.existsSymbol(l_value, symbolsTable.currentFramework)){

			STEntry* l_Symbol = symbolsTable.getSymbol(l_value, symbolsTable.currentFramework);

			if (l_Symbol->isConst()){
				addError(ERR_SEM_MSG_13, l_value, line);
			}else if(l_Symbol->getPointerDepth() > 0 && l_Symbol->isOverlappable()){
				l_Symbol->setOverlappable(bypass);
			}
		}else if (symbolsTable.existsSymbol(l_value, GLOBAL_FRAMEWORK)){
			if (symbolsTable.isConst(l_value, GLOBAL_FRAMEWORK)){
				addError(ERR_SEM_MSG_13, l_value, line);
			}
		}
	}
}





/**
* |------------------------------------------|
* |                                          |
* | Class: Syntax Tree                       |
* |                                          |
* |------------------------------------------|
*/

/**
* SyntaxTree
* 
* Class constructor.
*
* @param inOffset: Difference between the token values shown by gc.tab.h and the actually used by gc.tab.c
* @param inSymbolNameTable: Table which given a symbol number points a C string with its name
*/
SyntaxTree::SyntaxTree(){
	root            = NULL;
	offset          = 0;
	nTokens         = 0;
	symbolNameTable = NULL;
	constShortcut   = false;
}



/**
* SyntaxTree
* 
* Class constructor.
*
* @param inOffset: Difference between the token values shown by gc.tab.h and the actually used by gc.tab.c
* @param inSymbolNameTable: Table which given a symbol number points a C string with its name
*/
SyntaxTree::SyntaxTree(const int inOffset, int inNTokens, const char** inSymbolNameTable, bool inConstShortcut){
	root            = NULL;
	offset          = inOffset;
	nTokens         = inNTokens;
	symbolNameTable = inSymbolNameTable;
	constShortcut   = inConstShortcut;
}



/**
* ~SyntaxTree
* 
* Class destructor.
*/
SyntaxTree::~SyntaxTree(){ delete root; }



/**
* isIdentifier
* 
* Check if a certain symbol is an identifier.
*
* @param symbol: Symbol to check.
*
* @return true if the symbol is an identifier.
*/
bool SyntaxTree::isIdentifier(int symbol){ return symbol == (IDENTIFIER - offset); }



/**
* isType
* 
* Check if a certain symbol is a type name.
*
* @param symbol: Symbol to check.
*
* @return true if the symbol is a type name.
*/
bool SyntaxTree::isType(int symbol){ return symbol == (TYPE_NAME - offset); }



/**
* isLiteral
* 
* Check if a certain symbol is a literal.
*
* @param symbol: Symbol to check.
*
* @return true if the symbol is a literal.
*/
bool SyntaxTree::isLiteral(int symbol){ return symbol == (CONSTANT - offset); }



/**
* getSymbolName
* 
* Get the name of a certain symbol.
*
* @param symbol: the integer value of the symbol.
*
* @return the name the symbol.
*/
string SyntaxTree::getSymbolName(int symbol){ return symbolNameTable[symbol]; }



/**
* getRoot
* 
* Get the SyntaxTreeNode node root of the current tree.
*
* @return: a pointer to the SyntaxTreeNode root.
*/
SyntaxTreeNode* SyntaxTree::getRoot(){ return root; }



/**
* setRoot
* 
* Set a SyntaxTreeNode node as the root of the current tree.
*
* @param newRoot: a pointer to the SyntaxTreeNode which will be the new root.
*/
void SyntaxTree::setRoot(SyntaxTreeNode* newRoot){ root = newRoot; }



/**
* getNTerminals
* 
* @return the number of terminal tokens of the grammar
*/
int SyntaxTree::getNTerminals() { return nTokens; }



/**
* killUnusedSymbols
*
* Deletes every unused symbol of the symbols table.
*/
void SyntaxTree::killUnusedSymbols(){
	list<STFramework*> frameworks = symbolsTable.getFrameworks();

	auto f_checkSymbol = [this](STEntry* const x){
		if (x->getNUses() == 0 && !x->isVolatile() && x->getPointerDepth() == 0){

			string message = WAR_SEM_MSG_07;
			message.append(x->lex);
			message.append(" @ ");
			message.append(x->getFramework());

			errManager.addWarning(message);
			symbolsTable.delSymbol(x->lex, x->getFramework());
		}
	};


	auto f_checkSymbols = [f_checkSymbol](STFramework* const x){
		list<STEntry*> symbols = x->getSymbols();
		for_each(symbols.begin(), symbols.end(), f_checkSymbol);
	};


	for_each(frameworks.begin(), frameworks.end(), f_checkSymbols);
}



/**
* typeCheck
*
* @param recursionAllowed: Flag which informs if recursion is allowed or not.
*
* @return string containing the whole result of the syntax tree type check
*/
QualifiedType* SyntaxTree::typeCheck(bool recursionEnabled){
	if (root != NULL){
		QualifiedType* result = root->typeCheck();
		validateSemanticTree(recursionEnabled);
		killUnusedSymbols();
		return result;
	}else
		return NULL;
}



/**
* printSyntaxTree
*
* @return string containing the text formated SyntaxTree
*/
string SyntaxTree::toString(){ return root->toString(); }



/**
* |------------------------------------------|
* |                                          |
* | Class: Syntax Tree Node                  |
* |                                          |
* |------------------------------------------|
*/

/**
* SyntaxTreeNode
* 
* Class costructor.
*
* @param inSymbol: The symbol (token or non-terminal) which this node represents.
* @param inRule: The reduction rule to apply (if applicable) to their children.
*/
SyntaxTreeNode::SyntaxTreeNode(int inSymbol, int inRule, int inLine){
	symbol      = inSymbol;
	rule        = inRule;
	line        = inLine;
	value       = 0;
	valueIsUsed = false;
	returnDone  = false;
	lex         = "";
	children    = deque<SyntaxTreeNode*>();
	type        = QualifiedType();

	/* If the symbol is identifier, literal or type */
	if (syntaxTree.isLiteral(inSymbol)){
		string       newLiteral = symbolsTable.popTempLiteral();

		type.type   = TYPE_LITERAL;
		valueIsUsed = true;

		if(newLiteral[0] == '0' && newLiteral.size() > 1){
			if(newLiteral[1] == 'x'){
				/* Hex format*/
				stringstream ss(newLiteral.substr(2, newLiteral.npos));
				ss >> hex >> value;
			}else{
				/* Octal format */
				stringstream ss(newLiteral);
				ss >> oct >> value;
			}
		}else if(newLiteral[0] != '\''){
			/* Decimal format */
			stringstream ss(newLiteral);
			ss >> value;
		}else{
			/* Char */
			if(newLiteral.size() != 3)
				addError(ERR_SEM_MSG_05, newLiteral, line);
			else{
				value = (int) newLiteral[1];
				type.type = "char";
			}
		}

	}else if (syntaxTree.isIdentifier(inSymbol))
		lex = symbolsTable.popTempIdentifier();
	else if (syntaxTree.isType(inSymbol))
		type.type = symbolsTable.popTempType();
}



/**
* ~SyntaxTreeNode
* 
* Class destructor.
*/
SyntaxTreeNode::~SyntaxTreeNode(){

	/* Delete all children */
	auto f_delete = [](SyntaxTreeNode* const x){ delete x; };
	for_each(children.begin(), children.end(), f_delete);

	children.clear();
}



/**
* addChild
* 
* Add a new child to the node.
*
* @param node: The given child to add.
*/
void SyntaxTreeNode::addChild(SyntaxTreeNode* node){
	if (node != NULL) children.push_back(node);
}



/**
* toString
* 
* @return string with a tree text formatted vision of the node and all its children.
*/
string SyntaxTreeNode::toString(){
	ostringstream output;
	int           size = children.size();

	auto f_concat = [&output, &size](SyntaxTreeNode* const x){
		size--;
		if (size > 0)
			output << x->toString(" |  ");
		else
			output << x->toString("    ");
	};


	output << syntaxTree.getSymbolName(symbol) << endl;

	for_each(children.begin(), children.end(), f_concat);

	return output.str();
}



/**
* auxCheckExpression
*
* Auxiliar function to apply a generic type check to certain expressions and
* to promote the resultant type.
*
* @param op: Function which operates over the children nodes values.
* @param opSymbol: String with the operator symbol. Used to print errors.
* @param assignment: Flag which indicates if the expression is an assignment.
*/
void SyntaxTreeNode::auxCheckExpression(function<int (int, int)> op, const string &opSymbol, bool assignment, bool promotion){

	/*
	* 1 - Check operands
	* 2 - If !constShortcut force TYPE_LITERAL promotion
	* 3 - If this is an assignment, promote TYPE_LITERAL if necessary
	* 4 - If this requires promotion, get better inherited type
	* 5 - If this is an assignment, check type compatibility
	*/

	/* FUTURE <strings> <arrays> */
	string        auxMsg = "";
	QualifiedType tempType;


	/* 1 - Check operands */
	children[0]->typeCheck();
	children[1]->typeCheck();

	/* 2 - If !constShortcut force TYPE_LITERAL promotion */
	if (!constShortcut){
		promoteTypeLiteral(children[0], children[1], this);
	}else if(children[0]->type.type != TYPE_LITERAL || children[1]->type.type != TYPE_LITERAL){
		promoteTypeLiteral(children[0], children[1], this);
	}

	/* 3 - If this is an assignment, promote TYPE_LITERAL if necessary */
	if (assignment){
		/* children[0] is an l-value */

		/* Promote literal if neccesary */
		if (children[1]->type.type == TYPE_LITERAL){
			
			children[1]->type.type = "char";

			/* Choose 'signed' property*/
			if (children[1]->value <0){
				children[1]->type.b_unsigned = false;
			}else{
				children[1]->type.b_unsigned = children[0]->type.b_unsigned;
			}

			if (children[1]->value <-128)
				children[1]->type.type = "int";

			if (children[1]->value > 0xFF)
				children[1]->type.type = "int";

			auxMsg = "{" + children[0]->type.toString() + " " +
				children[0]->lex + "} " + opSymbol + " {" +
				children[1]->type.toString() + " " +
				children[1]->lex + "}";

			if (children[1]->value > 0x7F && !(children[1]->type.b_unsigned)){
				/* If type 0's size = 1, warn about different signation. */
				if (symbolsTable.getTypeSize(children[0]->type) == 1)
					children[1]->type.b_unsigned = true;
				else
					children[1]->type.type = "int";
			}

			/* Define boundaries */
			if (children[1]->value > 0xFFFF || children[1]->value < -0x7FFF)
				addError(ERR_SEM_MSG_14, auxMsg, line);
			if (children[1]->value > 0x7FFF && !(children[1]->type.b_unsigned))
				addError(ERR_SEM_MSG_14, auxMsg, line);
		}/* end TYPE_LITERAL */
	}


	/* 4 - If this requires promotion, get better inherited type */
	if (promotion){
		tempType = promoteExpressionType(children[0]->type, children[1]->type, line);

		if (children[0]->valueIsUsed && children[1]->valueIsUsed){
			valueIsUsed = true;
			value = op(children[0]->value, children[1]->value);
		}
	}


	/* 5 - If this is an assignment, check type compatibility */
	if (assignment){
		if (!promotion) tempType = children[1]->type;

		auxMsg = "{" + children[0]->type.toString() + " " +
			children[0]->lex + "} " + opSymbol + " {" +
			tempType.toString() + "}";

		int pointerDepth0 = children[0]->type.pointerDepth;
		int pointerDepth1 = tempType.pointerDepth;

		if (pointerDepth0 < 0) pointerDepth0 = 0;
		if (pointerDepth1 < 0) pointerDepth1 = 0;
	
		if((pointerDepth0 | pointerDepth1) == 0){
			/* Neither is a pointer */
			if(symbolsTable.getTypeSize(children[0]->type) < symbolsTable.getTypeSize(tempType))
				addWarning(WAR_SEM_MSG_06, auxMsg, line);
			if (children[0]->type.b_unsigned != tempType.b_unsigned)
				addWarning(WAR_SEM_MSG_01, auxMsg, line);
		}else{
			/* One or both are pointers */
			if (children[0]->type.type == "void" || tempType.type == "void"){
				/* If one of them is pointer to void is ok */ ;
			}else if(children[0]->type.sameOutput(tempType)){
				/* If both are the same kind of pointer is ok */ ;
			}else{
				/* A cast is required. This is an error */
				addError(ERR_SEM_MSG_06, auxMsg, line);
			}
		}

		type = children[0]->type;
	}else{
		type = tempType;
	}

}



/**
* typeCheck
* 
* The core of the semantic analysis. Decorate the node and all the children nodes.
* Perform the type check and store the labels, symbols and frameworks into the symbols table.
*
* @return QualifiedType* with the type of the node.
*/
QualifiedType* SyntaxTreeNode::typeCheck() {
	ostringstream errOutput;
	string auxBuf;
	 
	auto f_typeCheck = [this](SyntaxTreeNode* x){
		x->typeCheck();
		if (x->type.type == TYPE_ERR)
			this->type.type = TYPE_ERR;
	};


	auto f_returnCheck = [this](SyntaxTreeNode* x){
		if (x->returnDone){
			returnDone = true;
			if(children[children.size()-1] != x) addWarning(WAR_SEM_MSG_02, "", line);
		}
	};


	if(symbol < syntaxTree.getNTerminals()){
		if (syntaxTree.isLiteral(symbol)){
			stringstream constant;
		}
		/* FUTURE <strings> STRING_LITERAL */
		return &type;
	}else{
		if (children.size() > 0)
			line = children[0]->line;
	}

	switch (symbol - syntaxTree.getNTerminals()) {

	case NT_PRIMARY_EXPRESSION :
		checkPrimaryExpression();
		break;

	case NT_POSTFIX_EXPRESSION :
		checkPostfixExpression();
		break;

	case NT_ARGUMENT_EXPRESSION_LIST :
		/* assignment_expression /*
		/* | argument_expression_list ',' assignment_expression */
		for_each(children.begin(), children.end(), f_typeCheck);
		break;

	case NT_UNARY_EXPRESSION :
	case NT_UNARY_OPERATOR :
	case NT_CAST_EXPRESSION :
	case NT_MULTIPLICATIVE_EXPRESSION :
	case NT_ADDITIVE_EXPRESSION :
	case NT_SHIFT_EXPRESSION :
		checkArithmeticExpression();
		break;

	case NT_RELATIONAL_EXPRESSION :
	case NT_EQUALITY_EXPRESSION :
		checkRelationalExpression();
		break;

	case NT_AND_EXPRESSION :
	case NT_EXCLUSIVE_OR_EXPRESSION :
	case NT_INCLUSIVE_OR_EXPRESSION :
	case NT_LOGICAL_AND_EXPRESSION :
	case NT_LOGICAL_OR_EXPRESSION :
		checkLogicalExpression();
		break;

	case NT_CONDITIONAL_EXPRESSION :
		checkConditionalExpression();
		break;

	case NT_ASSIGNMENT_EXPRESSION :
		checkAssignmentExpression();
		break;

	case NT_ASSIGNMENT_OPERATOR :
		/* Never here */
		/*'='
		| MUL_ASSIGN
		| DIV_ASSIGN
		| MOD_ASSIGN
		| ADD_ASSIGN
		| SUB_ASSIGN
		| LEFT_ASSIGN
		| RIGHT_ASSIGN
		| AND_ASSIGN
		| XOR_ASSIGN
		| OR_ASSIGN */
		break;

	case NT_EXPRESSION :
		/* Inside the statements */
		switch (rule) {
		case 1: /* assignment_expression */
			for_each(children.begin(), children.end(), f_typeCheck);
			type        = children[children.size() - 1]->type;
			lex         = children[children.size() - 1]->lex;
			value       = children[children.size() - 1]->value;
			valueIsUsed = children[children.size() - 1]->valueIsUsed;
			break;
		case 2: /* | expression ',' assignment_expression */
			/* Never here */
			break;
		}
		break;

	case NT_CONSTANT_EXPRESSION :
		/* Never here */
		/*conditional_expression */
		break;

	case NT_DECLARATION :
		switch (rule) {
		case 1:
			/* Never here */
			/* declaration_specifiers ';' */
			break;
		case 2:
			/* | declaration_specifiers init_declarator_list ';' */
			performDeclaration();
			break;
		case 3:
			/* | error */
			/* Nothing to do here */
			break;
		}
		break;

	case NT_DECLARATION_SPECIFIERS :
		/* Managed in performDeclaration() */
		/*storage_class_specifier
		| storage_class_specifier declaration_specifiers
		| type_specifier
		| type_specifier declaration_specifiers
		| type_qualifier
		| type_qualifier declaration_specifiers */
		break;

	case NT_INIT_DECLARATOR_LIST :
		/* Managed in performDeclaration() */
		/*init_declarator
		| init_declarator_list ',' init_declarator */
		break;

	case NT_INIT_DECLARATOR :
		/* Managed in performDeclaration() */
		/*declarator
		| declarator '=' initializer */
		break;

	case NT_STORAGE_CLASS_SPECIFIER :
		/* Managed in performDeclaration() */
		/*TYPEDEF
		| EXTERN
		| STATIC
		| AUTO
		| REGISTER */
		break;

	case NT_TYPE_SPECIFIER :
		/* Managed in performDeclaration() */
		/*VOID
		| CHAR
		| SHORT
		| INT
		| LONG
		| FLOAT
		| DOUBLE
		| SIGNED
		| UNSIGNED
		| struct_or_union_specifier
		| enum_specifier
		| TYPE_NAME */
		break;

	case NT_STRUCT_OR_UNION_SPECIFIER :
		/* FUTURE <structs> <union> */
		/*struct_or_union IDENTIFIER '{' struct_declaration_list '}'
		| struct_or_union '{' struct_declaration_list '}'
		| struct_or_union IDENTIFIER */
		break;

	case NT_STRUCT_OR_UNION :
		/* FUTURE <structs> <union> */
		/*STRUCT
		| UNION */
		break;

	case NT_STRUCT_DECLARATION_LIST :
		/* FUTURE <structs> */
		/*struct_declaration
		| struct_declaration_list struct_declaration */
		break;

	case NT_STRUCT_DECLARATION :
		/* FUTURE <structs> */
		/*specifier_qualifier_list struct_declarator_list ';' */
		break;

	case NT_SPECIFIER_QUALIFIER_LIST :
		/*type_specifier specifier_qualifier_list
		| type_specifier
		| type_qualifier specifier_qualifier_list
		| type_qualifier */
		type = extractQualifiedType(this);
		break;

	case NT_STRUCT_DECLARATOR_LIST :
		/* FUTURE <structs> */
		/*struct_declarator
		| struct_declarator_list ',' struct_declarator */
		break;

	case NT_STRUCT_DECLARATOR :
		/* FUTURE <structs> */
		/*declarator
		| ':' constant_expression
		| declarator ':' constant_expression */
		break;

	case NT_ENUM_SPECIFIER :
		/* FUTURE <enum> */
		/*ENUM '{' enumerator_list '}'
		| ENUM IDENTIFIER '{' enumerator_list '}'
		| ENUM IDENTIFIER */
		break;

	case NT_ENUMERATOR_LIST :
		/* FUTURE <enum> */
		/*enumerator
		| enumerator_list ',' enumerator */
		break;

	case NT_ENUMERATOR :
		/* FUTURE <enum> */
		/*IDENTIFIER
		| IDENTIFIER '=' constant_expression */
		break;

	case NT_TYPE_QUALIFIER :
		/* Managed in performDeclaration() */
		/*CONST
		| VOLATILE
		| INTERRUPT */
		break;

	case NT_DECLARATOR :
		/* Managed in performDeclaration() */
		/*pointer direct_declarator
		| direct_declarator */
		break;

	case NT_DIRECT_DECLARATOR :
		/* Managed in performDeclaration() */
		/*IDENTIFIER
		| '(' declarator ')'
		| direct_declarator '[' constant_expression ']'
		| direct_declarator '[' ']'
		| direct_declarator '(' parameter_type_list ')'
		| direct_declarator '(' identifier_list ')'
		| direct_declarator '(' ')' */
		break;

	case NT_POINTER :
		/* Managed in performDeclaration() */
		/*'*'
		| '*' type_qualifier_list
		| '*' pointer
		| '*' type_qualifier_list pointer */
		break;

	case NT_TYPE_QUALIFIER_LIST :
		/* Managed in performDeclaration() */
		/*type_qualifier
		| type_qualifier_list type_qualifier */
		break;

	case NT_PARAMETER_TYPE_LIST :
		/* Managed in performDeclaration() */
		/* parameter_list */
		/* | parameter_list ',' ELLIPSIS */
		break;

	case NT_PARAMETER_LIST :
		/* parameter_declaration
		| parameter_list ',' parameter_declaration */
		for_each(children.begin(), children.end(), f_typeCheck);
		break;

	case NT_PARAMETER_DECLARATION :
		/* declaration_specifiers declarator
		|  declaration_specifiers abstract_declarator
		|  declaration_specifiers */
		performVariableDeclaration();
		break;

	case NT_IDENTIFIER_LIST :
		/* Managed in performDeclaration() */
		/*IDENTIFIER
		| identifier_list ',' IDENTIFIER */
		break;

	case NT_TYPE_NAME :
		/* specifier_qualifier_list */
			/* Never here */
		/* | specifier_qualifier_list abstract_declarator */
		children[0]->typeCheck();
		type = children[0]->type;
		getNameAndDepthPointer(children[1], &type);
		break;

	case NT_ABSTRACT_DECLARATOR :
		/* pointer */
			/* Never here */
		/* | direct_abstract_declarator */
			/* Never here */
		/* | pointer direct_abstract_declarator */
			/* FUTURE <arrays> */
		break;

	case NT_DIRECT_ABSTRACT_DECLARATOR :
		/* '(' abstract_declarator ')' */
			/* Never here */
		/* | '[' ']' */
			/* FUTURE <arrays> */
		/* | '[' constant_expression ']' */
			/* FUTURE <arrays> */
		/* | direct_abstract_declarator '[' ']' */
			/* FUTURE <arrays> */
		/* | direct_abstract_declarator '[' constant_expression ']' */
			/* FUTURE <arrays> */
		/* | '(' ')' */
			/* nothing to do here */
		/* | '(' parameter_type_list ')' */
			/* Never here */
		/* | direct_abstract_declarator '(' ')' */
			/* Never here */
		/* | direct_abstract_declarator '(' parameter_type_list ')' */
			/* FUTURE <arrays> */
		break;

	case NT_INITIALIZER :
		/* Managed in performDeclaration() */
		/*assignment_expression
		| '{' initializer_list '}'
		| '{' initializer_list ',' '}' */
		break;

	case NT_INITIALIZER_LIST :
		/* Managed in performDeclaration() */
		/*initializer
		| initializer_list ',' initializer */
		break;

	case NT_STATEMENT :
		/*labeled_statement
		| compound_statement
		| expression_statement
		| selection_statement
		| iteration_statement
		| jump_statement
		| error */
		if (rule != 7){
			/* Never here */
		} else {
			/* error */
			/* Nothing to do here */
		}
		break;

	case NT_LABELED_STATEMENT :
		checkLabeledStatement();
		break;

	case NT_COMPOUND_STATEMENT :
		switch(rule){
		case 1: /* '{' '}' */
			break;
		case 2: /* | '{' statement_list '}' */
			children[0]->typeCheck();
			type       = children[0]->type;
			returnDone = children[0]->returnDone;
			break;
		case 3: /* | '{' declaration_list '}' */
			children[0]->typeCheck();
			break;
		case 4: /* | '{' declaration_list statement_list '}' */
			children[0]->typeCheck();
			children[1]->typeCheck();
			type       = children[1]->type;
			returnDone = children[1]->returnDone;
			break;
		}
		break;

	case NT_DECLARATION_LIST :
		/*declaration
		| declaration_list declaration */
		for_each(children.begin(), children.end(), f_typeCheck);
		break;

	case NT_STATEMENT_LIST :
		/*statement
		| statement_list statement */
		for_each(children.begin(), children.end(), f_typeCheck);
		for_each(children.begin(), children.end(), f_returnCheck);
		break;

	case NT_EXPRESSION_STATEMENT :
		/* ';' */
		/* | expression ';' */
		/* Nothing to do here */
		break;

	case NT_SELECTION_STATEMENT :
		checkSelectionStatement();
		break;

	case NT_ITERATION_STATEMENT :
		checkIterationStatement();
		break;

	case NT_JUMP_STATEMENT :
		checkJumpStatement();
		break;

	case NT_TRANSLATION_UNIT :
		/* external_declaration */
		/* | translation_unit external_declaration */
		for_each(children.begin(), children.end(), f_typeCheck);
		break;

	case NT_EXTERNAL_DECLARATION :
		/* function_definition
		   | declaration
		   | error */
		if (rule != 3){
			/* Never here */
		} else {
			/* error */
			/* Nothing to do here */
		}
		break;

	case NT_FUNCTION_DEFINITION :
		performDefinition();
		break;

	default:
		/* Error! Node symbol unknown */
		addError(ERR_SEM_MSG_10, "", line);
	}

	return &type;
}



/**
* checkPrimaryExpression
* 
* Aux function. Perform the type check of a primary expression.
*/
void SyntaxTreeNode::checkPrimaryExpression(){
	switch (rule){
	case 1: /* IDENTIFIER*/
		lex = children[0]->lex;
		if (symbolsTable.existsSymbol(lex, symbolsTable.currentFramework)){
			children[0]->type = symbolsTable.getType(lex, symbolsTable.currentFramework);
			type = children[0]->type;
			symbolsTable.incNUses(lex, symbolsTable.currentFramework, 1);
			if (symbolsTable.isLiteral(lex, symbolsTable.currentFramework)){
				valueIsUsed = true;
				value       = symbolsTable.getAddress(lex, symbolsTable.currentFramework);
			}
		}else if (symbolsTable.existsSymbol(lex, GLOBAL_FRAMEWORK)){
			children[0]->type = symbolsTable.getType(lex, GLOBAL_FRAMEWORK);
			type = children[0]->type;
			symbolsTable.incNUses(lex, GLOBAL_FRAMEWORK, 1);
			if (symbolsTable.isLiteral(lex, GLOBAL_FRAMEWORK)){
				valueIsUsed = true;
				value       = symbolsTable.getAddress(lex, GLOBAL_FRAMEWORK);
			}
		}else if (symbolsTable.existsFramework(lex)){
			type = symbolsTable.getOutputType(lex);
			symbolsTable.addDependency(symbolsTable.currentFramework, lex);
		}else{
			type.type = TYPE_ERR;
			addError(ERR_SEM_MSG_00, children[0]->lex, line);
		}
		break;
	case 2: /* | CONSTANT*/
		children[0]->typeCheck();
		type = children[0]->type;
		if (children[0]->valueIsUsed){
			valueIsUsed = true;
			value = children[0]->value;
		}
		break;
	case 3: /* | STRING_LITERAL */
		type = children[0]->type;
		break;
	case 4: /* | '(' expression ')' */
		 /* Never here */
		break;
	}
}



/**
* checkPostfixExpression
* 
* Aux function. Perform the type check of a postfix expression.
*/
void SyntaxTreeNode::checkPostfixExpression(){
	children[0]->typeCheck();
	lex = children[0]->lex;

	switch (rule) {
	case 1: /* primary_expression */
		/* Never here */
		break;
	case 2: /* | postfix_expression '[' expression ']' */
		/* FUTURE <arrays> Don't forget to inherit the lex to use the '&' operator */
		break;
	case 3: /* | postfix_expression '(' ')' */
		/*
		--Deprecated at version v1.0.4--
		if (lex == symbolsTable.currentFramework){
			addError(ERR_SEM_MSG_16, lex, line);
		}*/
		if (symbolsTable.existsFramework(lex)){
			if (symbolsTable.getInputType(lex).empty()){
				type = symbolsTable.getOutputType(lex);
			}else{
				addError(ERR_SEM_MSG_01, lex, line);
			}
		}else{
			addError(ERR_SEM_MSG_02, lex, line);
		}
		break;
	case 4: /* | postfix_expression '(' argument_expression_list ')' */
		/*
		--Deprecated at version v1.0.4--
		if (lex == symbolsTable.currentFramework){
			addError(ERR_SEM_MSG_16, lex, line);
		}*/
		children[1]->typeCheck();
		if (symbolsTable.existsFramework(lex)){
			if (checkInputTypeCorrectness(lex, children[1])){
				type = symbolsTable.getOutputType(lex);
			} else {
				addError(ERR_SEM_MSG_01, lex, line);
			}
		}else{
			addError(ERR_SEM_MSG_02, lex, line);
		}
		break;
	case 5: /* | postfix_expression '.' IDENTIFIER */
		/* FUTURE <structs> */
		break;
	case 6: /* | postfix_expression PTR_OP IDENTIFIER */
		/* FUTURE <structs> */
		break;
	case 7: /* | postfix_expression INC_OP */
		checkConstantProperty(children[0]->lex, line, false);
		type = children[0]->type;
		if (children[0]->valueIsUsed){
			valueIsUsed = true;
			value       = children[0]->value;
		}
		break;
	case 8: /* | postfix_expression DEC_OP */
		checkConstantProperty(children[0]->lex, line, false);
		type = children[0]->type;
		if (children[0]->valueIsUsed){
			valueIsUsed = true;
			value       = children[0]->value;
		}
		break;
	}
}



/**
* checkArithmeticExpression
* 
* Aux function. Perform the type check of a arithmetic expression.
*/
void SyntaxTreeNode::checkArithmeticExpression(){
	switch (symbol - syntaxTree.getNTerminals()) {
	case NT_UNARY_EXPRESSION :
		switch (rule) {
		case 1: /*postfix_expression */
			/* Never here */
			break;
		case 2:	/*| INC_OP unary_expression */
			children[0]->typeCheck();
			checkConstantProperty(children[0]->lex, line, false);
			type = children[0]->type;
			if (children[0]->valueIsUsed){
				valueIsUsed = true;
				value       = (children[0]->value) + 1;
			}
			break;
		case 3:	/*| DEC_OP unary_expression */
			children[0]->typeCheck();
			checkConstantProperty(children[0]->lex, line, false);
			type = children[0]->type;
			if (children[0]->valueIsUsed){
				valueIsUsed = true;
				value       = (children[0]->value) - 1;
			}
			break;
		case 4:	/*| unary_operator cast_expression */
			children[1]->typeCheck();
			if(!constShortcut){
				promoteTypeLiteral(children[1], children[1], NULL);
			}
			children[0]->lex         = children[1]->lex;
			children[0]->type        = children[1]->type;
			children[0]->valueIsUsed = children[1]->valueIsUsed;
			children[0]->value       = children[1]->value;
			children[0]->typeCheck();
			lex         = children[0]->lex;
			type        = children[0]->type;
			value       = children[0]->value;
			valueIsUsed = children[0]->valueIsUsed;
			break;
		case 5:	/*| SIZEOF unary_expression */
			children[0]->typeCheck();
			type.type = TYPE_LITERAL;
			value = symbolsTable.getTypeSize(children[0]->type);
			if (value == -1)
				addError(ERR_SEM_MSG_03, children[0]->type.toString(), line);
			else
				valueIsUsed = true;
			break;
		case 6:	/*| SIZEOF '(' type_name ')' */
			children[0]->typeCheck();
			type.type = TYPE_LITERAL;
			value = symbolsTable.getTypeSize(children[0]->type);
			if (value == -1)
				addError(ERR_SEM_MSG_03, children[0]->type.toString(), line);
			else
				valueIsUsed = true;
			break;
		}
		break;

	case NT_UNARY_OPERATOR :
		if(!constShortcut){
			promoteTypeLiteral(this, this, NULL);
		}
		switch (rule) {
			/* Inherited type from the next expression */
		case 1: /* '&' Address of */
			type.pointerDepth++;
			/* The pointers to functions are not allowed! */
			if (lex == "" || !symbolsTable.existsSymbol(lex, symbolsTable.currentFramework))
				addError(ERR_SEM_MSG_06, "&", line);
			else
				lex = "";
			valueIsUsed = false;
			break;
		case 2: /* | '*' Pointer to  */
			type.pointerDepth--;
			valueIsUsed = false;
			break;
		case 3: /* | '+' */
			/* Same type */
			break;
		case 4: /* | '-' */
			if (type.pointerDepth > 0)
				addError(ERR_SEM_MSG_06, "-(" + type.toString() + ")", line);
			type.b_unsigned = false;
			if (valueIsUsed) value = -value;
			break;
		case 5: /* | '~' */
			/* Same type: Be careful in the code generation */
			if (symbolsTable.getTypeSize(type) > 1)
				addError(ERR_SEM_MSG_05, "~", line);
			if (valueIsUsed) value = ~(char)value;
			break;
		case 6: /* | '!' */
			/* Same type */
			if (symbolsTable.getTypeSize(type) > 1)
				addError(ERR_SEM_MSG_05, "!", line);
			if (valueIsUsed) value = !(char)value;
			break;
		}
		break;

	case NT_CAST_EXPRESSION :
		/*unary_expression */
			/* Never here */
		/* | '(' type_name ')' cast_expression */
		children[0]->typeCheck();
		children[1]->typeCheck();
		type = children[0]->type;
		if (children[1]->valueIsUsed){
			valueIsUsed = true;
			value       = children[1]->value;
		}

		if (symbolsTable.getTypeSize(type) < symbolsTable.getTypeSize(children[1]->type))
			addWarning(WAR_SEM_MSG_06, "cast (" + type.toString() + ") " + children[1]->type.toString(), line);
		break;

	case NT_MULTIPLICATIVE_EXPRESSION :
		/* Never here */
		/*cast_expression
		| multiplicative_expression '*' cast_expression
		| multiplicative_expression '/' cast_expression
		| multiplicative_expression '%' cast_expression */
		break;

	case NT_ADDITIVE_EXPRESSION :
		switch (rule) {
		case 1: /*multiplicative_expression
			/* Never here */
		case 2: /* | additive_expression '+' multiplicative_expression */
			auxCheckExpression(plus<int>(), "+", false, true);
			break;
		case 3: /* | additive_expression '-' multiplicative_expression */
			auxCheckExpression(minus<int>(), "-", false, true);
			break;
		}
		break;

	case NT_SHIFT_EXPRESSION :
		switch (rule) {
		case 1: /* additive_expression */
			/* Never here */
			break;
		case 2: /* | shift_expression LEFT_OP additive_expression */
			auxCheckExpression(bitwise_left<char>(), "<<", false, true);
			if (symbolsTable.getTypeSize(children[0]->type) > 1)
				addError(ERR_SEM_MSG_05, "<<", line);
			if (children[1]->valueIsUsed){
				if (children[1]->value > 7 || children[1]->value < 0){
					addWarning(WAR_SEM_MSG_00, "<<", line);
					value = 0;
				}
			}
			type = children[0]->type;
			break;
		case 3: /* | shift_expression RIGHT_OP additive_expression */
			auxCheckExpression(bitwise_right<char>(), ">>", false, true);
			if (symbolsTable.getTypeSize(children[0]->type) > 1)
				addError(ERR_SEM_MSG_05, ">>", line);
			if (children[1]->valueIsUsed){
				if (children[1]->value > 7 || children[1]->value < 0){
					addWarning(WAR_SEM_MSG_00, ">>", line);
					value = 0;
				}
			}
			type = children[0]->type;
			break;
		}
		break;
	}
}



/**
* checkRelationalExpression
* 
* Aux function. Perform the type check of a relational expression.
*/
void SyntaxTreeNode::checkRelationalExpression(){
	switch (symbol - syntaxTree.getNTerminals()) {
	case NT_RELATIONAL_EXPRESSION :
		switch (rule) {
		case 1: /* shift_expression */
			/* Never here */
			break;
		case 2: /* | relational_expression '<' shift_expression */
			auxCheckExpression(less<char>(), "<", false, true);
			type.type = "char";
			break;
		case 3: /* | relational_expression '>' shift_expression */
			auxCheckExpression(greater<char>(), ">", false, true);
			type.type = "char";
			break;
		case 4: /* | relational_expression LE_OP shift_expression */
			auxCheckExpression(less_equal<char>(), "<=", false, true);
			type.type = "char";
			break;
		case 5: /* | relational_expression GE_OP shift_expression */
			auxCheckExpression(greater_equal<char>(), ">=", false, true);
			type.type = "char";
			break;
		}
		break;

	case NT_EQUALITY_EXPRESSION :
		switch (rule) {
		case 1: /* relational_expression */
			/* Never here */
			break;
		case 2: /* | equality_expression EQ_OP relational_expression */
			auxCheckExpression(equal_to<char>(), "==", false, true);
			type.type = "char";
			break;
		case 3: /* | equality_expression NE_OP relational_expression */
			auxCheckExpression(not_equal_to<char>(), "!=", false, true);
			type.type = "char";
			break;
		}
		break;
	}
}



/**
* checkLogicalExpression
* 
* Aux function. Perform the type check of a logical expression.
*/
void SyntaxTreeNode::checkLogicalExpression(){
	switch (symbol - syntaxTree.getNTerminals()) {
	case NT_AND_EXPRESSION :
		switch (rule) {
		case 1: /* equality_expression */
			/* Never here */
			break;
		case 2: /* | and_expression '&' equality_expression */
			auxCheckExpression(numerical_and<int>(), "&", false, true);
			break;
		}
		break;

	case NT_EXCLUSIVE_OR_EXPRESSION :
		switch (rule) {
		case 1: /* and_expression */
			/* Never here */
			break;
		case 2: /* | exclusive_or_expression '^' and_expression */
			auxCheckExpression(numerical_eor<int>(), "^", false, true);
			break;
		}
		break;

	case NT_INCLUSIVE_OR_EXPRESSION :
		switch (rule) {
		case 1: /* exclusive_or_expression */
			/* Never here */
			break;
		case 2: /* | inclusive_or_expression '|' exclusive_or_expression */
			auxCheckExpression(numerical_or<int>(), "|", false, true);
			break;
		}
		break;

	case NT_LOGICAL_AND_EXPRESSION :
		switch (rule) {
		case 1: /* inclusive_or_expression */
			/* Never here */
			break;
		case 2: /* | logical_and_expression AND_OP inclusive_or_expression */
			auxCheckExpression(logical_and<char>(), "&&", false, true);
			type.type = "char";
			break;
		}
		break;

	case NT_LOGICAL_OR_EXPRESSION :
		switch (rule) {
		case 1: /* logical_and_expression */
			/* Never here */
			break;
		case 2: /* | logical_or_expression OR_OP logical_and_expression */
			auxCheckExpression(logical_or<char>(), "||", false, true);
			type.type = "char";
			break;
		}
		break;
	}
}



/**
* checkConditionalExpression
* 
* Aux function. Perform the type check of a conditional expression.
*/
void SyntaxTreeNode::checkConditionalExpression(){
	switch (rule) {
	case 1: /* logical_or_expression */
		/* Never here */
		break;
	case 2: /* | logical_or_expression '?' expression ':' conditional_expression */
		children[0]->typeCheck();
		children[1]->typeCheck();
		children[2]->typeCheck();
		if (children[1]->type.sameOutput(children[2]->type))
			type = promoteExpressionType(children[1]->type, children[2]->type, line);
		else
			addError(ERR_SEM_MSG_06, "?", line);
		break;
	}
}



/**
* checkAssignmentExpression
* 
* Aux function. Perform the type check of a assignment expression.
*/
void SyntaxTreeNode::checkAssignmentExpression(){
	string auxBuf;
	bool bypass = false;

	switch (rule) {
	case 1: /* conditional_expression */
		/* Never here */
		break;
	case 2: /* | unary_expression assignment_operator assignment_expression */
		SyntaxTreeNode *temp = children[1];
		children[1] = children[2];
		switch(temp->rule){

		case 1: 
			auxCheckExpression(assign<int>(), "=" , true, false);
			if (children[2]->valueIsUsed){
				valueIsUsed = true;
				value       = children[2]->value;
			}else if(/* children[0]->symbol - syntaxTree.getNTerminals() == NT_UNARY_EXPRESSION && */
				children[0]->children[0]->symbol - syntaxTree.getNTerminals() ==  NT_UNARY_OPERATOR &&
				children[0]->children[0]->rule == 2){
				/*
				(*x) = ...   => A bypass can be performed to allow overlap 
				*/
				bypass = true;
			}else if(/* children[2]->symbol - syntaxTree.getNTerminals() == NT_UNARY_EXPRESSION && */
				children[2]->children[0]->symbol - syntaxTree.getNTerminals() ==  NT_UNARY_OPERATOR &&
				children[2]->children[0]->rule == 1){
				/*
				... = (&x)   => A bypass can be performed to allow overlap 
				*/
				bypass = true;
			}
			break;
		case 5:  auxCheckExpression(plus<int>()         , "+=", true, true); break;
		case 6:  auxCheckExpression(minus<int>()        , "-=", true, true); break;
		case 7:
		case 8:
			auxBuf = (children[1]->rule == 7) ? "<<=" : ">>=";
			if (children[1]->rule == 7)
				auxCheckExpression(bitwise_left<char>(), "<<=", true, true);
			else
				auxCheckExpression(bitwise_left<char>(), ">>=", true, true);
			if (symbolsTable.getTypeSize(children[0]->type) > 1)
				addError(ERR_SEM_MSG_05, auxBuf, line);
			if (children[1]->valueIsUsed){
				if (children[1]->value > 7 || children[1]->value < 0){
					addWarning(WAR_SEM_MSG_00, auxBuf, line);
					value = 0;
				}
			}
			break;
		case 9:  auxCheckExpression(numerical_and<int>(), "&=", true, true); break;
		case 10: auxCheckExpression(numerical_eor<int>(), "^=", true, true); break;
		case 11: auxCheckExpression(numerical_or<int>() , "|=", true, true); break;
		}
		children[1] = temp;
		checkConstantProperty(children[0]->lex, line, bypass);
		break;
	}
}



/**
* checkLabeledStatement
* 
* Aux function. Perform the type check of a labeled expression.
*/
void SyntaxTreeNode::checkLabeledStatement(){
	switch(rule){
	case 1: /* IDENTIFIER ':' statement */
		/* A jump-statement must reside in the same function and can appear before only one
		statement in the same function. The set of identifier names following a goto has
		its own name space so the names do not interfere with other identifiers. Labels
		cannot be redeclared. */
		if (!symbolsTable.addLabel(symbolsTable.currentFramework, children[0]->lex))
			addError(ERR_SEM_MSG_0C,
					 symbolsTable.currentFramework + ":" + children[0]->lex,
					 line);
		children[1]->typeCheck();
		returnDone = children[1]->returnDone;
		type       = children[1]->type;
		break;
	case 2: /* | CASE constant_expression ':' statement */
		children[0]->typeCheck();
		children[1]->typeCheck();
		returnDone = children[1]->returnDone;
		type       = promoteExpressionType(currCaseType.back(), children[0]->type, line);
		break;
	case 3: /* | DEFAULT ':' statement */
		children[0]->typeCheck();
		returnDone = children[0]->returnDone;
		type       = children[0]->type;
		break;
	}
}



/**
* checkSelectionStatement
* 
* Aux function. Perform the type check of a selection expression.
*/
void SyntaxTreeNode::checkSelectionStatement(){
	switch(rule){
	case 1: /* IF '(' expression ')' statement */
		children[0]->typeCheck();
		children[1]->typeCheck();
		/* @returnDone always false */
		if (children[0]->type.type != TYPE_ERR)
			type = children[1]->type;
		else
			type.type = TYPE_ERR;
		break;
	case 2: /* | IF '(' expression ')' statement ELSE statement */
		children[0]->typeCheck();
		children[1]->typeCheck();
		children[2]->typeCheck();
		returnDone = children[1]->returnDone && children[2]->returnDone;
		if ((children[0]->type.type == TYPE_ERR) ||
			(children[1]->type.type == TYPE_ERR) ||
			(children[2]->type.type == TYPE_ERR))
			type.type = TYPE_ERR;
		break;
	case 3: /* | SWITCH '(' expression ')' statement */
		children[0]->typeCheck();
		breakable++;

		currCaseType.push_back(promoteExpressionType(QualifiedType("char"), children[0]->type, line));

		children[1]->typeCheck();
		currCaseType.pop_back();
		breakable--;
		returnDone = children[1]->returnDone;
		if ((children[0]->type.type == TYPE_ERR) ||
			(children[1]->type.type == TYPE_ERR))
			type.type = TYPE_ERR;
		break;
	}
}



/**
* checkIterationStatement
* 
* Aux function. Perform the type check of a iteration expression.
*/
void SyntaxTreeNode::checkIterationStatement(){
	switch(rule){
	case 1: /* WHILE '(' expression ')' statement */
		children[0]->typeCheck();
		if (children[0]->valueIsUsed && !value)
			addWarning(WAR_SEM_MSG_04, "while", line);
		else{
			continuable++;
			breakable++;
			children[1]->typeCheck();
			continuable--;
			breakable--;
			returnDone = children[1]->returnDone;
			type       = children[1]->type;
		}
		break;
	case 2: /* | DO statement WHILE '(' expression ')' ';' */
		continuable++;
		breakable++;
		children[0]->typeCheck();
		continuable--;
		breakable--;
		children[1]->typeCheck();
		returnDone = children[0]->returnDone;
		if (symbolsTable.getTypeSize(children[1]->type) != 1){
			addError(ERR_SEM_MSG_05, "do-while(expression)", line);
			type.type = TYPE_ERR;
		}else
			type = children[0]->type;
		break;
	case 3:	/* | FOR '(' expression_statement expression_statement ')' statement */
		children[0]->typeCheck();
		children[1]->typeCheck();
		if(children[1]->valueIsUsed && !children[1]->value){
			/* loop condition always false */
			addWarning(WAR_SEM_MSG_04, "for", line);
			type = children[0]->type;
		}else{
			/* if (children[1]->symbol - syntaxTree.getNTerminals() == NT_EXPRESSION_STATEMENT)
				/* loop condition always true */
			continuable++;
			breakable++;
			children[2]->typeCheck();
			continuable--;
			breakable--;
			returnDone = children[2]->returnDone;
			if ((children[0]->type.type != TYPE_ERR) && (children[1]->type.type != TYPE_ERR))
				type = children[2]->type;
			else
				type.type = TYPE_ERR;
		}
		break;
	case 4:	/* | FOR '(' expression_statement expression_statement expression ')' statement */
		children[0]->typeCheck();
		children[1]->typeCheck();
		children[2]->typeCheck();
		if(children[1]->valueIsUsed && !children[1]->value){
			/* loop condition always false */
			addWarning(WAR_SEM_MSG_04, "for", line);
			if ((children[0]->type.type != TYPE_ERR) && (children[2]->type.type != TYPE_ERR))
				type = children[0]->type;
			else
				type.type = TYPE_ERR;
		}else{
			/* if (children[1]->symbol - syntaxTree.getNTerminals() == NT_EXPRESSION_STATEMENT)
				/* loop condition always true */
			continuable++;
			breakable++;
			children[3]->typeCheck();
			continuable--;
			breakable--;
			returnDone = children[3]->returnDone;
			if ((children[0]->type.type != TYPE_ERR) &&
				(children[1]->type.type != TYPE_ERR) &&
				(children[2]->type.type != TYPE_ERR))
				type = children[3]->type;
			else
				type.type = TYPE_ERR;
		}
		break;
	}
}



/**
* checkJumpStatement
* 
* Aux function. Perform the type check of a jump expression.
*/
void SyntaxTreeNode::checkJumpStatement(){
	switch(rule){
	case 1:	/* GOTO IDENTIFIER ';' */
		labelsExpected.push_back(make_pair(children[0]->lex, line));
		break;
	case 2:	/* | CONTINUE ';' */
		if (continuable < 1)
			addError(ERR_SEM_MSG_0D, "continue", line);
		break;
	case 3:	/* | BREAK ';' */
		if (breakable < 1)
			addError(ERR_SEM_MSG_0D, "break", line);
		break;
	case 4:	/* | RETURN ';' */
		returnDone = true;
		if (symbolsTable.getTypeSize(symbolsTable.getOutputType(symbolsTable.currentFramework)) != 0)
			addWarning(WAR_SEM_MSG_03, "return;", line);
		break;
	case 5:	/* | RETURN expression ';' */
		children[0]->typeCheck();
		type = children[0]->type;
		returnDone = true;
		QualifiedType fraweworkType = symbolsTable.getOutputType(symbolsTable.currentFramework);
		if (!fraweworkType.sameOutput(children[0]->type))
			addError(ERR_SEM_MSG_06,
					 "return " + type.toString() + " != " + fraweworkType.toString(),
					 line);
		symbolsTable.incNUses(SEM_RETURN_SYMBOL, symbolsTable.currentFramework, 1);
		break;
	}
}



/**
* extractQualifiedType
* 
* Extract all the type information stored in a node and it children.
*
* @param node: Node which contains the children with all type qualifiers and specifiers.
*
* @return a QualifiedType containing all the relevant data.
*/
QualifiedType SyntaxTreeNode::extractQualifiedType (SyntaxTreeNode* node){
	QualifiedType output     = QualifiedType();
	bool          b_lengChkd = false;
	bool          b_signChkd = false;
	bool          b_long     = false;

	output.type = "";
	

	auto f_checkSpecifiers = [&b_lengChkd, &b_signChkd, &b_long, &output, this](SyntaxTreeNode* x){
		if ((x->symbol) - syntaxTree.getNTerminals() == NT_TYPE_QUALIFIER){
			/* type_qualifier */
			if((x->rule) == 1){
				/* CONST */
				if (output.b_const)
					addError(ERR_SEM_MSG_07, "", line);
				else
					output.b_const = true;
			}else if((x->rule) == 2){
				/* VOLATILE */
				if (output.b_volatile)
					addError(ERR_SEM_MSG_07, "", line);
				else
					output.b_volatile = true;
			}else{
				/* INTERRUPT */
				if (output.b_interrupt)
					addError(ERR_SEM_MSG_07, "", line);
				else
					output.b_interrupt = true;
			}
		}else if ((x->symbol) - syntaxTree.getNTerminals() == NT_TYPE_SPECIFIER){
			/* type_specifier */
			switch(x->rule){

			case 1:
				if (output.type != "")
					addError(ERR_SEM_MSG_07, "void", line);
				else
					output.type = "void";
				break;

			case 2:
				if (output.type != "")
					addError(ERR_SEM_MSG_07, "char", line);
				else
					output.type = "char";
				break;

			case 3:
				if (b_lengChkd)
					addError(ERR_SEM_MSG_07, "short", line);
				else
					b_lengChkd = true;
				break;

			case 4:
				if (output.type != "")
					addError(ERR_SEM_MSG_07, "int", line);
				else
					output.type = "int";
				break;

			case 5:
				if (b_lengChkd)
					addError(ERR_SEM_MSG_07, "long", line);
				else{
					b_lengChkd = true;
					b_long     = true;
				}
				break;

			case 6: /* Not supported by 6502 */
			case 7: /* Not supported by 6502 */
				break;

			case 8:
				if (b_signChkd)
					addError(ERR_SEM_MSG_07, "signed", line);
				else
					b_signChkd = true;
				break;

			case 9:
				if (b_signChkd)
					addError(ERR_SEM_MSG_07, "unsigned", line);
				else{
					b_signChkd = true;
					output.b_unsigned = true;
				}
				break;

			case 10: /* FUTURE <structs> */
			case 11: /* FUTURE <enum> */
				break;

			case 12:
				if (output.type != "")
					addError(ERR_SEM_MSG_07, x->lex, line);
				else
					output.type = x->lex;
				break;

			}

		}else{
			/* FUTURE <pre_compiler> */ /* FUTURE <register> */ /* storage_class_specifier */
		}
	};


	auto f_correctSpecifiers = [&b_lengChkd, &output, &b_long, this](){
		if (b_lengChkd){
			if (output.type == "" || output.type == "int"){
				if (b_long)
					output.type = "int";
				else
					output.type = "char";
			}else{
				addError(ERR_SEM_MSG_07, output.type, line);
			}
		}
		if (output.type == "") addError(ERR_SEM_MSG_07, "", line);
	};


	for_each(node->children.begin(), node->children.end(), f_checkSpecifiers);
	f_correctSpecifiers();

	return output;
}



/**
* getNameAndDepthPointer
* 
* Extract the name and abstraction info stored in a node and it children.
*
* @param node: Node which contains the children with the identifier and the abstract declaration info.
* @param newType: Input, output QualifiedType. Used to overwrite the pointer depth extracted here.
* @param paramList: Node containing the parameter list to be used in a function definition.
*
* @return a string containing the name of the identifier.
*/
string SyntaxTreeNode::getNameAndDepthPointer(SyntaxTreeNode* node, QualifiedType* newType, SyntaxTreeNode* &paramList){
	string output = getNameAndDepthPointer(node, newType);

	if ((node->symbol) - syntaxTree.getNTerminals() == NT_DIRECT_DECLARATOR){
		if (node->children.size() > 1)
			paramList = node->children[1];
	}else if (node->children.size() > 1){
		if (!syntaxTree.isIdentifier(node->children[1]->symbol))
			if (node->children[1]->children.size() > 1)
				paramList = node->children[1]->children[1];
	}

	return output;
}



/**
* getNameAndDepthPointer
* 
* Extract the name and abstraction info stored in a node and it children.
*
* @param node: Node which contains the children with the identifier and the abstract declaration info.
* @param newType: Input, output QualifiedType. Used to overwrite the pointer depth extracted here.
*
* @return a string containing the name of the identifier.
*/
string SyntaxTreeNode::getNameAndDepthPointer(SyntaxTreeNode* node, QualifiedType* newType){
	string output = "";


	auto f_calculateDepth = [&newType](SyntaxTreeNode* x){
		SyntaxTreeNode* auxNode = x;
		newType->pointerDepth = 1;
		while (auxNode->children.size() > 0){
			auxNode = auxNode->children[0];
			(newType->pointerDepth)++;
		}
	};


	if (syntaxTree.isIdentifier(node->symbol)){
		output = node->lex;
	}else if ((node->symbol) - syntaxTree.getNTerminals() == NT_DIRECT_DECLARATOR){
		output = node->children[0]->lex;
	}else if ((node->symbol) - syntaxTree.getNTerminals() == NT_POINTER){
		f_calculateDepth(node);
	}else if ((node->symbol) - syntaxTree.getNTerminals() == NT_INIT_DECLARATOR){
		output = getNameAndDepthPointer(node->children[0], newType);
	}else{
		f_calculateDepth(node->children[0]);
		if (node->children.size() > 1){
			if (syntaxTree.isIdentifier(node->children[1]->symbol))
				output = node->children[1]->lex;
			else{
				output = node->children[1]->children[0]->lex;
			}
		}
	}

	return output;
}



/**
* performDefinition
* 
* Take a NT_FUNCTION_DEFINITION node and perform the declarations into the symbols table.
*/
void SyntaxTreeNode::performDefinition(){
	string              name;
	QualifiedType       newType;
	SyntaxTreeNode*     paramList = NULL;
	list<QualifiedType> prevInputTypes;
	list<QualifiedType> inputTypes;

	switch(rule){
	case 1:
		/* declaration_specifiers declarator declaration_list compound_statement */
		newType = extractQualifiedType(children[0]);
		name    = getNameAndDepthPointer(children[1], &newType, paramList);
		if (symbolsTable.existsFramework(name)){
			if (!(symbolsTable.getOutputType(name).sameOutput(newType)))
				addError(ERR_SEM_MSG_0B, name, line);
		}else{
			symbolsTable.addFramework(name);
			symbolsTable.setOutputType(name, newType);
		}

		prevInputTypes = symbolsTable.getInputType(name);
		symbolsTable.resetInputType(name);

		symbolsTable.currentFramework = name;
		if (paramList != NULL) paramList->typeCheck();
		labelsExpected.clear();
		children[3]->typeCheck();
		checkLabelsDefinition();
		symbolsTable.currentFramework = GLOBAL_FRAMEWORK;

		inputTypes = symbolsTable.getInputType(name);

		if (symbolsTable.getTypeSize(newType) != 0 && !children[3]->returnDone)
			addWarning(WAR_SEM_MSG_03, name, line);
		break;
	case 2:
		/* | declaration_specifiers declarator compound_statement */
		newType = extractQualifiedType(children[0]);
		name    = getNameAndDepthPointer(children[1], &newType, paramList);

		if (symbolsTable.existsFramework(name)){
			if (!(symbolsTable.getOutputType(name).sameOutput(newType)))
				addError(ERR_SEM_MSG_0B, name, line);
		}else{
			symbolsTable.addFramework(name);
			symbolsTable.setOutputType(name, newType);
		}

		prevInputTypes = symbolsTable.getInputType(name);
		symbolsTable.resetInputType(name);

		symbolsTable.currentFramework = name;
		if (paramList != NULL) paramList->typeCheck();
		labelsExpected.clear();
		children[2]->typeCheck();
		checkLabelsDefinition();
		symbolsTable.currentFramework = GLOBAL_FRAMEWORK;

		inputTypes = symbolsTable.getInputType(name);

		if (symbolsTable.getTypeSize(newType) != 0 && !children[2]->returnDone)
			addWarning(WAR_SEM_MSG_03, name, line);
		break;
	case 3:
		/* | declarator declaration_list compound_statement */
		newType = QualifiedType("int");
		name    = getNameAndDepthPointer(children[0], &newType, paramList);

		if (symbolsTable.existsFramework(name)){
			if (!(symbolsTable.getOutputType(name).sameOutput(newType)))
				addError(ERR_SEM_MSG_0B, name, line);
		}else{
			symbolsTable.addFramework(name);
			symbolsTable.setOutputType(name, newType);
		}

		prevInputTypes = symbolsTable.getInputType(name);
		symbolsTable.resetInputType(name);

		symbolsTable.currentFramework = name;
		if (paramList != NULL) paramList->typeCheck();
		labelsExpected.clear();
		children[2]->typeCheck();
		checkLabelsDefinition();
		symbolsTable.currentFramework = GLOBAL_FRAMEWORK;

		inputTypes = symbolsTable.getInputType(name);

		if (!children[2]->returnDone)
			addWarning(WAR_SEM_MSG_03, name, line);
		break;
	case 4:
		/* | declarator compound_statement */
		newType = QualifiedType("int");
		name    = getNameAndDepthPointer(children[0], &newType, paramList);

		if (symbolsTable.existsFramework(name)){
			if (!(symbolsTable.getOutputType(name).sameOutput(newType)))
				addError(ERR_SEM_MSG_0B, name, line);
		}else{
			symbolsTable.addFramework(name);
			symbolsTable.setOutputType(name, newType);
		}

		prevInputTypes = symbolsTable.getInputType(name);
		symbolsTable.resetInputType(name);

		symbolsTable.currentFramework = name;
		if (paramList != NULL) paramList->typeCheck();
		labelsExpected.clear();
		children[1]->typeCheck();
		checkLabelsDefinition();
		symbolsTable.currentFramework = GLOBAL_FRAMEWORK;

		inputTypes = symbolsTable.getInputType(name);

		if (!children[1]->returnDone)
			addWarning(WAR_SEM_MSG_03, name, line);
		break;
	}

	/* Tag the function name */
	lex = name;

	/* Add the "return" symbol */
	if(symbolsTable.getTypeSize(newType) > 0)
		symbolsTable.addSymbol(SEM_RETURN_SYMBOL, newType, name);

	/* Check if the definition have the same inputs as the declaration */
 	if (prevInputTypes.size() == 0){
		symbolsTable.setAsDefined(name);
		return;
	}
	if (prevInputTypes.size() != inputTypes.size()){
		addError(ERR_SEM_MSG_0B, name, line);
		return;
	}
	
	list<QualifiedType>::iterator it1, it2;
	for(it1 = prevInputTypes.begin(), it2 = inputTypes.begin() ; it1 != prevInputTypes.end() ; ++it1, ++it2){
		if (!(it1->sameInput(*it2))){
			addError(ERR_SEM_MSG_0B, name, line);
		}
	}

	symbolsTable.setAsDefined(name);
}



/**
* performDeclaration
* 
* Take a NT_DECLARATION node and perform the declaration into the symbols table.
*/
void SyntaxTreeNode::performDeclaration(){

	if (isNewFramework()){
		/* Is new framework */
		performFrameworkDeclaration();
	}else{
		/* Is new variable */
		performVariableDeclaration();
	}
}



/**
* performFrameworkDeclaration
* 
* Take a NT_DECLARATION node representing a framework declaration and perform it into the symbols table.
*/
void SyntaxTreeNode::performFrameworkDeclaration(){
	string          name;
	SyntaxTreeNode* directDeclarator;
	QualifiedType   newType;


	auto f_declareArgument = [&newType, &name, this](SyntaxTreeNode* x){
		newType = extractQualifiedType(x->children[0]); 

		if (x->children.size() > 1)
			getNameAndDepthPointer(x->children[1], &newType);

		symbolsTable.addInputType(name, newType);
	};


	/* Check children[0] with extractQualifiedType */
	newType = extractQualifiedType(children[0]);

	/* if children[1]->children[0] is a direct_declarator, all right */
	/* if children[1]->children[0] is a declarator, children[0] is pointerDepth */
	/*                                              children[1] is direct_declarator */
	
	if (children[1]->children[0]->symbol - syntaxTree.getNTerminals() == NT_DIRECT_DECLARATOR)
		directDeclarator = children[1]->children[0];
	else{
		getNameAndDepthPointer(children[1]->children[0]->children[0], &newType);
		directDeclarator = children[1]->children[0]->children[1];
	}

	/* The children[0] of direct_declarator is the IDENTIFIER method name */
	name = directDeclarator->children[0]->lex;

	/* Declare new framework with the name, the output type and the 'return' symbol */
	symbolsTable.addFramework(name);
	symbolsTable.setOutputType(name, newType);
	if(symbolsTable.getTypeSize(newType) > 0)
		symbolsTable.addSymbol(SEM_RETURN_SYMBOL, newType, name);
	lex = name;

	/* The children[1] is parameter_list ... */
	if (directDeclarator->children.size() > 1){
		for_each(directDeclarator->children[1]->children.begin(),
				 directDeclarator->children[1]->children.end(),
				 f_declareArgument);
	}
}



/**
* performVariableDeclaration
* 
* Take a NT_DECLARATION node representing a variable declaration and perform it into the symbols table.
*/
void SyntaxTreeNode::performVariableDeclaration(){
	/* FUTURE <pre_compiler> deal with typedef, extern and static (storage_class_specifier) */
	/* FUTURE <enum> deal with enum (type_specifier) */
	/* FUTURE <structs> deal with struct and union (struct_or_union_specifier) */
	/* FUTURE <arrays> deal with arrays (direct_declarator) */
	QualifiedType newType;
	bool          isFrameworkInput  = false;

	auto f_declareVars = [&newType, &isFrameworkInput, this](SyntaxTreeNode* x){
		string        name     = "";
		int           auxDepth = newType.pointerDepth;
		QualifiedType copyType = newType;

		name = getNameAndDepthPointer(x, &newType);
		lex  = name;
		
		if (symbolsTable.existsSymbol(name, symbolsTable.currentFramework))
			addError(ERR_SEM_MSG_09, name, line);
		else{
			symbolsTable.addSymbol(name, newType, symbolsTable.currentFramework);
			
			/* FUTURE <initializer> */
			if ((x->symbol) - syntaxTree.getNTerminals() == NT_INIT_DECLARATOR){
				x->children[0]->type = newType;
				x->children[1]->typeCheck();
				
				x->auxCheckExpression(assign<int>(), "=" , true, false);

				if(x->children[1]->type.type != x->children[0]->type.type){
					x->children[1]->type = x->children[0]->type;
				}

				if (constShortcut && newType.b_const && !newType.b_volatile && x->children[1]->valueIsUsed){
					/* Symbol value is const, known in compilation time and can be optimized */
					symbolsTable.setLiteral(name, symbolsTable.currentFramework);
					symbolsTable.setAddress(name, symbolsTable.currentFramework, x->children[1]->value);
				}
			}

			if (isFrameworkInput){
				STEntry* newSymbol = symbolsTable.getSymbol(name, symbolsTable.currentFramework);

				symbolsTable.addInputType(symbolsTable.currentFramework, newType, name);

				/* If an argument (framework input) is const or input/output, it may be memory overlapped (see asmGen.cpp) */
				if (newSymbol->isConst() || newSymbol->getPointerDepth() > 0){
					newSymbol->setOverlappable(true);
				}
			}
		}

		newType.pointerDepth = auxDepth;
	};


	/* Check type */
	newType = extractQualifiedType(children[0]);
	if (newType.b_interrupt)
		addError(ERR_SEM_MSG_08, "", line);

	/* Declare into the symbols table */
	if (symbol - syntaxTree.getNTerminals() != NT_PARAMETER_DECLARATION)
		for_each (children[1]->children.begin(), children[1]->children.end(), f_declareVars);
	else{
		isFrameworkInput = true;
		f_declareVars(children[1]);
	}
}



/**
* isNewFramework
* 
* Take a NT_DECLARATION node and check if is a variable declaration or a method declaration.
*
* @return true if is a method declaration.
*/
bool SyntaxTreeNode::isNewFramework(){
	bool            exit       = false;
	SyntaxTreeNode* declarator = (children[1]->children[0]);

	string aux;

	if (declarator->symbol - syntaxTree.getNTerminals() == NT_DIRECT_DECLARATOR) exit = true;
	if (declarator->symbol - syntaxTree.getNTerminals() == NT_DECLARATOR)
		if (declarator->children[1]->symbol - syntaxTree.getNTerminals() == NT_DIRECT_DECLARATOR)
			exit = true;

	return exit;
}



/**
* checkInputTypeCorrectness
* 
* Takes an argument list and a framework name and check if the types in the list are the expected by a
* call to the function represents that framework.
*
* @param name: Function which operates over the children nodes values.
* @param nodeList: String with the operator symbol. Used to print errors.
*
* @return true if the call is properly performed.
*/
bool SyntaxTreeNode::checkInputTypeCorrectness(const string &name, SyntaxTreeNode* nodeList){
	list<QualifiedType> typesExpected = symbolsTable.getInputType(name);
	unsigned int        nArguments    = nodeList->children.size();
	bool                exit          = true;
	int                 i             = 0;

	auto f_checkTypeCorrectness = [&i, &exit, &nodeList](const QualifiedType &x){
		if (!nodeList->children[i]->type.sameInput(x)) exit = false;
		i++;
	};


	if (typesExpected.size() != nArguments)
		return false;

	for_each(typesExpected.begin(), typesExpected.end(), f_checkTypeCorrectness);
	return exit;
}



/**
* toString
* 
* Same function as toString() but with some whitespace at the left.
*
* @param depth: Number of tab spaces to print at the left of each node.
*/
string SyntaxTreeNode::toString(const string &left){
	ostringstream output;
	int           size = children.size();

	auto f_concat = [&output, &size, &left](SyntaxTreeNode* const x){
		size--;
		if (size > 0)
			output << x->toString(left + "|  ");
		else
			output << x->toString(left + "   ");
	};


	output << left.substr(0, (left.size()-3)) << "+-";
	output << syntaxTree.getSymbolName(symbol);
	output << " : (" << type.toString() << ")";

	if (lex != "") output << " (" << lex   << ")";
	if (valueIsUsed)                output << " (" << value << ")";

	output << endl;

	for_each(children.begin(), children.end(), f_concat);

	return output.str();
}




/**
* |------------------------------------------|
* |                                          |
* | Class: Parser                            |
* |                                          |
* |------------------------------------------|
*/

extern "C"
{
	void*      pInputFile;    /* Type ifstream     */
	void*      pSyntaxTree;   /* Type SyntaxTree   */

	extern int          getOffset();
	extern int          getNTokens();
	extern const char** getSymbolNameTable();

	extern int gcparse();

	extern int c_readsome (void*, char *, int);

	extern int n_chrcnt;
	extern int n_linecnt;
}



/**
* Parser
* 
* Class costructor.
*/
Parser::Parser(){
	offset          = getOffset();
	nTokens         = getNTokens();
	symbolNameTable = getSymbolNameTable();

	pSyntaxTree   = &syntaxTree;
}



/**
* ~Parser
* 
* Class destructor.
*/
Parser::~Parser(){

}



/**
* ~Parser
* 
* Perform the parsing tasks.
*/
int Parser::parse(ifstream* inputFile){
	pInputFile    = inputFile;
	return 	gcparse();;
}





/**
* |------------------------------------------|
* |                                          |
* | External C functions                     |
* |                                          |
* |------------------------------------------|
*/

/**
* c_readsome
*
* Bridge method to allow Flex using ifstreams
*
* @param inFile: The ifstream to load from.
* @param buf: Buffer where the content read will be stored.
* @param max: Size in characters of the block of data to be read.
*
* @return the number of characters read.
*/
#include <fstream>
int c_readsome (void* inFile, char *buf, int max){
	((ifstream*)inFile)->read(buf, max);
	return (int)((ifstream*)inFile)->gcount();
}

extern "C" 
{
#include <stdarg.h>
	/* C bridge function to SyntaxTreeNode::SyntaxTreeNode */
	void* newNode(int symbol, int rule, int line, ...){
		void* output = new SyntaxTreeNode(symbol, rule, line);

		va_list         argPtr;
		SyntaxTreeNode* arg;

		va_start(argPtr, line);

		while ((arg = va_arg(argPtr, SyntaxTreeNode*)) != NULL){
			((SyntaxTreeNode*)output)->addChild((SyntaxTreeNode*)arg);
		}

		va_end(argPtr);

		return output;
	}

	/* C bridge function to SyntaxTreeNode::addChild */
	void addChildren(void* syntaxTreeNode, ...){
		va_list         argPtr;
		SyntaxTreeNode* arg;

		va_start(argPtr, syntaxTreeNode);

		while ((arg = va_arg(argPtr, SyntaxTreeNode*)) != NULL){
			((SyntaxTreeNode*)syntaxTreeNode)->addChild((SyntaxTreeNode*)arg);
		}

		va_end(argPtr);
	}

	/* C bridge function to set the root of syntaxTree */
	void setAsRoot(void* node){
		syntaxTree.setRoot((SyntaxTreeNode*)node);
	}



	/* C bridge function to SymbolsTable::existsType */
	int c_checkType(const char* lex){ return symbolsTable.existsType(string(lex)); }

	/* C bridge function to SymbolsTable::pushTempIdentifier */
	void c_newIdentifier(const char* lex) { symbolsTable.pushTempIdentifier(string(lex)); }

	/* C bridge function to SymbolsTable::pushTempLiteral */
	void c_newLiteral(const char* lex) { symbolsTable.pushTempLiteral(string(lex)); }

	/* C bridge function to SymbolsTable::pushTempType */
	void c_newType(const char* lex) { symbolsTable.pushTempType(string(lex)); }



	/* C bridge function to ErrManager::addError */
	void c_addError(const char* message){ errManager.addError(string(message)); }

	/* C bridge function to ErrManager::addWarning */
	void c_addWarning(const char* message){ errManager.addWarning(string(message)); }

	/* C bridge function to ErrManager::addInfo */
	void c_addInfo(const char* message){ errManager.addInfo(string(message)); }
}





/**
* |------------------------------------------|
* |                                          |
* | Language dependant tasks                 |
* |                                          |
* |------------------------------------------|
*/

/**
* initLanguageRequirements
*
* Perform the init tasks to make the program able to analysis a language.
* The current configuration is done to analyze an ANSI-C-like language.
* In order to build a different front-end, this piece of code should be changed.
*/
void initLanguageRequirements(){
	symbolsTable.addType(TYPE_ERR    , -1);
	symbolsTable.addType(TYPE_OK     , 0);
	symbolsTable.addType(TYPE_LITERAL, 0);
	symbolsTable.addType("void"      , 0);
	symbolsTable.addType("char"      , 1);
	symbolsTable.addType("int"       , 2);

	symbolsTable.addFramework(GLOBAL_FRAMEWORK);
	symbolsTable.currentFramework = GLOBAL_FRAMEWORK;
	symbolsTable.setAsDefined(GLOBAL_FRAMEWORK);

	/* FUTURE <pre_compiler> add the constants __DATE__, __TIME__, __STDC__, ...*/
}



/**
* validateSemanticTree
*
* @param recursionEnabled: Flag which can allow recursion.
*
* Perform the final tasks to ensure the semantic tree represents a valid one.
* The current configuration is done to fit an ANSI-C-like without input arguments in the main function.
* In order to build a different front-end, this piece of code should be changed.
*
* @return true if the analysis allow to start with code generation.
*/
bool validateSemanticTree(bool recursionEnabled){
	/* Ensure a 'main' framework */
	if (!symbolsTable.existsFramework("main")){
		errManager.addError(ERR_SEM_MSG_0A);
		return false;
	}


	/* Ensure that main don't have input arguments */
	if(symbolsTable.getInputType("main").size() > 0){
		errManager.addError(ERR_SEM_MSG_0E);
		return false;
	}


	/* Ensure that main output is 'void' */
	if(symbolsTable.getTypeSize(symbolsTable.getOutputType("main")) != 0){
		errManager.addError(ERR_SEM_MSG_0E);
		return false;
	}


	/* Ensure that every declared framework is also defined */
	list <STFramework*> frameworks = symbolsTable.getFrameworks();
	bool                allDefined = true;
	auto f_checkIsDefined = [&allDefined](STFramework* x){
		string errMessage;
		if (!(x->isDefined())){
			allDefined = false;
			errMessage = x->getName();
			errMessage.append(": ");
			errManager.addError(errMessage.append(ERR_SEM_MSG_15));
		}
	};
    for_each(frameworks.begin(), frameworks.end(), f_checkIsDefined);


	/* Ensure there are no call cycles if recursion disabled or mark frameworks otherwise */
	list<STFramework*> visitedFrameworks  = list<STFramework*>();

    function<void(STFramework*)> f_checkRecursion;
    f_checkRecursion = [&visitedFrameworks, &recursionEnabled, &f_checkRecursion](STFramework* x)->void{
        bool output = false;

        list<STFramework*>::iterator it;
        list<STFramework*>           dependencies;

		if(errManager.existsError())
			return;

        /* If this framework was previously visited: */
        it = find(visitedFrameworks.begin(), visitedFrameworks.end(), x);
        if (it != visitedFrameworks.end()){
            /* If isRecursive, just return */
            if((*it)->isRecursive())
                return;
            else{
                /* Else, a cycle has been found. Set recursive every framework in the cycle return true */
				/* If recursion is not allowed, generate an error */
                (*it)->setRecursive();

				string message = ERR_SEM_MSG_16;
		        if (!recursionEnabled){
					message.append(string(" "));
					message.append(x->getName());
				}

				for(list<STFramework*>::iterator rit = visitedFrameworks.end() ; rit != it ; ){
					rit--;
					if (!recursionEnabled){
						message.append(string(" -> "));
						message.append((*rit)->getName());
					}
					(*rit)->setRecursive();
				}
				if (!recursionEnabled){
					message.append(ERR_SEM_MSG_17);
					errManager.addError(message);
				}
                return;
            }
        }

        dependencies = x->getDependencies();
        visitedFrameworks.push_back(x);

        for_each(dependencies.begin(), dependencies.end(), f_checkRecursion);
        
        visitedFrameworks.pop_back();
    };

	if (!errManager.existsError()){
		f_checkRecursion(symbolsTable.getFramework("main"));
	}

	/* Ensure that any error have been detected */
	if (errManager.existsError())
		return false;

	return true;
}
