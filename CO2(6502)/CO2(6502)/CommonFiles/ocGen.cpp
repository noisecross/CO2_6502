/**
 * |------------------------------------------|
 * | CO2 6502, COMPILER OPTIMIZER TO 6502     |
 * | File: ocGen.cpp                          |
 * | v1.0, April 2012                         |
 * | Author: Emilio Arango Delgado de Mendoza |
 * |------------------------------------------|
 */

#include "ocGen.h"
#include "../main.h"
#include "../FrontEnd/semanticAnalyzer.h"
#include <algorithm>
#include <iomanip>



extern SymbolsTable symbolsTable;





/**
* |------------------------------------------|
* |                                          |
* | Class: Quad                              |
* |                                          |
* |------------------------------------------|
*/

/**
* Quad
* 
* Class constructor.
*/
Quad::Quad(){
	opcode   = ERR;

	opX.opSymbol.CONST = 0;
	opX.opType         = CONST;

	opY.opSymbol.CONST = 0;
	opY.opType         = CONST;
	
	opZ.opSymbol.CONST = 0;
	opZ.opType         = CONST;

	size               = 0;
}



/**
* Quad
* 
* Class constructor.
*/
Quad::Quad(Opcode inOpcode, Operand inX, Operand inY, Operand inZ){
	opcode  = inOpcode;
	opX     = inX;
	opY     = inY;
	opZ     = inZ;
	size    = 0;
}



/**
* Quad
* 
* Class constructor.
*/
Quad::~Quad(){ }



/**
* toString
* 
* @return string containing the text formated quad.
*/
string Quad::toString() const{

	const string opcodeTxt[] ={
		"NOP ", "ADD ", "SUB ", "MUL ", "DIV ", "MOD ", "INC ", "DEC ", 
		"LES ", "RIS ",
		"NEG ", "BR  ", "BEQ ", "BNE ", "BGR ", "BLS ",
		"EQ  ", "NE  ", "GR  ", "LS  ", "LAND", "LOR ",
		"AND ", "OR  ", "XOR ", "NOT ",
		"MOVE", "MVP ", "MVA ", "STP ", "STA ",
		"LABL",
		"PUSH", "POP ", "CALL", "RET ", "HALT" };


	ostringstream result;
	int labSize;
	int opSize;
	int remainingTabs;

	result.fill('\t');

	/* Label if exists */
	#pragma warning ( disable : 4127 )
	if (opZ.opType == LABEL && opcode == LABL){
		if(JUST_ONE_TAB){
			result << opZ.label << ":";
			result << "\t";
		}else{
			labSize = MAX_ID_LEN + 5 - opZ.label.size();
			result << opZ.label << ":";
			result << setw(labSize / TAB_SIZE) << "";
		}
	}else{
		if(JUST_ONE_TAB){
			result << "\t";
		}else{
			result << left << setw((MAX_ID_LEN + 6) / TAB_SIZE) << "";
		}
	}
	#pragma warning ( default : 4127 )

	/* Opcode */
	if (opcode < 0)
		result << left << setw(5) << "ERR ";
	else
		result << left << setw(5) << opcodeTxt[opcode];

	result.fill('\t');

	/* Operands */
	/* OpType { UNDEF = -1, VAR, ADDRESS, CONST, LABEL }; */
	switch(opX.opType){
	case VAR :
		opSize = (opX.opSymbol.VAR->lex.size() > 15) ? 16 : opX.opSymbol.VAR->lex.size();
		remainingTabs = opSize + (16 - opSize) / TAB_SIZE;
		result << setw(remainingTabs) << (opX.opSymbol.VAR->lex).substr(0, 15);
		break;
	/* case ADDRESS :
		opSize = (opX.opSymbol.VAR->lex.size() > 13) ? 14 : opX.opSymbol.VAR->lex.size();
		remainingTabs = opSize + (16 - opSize) / TAB_SIZE;
		result << setw(remainingTabs) << string("[").append((opX.opSymbol.ADDRESS->lex).substr(0, 13).append("]"));
		break; */
	case CONST :
		result << right;
		result.fill('0');
		result << "0x" << setw(4) << hex << (opX.opSymbol.CONST & 0xFFFF);
		result.fill('\t');
		result << left << setw(10 / TAB_SIZE) << "";
		break;
	default: 
		result << setw(16 / TAB_SIZE) << "-";
	}

	switch(opY.opType){
	case VAR :
		opSize = (opY.opSymbol.VAR->lex.size() > 15) ? 16 : opY.opSymbol.VAR->lex.size();
		remainingTabs = opSize + (16 - opSize) / TAB_SIZE;
		result << setw(remainingTabs) << (opY.opSymbol.VAR->lex).substr(0, 15);
		break;
	/* case ADDRESS :
		opSize = (opY.opSymbol.VAR->lex.size() > 13) ? 14 : opY.opSymbol.VAR->lex.size();
		remainingTabs = opSize + (16 - opSize) / TAB_SIZE;
		result << setw(remainingTabs) << string("[").append((opY.opSymbol.ADDRESS->lex).substr(0, 13).append("]"));
		break; */
	case CONST :
		result << right;
		result.fill('0');
		result << "0x" << setw(4) << hex << (opY.opSymbol.CONST & 0xFFFF);
		result.fill('\t');
		result << left << setw(10 / TAB_SIZE) << "";
		break;
	default: 
		result << setw(16 / TAB_SIZE) << "-";
	}

	switch(opZ.opType){
	case VAR :
		opSize = (opZ.opSymbol.VAR->lex.size() > 15) ? 16 : opZ.opSymbol.VAR->lex.size();
		remainingTabs = opSize + (16 - opSize) / TAB_SIZE;
		result << setw(remainingTabs) << (opZ.opSymbol.VAR->lex).substr(0, 15);
		break;
	/* case ADDRESS :
		opSize = (opZ.opSymbol.VAR->lex.size() > 13) ? 14 : opZ.opSymbol.VAR->lex.size();
		remainingTabs = opSize + (16 - opSize) / TAB_SIZE;
		result << setw(remainingTabs) << string("[").append((opZ.opSymbol.ADDRESS->lex).substr(0, 13).append("]"));
		break; */
	case CONST :
		result << right;
		result.fill('0');
		result << "0x" << setw(4) << hex << (opZ.opSymbol.CONST & 0xFFFF);
		result.fill('\t');
		result << left << setw(10 / TAB_SIZE) << "";
		break;
	case LABEL :
		result << opZ.label;
		break;
	default: 
		result << setw(16 / TAB_SIZE) << "-";
	}

	/*
	if (opcode == Quad::RET)
		result << endl << endl << endl;
	*/

	return result.str();
}





/**
* |------------------------------------------|
* |                                          |
* | Class: OCodeGenerator                    |
* |                                          |
* |------------------------------------------|
*/

/**
* OCodeGenerator
* 
* Class constructor.
*/
OCodeGenerator::OCodeGenerator(){
	code                  = list <list <Quad>>();
	breakLabel	          = list <string>();
	continueLabel         = list <string>();
	caseTest              = list <list <Quad>>();
	caseOperand           = list <Quad::Operand>();
	caseDefault           = list <string>();

	trueFalseLabelDone    = list <bool>();
	trueLabels            = list <string>();
	falseLabels           = list <string>();
	
	tempVarCount          = 0;
	trueFalseLabelEnabled = false;
	trueFalseLabelDone.push_back(false);
	currentFramework = GLOBAL_FRAMEWORK;
}



/**
* OCodeGenerator
* 
* Class destructor.
*/
OCodeGenerator::~OCodeGenerator(){
	code.clear();
	breakLabel.clear();
	continueLabel.clear();
	caseTest.clear();
	caseOperand.clear();
	caseDefault.clear();
	trueFalseLabelDone.clear();
	trueLabels.clear();
	falseLabels.clear();
}



/**
* getTemp
* 
* Generates a new temporal variable used to store the temporal values
* needed for the expressions.
*
* @return a pointer to a new temporal variable.
*/
STEntry* OCodeGenerator::getTemp(QualifiedType inType){
	ostringstream aux;
	string output;

	do{
		aux.clear();
		aux.fill('0');
		aux << OC_TEMP_PREFIX << setw(4) << hex << tempVarCount++;
		output = aux.str();
	}while(symbolsTable.existsSymbol(output, currentFramework));

	symbolsTable.addSymbol(output, inType, currentFramework);

	return symbolsTable.getSymbol(output, currentFramework);
}



/**
* releaseTemp
* 
* Reset the temporal variables generator.
*/
void OCodeGenerator::releaseTemp(){ tempVarCount = 0; }



/**
* enableTrueFalse
* 
* Enable the generation of logical contitional labels.
*/
void OCodeGenerator::enableTrueFalse(const string &trueLabel, const string &falseLabel){
	trueFalseLabelEnabled = true;

	trueLabels.push_back(trueLabel);
	falseLabels.push_back(falseLabel);
}



/**
* disableTrueFalse
* 
* Disable the generation of conditional labels.
*/
void OCodeGenerator::disableTrueFalse(){
	trueFalseLabelEnabled = false;

	trueLabels.pop_back();
	falseLabels.pop_back();
}



/**
* swapTrueFalse
* 
* Swap the trueLabels and falseLabels values to perform logical negation.
*/
void OCodeGenerator::swapTrueFalse(){
	string newFalseLabel = trueLabels.back();
	string newTrueLabel  = falseLabels.back();

	trueLabels.pop_back();
	falseLabels.pop_back();

	trueLabels.push_back(newTrueLabel);
	falseLabels.push_back(newFalseLabel);
}



/**
* generate
* 
* Perform the code generation order.
*
* @param syntaxTree: A pointer to the syntax tree which is going to be translated.
* @param inConstShortcut: A flag marking if is allowed to ignore code generation to
*                         the previously calculated values.
*/
void OCodeGenerator::generate(SyntaxTree* syntaxTree, LabelGenerator* inLabelGenerator, bool inConstShortcut){
	symbolsOffset = syntaxTree->getNTerminals();
	list<list<Quad>> genCode = list<list<Quad>>();

	constShortcut  = inConstShortcut;
	labelGenerator = inLabelGenerator;

	symbolsTable.resetNUses();

	generateCode(syntaxTree->getRoot(), &genCode, false);

	code.splice(code.begin(), genCode);
}



/**
* generateCode
* 
* Manage the intermediate code production.
*
* @param node: The node which is being translated.
* @param codeList: The quads list where the intermediate code is written.
* @param shortcutDone: A flag which shows if a constant shortcut has been performed.
*                      A 'true' value disallow the the children generate 'expression' codes.
*/
void OCodeGenerator::generateCode(const SyntaxTreeNode* node, list<list<Quad>> *codeList, bool shortcutDone){

	auto f_generateCode = [this, &codeList, &shortcutDone](SyntaxTreeNode* x){
		generateCode(x, codeList, shortcutDone);
	};



	switch (node->symbol - symbolsOffset) {

	case NT_TRANSLATION_UNIT : /* list <external_declaration> */
		for_each(node->children.begin(), node->children.end(), f_generateCode);
		break;
	case NT_EXTERNAL_DECLARATION : /*function_definition | declaration */
		generateCode(node->children[0], &(codeList->back()), false);
		break;
	case NT_FUNCTION_DEFINITION :
		generateFunctionDefinition(node, codeList);
		break;
	default :
		list<Quad> newCode = list<Quad>();
		generateCode(node, &newCode, false);
		if (!codeList->empty())
			codeList->back().splice(codeList->back().end(), newCode);
	}
}



/**
* generateCode
* 
* Manage the intermediate code production.
*
* @param node: The node which is being translated.
* @param codeList: The quads list where the intermediate code is written.
* @param shortcutDone: A flag which shows if a constant shortcut has been performed.
*                      A 'true' value disallow the the children generate 'expression' codes.
*
* @return inherited operand object.
*/
Quad::Operand OCodeGenerator::generateCode(const SyntaxTreeNode* node, list<Quad> *codeList, bool shortcutDone){
	
	Quad::Operand const nullOperand = {Quad::UNDEF, "", NULL};
	Quad::Operand       tempOperand = nullOperand;
	Quad::Operand       output      = nullOperand;
	int symbol = node->symbol;


	auto f_generateCode = [this, &output, &codeList, &shortcutDone](SyntaxTreeNode* x){
		output = generateCode(x, codeList, shortcutDone);
	};


	switch (symbol - symbolsOffset) {
	
	case NT_PRIMARY_EXPRESSION :
		output = generatePrimaryExpression(node);
		break;

	case NT_POSTFIX_EXPRESSION :
	case NT_ARGUMENT_EXPRESSION_LIST :
		output = generatePostfixExpression(node, codeList, shortcutDone);
		break;
		
	case NT_UNARY_EXPRESSION :
	case NT_UNARY_OPERATOR :
	case NT_CAST_EXPRESSION :
	case NT_MULTIPLICATIVE_EXPRESSION :
	case NT_ADDITIVE_EXPRESSION :
	case NT_SHIFT_EXPRESSION :
		output = generateArithmeticExpression(node, codeList, shortcutDone);
		break;
		
	case NT_RELATIONAL_EXPRESSION :
	case NT_EQUALITY_EXPRESSION :
		output = generateRelationalExpression(node, codeList, shortcutDone);
		break;
		
	case NT_AND_EXPRESSION :
	case NT_EXCLUSIVE_OR_EXPRESSION :
	case NT_INCLUSIVE_OR_EXPRESSION :
	case NT_LOGICAL_AND_EXPRESSION :
	case NT_LOGICAL_OR_EXPRESSION :
		output = generateLogicalExpression(node, codeList, shortcutDone);
		break;

	case NT_CONDITIONAL_EXPRESSION :
		output = generateConditionalExpression(node, codeList, shortcutDone);
		break;

	case NT_ASSIGNMENT_EXPRESSION :
		output = generateAssignmentExpression(node, codeList, shortcutDone);
		break;

	case NT_EXPRESSION :
		output = generateExpression(node, codeList, shortcutDone);
		break;


	case NT_DECLARATION : /*declaration_specifiers ';' | declaration_specifiers init_declarator_list ';' */
		if (node->rule == 2)
			output = generateCode(node->children[1], codeList, false);
		break;	
	case NT_INIT_DECLARATOR_LIST : /* init_declarator | init_declarator_list ',' init_declarator */
		for_each(node->children.begin(), node->children.end(), f_generateCode);
		break;
	case NT_INIT_DECLARATOR : /* declarator | declarator '=' initializer */
		if (node->rule == 2){
			output.opType       = Quad::VAR;
			string declarator;

			/* Check if the l-value is an unused symbol */
			if (node->children[0]->lex != ""){
				if (symbolsTable.getSymbol(node->children[0]->lex, currentFramework) == NULL){
					/* Is an unused symbol */
					output = generateCode(node->children[1], codeList, true);
					return output;
				}else{
					declarator = node->children[0]->lex;
				}
			}else{
				declarator = node->children[0]->children[1]->lex;
			}

			output.opSymbol.VAR = symbolsTable.getSymbol(declarator, currentFramework);
			tempOperand = generateCode(node->children[1], codeList, false);

			if(tempOperand.opType == Quad::VAR && (tempOperand.opSymbol.VAR->getType().type != output.opSymbol.VAR->getType().type)){
				tempOperand.opSymbol.VAR->setType(output.opSymbol.VAR->getType().type);
			}

			Quad quad = Quad(Quad::MOVE, output, tempOperand, nullOperand);
			addQuad(codeList, quad);
		}
		break;


	case NT_ABSTRACT_DECLARATOR :
		/* FUTURE <arrays> */
		break;
	case NT_DIRECT_ABSTRACT_DECLARATOR :
		/* FUTURE <arrays> */
		break;


	case NT_INITIALIZER :
		/*   assignment_expression **Never here**
		/* | '{' initializer_list '}'     */
		/* | '{' initializer_list ',' '}' */
		/* FUTURE <initializer> */
		break;
	case NT_INITIALIZER_LIST :
		/* initializer | initializer_list ',' initializer */
		/* FUTURE <initializer> */
		break;


	case NT_LABELED_STATEMENT :
		generateLabeledStatement(node, codeList);
		break;

	case NT_COMPOUND_STATEMENT :
		/* '{' '}' | '{' statement_list '}' | '{' declaration_list '}' | '{' declaration_list statement_list '}' */
		if (node->rule == 4){
			generateCode(node->children[0], codeList, false);
			generateCode(node->children[1], codeList, false);
		}else if(node->rule > 1)
			generateCode(node->children[0], codeList, false);
		break;

	case NT_DECLARATION_LIST : /* list <declaration> */
		for_each(node->children.begin(), node->children.end(), f_generateCode);
		break;
	case NT_STATEMENT_LIST : /* list <statement> */
		for_each(node->children.begin(), node->children.end(), f_generateCode);
		break;

	case NT_EXPRESSION_STATEMENT : /*';' | expression ';' */
		/* **Nothing to do here** | **Never here** */
		break;
	case NT_SELECTION_STATEMENT :
		generateSelectionStatement(node, codeList);
		break;
	case NT_ITERATION_STATEMENT :
		generateIterationStatement(node, codeList);
		break;
	case NT_JUMP_STATEMENT :
		generateJumpStatement(node, codeList);
		break;


	case NT_TRANSLATION_UNIT : /* list <external_declaration> */
		for_each(node->children.begin(), node->children.end(), f_generateCode);
		break;
	case NT_EXTERNAL_DECLARATION : /*function_definition | declaration */
		output = generateCode(node->children[0], codeList, false);
		break;
	case NT_FUNCTION_DEFINITION :
		/* Never here */
		/* generateFunctionDefinition(node, codeList); */
		break;


	default: ;
		/* stringstream var;
		var << symbol - symbolsOffset;
		output.opSymbol.LABEL = new string(var.str());
		output.opType = Quad::LABEL;
		Quad quad = Quad(Quad::LABL, output, output, output);
		addQuad(codeList, quad); */
		/* Nothing to do here */
	}


	return output;
}



/**
* generatePrimaryExpression
* 
* Aux function. Perform the code generation of a primary expression.
*
* @param node: The node which is being translated.
* @param codeList: The quads list where the intermediate code is written.
* @param shortcutDone: A flag which shows if a constant shortcut has been performed.
*                      A 'true' value disallow the the children generate 'expression' codes.
*
* @return a operand object representing the expression result.
*/
Quad::Operand OCodeGenerator::generatePrimaryExpression(const SyntaxTreeNode* node){
	Quad::Operand const nullOperand   = {Quad::UNDEF, "", NULL};
	Quad::Operand       tempOperand   = nullOperand;

	/* Labels to jump to true or false labes check */
	if(trueFalseLabelEnabled && trueFalseLabelDone.back()){
		trueFalseLabelEnabled = false;
		trueFalseLabelDone.pop_back();
		trueFalseLabelDone.push_back(false);
	}


	switch (node->rule) {
		case 1 : /* IDENTIFIER*/
			tempOperand.opType       = Quad::VAR;
			tempOperand.opSymbol.VAR = symbolsTable.getSymbol(node->lex, currentFramework);
			break;
		case 2 : /* | CONSTANT*/
			tempOperand.opType         = Quad::CONST;
			tempOperand.opSymbol.CONST = node->value;
			break;
		case 3 : /* | STRING_LITERAL */
			/* FUTURE <strings> */
			break;
		case 4 : /* | '(' expression ')' */
			/* Never here */
			break;
		default : ;
	}
	
	return tempOperand;
}



/**
* generatePostfixExpression
* 
* Aux function. Perform the code generation of a postfix expression.
*
* @param node: The node which is being translated.
* @param codeList: The quads list where the intermediate code is written.
* @param shortcutDone: A flag which shows if a constant shortcut has been performed.
*                      A 'true' value disallow the the children generate 'expression' codes.
*
* @return a operand object representing the expression result.
*/
Quad::Operand OCodeGenerator::generatePostfixExpression(const SyntaxTreeNode* node, list<Quad>* codeList, bool shortcutDone){
	Quad::Operand const    nullOperand  = {Quad::UNDEF, "", NULL};
	Quad::Operand          tempOperand  = nullOperand;
	Quad::Operand          tempOperandY = nullOperand;
	Quad::Operand          destOperand  = nullOperand;

	Quad                   quad;

	list<string>::iterator itInput;
	string				   funcName;
	list<string>           inputSymbols;
	list<STEntry*>         symbols;
	list<Quad>             argsStrInstr = list<Quad>();

	int                    argPos;
	bool                   storeStatusAtStack;

	

    auto f_generateArgsCode = [this, &argPos, &nullOperand, &tempOperand, &destOperand, &quad, &codeList,
                               &argsStrInstr, &shortcutDone, &itInput, &inputSymbols, &funcName]
                              (SyntaxTreeNode* x){

        STEntry* newArgument = symbolsTable.getSymbol(*(itInput++), funcName);

        /* Check if the l-value is an unused symbol */
        if (newArgument == NULL){
            /* Is an unused symbol */
            tempOperand = generateCode(x, codeList, true);
        }else{
            /* Normal behavior */
            destOperand.opType       = Quad::VAR;
            destOperand.opSymbol.VAR = newArgument;
            tempOperand              = generateCode(x, codeList, shortcutDone);
            quad                     = Quad(Quad::MOVE, destOperand, tempOperand, nullOperand);
            argsStrInstr.push_back(quad);

            /* Add a new argument refference to optimize (if possible) */
            if (newArgument->isConst() || newArgument->getPointerDepth() > 0){
                symbolsTable.addInputArgument(funcName, tempOperand.opSymbol.VAR, argPos);
            }
        }
        argPos++;
    };


	auto f_storeStatus = [this, &codeList, &nullOperand, &quad](STEntry* x){
		/* FUTURE <Optimize> to store only the symbols which can be smashed by recursion */
		if (x->lex == SEM_RETURN_SYMBOL)
			return;
		Quad::Operand operand = {Quad::VAR, "", x};
		quad = Quad(Quad::PUSH, operand, nullOperand, nullOperand);
		addQuad(codeList, quad);       
	};    
	auto f_restoreStatus = [this, &codeList, &nullOperand, &quad](STEntry* x){
		/* FUTURE <Optimize> to restore only the symbols which can be smashed by recursion */
		if (x->lex == SEM_RETURN_SYMBOL)
			return;
		Quad::Operand operand = {Quad::VAR, "", x};
		quad = Quad(Quad::POP, operand, nullOperand, nullOperand);
		addQuad(codeList, quad);
	};


	/* Labels to jump to true or false labes check */
	if(trueFalseLabelEnabled && trueFalseLabelDone.back()){
		trueFalseLabelEnabled = false;
		trueFalseLabelDone.pop_back();
		trueFalseLabelDone.push_back(false);
	}

	shortcutExpressionAux(node, codeList, &shortcutDone, &tempOperand);
	if (shortcutDone)
		return tempOperand;


	switch (node->symbol - symbolsOffset) {

	case NT_POSTFIX_EXPRESSION :
		switch(node->rule){
		case 1 : /* primary_expression */
			/* Never here */
			break;
		case 2 : /* |  postfix_expression '[' expression ']' */
			/* FUTURE <arrays> */
			break;
		case 3 : /* |  postfix_expression '(' ')' */
			/* Check if is a recursive call or a cycle call */
			storeStatusAtStack = (symbolsTable.isRecursive(currentFramework) &&
				                  symbolsTable.isRecursive(node->children[0]->lex));

			if(storeStatusAtStack){
				/* Save framework state at the stack */
				symbols = symbolsTable.getSymbols(currentFramework);
				for_each(symbols.begin(), symbols.end(), f_storeStatus);
			}

			tempOperand.opType = Quad::LABEL;
			tempOperand.label  = string(SEM_FUNCTION_PREFIX).append(node->children[0]->lex);
			quad = Quad(Quad::CALL, nullOperand, nullOperand, tempOperand);
			addQuad(codeList, quad);

			if(storeStatusAtStack){
				/* Restore framework state at the stack */
				for_each(symbols.rbegin(), symbols.rend(), f_restoreStatus);
			}

			if (symbolsTable.getTypeSize(symbolsTable.getOutputType(node->children[0]->lex)) > 0){
				/* Call to a function with return value */
				tempOperand.opType       = Quad::VAR;
				tempOperand.opSymbol.VAR = symbolsTable.getSymbol(SEM_RETURN_SYMBOL, node->children[0]->lex);
			}else{
				/* Call to a function without return value */
				tempOperand = nullOperand;
			}
			break;
		case 4 : /* |  postfix_expression '(' argument_expression_list ')' */
			storeStatusAtStack = (symbolsTable.isRecursive(currentFramework) &&
				                  symbolsTable.isRecursive(node->children[0]->lex));

			/* 1- Calculate arguments               */
			/* 2- If recursive, push current status */
			/* 3- Send arguments to subroutine      */
			/* 4- Call subroutine                   */
			/* 5- If recursive, pop current status  */

			argPos       = 0;
			funcName     = node->children[0]->lex;
			inputSymbols = symbolsTable.getInputSymbols(funcName);
			itInput      = inputSymbols.begin();
			for_each(node->children[1]->children.begin(),
				     node->children[1]->children.end(),
					 f_generateArgsCode );

			if(storeStatusAtStack){
				/* Save framework state at the stack */
				symbols = symbolsTable.getSymbols(currentFramework);
				for_each(symbols.begin(), symbols.end(), f_storeStatus);
			}

		    for_each(argsStrInstr.begin(), argsStrInstr.end(), [this, &codeList](Quad x){
		        addQuad(codeList, x);
			});

			tempOperand.opType = Quad::LABEL;
			tempOperand.label  = string(SEM_FUNCTION_PREFIX).append(node->children[0]->lex);
			quad = Quad(Quad::CALL, nullOperand, nullOperand, tempOperand);
			addQuad(codeList, quad);

			if(storeStatusAtStack){
				/* Restore framework state at the stack */
				for_each(symbols.rbegin(), symbols.rend(), f_restoreStatus);
			}

			if (symbolsTable.getTypeSize(symbolsTable.getOutputType(node->children[0]->lex)) > 0){
				/* Call to a function with return value */
				tempOperand.opType       = Quad::VAR;
				tempOperand.opSymbol.VAR = symbolsTable.getSymbol(SEM_RETURN_SYMBOL, node->children[0]->lex);
			}else{
				/* Call to a function without return value */
				tempOperand = nullOperand;
			}
			break;
		case 5 : /* |  postfix_expression '.' IDENTIFIER */
			/* FUTURE <structs> */
			break;
		case 6 : /* |  postfix_expression PTR_OP IDENTIFIER */
			/* FUTURE <structs> */
			break;
		case 7 : /* |  postfix_expression INC_OP */
			tempOperandY = generateCode(node->children[0], codeList, false);
			tempOperand.opType       = Quad::VAR;
			tempOperand.opSymbol.VAR = getTemp(node->type);
			quad        = Quad(Quad::MOVE, tempOperand, tempOperandY, nullOperand);
			addQuad(codeList, quad);
			quad        = Quad(Quad::INC, tempOperandY, tempOperandY, nullOperand);
			addQuad(codeList, quad);
			break;
		case 8 : /* |  postfix_expression DEC_OP */
			tempOperandY = generateCode(node->children[0], codeList, false);
			tempOperand.opType       = Quad::VAR;
			tempOperand.opSymbol.VAR = getTemp(node->type);
			quad        = Quad(Quad::MOVE, tempOperand, tempOperandY, nullOperand);
			addQuad(codeList, quad);
			quad        = Quad(Quad::DEC, tempOperandY, tempOperandY, nullOperand);
			addQuad(codeList, quad);
			break;
		default : /* FUTURE <error> */;
		}
		break;

	case NT_ARGUMENT_EXPRESSION_LIST :
		switch(node->rule){
		case 1 : /* assignment_expression */
			tempOperand = generateCode(node->children[0], codeList, false);
			break;
		case 2 : /* | argument_expression_list ',' assignment_expression */
			
			break;
		default : /* FUTURE <error> */;
		}
		break;

	default: 
		/* FUTURE <error> */;
	}

	return tempOperand;
}



/**
* generateArithmeticExpression
* 
* Aux function. Perform the code generation of an arithmetic expression.
*
* @param node: The node which is being translated.
* @param codeList: The quads list where the intermediate code is written.
* @param shortcutDone: A flag which shows if a constant shortcut has been performed.
*                      A 'true' value disallow the the children generate 'expression' codes.
*
* @return a operand object representing the expression result.
*/
Quad::Operand OCodeGenerator::generateArithmeticExpression(const SyntaxTreeNode* node, list<Quad>* codeList, bool shortcutDone){
	Quad::Operand const nullOperand  = {Quad::UNDEF, "", NULL};
	Quad::Operand       tempOperand  = nullOperand;
	Quad::Operand       tempOperandP = nullOperand;
	Quad::Operand       tempOperandY = nullOperand;
	Quad::Operand       tempOperandZ = nullOperand;

	Quad                quad;
	bool                labelAux = false;

	shortcutExpressionAux(node, codeList, &shortcutDone, &tempOperand);
	if (shortcutDone)
		return tempOperand;

	/* Labels to jump to true or false labes check */
	if(trueFalseLabelEnabled && trueFalseLabelDone.back()){
		labelAux = true;
		trueFalseLabelEnabled = false;
		trueFalseLabelDone.pop_back();
		trueFalseLabelDone.push_back(false);
	}


	auto f_generateCode =
		[this, &nullOperand, &tempOperand, &tempOperandP, &tempOperandY, &tempOperandZ, &quad, &codeList, &shortcutDone]
	(SyntaxTreeNode* x, SyntaxTreeNode* castExpression){

		tempOperandY = generateCode(castExpression, codeList, shortcutDone);

		tempOperand.opType       = Quad::VAR;
		tempOperand.opSymbol.VAR = getTemp(x->type);

		switch (x->rule){

		case 1 : /* '&' */
			quad = Quad(Quad::MVA, tempOperand, tempOperandY, nullOperand);
			addQuad(codeList, quad);
			break;
		case 2 : /* | '*' */
			quad = Quad(Quad::MVP, tempOperand, tempOperandY, nullOperand);
			addQuad(codeList, quad);
			break;
		case 3 : /* | '+' */
			tempOperand = tempOperandY;
			break;
		case 4 : /* | '-' */
			quad = Quad(Quad::NEG, tempOperand, tempOperandY, nullOperand);
			addQuad(codeList, quad);
			break;
		case 5 : /* | '~' */
			/* Only 1 byte types allowed */
			tempOperandZ.opType         = Quad::CONST;
			tempOperandZ.opSymbol.CONST = 0xFF;
			quad = Quad(Quad::XOR, tempOperand, tempOperandY, tempOperandZ);
			addQuad(codeList, quad);
			quad = Quad(Quad::INC, tempOperand, tempOperand, nullOperand);
			addQuad(codeList, quad);
			break;
		case 6 : /* | '!' */
			/* Only 1 byte types allowed */
			quad = Quad(Quad::NOT, tempOperand, tempOperandY, nullOperand);
			addQuad(codeList, quad);
			break;
		default : /* FUTURE <error> */;
		}
	};


	switch (node->symbol - symbolsOffset) {

	case NT_UNARY_EXPRESSION :
		switch(node->rule){
		case 1 : /* postfix_expression */
			/* Never here */
			break;
		case 2 : /* | INC_OP unary_expression */
			tempOperand = generateCode(node->children[0], codeList, false);
			quad        = Quad(Quad::INC, tempOperand, tempOperand, nullOperand);
			addQuad(codeList, quad);
			break;
		case 3 : /* | DEC_OP unary_expression */
			tempOperand = generateCode(node->children[0], codeList, false);
			quad        = Quad(Quad::DEC, tempOperand, tempOperand, nullOperand);
			addQuad(codeList, quad);
			break;
		case 4 : /* | unary_operator cast_expression */
			if (labelAux && node->children[0]->rule == 6 /* '!' */){
				/* The only case where a label expression can be generated: Unary operator == '!' */
				trueFalseLabelEnabled = true;
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(true);
				swapTrueFalse();
				tempOperand = generateCode(node->children[1], codeList, false);
				swapTrueFalse();
			}else{
				f_generateCode(node->children[0], node->children[1]);
			}
			break;
		case 5 : /* | SIZEOF unary_expression */
			tempOperand.opType         = Quad::CONST;
			tempOperand.opSymbol.CONST = node->value;
			break;
		case 6 : /* | SIZEOF '(' type_name ')' */
			tempOperand.opType         = Quad::CONST;
			tempOperand.opSymbol.CONST = node->value;
			break;
		default : ; /* FUTURE <error> */
		}
		break;

	case NT_CAST_EXPRESSION :
		switch(node->rule){
		case 1: /* unary_expression */
			/* Never here */
			break;
		case 2: /* | '(' type_name ')' cast_expression */
			tempOperandY = generateCode(node->children[1], codeList, shortcutDone);
			tempOperand.opType       = Quad::VAR;
			tempOperand.opSymbol.VAR = getTemp(node->type);
			quad        = Quad(Quad::MOVE, tempOperand, tempOperandY, nullOperand);
			addQuad(codeList, quad);
			break;
		default : ;/* FUTURE <error> */
		}
		break;

	case NT_MULTIPLICATIVE_EXPRESSION :
		switch(node->rule){
		case 1: /* cast_expression */
			/* Never here */
			break;
		case 2: /* | multiplicative_expression '*' cast_expression */
		case 3: /* | multiplicative_expression '/' cast_expression */
		case 4: /* | multiplicative_expression '%' cast_expression */
			/* Not supported by 6502 */
			break;
		}
		break;

	case NT_ADDITIVE_EXPRESSION :
		switch(node->rule){
		case 1: /* multiplicative_expression */
			/* Never here */
			break;
		case 2: /* | additive_expression '+' multiplicative_expression */
			tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
			tempOperandZ = generateCode(node->children[1], codeList, shortcutDone);
			tempOperand.opType       = Quad::VAR;
			tempOperand.opSymbol.VAR = getTemp(node->type);
			quad        = Quad(Quad::ADD, tempOperand, tempOperandY, tempOperandZ);
			addQuad(codeList, quad);
			break;
		case 3: /* | additive_expression '-' multiplicative_expression */
			tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
			tempOperandZ = generateCode(node->children[1], codeList, shortcutDone);
			tempOperand.opType       = Quad::VAR;
			tempOperand.opSymbol.VAR = getTemp(node->type);
			quad        = Quad(Quad::SUB, tempOperand, tempOperandY, tempOperandZ);
			addQuad(codeList, quad);			
			break;
		default : ; /* FUTURE <error> */
		}
		break;

	case NT_SHIFT_EXPRESSION :
		switch(node->rule){
		case 1: /* additive_expression */
			/* Never here */
			break;
		case 2: /* | shift_expression LEFT_OP additive_expression */
			tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
			tempOperandZ = generateCode(node->children[1], codeList, shortcutDone);
			tempOperand.opType       = Quad::VAR;
			tempOperand.opSymbol.VAR = getTemp(node->type);
			quad        = Quad(Quad::LES, tempOperand, tempOperandY, tempOperandZ);
			addQuad(codeList, quad);
			break;
		case 3: /* | shift_expression RIGHT_OP additive_expression */
			tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
			tempOperandZ = generateCode(node->children[1], codeList, shortcutDone);
			tempOperand.opType       = Quad::VAR;
			tempOperand.opSymbol.VAR = getTemp(node->type);
			quad        = Quad(Quad::RIS, tempOperand, tempOperandY, tempOperandZ);
			addQuad(codeList, quad);
			break;
		default : ; /* FUTURE <error> */
			break;
		}
	default : /* FUTURE <error> */;
	}

	return tempOperand;
}



/**
* generateRelationalExpression
* 
* Aux function. Perform the code generation of a relational expression.
*
* @param node: The node which is being translated.
* @param codeList: The quads list where the intermediate code is written.
* @param shortcutDone: A flag which shows if a constant shortcut has been performed.
*                      A 'true' value disallow the the children generate 'expression' codes.
*
* @return a operand object representing the expression result.
*/
Quad::Operand OCodeGenerator::generateRelationalExpression(const SyntaxTreeNode* node, list<Quad>* codeList, bool shortcutDone){
	Quad::Operand const nullOperand  = {Quad::UNDEF, "", NULL};
	Quad::Operand       tempOperand  = nullOperand;
	Quad::Operand       tempOperandY = nullOperand;
	Quad::Operand       tempOperandZ = nullOperand;
	Quad::Operand       labelOperand = nullOperand;

	Quad                quad;
	bool                auxTFLenable = trueFalseLabelEnabled;
	labelOperand.opType = Quad::LABEL;

	shortcutExpressionAux(node, codeList, &shortcutDone, &tempOperand);
	if (shortcutDone){
		/* Labels to jump to true or false labes check */
		if(trueFalseLabelDone.back()){
			trueFalseLabelDone.pop_back();
			trueFalseLabelDone.push_back(false);
		}
		return tempOperand;
	}


	switch (node->symbol - symbolsOffset) {

	case NT_RELATIONAL_EXPRESSION :
		switch (node->rule){
		case 1 : /* shift_expression */
			/* Never here */
			break;
		case 2 : /* | relational_expression '<' shift_expression */
			tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
			tempOperandZ = generateCode(node->children[1], codeList, shortcutDone);
			if(auxTFLenable){
				trueFalseLabelEnabled       = true;
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(true);
				labelOperand.label          = trueLabels.back();
				quad = Quad(Quad::BLS, tempOperandY, tempOperandZ, labelOperand);
				addQuad(codeList, quad);
				labelOperand.label          = falseLabels.back();
				quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
				addQuad(codeList, quad);
			}else{
				tempOperand.opType       = Quad::VAR;
				tempOperand.opSymbol.VAR = getTemp(node->type);
				quad        = Quad(Quad::LS, tempOperand, tempOperandY, tempOperandZ);
				addQuad(codeList, quad);
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(false);
			}
			break;
		case 3 : /* | relational_expression '>' shift_expression */
			tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
			tempOperandZ = generateCode(node->children[1], codeList, shortcutDone);
			if(auxTFLenable){
				trueFalseLabelEnabled       = true;
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(true);
				labelOperand.label          = trueLabels.back();
				quad = Quad(Quad::BGR, tempOperandY, tempOperandZ, labelOperand);
				addQuad(codeList, quad);
				labelOperand.label          = falseLabels.back();
				quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
				addQuad(codeList, quad);
			}else{
				tempOperand.opType       = Quad::VAR;
				tempOperand.opSymbol.VAR = getTemp(node->type);
				quad        = Quad(Quad::GR, tempOperand, tempOperandY, tempOperandZ);
				addQuad(codeList, quad);
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(false);
			}
			break;
		case 4 : /* | relational_expression LE_OP shift_expression */
			tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
			tempOperandZ = generateCode(node->children[1], codeList, shortcutDone);
			if(auxTFLenable){
				trueFalseLabelEnabled       = true;
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(true);
				labelOperand.label          = falseLabels.back();
				quad = Quad(Quad::BGR, tempOperandY, tempOperandZ, labelOperand);
				addQuad(codeList, quad);
				labelOperand.label          = trueLabels.back();
				quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
				addQuad(codeList, quad);
			}else{
				tempOperand.opType       = Quad::VAR;
				tempOperand.opSymbol.VAR = getTemp(node->type);
				quad        = Quad(Quad::GR, tempOperand, tempOperandY, tempOperandZ);
				addQuad(codeList, quad);
				quad        = Quad(Quad::NOT, tempOperand, tempOperand, nullOperand);
				addQuad(codeList, quad);
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(false);
			}
			break;
		case 5 : /* | relational_expression GE_OP shift_expression */
			tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
			tempOperandZ = generateCode(node->children[1], codeList, shortcutDone);
			if(auxTFLenable){
				trueFalseLabelEnabled       = true;
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(true);
				labelOperand.label          = falseLabels.back();
				quad = Quad(Quad::BLS, tempOperandY, tempOperandZ, labelOperand);
				addQuad(codeList, quad);
				labelOperand.label          = trueLabels.back();
				quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
				addQuad(codeList, quad);
			}else{
				tempOperand.opType       = Quad::VAR;
				tempOperand.opSymbol.VAR = getTemp(node->type);
				quad        = Quad(Quad::LS, tempOperand, tempOperandY, tempOperandZ);
				addQuad(codeList, quad);
				quad        = Quad(Quad::NOT, tempOperand, tempOperand, nullOperand);
				addQuad(codeList, quad);
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(false);
			}
			break;
		default : /* FUTURE <error> */;
		}
		break;

	case NT_EQUALITY_EXPRESSION :
		switch (node->rule){
		case 1 : /* relational_expression */
			/* Never here */
			break;
		case 2 : /* | equality_expression EQ_OP relational_expression */
			tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
			tempOperandZ = generateCode(node->children[1], codeList, shortcutDone);
			if(auxTFLenable){
				trueFalseLabelEnabled       = true;
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(true);
				labelOperand.label          = trueLabels.back();
				quad = Quad(Quad::BEQ, tempOperandY, tempOperandZ, labelOperand);
				addQuad(codeList, quad);
				labelOperand.label          = falseLabels.back();
				quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
				addQuad(codeList, quad);
			}else{
				tempOperand.opType       = Quad::VAR;
				tempOperand.opSymbol.VAR = getTemp(node->type);
				quad        = Quad(Quad::EQ, tempOperand, tempOperandY, tempOperandZ);
				addQuad(codeList, quad);			
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(false);
			}
			break;
		case 3 : /* | equality_expression NE_OP relational_expression */
			tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
			tempOperandZ = generateCode(node->children[1], codeList, shortcutDone);
			if(auxTFLenable){
				trueFalseLabelEnabled       = true;
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(true);
				labelOperand.label          = trueLabels.back();
				quad = Quad(Quad::BNE, tempOperandY, tempOperandZ, labelOperand);
				addQuad(codeList, quad);
				labelOperand.label          = falseLabels.back();
				quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
				addQuad(codeList, quad);
			}else{
				tempOperand.opType       = Quad::VAR;
				tempOperand.opSymbol.VAR = getTemp(node->type);
				quad        = Quad(Quad::NE, tempOperand, tempOperandY, tempOperandZ);
				addQuad(codeList, quad);
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(false);
			}
			break;
		default : /* FUTURE <error> */;
		}
		break;
	default : /* FUTURE <error> */;
	}

	return tempOperand;
}



/**
* generateLogicalExpression
* 
* Aux function. Perform the code generation of a logical expression.
*
* @param node: The node which is being translated.
* @param codeList: The quads list where the intermediate code is written.
* @param shortcutDone: A flag which shows if a constant shortcut has been performed.
*                      A 'true' value disallow the the children generate 'expression' codes.
*
* @return a operand object representing the expression result.
*/
Quad::Operand OCodeGenerator::generateLogicalExpression(const SyntaxTreeNode* node, list<Quad>* codeList, bool shortcutDone){
	Quad::Operand const nullOperand  = {Quad::UNDEF, "", NULL};
	Quad::Operand       tempOperand  = nullOperand;
	Quad::Operand       tempOperandY = nullOperand;
	Quad::Operand       tempOperandZ = nullOperand;
	Quad::Operand       labelOperand = nullOperand;

	Quad                quad;
	labelOperand.opType = Quad::LABEL;

	string              branchLabel  = "";
	bool                labelAux     = false;


	shortcutExpressionAux(node, codeList, &shortcutDone, &tempOperand);
	if (shortcutDone)
		return tempOperand;


	/* Labels to jump to true or false labes check */
	if(trueFalseLabelEnabled && trueFalseLabelDone.back()){
		labelAux = true;
		trueFalseLabelEnabled = false;
		trueFalseLabelDone.pop_back();
		trueFalseLabelDone.push_back(false);
	}


	switch (node->symbol - symbolsOffset) {

	case NT_AND_EXPRESSION :
		/* equality_expression */ /* Never here */
		/* | and_expression '&' equality_expression */
		trueFalseLabelEnabled = false;

		tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
		tempOperandZ = generateCode(node->children[1], codeList, shortcutDone);
		tempOperand.opType       = Quad::VAR;
		tempOperand.opSymbol.VAR = getTemp(node->type);
		quad        = Quad(Quad::AND, tempOperand, tempOperandY, tempOperandZ);
		addQuad(codeList, quad);
		break;
	case NT_EXCLUSIVE_OR_EXPRESSION :
		/* and_expression */ /* Never here */
		/* | exclusive_or_expression '^' and_expression */
		tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
		tempOperandZ = generateCode(node->children[1], codeList, shortcutDone);
		tempOperand.opType       = Quad::VAR;
		tempOperand.opSymbol.VAR = getTemp(node->type);
		quad        = Quad(Quad::XOR, tempOperand, tempOperandY, tempOperandZ);
		addQuad(codeList, quad);
		break;
	case NT_INCLUSIVE_OR_EXPRESSION :
		/* exclusive_or_expression */ /* Never here */
		/* | inclusive_or_expression '|' exclusive_or_expression */
		tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
		tempOperandZ = generateCode(node->children[1], codeList, shortcutDone);
		tempOperand.opType       = Quad::VAR;
		tempOperand.opSymbol.VAR = getTemp(node->type);
		quad        = Quad(Quad::OR, tempOperand, tempOperandY, tempOperandZ);
		addQuad(codeList, quad);
		break;
	case NT_LOGICAL_AND_EXPRESSION :
		/* inclusive_or_expression */ /* Never here */
		/* | logical_and_expression AND_OP inclusive_or_expression */
		if(labelAux){
			trueFalseLabelEnabled = true;
			trueFalseLabelDone.pop_back();
			trueFalseLabelDone.push_back(true);
		}
		if(trueFalseLabelEnabled){
			/* The expression should perform a jump */
			branchLabel = "an_";
			branchLabel.append(labelGenerator->getNewLabel());
			labelOperand.label = branchLabel;
			trueLabels.push_back(branchLabel);

			/* First Operand */
			tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
			trueFalseLabelEnabled = true;
			if(trueFalseLabelDone.back() != true){
				tempOperand.opType         = Quad::CONST;
				tempOperand.opSymbol.CONST = 0x00;
				labelOperand.label          = falseLabels.back();
				quad = Quad(Quad::BEQ, tempOperandY, tempOperand, labelOperand);
				addQuad(codeList, quad);
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(true);
			}
			/* Label to second Operand */
			quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
			addQuad(codeList, quad);
			trueLabels.pop_back();

			/* Second operand */
			tempOperandZ = generateCode(node->children[1], codeList, shortcutDone);
			trueFalseLabelEnabled = true;
			if(trueFalseLabelDone.back() != true){
				tempOperand.opType         = Quad::CONST;
				tempOperand.opSymbol.CONST = 0x00;
				labelOperand.label          = falseLabels.back();
				quad = Quad(Quad::BEQ, tempOperandZ, tempOperand, labelOperand);
				addQuad(codeList, quad);
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(true);
			}
		}else{
			/* The expression should be stored into a variable */
			tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
			tempOperandZ = generateCode(node->children[1], codeList, shortcutDone);
			tempOperand.opType       = Quad::VAR;
			tempOperand.opSymbol.VAR = getTemp(node->type);
			quad        = Quad(Quad::LAND, tempOperand, tempOperandY, tempOperandZ);
			addQuad(codeList, quad);
		}
		break;
	case NT_LOGICAL_OR_EXPRESSION :
		/* logical_and_expression */ /* Never here */
		/* | logical_or_expression OR_OP logical_and_expression */
		if(labelAux){
			trueFalseLabelEnabled = true;
			trueFalseLabelDone.pop_back();
			trueFalseLabelDone.push_back(true);
		}
		if(trueFalseLabelEnabled){
			/* The expression should perform a jump */
			branchLabel = "or_";
			branchLabel.append(labelGenerator->getNewLabel());
			labelOperand.label = branchLabel;
			falseLabels.push_back(branchLabel);

			/* First Operand */
			tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
			trueFalseLabelEnabled = true;
			if(trueFalseLabelDone.back() != true){
				tempOperand.opType         = Quad::CONST;
				tempOperand.opSymbol.CONST = 0x00;
				labelOperand.label          = trueLabels.back();
				quad = Quad(Quad::BNE, tempOperandY, tempOperand, labelOperand);
				addQuad(codeList, quad);
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(true);
			}
			/* Label to second Operand */
			quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
			addQuad(codeList, quad);
			falseLabels.pop_back();

			/* Second operand */
			tempOperandZ = generateCode(node->children[1], codeList, shortcutDone);
			trueFalseLabelEnabled = true;
			if(trueFalseLabelDone.back() != true){
				tempOperand.opType         = Quad::CONST;
				tempOperand.opSymbol.CONST = 0x00;
				labelOperand.label          = trueLabels.back();
				quad = Quad(Quad::BNE, tempOperandZ, tempOperand, labelOperand);
				addQuad(codeList, quad);
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(true);
			}
			labelOperand.label          = falseLabels.back();
			quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
			addQuad(codeList, quad);
		}else{
			/* The expression should be stored into a variable */	tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
			tempOperandZ = generateCode(node->children[1], codeList, shortcutDone);
			tempOperand.opType       = Quad::VAR;
			tempOperand.opSymbol.VAR = getTemp(node->type);
			quad        = Quad(Quad::LOR, tempOperand, tempOperandY, tempOperandZ);
			addQuad(codeList, quad);
		}
		break;
	default : /* FUTURE <error> */;
	}

	return tempOperand;
}



/**
* generateConditionalExpression
* 
* Aux function. Perform the code generation of a conditional expression.
*
* @param node: The node which is being translated.
* @param codeList: The quads list where the intermediate code is written.
* @param shortcutDone: A flag which shows if a constant shortcut has been performed.
*                      A 'true' value disallow the the children generate 'expression' codes.
*
* @return a operand object representing the expression result.
*/
Quad::Operand OCodeGenerator::generateConditionalExpression(const SyntaxTreeNode* node, list<Quad>* codeList, bool shortcutDone){
	Quad::Operand const nullOperand  = {Quad::UNDEF, "", NULL};
	Quad::Operand       tempOperand  = nullOperand;
	Quad::Operand       tempOperandY = nullOperand;
	Quad::Operand       tempOperandZ = nullOperand;
	Quad::Operand       labelOperand = nullOperand;

	Quad                quad;

	/* logical_or_expression */ /* Never here */
	/* | logical_or_expression '?' expression ':' conditional_expression */

	if (node->children[0]->valueIsUsed){
		generateCode(node->children[0], codeList, true);

		if (node->children[0]->value){
			tempOperand = generateCode(node->children[1], codeList, false);
		}else{
			tempOperand = generateCode(node->children[2], codeList, false);
		}

		return tempOperand;;
	}

	/* 1 - tempY = logical_or_expression 
	       if tempY GOTO labelBegin: else GOTO labelOperand: */
	/* 2 - labelBegin: */
	/* 3 - temp = t1 = expression */
	/* 4 - GOTO labelEnd */
	/* 5 - labelOperand: */
	/* 6 - temp = t1 = conditional_expression */
	/* 7 - labelEnd: */
	
	string newLabel     = labelGenerator->getNewLabel();
	string labelBeg     = "";
	string labelAlt     = "";
	string labelEnd     = "";
	labelOperand.opType = Quad::LABEL;
	labelBeg.append("beg_cd_").append(newLabel);
	labelAlt.append("alt_cd_").append(newLabel);
	labelEnd.append("end_cd_").append(newLabel);

	/* 1 - tempY = logical_or_expression 
	       if tempY GOTO labelBegin: else GOTO labelOperand: */
	tempOperand.opType        = Quad::VAR;
	tempOperand.opSymbol.VAR  = getTemp(node->type);

	enableTrueFalse(labelBeg, labelAlt);
	trueFalseLabelDone.push_back(true);

	tempOperandY = generateCode(node->children[0], codeList, shortcutDone);
	
	if(trueFalseLabelDone.back() != true){
		tempOperandZ.opType         = Quad::CONST;
		tempOperandZ.opSymbol.CONST = 0x00;

		labelOperand.label          = falseLabels.back();
		quad = Quad(Quad::BEQ, tempOperandY, tempOperandZ, labelOperand);
		addQuad(codeList, quad);

		labelOperand.label          = trueLabels.back();
		quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);
	}	
	trueFalseLabelDone.pop_back();
	disableTrueFalse();

	/* 2 - labelBegin: */
	labelOperand.label = labelBeg;
	quad        = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
	addQuad(codeList, quad);

	/* 3 - temp = expression */
	tempOperand.opType       = Quad::VAR;
	tempOperand.opSymbol.VAR = getTemp(node->type);

	tempOperandY = generateCode(node->children[1], codeList, shortcutDone);
	quad         = Quad(Quad::MOVE, tempOperand, tempOperandY, nullOperand);
	addQuad(codeList, quad);
	
	/* 4 - GOTO labelEnd */
	labelOperand.label = labelEnd;
	quad               = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
	addQuad(codeList, quad);
	
	/* 5 - labelOperand: */
	labelOperand.label = labelAlt;
	quad        = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
	addQuad(codeList, quad);
		
	/* 6 - temp = conditional_expression */
	tempOperandY = generateCode(node->children[2], codeList, shortcutDone);
	quad         = Quad(Quad::MOVE, tempOperand, tempOperandY, nullOperand);
	addQuad(codeList, quad);
	
	/* 7 - labelEnd: */
	labelOperand.label = labelEnd;
	quad        = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
	addQuad(codeList, quad);

	return tempOperand;
}



/**
* generateAssignmentExpression
* 
* Aux function. Perform the code generation of an assignment expression.
*
* @param node: The node which is being translated.
* @param codeList: The quads list where the intermediate code is written.
* @param shortcutDone: A flag which shows if a constant shortcut has been performed.
*                      A 'true' value disallow the the children generate 'expression' codes.
*
* @return a operand object representing the expression result.
*/
Quad::Operand OCodeGenerator::generateAssignmentExpression(const SyntaxTreeNode* node, list<Quad>* codeList, bool shortcutDone){
	Quad::Operand const nullOperand  = {Quad::UNDEF, "", NULL};
	Quad::Operand       tempOperand  = nullOperand;
	Quad::Operand       tempOperandY = nullOperand;
	Quad::Operand       tempOperandZ = nullOperand;
	Quad::Operand       tempOperandP = nullOperand;

	Quad                quad;
	bool                lValueIsPointer = false;

	/* Shortcut not performed here */

	/* Labels to jump to true or false labes check */
	if(trueFalseLabelEnabled && trueFalseLabelDone.back()){
		trueFalseLabelEnabled = false;
		trueFalseLabelDone.pop_back();
		trueFalseLabelDone.push_back(false);
	}


	/* conditional_expression  */ /*Never here*/
	/* | unary_expression assignment_operator assignment_expression */

	/* Check if the l-value is an unused symbol */
	if (node->children[0]->lex != "" && symbolsTable.getSymbol(node->children[0]->lex, currentFramework) == NULL){
		/* Is an unused symbol */
		tempOperand = generateCode(node->children[2], codeList, true);
		return tempOperand;
	}

	/* Calculate l-value expression and check if is an indirect assignment */
	if ((node->children[0]->symbol - symbolsOffset == NT_UNARY_EXPRESSION) &&
		(node->children[0]->children[0]->rule == 2)){
		/* if L-Value is a pointer (is unary_expression and the operator is '*')... */
		tempOperandP    = generateCode(node->children[0]->children[1], codeList, shortcutDone);
		lValueIsPointer = true;
	}else{
		tempOperand  = generateCode(node->children[0], codeList, shortcutDone);
	}

	/* Calculate r-value expression */
	tempOperandZ = generateCode(node->children[2], codeList, shortcutDone);

	/* If is an indirect expression with operand, dereference pointer */
	if (lValueIsPointer){

		if (node->children[1]->rule != 1){
			/* Dereference to operate if is an assignment with operation, that is, rule is not '=' */
			tempOperand.opType       = Quad::VAR;
			tempOperand.opSymbol.VAR = getTemp(node->children[0]->type);
			quad        = Quad(Quad::MVP, tempOperand, tempOperandP, nullOperand);
			addQuad(codeList, quad);
		}
	}

	/* case NT_ASSIGNMENT_OPERATOR : */
	switch(node->children[1]->rule){
	case  1 : /* '=' */
		if (lValueIsPointer){
			tempOperand           = tempOperandP;
			quad                  = Quad(Quad::STP, tempOperand, tempOperandZ, nullOperand);
			QualifiedType newType = node->children[0]->type;
			quad.size             = symbolsTable.getTypeSize(newType);
		}else{
			quad        = Quad(Quad::MOVE, tempOperand, tempOperandZ, nullOperand);
		}
		addQuad(codeList, quad);
		tempOperand = tempOperandZ;
		break;
	case  2 : /* | MUL_ASSIGN */ /* Not supported by 6502 */
	case  3 : /* | DIV_ASSIGN */ /* Not supported by 6502 */
	case  4 : /* | MOD_ASSIGN */ /* Not supported by 6502 */
		/* FUTURE <error> */
		break;
	case  5 : /* | ADD_ASSIGN */
		quad        = Quad(Quad::ADD, tempOperand, tempOperand, tempOperandZ);
		addQuad(codeList, quad);
		break;
	case  6 : /* | SUB_ASSIGN */
		quad        = Quad(Quad::SUB, tempOperand, tempOperand, tempOperandZ);
		addQuad(codeList, quad);
		break;
	case  7 : /* | LEFT_ASSIGN */
		quad        = Quad(Quad::LES, tempOperand, tempOperand, tempOperandZ);
		addQuad(codeList, quad);
		break;
	case  8 : /* | RIGHT_ASSIGN */
		quad        = Quad(Quad::RIS, tempOperand, tempOperand, tempOperandZ);
		addQuad(codeList, quad);
		break;
	case  9 : /* | AND_ASSIGN */
		quad        = Quad(Quad::AND, tempOperand, tempOperand, tempOperandZ);
		addQuad(codeList, quad);
		break;
	case 10 : /* | XOR_ASSIGN */
		quad        = Quad(Quad::XOR, tempOperand, tempOperand, tempOperandZ);
		addQuad(codeList, quad);
		break;
	case 11 : /* | OR_ASSIGN */
		quad        = Quad(Quad::OR, tempOperand, tempOperand, tempOperandZ);
		addQuad(codeList, quad);
		break;
	}

	/* If is an indirect expression with operand, set new value in its place */
	if (lValueIsPointer && node->children[1]->rule != 1){
		quad                  = Quad(Quad::STP, tempOperandP, tempOperand, nullOperand);
		QualifiedType newType = node->children[0]->type;
		quad.size             = symbolsTable.getTypeSize(newType);
		addQuad(codeList, quad);
	}

	/* FUTURE <error> */

	return tempOperand;
}



/**
* generateExpression
*
* Aux function. Perform the code generation of an expression.
*
* @param node: The node which is being translated.
* @param codeList: The quads list where the intermediate code is written.
* @param shortcutDone: A flag which shows if a constant shortcut has been performed.
*                      A 'true' value disallow the the children generate 'expression' codes.
*
* @return a operand object representing the expression result.
*/
Quad::Operand OCodeGenerator::generateExpression(const SyntaxTreeNode* node, list<Quad>* codeList, bool shortcutDone){
	Quad::Operand const nullOperand  = {Quad::UNDEF, "", NULL};
	Quad::Operand       tempOperand  = nullOperand;
	Quad::Operand       tempOperandY = nullOperand;
	Quad::Operand       labelOperand = nullOperand;

	Quad                quad;


	tempOperandY.opType = Quad::CONST;
	labelOperand.opType = Quad::LABEL;

	/* Shortcut is done implicitly */

	auto f_generateCode = [this, &nullOperand, &tempOperand, &tempOperandY, &labelOperand, &quad, &codeList, &shortcutDone, &node]
	(SyntaxTreeNode* x){
		if (x == (node->children[node->children.size() - 1]) && trueFalseLabelEnabled){
			/* Last children and trueFalseLabelEnabled */
			trueFalseLabelDone.push_back(true);
			tempOperand  = generateCode(x, codeList, shortcutDone);

			if(trueFalseLabelDone.back() != true){
				tempOperandY.opSymbol.CONST = 0x00;
				labelOperand.label          = falseLabels.back();
				quad = Quad(Quad::BEQ, tempOperand, tempOperandY, labelOperand);
				addQuad(codeList, quad);
				labelOperand.label          = trueLabels.back();
				quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
				addQuad(codeList, quad);
			}

			trueFalseLabelDone.pop_back();
		}else if(x == (node->children[node->children.size() - 1])){
			/* Last children */
			tempOperand = generateCode(x, codeList, shortcutDone);
		}else{
			generateCode(x, codeList, shortcutDone);
		}
	};

	/* assignment_expression */
	/* | expression ',' assignment_expression */
	for_each(node->children.begin(), node->children.end(), f_generateCode);

	/* FUTURE <error> */

	return tempOperand;
}



/**
* generateLabeledStatement
* 
* Aux function. Perform the code generation of a labeled statement.
*
* @param node: The node which is being translated.
* @param codeList: The quads list where the intermediate code is written.
*/
void OCodeGenerator::generateLabeledStatement(const SyntaxTreeNode* node, list<Quad>* codeList){
	Quad::Operand const nullOperand  = {Quad::UNDEF, "", NULL};
	Quad::Operand       tempOperand  = nullOperand;
	Quad::Operand       tempOperandY = nullOperand;
	Quad::Operand       labelOperand = nullOperand;

	Quad                quad;

	labelOperand.opType = Quad::LABEL;

	string caseLabel = "c_";
	ostringstream newLabel;
	
	switch(node->rule){

	case 1 : /* IDENTIFIER ':' statement */
		newLabel << "_" << symbolsTable.getId(currentFramework) << "l_" << node->children[0]->lex;
		labelOperand.label = newLabel.str();
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);
		tempOperand = generateCode(node->children[1], codeList, false);
		break;
	case 2 : /* | CASE constant_expression ':' statement */
		caseLabel.append(labelGenerator->getNewLabel());
		
		/* 1- Generate label */
		labelOperand.label = caseLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);
		
		/* 2- statement */
		generateCode(node->children[1], codeList, false);

		/* 3- if caseOperand.back() = constant_expression GOTO label */
		tempOperandY = generateCode(node->children[0], &(caseTest.back()), false);
		labelOperand.label = caseLabel;
		quad = Quad(Quad::BEQ, tempOperandY, caseOperand.back(), labelOperand);		
		addQuad(&(caseTest.back()), quad);

		break;
	case 3 : /* | DEFAULT ':' statement */
		/* 1- Generate label */
		labelOperand.label = caseDefault.back();
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		/* 2- statement */
		generateCode(node->children[0], codeList, false);
		
		/* 3- if GOTO label */
		labelOperand.label= caseDefault.back();
		quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);		
		addQuad(&(caseTest.back()), quad);
		
		break;
	default :
		/* Compiler error! */
		/* FUTURE <error> */;
	}
}



/**
* generateSelectionStatement
* 
* Aux function. Perform the code generation of a selection statement.
*
* @param node: The node which is being translated.
* @param codeList: The quads list where the intermediate code is written.
*/
void OCodeGenerator::generateSelectionStatement(const SyntaxTreeNode* node, list<Quad>* codeList){
	Quad::Operand const nullOperand  = {Quad::UNDEF, "", NULL};
	Quad::Operand       tempOperand  = nullOperand;
	Quad::Operand       labelOperand = nullOperand;

	Quad                quad;

	labelOperand.opType = Quad::LABEL;

	string newLabel   = labelGenerator->getNewLabel();
	string begLabel   = "";
	string endLabel   = "";
	string elseLabel  = "";

	list <Quad> newTestList       = list <Quad>();
	list <Quad> newCaseStatements = list <Quad>();
	string brkLabel         = "";
	string defLabel         = "";
	string caseTestLabel    = "";
	

	switch(node->rule){

	case 1 : /* IF '(' expression ')' statement */
		begLabel.append("beg_if_").append(newLabel);
		endLabel.append("end_if_").append(newLabel);

		/* 1- temp = expression */
		enableTrueFalse(begLabel, endLabel);
		tempOperand = generateCode(node->children[0], codeList, false);
		disableTrueFalse();

		/* 2- generar beg_if_label */
		labelOperand.label = begLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		/* 3- statement */
		generateCode(node->children[1], codeList, false);

		/* 4- generar end_if_label */
		labelOperand.label = endLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		break;

	case 2 : /* | IF '(' expression ')' statement ELSE statement */
		begLabel.append("beg_if_").append(newLabel);
		endLabel.append("end_if_").append(newLabel);
		elseLabel.append("beg_el_").append(newLabel);

		/* 1- temp = expression */
		enableTrueFalse(begLabel, elseLabel);
		tempOperand = generateCode(node->children[0], codeList, false);
		disableTrueFalse();

		/* 2- generar beg_if_label */
		labelOperand.label = begLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		/* 3- statement */
		generateCode(node->children[1], codeList, false);

		/* 4- generar GOTO end_if_label */
		labelOperand.label = endLabel;
		quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		/* 5- generar else_if_label */
		labelOperand.label = elseLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		/* 6- statement */
		generateCode(node->children[2], codeList, false);

		/* 7- generar end_if_label */
		labelOperand.label = endLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		break;

	case 3 : /* | SWITCH '(' expression ')' statement */
		caseTestLabel.append("c_test_").append(newLabel);
		defLabel.append("c_def_").append(newLabel);
		endLabel.append("end_c_").append(newLabel);
		brkLabel = endLabel;

		/* 1- temp = expression */
		tempOperand = generateCode(node->children[0], codeList, false);
		caseOperand.push_back(tempOperand);

		/* 2- push break = GOTO end_case
		new caseTest list     */
		caseTest.push_back(newTestList);
		breakLabel.push_back(brkLabel);
		caseDefault.push_back(defLabel);

		/* 2- generate code and case tests */
		generateCode(node->children[1], &newCaseStatements, false);

		/* 3- perform case tests (fill code with the newTestList) */
		/* if test = v1 goto s1 */
		/* ... */
		/* if test = vn goto sn */
		/* goto sd */
		codeList->splice(codeList->end(), caseTest.back());

		/* 4- goto end_case if all else fails */
		labelOperand.label = endLabel;
		quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		/* 5- statement (fill code with the newCaseStatements) */
		/* S1_label */
		/* S1 code  */ /* break = GOTO end_case */
		/* ... */
		/* Sn_label */
		/* Sn code  */ /* break = GOTO end_case */
		/* Sd_label */
		/* Sd code  */ /* break = GOTO end_case */
		codeList->splice(codeList->end(), newCaseStatements);

		/* 6- push break = GOTO end_case
		new caseTest list     */
		newTestList = caseTest.back();
		caseTest.pop_back();
		breakLabel.pop_back();
		caseOperand.pop_back();
		caseDefault.pop_back();

		/* 7- Generate end_case_label */
		labelOperand.label = endLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		break;

	default : /* FUTURE <error> */;

	}
}



/**
* generateIterationStatement
* 
* Aux function. Perform the code generation of an iteration statement.
*
* @param node: The node which is being translated.
* @param codeList: The quads list where the intermediate code is written.
*/
void OCodeGenerator::generateIterationStatement(const SyntaxTreeNode* node, list<Quad>* codeList){
	Quad::Operand const nullOperand  = {Quad::UNDEF, "", NULL};
	Quad::Operand       tempOperand  = nullOperand;
	Quad::Operand       labelOperand = nullOperand;

	Quad                quad;

	labelOperand.opType = Quad::LABEL;
	
	string newLabel   = labelGenerator->getNewLabel();
	string brkLabel   = "";
	string cntLabel   = "";
	string beginLabel = "";
	string endLabel   = "";
	string staLabel   = "";
	
	switch(node->rule){

	case 1 : /* WHILE '(' expression ')' statement */
		beginLabel.append("beg_wh_").append(newLabel);
		staLabel.append("sta_wh_").append(newLabel);
		endLabel.append("end_wh_").append(newLabel);
		cntLabel = beginLabel;
		brkLabel = endLabel;
		
		/* 1- Generate: beg_wh_label
		   2- temp = expression
		   3- Generate: sta_wh_label
		   4- push break and continue
		   5- statement
		   6- pop break and continue
		   7- Generate: GOTO beg_wh_label
		   8- Generate: end_wh_label */
		
		/* 1 */
		labelOperand.label = beginLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);
		
		/* 2 */
		enableTrueFalse(staLabel, endLabel);
		tempOperand = generateCode(node->children[0], codeList, false);
		disableTrueFalse();
		
		/* 3- */
		labelOperand.label = staLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);
		
		/* 4- */
		breakLabel.push_back(brkLabel);
		continueLabel.push_back(cntLabel);
		
		/* 5- */
		generateCode(node->children[1], codeList, false);
		
		/* 6- */
		breakLabel.pop_back();
		continueLabel.pop_back();
		
		/* 7- */
		labelOperand.label = beginLabel;
		quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		/* 8- */
		labelOperand.label = endLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);
		
		break;
	case 2 : /* | DO statement WHILE '(' expression ')' ';' */
		beginLabel.append("beg_wh_").append(newLabel);
		endLabel.append("end_wh_").append(newLabel);
		cntLabel.append("cnt_wh_").append(newLabel);
		brkLabel = endLabel;
		
		/* 1- Generate: beg_while_lab
		   2- push break and continue
		   3- statement
		   4- pop break and continue
		   5- Generate: cnt_while_lab
		   6- temp = expression
		      Generate: if temp GOTO begin_while_label
		   7- Generate: end_while_lab */
		   
		/* 1- */
		labelOperand.label = beginLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		/* 2- */
		breakLabel.push_back(brkLabel);
		continueLabel.push_back(cntLabel);

		/* 3- */
		generateCode(node->children[0], codeList, false);

		/* 4- */
		breakLabel.pop_back();
		continueLabel.pop_back();

		/* 5- */
		labelOperand.label = cntLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		/* 6- */
		enableTrueFalse(beginLabel, endLabel);
		tempOperand = generateCode(node->children[1], codeList, false);
		disableTrueFalse();

		/* 7- */
		labelOperand.label = endLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		break;
	case 3 : /* | FOR '(' expression_statement expression_statement ')' statement */
		beginLabel.append("beg_fr_").append(newLabel);
		staLabel.append("sta_fr_").append(newLabel);
		endLabel.append("end_fr_").append(newLabel);
		cntLabel = beginLabel;
		brkLabel = endLabel;
		
		/* 1- initialization
		   2- Generate: beg_for_lab
		   3- temp = expression
		      if !temp GOTO end_for_lab
		   4- Generate: sta_fr_label
		   5- push break and continue
		   6- statements
		   7- pop break and continue
		   8- Generate: GOTO beg_for_lab
		   9- Generate: end_for_lab */
		
		/* 1- */
		generateCode(node->children[0], codeList, false);

		/* 2- */
		labelOperand.label = beginLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		/* 3- */
		enableTrueFalse(staLabel, endLabel);
		tempOperand = generateCode(node->children[1], codeList, false);
		disableTrueFalse();

		/* 4- */
		labelOperand.label = staLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		/* 5- */
		breakLabel.push_back(brkLabel);
		continueLabel.push_back(cntLabel);

		/* 6- */
		generateCode(node->children[2], codeList, false);

		/* 7- */
		breakLabel.pop_back();
		continueLabel.pop_back();

		/* 8- */
		labelOperand.label = beginLabel;
		quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		/* 9- */
		labelOperand.label = endLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		break;
	case 4 : /* | FOR '(' expression_statement expression_statement expression ')' statement */
		beginLabel.append("beg_fr_").append(newLabel);
		staLabel.append("sta_fr_").append(newLabel);
		endLabel.append("end_fr_").append(newLabel);
		cntLabel.append("cnt_fr_").append(newLabel);
		brkLabel = endLabel;
		
		/* 1- initialization
		   2- Generate: beg_for_lab
		   3- temp = expression
		   4- Generate: if !temp GOTO end_for_lab (only if temp != nullOperand)
		   5- push break and continue
		   6- statements
		   7- pop break and continue
		   8- Generate: for_cont_label
		   9- expresión final
		  10- Generate: GOTO beg_for_lab
		  11- Generate: end_for_lab */
		
		/* 1- */
		generateCode(node->children[0], codeList, false);

		/* 2- */
		labelOperand.label = beginLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		/* 3- */
		enableTrueFalse(staLabel, endLabel);
		tempOperand = generateCode(node->children[1], codeList, false);
		disableTrueFalse();

		/* 4- */
		labelOperand.label = staLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		/* 5- */
		breakLabel.push_back(brkLabel);
		continueLabel.push_back(cntLabel);

		/* 6- */
		generateCode(node->children[3], codeList, false);

		/* 7- */		
		breakLabel.pop_back();
		continueLabel.pop_back();

		/* 8- */
		labelOperand.label = cntLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		/* 9- */		
		generateCode(node->children[2], codeList, false);

		/*10- */
		labelOperand.label = beginLabel;
		quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		/*11- */
		labelOperand.label = endLabel;
		quad = Quad(Quad::LABL, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);

		break;
	default : /* FUTURE <error> */;
	}
}



/**
* generateJumpStatement
* 
* Aux function. Perform the code generation of a jump statement.
*
* @param node: The node which is being translated.
* @param codeList: The quads list where the intermediate code is written.
*/
void OCodeGenerator::generateJumpStatement(const SyntaxTreeNode* node, list<Quad>* codeList){
	Quad::Operand const nullOperand  = {Quad::UNDEF, "", NULL};
	Quad::Operand       tempOperand  = nullOperand;
	Quad::Operand       retOperand   = nullOperand;
	Quad::Operand       labelOperand = nullOperand;

	Quad                quad;
	ostringstream       newLabel;

	labelOperand.opType = Quad::LABEL;

	switch(node->rule){
	case 1 : /* GOTO IDENTIFIER ';' */
		newLabel << "_" << symbolsTable.getId(currentFramework) << "l_" << node->children[0]->lex;
		labelOperand.label = newLabel.str();
		quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);
		break;
	case 2 : /* | CONTINUE ';' */
		labelOperand.label = continueLabel.back();
		quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);
		break;
	case 3: /* | BREAK ';' */
		labelOperand.label = breakLabel.back();
		quad = Quad(Quad::BR, nullOperand, nullOperand, labelOperand);
		addQuad(codeList, quad);
		break;
	case 4 : /* | RETURN ';' */
		quad = Quad(Quad::RET, nullOperand, nullOperand, nullOperand);
		addQuad(codeList, quad);
		break;
	case 5 : /* | RETURN expression ';' */
		tempOperand             = generateCode(node->children[0], codeList, false);
		retOperand.opType       = Quad::VAR;
		retOperand.opSymbol.VAR = symbolsTable.getSymbol(SEM_RETURN_SYMBOL, currentFramework);
		quad = Quad(Quad::MOVE, retOperand, tempOperand, nullOperand);
		addQuad(codeList, quad);
		quad = Quad(Quad::RET,  retOperand, nullOperand, nullOperand);
		addQuad(codeList, quad);
		break;
	default : /* FUTURE <error> */;
	}
}



/**
* generateFunctionDefinition
* 
* Aux function. Perform the code generation of a function.
*/
void OCodeGenerator::generateFunctionDefinition(const SyntaxTreeNode* node, list<list<Quad>> *codeList){

	string              tempFramework = currentFramework;
	Quad::Operand const nullOperand   = {Quad::UNDEF, "", NULL};
	Quad::Operand       tempOperand   = {Quad::LABEL, "", NULL};
	Quad                quad;

	SyntaxTreeNode*     statements;

	list<Quad>          newList = list<Quad>();


	releaseTemp();

	switch(node->rule){

	case 1 : /* declaration_specifiers declarator declaration_list compound_statement */
		currentFramework = node->lex;
		statements       = node->children[3];
		break;
	case 2 : /* | declaration_specifiers declarator compound_statement */
		currentFramework = node->lex;
		statements       = node->children[2];
		break;
	case 3 : /* | declarator declaration_list compound_statement */
		currentFramework = node->lex;
		statements       = node->children[2];
		break;
	case 4: /* | declarator compound_statement */
		currentFramework = node->lex;
		statements       = node->children[1];
		break;
	default :
		/* Compiler error! */
		statements       = NULL;
		/* FUTURE <error> */
		;
	}

	tempOperand.label  = string(SEM_FUNCTION_PREFIX).append(currentFramework);
	tempOperand.opType = Quad::LABEL;
	quad = Quad(Quad::LABL, nullOperand, nullOperand, tempOperand);

	newList.push_back(quad);
	generateCode(statements, &newList, false);
	currentFramework = tempFramework;

	/* Halt main */
	if (currentFramework == "main")
	{
		quad = Quad(Quad::HALT, nullOperand, nullOperand, nullOperand);
		newList.push_back(quad);
	}

	/* Add implicit return */
	else if (newList.back().opcode != Quad::RET){
		quad = Quad(Quad::RET, nullOperand, nullOperand, nullOperand);
		newList.push_back(quad);
	}

	codeList->push_back(newList);
}



/**
* shortcutExpressionAux
* 
* Aux function. Try to shortcut an expression.
*
* @param node: The node which is being translated.
* @param codeList: The quads list where the intermediate code is written.
* @param shortcutDone: A flag which shows if a constant shortcut has been performed.
*                      A 'true' value disallow the the children generate 'expression' codes.
* @param tempOperand: The operand result from the shortcut.
*/
void OCodeGenerator::shortcutExpressionAux(const SyntaxTreeNode* node, list<Quad>* codeList, bool *shortcutDone, Quad::Operand* tempOperand){

	auto f_genChildren = [this, &tempOperand, &codeList](SyntaxTreeNode* x){
		*tempOperand = generateCode(x, codeList, true);
	};


	if (constShortcut){
		if (node->valueIsUsed || (*shortcutDone)){
			*shortcutDone = true;

			for_each(node->children.begin(), node->children.end(), f_genChildren);

			tempOperand->opType         = Quad::CONST;
			tempOperand->opSymbol.CONST = node->value;

			if(trueFalseLabelEnabled && trueFalseLabelDone.back()){
				trueFalseLabelDone.pop_back();
				trueFalseLabelDone.push_back(false);
			}
		}
	}

}



/**
* addQuad
*
* Add a quad into a codeList and execute the usage increment if neccesary
*
* @param oCodeList: The quads list where the intermediate code is written.
* @param quad: The quad to be written into the list.
*/
void OCodeGenerator::addQuad(list<Quad>* oCodeList, Quad quad){
    oCodeList->push_back(quad);

    if((quad.opcode >= Quad::ADD && quad.opcode <= Quad::NEG) ||
       (quad.opcode >= Quad::EQ  && quad.opcode <= Quad::MOVE)){
        /*
        ADD  xyz,  SUB  xyz,  MUL  xyz,  DIV  xyz,  MOD  xyz,  INC  xy-,  DEC  xy-,
        LES  xyz,  RIS  xyz,
        NEG  xy-, 
        */
        /*
        EQ   xyz,  NE   xyz, GR   xyz,  LS   xyz,  LAND xyz,  LOR  xyz,
        AND  xyz,  OR   xyz,  XOR  xyz,  NOT  xy-,
        MOVE xy-,
        */
        if(quad.opY.opType == Quad::VAR)
            quad.opY.opSymbol.VAR->setNUses(quad.opY.opSymbol.VAR->getNUses() + 1);

        if(quad.opZ.opType == Quad::VAR)
            quad.opZ.opSymbol.VAR->setNUses(quad.opZ.opSymbol.VAR->getNUses() + 1);

    }else if(quad.opcode >= Quad::BEQ && quad.opcode <= Quad::BLS){
        /* BEQ  xyL,  BNE  xyL,  BGR  xyL,  BLS  xyL, */
        if(quad.opX.opType == Quad::VAR)
            quad.opX.opSymbol.VAR->setNUses(quad.opX.opSymbol.VAR->getNUses() + 1);

        if(quad.opY.opType == Quad::VAR)
            quad.opY.opSymbol.VAR->setNUses(quad.opY.opSymbol.VAR->getNUses() + 1);

    }else if(quad.opcode == Quad::MVP){
        /* MVP  xy- */
        /* x := *y */
        if(quad.opY.opType == Quad::VAR)
            quad.opY.opSymbol.VAR->setNUses(quad.opY.opSymbol.VAR->getNUses() + 1);

    }else if(quad.opcode == Quad::STP){
        /* STP  xy- */
        /* *x := y */
        if(quad.opX.opType == Quad::VAR)
            quad.opX.opSymbol.VAR->setNUses(quad.opX.opSymbol.VAR->getNUses() + 1);
        if(quad.opY.opType == Quad::VAR)
            quad.opY.opSymbol.VAR->setNUses(quad.opY.opSymbol.VAR->getNUses() + 1);
    }else if(quad.opcode == Quad::MVA){
        /* MVA  xy- */
        /* x := &y */
        if(quad.opY.opType == Quad::VAR)
            quad.opY.opSymbol.VAR->setNUses(quad.opY.opSymbol.VAR->getNUses() + 1);
    }
}



/**
* toString
* 
* Get the object code info and stored into a string.
*
* @return a formatted string containing all the relevant stored messages
*/
string OCodeGenerator::toString() const{
	ostringstream output;

	auto f_concat = [&output](Quad const &x){
		output << x.toString() << endl;
	};

	auto f_concatList = [&output, f_concat](list<Quad> const &x){
		for_each(x.begin(), x.end(), f_concat);
		output << endl << endl;
	};

	for_each(code.begin(), code.end(), f_concatList);

	return output.str();
}





/**
* |------------------------------------------|
* |                                          |
* | Class: LabelGenerator                    |
* |                                          |
* |------------------------------------------|
*/

/**
* LabelGenerator
* 
* Class Constructor.
*/
LabelGenerator::LabelGenerator() { label = 0; }



/**
* ~LabelGenerator
* 
* Class destructor.
*/
LabelGenerator::~LabelGenerator() { }



/**
* getNewLabel
* 
* @return a label with a different name of all previous generated labels.
*/
string LabelGenerator::getNewLabel() {
	ostringstream output;
	output.fill('0');
	output << "lab_0x" << setw(4) << hex << label++;
	return output.str();
}
