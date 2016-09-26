/**
 * |------------------------------------------|
 * | CO2 6502, COMPILER OPTIMIZER TO 6502     |
 * | File: Main.cpp                           |
 * | v1.0, June 2013                          |
 * | Author: Emilio Arango Delgado de Mendoza |
 * |------------------------------------------|
 */

/**
 * ------------------------------------------
 * 
 * INTERFACE
 * 
 * ------------------------------------------
 * 
 * Usage: CO2(6502) [options] <input_file>
 * 
 * Options:
 * 	-e <number> Set the error messages shown (3 by default)
 * 		0	Doesn't show any messages
 * 		1	Show error messages only
 * 		2	Show also warnings
 * 		3	Show also info messages
 *	-h Show this help
 *  -r Allow recursion
 *	-O <number> Set the optimization degree  (2 by default)
 *		0	Optimization disabled
 *		1	Few optimizations
 *		2	Medium grade optimizations
 *		3	All optimizations
 *	-T <number> Syntax tree visibility       (0 by default)
 *	-S <number> Symbols table visibility     (0 by default)
 *	-C <number> Intermediate code visibility (0 by default)
 *	-F <number> Final code visibility        (2 by default)
 *		0	Doesn't show anything
 *		1	Displayed by console
 *		2	Stored in a file
 *	-v Show the compiler version
 * ------------------------------------------
 */ 


 
#pragma warning ( disable : 4267 ) /* 'var' : conversion from 'size_t' to 'type', possible loss of data */

#include <clocale>
#include <ctime>
#include <fstream>
#include <iostream>
#include <stdio.h>

#ifdef WIN32
#include <conio.h>
#endif

#include "main.h"
#include "CommonFiles/ocGen.h"

using namespace std;



/* Classes */
SymbolsTable symbolsTable;
ErrManager   errManager;
SyntaxTree   syntaxTree;





/**
 * main
 *
 * All the magic starts and ends here.
 */
int main (int argc, char* argv[]) {
	return Main().main(argc, argv);
}





/**
* |------------------------------------------|
* |                                          |
* | Class: Main                              |
* |                                          |
* |------------------------------------------|
*/

/**
* Main
* 
* Class constructor.
*/
Main::Main(){
	showSyntaxTree         = MAIN_DEF_SHOWSYNTAXTREE;
	showSymbolsTable       = MAIN_DEF_SHOWSYMBOLSTABLE;
	showObjectCode         = MAIN_DEF_SHOWOBJECTCODE;
	performCodeTranslation = MAIN_DEF_CODETRANSLATION;

	constShortcut          = MAIN_DEF_CONSTSHORTCUT;
	optimizationGrade      = MAIN_DEF_OPTIMIZATION_G;
	errorShownGrade        = MAIN_DEF_SHOWSYMBOLSTABLE;

	allowRecursion         = MAIN_DEF_ALLOWRECURSION;
}



/**
* ~Main
* 
* Class destructor.
*/
Main::~Main(){ }



/**
* main
* 
* System director
*
* @param argc: Number of arguments in call.
* @param argv: Strings given as arguments.
*
* @return: Output resolution code
*/
int Main::main(int argc, char* argv[]){
	int output = 0;

	/* Set the local language*/
	setlocale(LC_ALL, MSG_LANGUAGE);

	/* Parse the input arguments */
	/* Open the file to compile  */
	switch (parseargs(argc, argv)) {

	case MAIN_RET_HELP:     /* Help : Show usage              */
		usage();
		output = MAIN_RET_HELP;
		break;
	case MAIN_RET_VER:      /* Just asked to show the version */
		output = MAIN_RET_VER;
		break;
	case MAIN_RET_BAD_ARGS: /* Input error: Show usage        */
		usage();
		output = MAIN_RET_BAD_ARGS;
		break;
	case MAIN_RET_FILE_ERR:	/* Input file not found. Exit.    */
		output = MAIN_RET_FILE_ERR;
		break;
	default: ;

	}


	if(output == 0){
		/*
		* 01 INITIALIZE
		*/
		initSystem();


		/*
		* 02 PARSING & 03 BUILD SYNTAX TREE
		*/
		cout << MAIN_MSG_01 << endl;
		int analysisResult = parser.parse(&inputFile);


		/*
		* 04 SEMANTIC ANALYSIS
		*/
		cout << MAIN_MSG_02 << endl;
		syntaxTree.typeCheck(allowRecursion);


		/*
		* 05 OBJECT CODE GENERATION
		*/
		if (performCodeTranslation && !errManager.existsError()){
			cout << MAIN_MSG_03 << endl << endl;
			oCodeGen.generate(&syntaxTree, &labelGenerator, constShortcut);
		}


		/*
		* 06 OPTIMIZATION
		*/
		if (performCodeTranslation && !errManager.existsError() && (optimizationGrade > 0)){
				cout << MAIN_MSG_04 << endl << endl;
				optimizer.optimize(&(oCodeGen.code), optimizationGrade);
		}


		/*
		* 07 FINAL CODE GENERATION
		*/
		if (performCodeTranslation){
			if(!errManager.existsError()){
				cout << MAIN_MSG_05 << endl << endl;
				asmGen.memoryAllocation(MAIN_MAIN_METHOD, (optimizationGrade > 1));
			}

			if(!errManager.existsError()){
				asmGen.generate(oCodeGen.code, &labelGenerator);
			}

			if (optimizationGrade > 1){
				optimizer.optimize(&asmGen.code);
			}
		}


		/*
		* 08 END PROCESS
		*/
		if (errManager.existsError())
			output = MAIN_RET_CODE_ERR;
		else
			output = MAIN_RET_OK;
		haltSystem(analysisResult);
	}

#ifdef WIN32
	_getch();
#endif
	return output;
}



/**
 * parseargs
 * 
 * Parse commandline arguments.
 * Instead of mixing this code into the code in main, we use this function
 * to allow easy extension of the number of arguments.
 *
 * @param argc: Number of arguments in call.
 * @param argv: Strings given as arguments.
 *
 * @return RET_OK if the invocation parameters are right or <0 otherwise.
 */
int Main::parseargs(int argc, char* argv[]) {
	/* Usage: CO2(6502) [options] <input_file> */

	stringstream auxStream;
	int          auxInt;
	bool         showVersionFlag = false;

	if (argc < 2)
		return MAIN_RET_BAD_ARGS;

	if (argc == 2 && argv[1][0]== '-'){
		switch(argv[1][1]){
		
		case 'h':	/* Show help */
			if (argv[1][2] != '\0')
				return MAIN_RET_BAD_ARGS;
			return MAIN_RET_HELP;
			break;

		case 'v':  /* Show version */
			if (argv[1][2] != '\0')
				return MAIN_RET_BAD_ARGS;
			showVersionFlag = true;
			break;

		default:
			return MAIN_RET_BAD_ARGS;
		}
	}
	
	for (int i = 1; i < argc - 1; i++){

		if (argv[i][0]!= '-')
			return MAIN_RET_BAD_ARGS;

		auxStream.clear();

		switch(argv[i][1]){

		case 'O':	/* Set the optimization grade */
			auxStream << argv[i][2];
			auxStream >> auxInt;

			if (auxInt < 0 || auxInt > 3)
				return MAIN_RET_BAD_ARGS;
			if (argv[i][3] != '\0')
				return MAIN_RET_BAD_ARGS;
			optimizationGrade = auxInt;
			constShortcut     = (optimizationGrade > 0);
			break;

		case 'h':	/* Show help */
			if (argv[i][2] != '\0')
				return MAIN_RET_BAD_ARGS;
			return MAIN_RET_HELP;
			break;

		case 'r':  /* Allow recursion */
			if (argv[i][2] != '\0')
				return MAIN_RET_BAD_ARGS;
			allowRecursion = true;
			break;

		case 'e':	/* Set the error messages shown */
			auxStream << argv[i][2];
			auxStream >> auxInt;

			if (auxInt < 0 || auxInt > 3)
				return MAIN_RET_BAD_ARGS;
			if (argv[i][3] != '\0')
				return MAIN_RET_BAD_ARGS;
			errorShownGrade = auxInt;
			break;

		case 'T':	/* Set the syntax tree visibility */
			auxStream << argv[i][2];
			auxStream >> auxInt;

			if (auxInt < 0 || auxInt > 2)
				return MAIN_RET_BAD_ARGS;
			if (argv[i][3] != '\0')
				return MAIN_RET_BAD_ARGS;
			showSyntaxTree = auxInt;
			break;

		case 'S':	/* Set the symbols table visibility */
			auxStream << argv[i][2];
			auxStream >> auxInt;

			if (auxInt < 0 || auxInt > 2)
				return MAIN_RET_BAD_ARGS;
			if (argv[i][3] != '\0')
				return MAIN_RET_BAD_ARGS;
			showSymbolsTable = auxInt;
			break;

		case 'C':	/* Set the intermediate code visibility */
			auxStream << argv[i][2];
			auxStream >> auxInt;

			if (auxInt < 0 || auxInt > 2)
				return MAIN_RET_BAD_ARGS;
			if (argv[i][3] != '\0')
				return MAIN_RET_BAD_ARGS;
			showObjectCode = auxInt;
			break;

		case 'F':	/* Set the final code visibility */
			auxStream << argv[i][2];
			auxStream >> auxInt;

			if (auxInt < 0 || auxInt > 2)
				return MAIN_RET_BAD_ARGS;
			if (argv[i][3] != '\0')
				return MAIN_RET_BAD_ARGS;
			performCodeTranslation = auxInt;
			break;

		case 'v':  /* Show version */
			if (argv[i][2] != '\0')
				return MAIN_RET_BAD_ARGS;
			showVersionFlag = true;
			break;

		default:
			return MAIN_RET_BAD_ARGS;
		}

	}

	if(showVersionFlag){
		cout << endl << VERSION << endl << endl;
		if (argc == 2)
			return MAIN_RET_VER;
	}
	
	if(allowRecursion){
		cout << endl << MAIN_PARSEARGS_MSG_06 << endl << endl;
	}

	programName = string(argv[argc - 1]);
	inputFile.open(programName);

	if(inputFile.bad())
	{
		cout <<  MAIN_PARSEARGS_MSG_01 << programName << MAIN_PARSEARGS_MSG_02 << endl;
		return MAIN_RET_FILE_ERR;
	} else {
		cout <<  MAIN_PARSEARGS_MSG_03 << programName << "..." << endl;
	}

	outputFile.open(programName + ".asm");
	if(outputFile.bad())
	{
		inputFile.close();
		cout <<  MAIN_PARSEARGS_MSG_04 << programName << ".asm" << MAIN_PARSEARGS_MSG_05 << endl;
		return MAIN_RET_FILE_ERR;
	}

	/* OK */
	return MAIN_RET_OK;
}



/**
 * usage
 *
 * Show an usage message.
 */
void Main::usage(){
	cout << MAIN_USAGE_01 << endl;
	cout << MAIN_USAGE_02 << endl;
	cout << MAIN_USAGE_03 << endl;
	cout << MAIN_USAGE_04 << endl;
	cout << MAIN_USAGE_05 << endl;
	cout << MAIN_USAGE_06 << endl;
	cout << MAIN_USAGE_07 << endl;
	cout << MAIN_USAGE_08 << endl;
	cout << MAIN_USAGE_09 << endl;
	cout << MAIN_USAGE_10 << endl;
	cout << MAIN_USAGE_11 << endl;
	cout << MAIN_USAGE_12 << endl;
	cout << MAIN_USAGE_13 << endl;
	cout << MAIN_USAGE_14 << endl;
	cout << MAIN_USAGE_15 << endl;
	cout << MAIN_USAGE_16 << endl;
	cout << MAIN_USAGE_17 << endl;
	cout << MAIN_USAGE_18 << endl;
	cout << MAIN_USAGE_19 << endl;
	cout << MAIN_USAGE_20 << endl;
	cout << MAIN_USAGE_21 << endl;
	cout << MAIN_USAGE_22 << endl;
	cout << endl;
}



/**
* initSystem
*
* Build the system control objects.
*/
void Main::initSystem(){
	/* Init the data structures */
	/*   symbolsTable           */
	/*   errManager             */
	/*   parser                 */
	/*   sytaxTree              */
	/*   oCodeGen               */
	/*   optimizer              */
	/*   asmGen                 */
	symbolsTable   = SymbolsTable();
	errManager     = ErrManager(errorShownGrade > 1, errorShownGrade > 2);
	parser         = Parser();
	syntaxTree     = SyntaxTree(parser.offset, parser.nTokens, parser.symbolNameTable, constShortcut);
	labelGenerator = LabelGenerator();
	oCodeGen       = OCodeGenerator();
	optimizer      = Optimizer();
	asmGen         = ASMGenerator();

	/* Init language requirements */
	initLanguageRequirements();
}



/**
* haltSystem
*
* Destroy the system objects.
*/
void Main::haltSystem(int analysisResult){
	if(!errManager.existsError() && analysisResult == 0){
		if(showSyntaxTree)         printSytaxTree();    /* Print syntax tree in a file    */
		if(showSymbolsTable)       printSymbolsTable(); /* Print symbols table in a file  */
		if(showObjectCode)         printOCode();        /* Print object code in a file    */
		if(performCodeTranslation) printASMCode();      /* Print final code in a asm file */
		cout << MAIN_MSG_10 << endl;                    /* Success message                */
		cout << errManager.toString();                  /* Warning list                   */
	} else {
		cout << endl << MAIN_MSG_11 << endl << endl;    /* Failure message    */
		cout << errManager.toString();                  /* Complete error list*/
	}

	inputFile.close();
	outputFile.close();
}



/**
* printSytaxTree
*
* Print a representation of the syntax tree into a txt file.
*/
void Main::printSytaxTree(){

	ofstream mainOutSyntaxTree;

	if (showSyntaxTree == 0)
		return;

	if (showSyntaxTree > 1)
		mainOutSyntaxTree.open(programName + MAIN_SYNTAX_PATH);

	ostream & outSyntaxTree = ((showSyntaxTree > 1) ? mainOutSyntaxTree : cout);


	if(showSyntaxTree > 1 && mainOutSyntaxTree.bad())
	{
		mainOutSyntaxTree.close();
		cout <<  MAIN_PARSEARGS_MSG_04 << programName << MAIN_SYNTAX_PATH << endl;
	}else{
		try{
			SET_DATE(date);
			outSyntaxTree << MAIN_PRINTSYNTAXTREE_MSG_01 << date << endl;
			outSyntaxTree << syntaxTree.toString();
		}catch(const exception& e){
			cout << e.what() << endl;
		}
		if (showSyntaxTree > 1)
			mainOutSyntaxTree.close();
	}
}



/**
* printSymbolsTable
*
* Print a representation of the symbols table into a txt file.
*/
void Main::printSymbolsTable(){

	ofstream  mainOutSymbolsTable;

	if (showSymbolsTable == 0)
		return;

	if (showSymbolsTable > 1)
		mainOutSymbolsTable.open(programName + MAIN_SYMBOLS_PATH);

	ostream & outSymbolsTable = ((showSymbolsTable > 1) ? mainOutSymbolsTable : cout);

	
	if(showSymbolsTable > 1 && mainOutSymbolsTable.bad())
	{
		mainOutSymbolsTable.close();
		cout <<  MAIN_PARSEARGS_MSG_04 << programName << MAIN_SYMBOLS_PATH << endl;
	}else{
		try{
			SET_DATE(date);
			outSymbolsTable << MAIN_PRINTSYNTAXTREE_MSG_01 << date << endl;
			outSymbolsTable << symbolsTable.toString();
		}catch(const exception& e){
			cout << e.what() << endl;
		}
		if (showSymbolsTable > 1)
			mainOutSymbolsTable.close();
	}
}



/**
* printOCode
*
* Print a representation of the medium code into a txt file.
*/
void Main::printOCode(){

	ofstream  mainOutOCode;

	if (showObjectCode == 0)
		return;

	if (showObjectCode > 1)
		mainOutOCode.open(programName + MAIN_OCODE_PATH);

	ostream & outOCode = ((showObjectCode > 1) ? mainOutOCode : cout);

	if(showObjectCode > 1 && mainOutOCode.bad())
	{
		mainOutOCode.close();
		cout <<  MAIN_PARSEARGS_MSG_04 << programName << MAIN_OCODE_PATH << endl;
	}else{
		try{
			SET_DATE(date);
			outOCode << MAIN_PRINTSYNTAXTREE_MSG_01 << date << endl;
			outOCode << oCodeGen.toString();
		}catch(const exception& e){
			cout << e.what() << endl;
		}
		if(showObjectCode > 1)
			mainOutOCode.close();
	}
}



/**
* printASMCode
*
* Print a representation of the ASM code into a asm file.
*/
void Main::printASMCode(){
	const string tabs = "\t\t\t\t\t\t\t\t\t\t\t\t\t";

	if (performCodeTranslation == 1){
		SET_DATE(date);
		cout << tabs << "; " << endl;
		cout << tabs << "; " << MAIN_PRINTSYNTAXTREE_MSG_01 << date;
		cout << tabs << "; " << endl << endl;
		cout << asmGen.toString() << endl;
	}else if (performCodeTranslation == 2){
		try{
			SET_DATE(date);
			outputFile << tabs << "; " << endl;
			outputFile << tabs << "; " << MAIN_PRINTSYNTAXTREE_MSG_01 << date;
			outputFile << tabs << "; " << endl << endl;
			outputFile << asmGen.toString();
		}catch(const exception& e){
			cout << e.what() << endl;
		}
	}
}
