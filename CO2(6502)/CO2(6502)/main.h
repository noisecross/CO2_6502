/**
 * |------------------------------------------|
 * | CO2 6502, COMPILER OPTIMIZER TO 6502     |
 * | File: Main.h                             |
 * | v1.0, June 2013                          |
 * | Author: Emilio Arango Delgado de Mendoza |
 * |------------------------------------------|
 */

#ifndef GlobalH
#define GlobalH

#define TAB_SIZE     4 /* #define TAB_SIZE     8 */
#define JUST_ONE_TAB 0 /* #define JUST_ONE_TAB 1 */
#define IDS_TAB_SIZE 5
#define MAX_ID_LEN   (TAB_SIZE * IDS_TAB_SIZE) -3
#define MAX_FW_LEN   (TAB_SIZE * IDS_TAB_SIZE) -4
#define VERSION      "CO2(6502) v1.0.4 escrito por Emilio Arango Delgado de Mendoza"

#endif





#ifdef __cplusplus

#ifndef MainH
#define MainH

#define MAIN_RET_OK         0
#define MAIN_RET_HELP      -1
#define MAIN_RET_VER       -2
#define MAIN_RET_BAD_ARGS  -3
#define MAIN_RET_FILE_ERR  -4
#define MAIN_RET_CODE_ERR  -5

#define MAIN_MAIN_METHOD   "main"

#define MAIN_SYNTAX_PATH   ".SyntaxTree.txt"
#define MAIN_SYMBOLS_PATH  ".SymbolsTable.txt"
#define MAIN_OCODE_PATH    ".OCode.txt"

#define MAIN_DEF_SHOWSYNTAXTREE   0
#define MAIN_DEF_SHOWSYMBOLSTABLE 2
#define MAIN_DEF_SHOWOBJECTCODE   0
#define MAIN_DEF_CODETRANSLATION  2
#define MAIN_DEF_OPTIMIZATION_G   1
#define MAIN_DEF_CONSTSHORTCUT    MAIN_DEF_OPTIMIZATION_G > 0
#define MAIN_DEF_ALLOWRECURSION   false

/*
* Define SET_DATE(x,y) to avoid problems in win32 systems
*
* @param x The name of the char* var to store the date
*/
#ifdef WIN32
#define SET_DATE(x)                  \
	    char x[255];                 \
	    time_t current = time(NULL); \
        struct tm ts;                \
		localtime_s(&ts, &current);  \
		asctime_s(x, 255, &ts);
#else
#define SET_DATE(x)                  \
	    time_t current = time(NULL); \
	    char* x = asctime(localtime(&current));
#endif

/*
* Messages
*/
#define MSG_LANGUAGE "Spanish" /* System specific (const char *) defining the program language */

#define MAIN_MSG_01 "Construyendo árbol sintáctico..."
#define MAIN_MSG_02 "Ejecutando análisis semántico..."
#define MAIN_MSG_03 "Ejecutando generación de código intermedio..."
#define MAIN_MSG_04 "Aplicando optimizaciones..."
#define MAIN_MSG_05 "Generando código final..."

#define MAIN_MSG_10 "Compilación realizada satisfactoriamente"
#define MAIN_MSG_11 "Fallo en compilación"

#define MAIN_PARSEARGS_MSG_01 "Error en la entrada: Archivo "                 /* "Input Error: File "                                  */
#define MAIN_PARSEARGS_MSG_02 " no encontrado"                                /* " was not found"                                      */
#define MAIN_PARSEARGS_MSG_03 "Cargando "                                     /* "Loading file "                                       */
#define MAIN_PARSEARGS_MSG_04 "Error de salida: No se pudo abrir el archivo " /* "Output Error: File "                                 */
#define MAIN_PARSEARGS_MSG_05 ""                                              /* " unable to open"                                     */
#define MAIN_PARSEARGS_MSG_06 "El uso de funciones recursivas puede \
perjudicar el rendimiento del código final generado"                          /* "Recursion usage can lack the final code performance" */

#define MAIN_PRINTSYNTAXTREE_MSG_01 "Compilación realizada en "

#define MAIN_USAGE_01 "Uso: CO2(6502) [opciones] <archivo_entrada>"                                  /* "Usage: CO2(6502) [options] <input_file>" */
#define MAIN_USAGE_02 "Opciones:"                                                                    /* "Options:" */
#define MAIN_USAGE_03 "\t-e <número> Establace los mensajes de error mostrado (3 por defecto)"       /* "\t-e <number> Set the error messages shown (3 by default)" */
#define MAIN_USAGE_04 "\t\t 0\tNo muestra ningún mensaje de error"                                   /* "\t\t 0\tDoesn't show any messages" */
#define MAIN_USAGE_05 "\t\t 1\tMuestra solamente mensajes de error"                                  /* "\t\t 1\tShow error messages only" */
#define MAIN_USAGE_06 "\t\t 2\tMuestra también mensajes de advertencia"                              /* "\t\t 2\tShow also warnings" */
#define MAIN_USAGE_07 "\t\t 3\tMuestra también mensajes de información"                              /* "\t\t 3\tShow also info messages" */
#define MAIN_USAGE_08 "\t-h Muestra esta ayuda"                                                      /* "\t-h Show this help" */
#define MAIN_USAGE_09 "\t-r Permite recursividad"                                                    /* "\t-r Allow recursion" */
#define MAIN_USAGE_10 "\t-O <número> Establede el grado de optimización  (1 por defecto)"            /* "\t-O <number> Set the optimization grade (2 by default)" */
#define MAIN_USAGE_11 "\t\t 0\tOptimizaciones deshabilitadas"                                        /* "\t\t 0\tOptimization disabled" */
#define MAIN_USAGE_12 "\t\t 1\tOptimizaciones básicas"                                               /* "\t\t 1\tFew optimizations" */
#define MAIN_USAGE_13 "\t\t 2\tOptimizaciones de grado medio"                                        /* "\t\t 2\tMid grade optimizations" */
#define MAIN_USAGE_14 "\t\t 3\tTodas las optimizaciones disponibles"                                 /* "\t\t 3\tAll optimizations" */
#define MAIN_USAGE_15 "\t-T <número> Visibilidad del árbol sintáctico    (0 por defecto)"
#define MAIN_USAGE_16 "\t-S <número> Visibilidad de la tabla de símbolos (0 por defecto)"
#define MAIN_USAGE_17 "\t-C <número> Visibilidad del código intermedio   (0 por defecto)"
#define MAIN_USAGE_18 "\t-F <número> Visibilidad del código final        (2 por defecto)"
#define MAIN_USAGE_19 "\t\t 0\tSin visibilidad"
#define MAIN_USAGE_20 "\t\t 1\tSe muestra por consola"
#define MAIN_USAGE_21 "\t\t 2\tSe guarda en un archivo"
#define MAIN_USAGE_22 "\t-v Muestra la versión del compilador"                                       /* "\t-v Show the compiler version" */





#include "CommonFiles/errManager.h"
#include "CommonFiles/symbolsTable.h"
#include "FrontEnd/semanticAnalyzer.h"
#include "CommonFiles/ocGen.h"
#include "BackEnd/optimizer.h"
#include "BackEnd/asmGen.h"

using namespace std;



/**
* Main
*
* The main class
*/
class Main{

private:
	string         programName;
	ifstream       inputFile;
	ofstream       outputFile;

	int            showSyntaxTree;
	int            showSymbolsTable;
	int            showObjectCode;
	int            performCodeTranslation;

	int            optimizationGrade;
	bool           constShortcut;
	bool           allowRecursion;

	int            errorShownGrade;

	Parser         parser;
	LabelGenerator labelGenerator;
	OCodeGenerator oCodeGen;
	Optimizer      optimizer;
	ASMGenerator   asmGen;

	void usage();
	int  parseargs(int, char*[]);
	void initSystem();
	void haltSystem(int);

	void printSytaxTree();
	void printSymbolsTable();
	void printOCode();
	void printASMCode();

public:
	Main();
	~Main();
	int main(int argc, char* argv[]);
};

#endif
#endif
