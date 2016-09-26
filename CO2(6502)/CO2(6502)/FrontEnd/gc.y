/**
* |------------------------------------------|
* | CO2 6502, COMPILER OPTIMIZER TO 6502     |
* | File: gc.y                               |
* | v1.0, September 2012, 1985               |
* | Source: Jeff Lee (1985)                  |
* | Author: Emilio Arango Delgado de Mendoza |
* |------------------------------------------|
*/

/**
* |------------------------------------------|
* |                                          |
* | Bison syntax analyzer prologue           |
* |                                          |
* |------------------------------------------|
*/

%{

#include <stdio.h>
#include "CommonFiles/symbolsTable.h"
#include "CommonFiles/errManager.h"
#include "FrontEnd/semanticAnalyzer.h"

#define YYSTYPE void*

/* Allow to export yytname[] with the name of every symbol of the grammar */
#define YYTOKEN_TABLE 1
#define YYPRINT       1

/* Redefine snprintf name to avoid problems in win32 systems */
#ifdef WIN32
#define snprintf _snprintf
#endif

/* Use a user-defined method to read the input file */
/* The input file will be managed with a ifstream instead of a FILE* */
#undef YY_INPUT
#define YY_INPUT(buf,result,max) ( c_readsome (pInputFile, buf, &result, max) )

/* Transform the yytokentype values into current syntaxtree values */
#define LEAF(x) ((x) - yytoknum[1] + 1)



/* Prototypes */
void         yyerror(const char *);
int          getOffset();
int          getNTokens();
const char** getSymbolNameTable();
 


/* Imports from lex */
extern int  yylex();
extern const char* getYytext();

extern int n_chrcnt;
extern int n_linecnt;


/* Imports from semantic */
extern void* newNode(int, int, int, ...);


/* Imports from main */
extern void* pSymbolsTable;
extern void* pErrManager;
extern void* pInputFile;



void yyerror(const char *s)
{
	char buf [255];
	fflush(stdout);

	snprintf(buf, sizeof(buf), "\t%.16s (%d:%d)\n\t%s %s\n", getYytext(), n_linecnt, n_chrcnt, "^", s);

	c_addError(buf);
}



%}



/**
* |------------------------------------------|
* |                                          |
* | Bison declarations                       |
* |                                          |
* |------------------------------------------|
*/

%code provides {

	enum {
		NT_PRIMARY_EXPRESSION = 1, NT_POSTFIX_EXPRESSION, NT_ARGUMENT_EXPRESSION_LIST, NT_UNARY_EXPRESSION,
		NT_UNARY_OPERATOR, NT_CAST_EXPRESSION, NT_MULTIPLICATIVE_EXPRESSION, NT_ADDITIVE_EXPRESSION,
		NT_SHIFT_EXPRESSION, NT_RELATIONAL_EXPRESSION, NT_EQUALITY_EXPRESSION, NT_AND_EXPRESSION,
		NT_EXCLUSIVE_OR_EXPRESSION, NT_INCLUSIVE_OR_EXPRESSION, NT_LOGICAL_AND_EXPRESSION,
		NT_LOGICAL_OR_EXPRESSION, NT_CONDITIONAL_EXPRESSION, NT_ASSIGNMENT_EXPRESSION, NT_ASSIGNMENT_OPERATOR,
		NT_EXPRESSION, NT_CONSTANT_EXPRESSION, NT_DECLARATION, NT_DECLARATION_SPECIFIERS, NT_INIT_DECLARATOR_LIST,
		NT_INIT_DECLARATOR, NT_STORAGE_CLASS_SPECIFIER, NT_TYPE_SPECIFIER, NT_STRUCT_OR_UNION_SPECIFIER,
		NT_STRUCT_OR_UNION, NT_STRUCT_DECLARATION_LIST, NT_STRUCT_DECLARATION, NT_SPECIFIER_QUALIFIER_LIST,
		NT_STRUCT_DECLARATOR_LIST, NT_STRUCT_DECLARATOR, NT_ENUM_SPECIFIER, NT_ENUMERATOR_LIST, NT_ENUMERATOR,
		NT_TYPE_QUALIFIER, NT_DECLARATOR, NT_DIRECT_DECLARATOR, NT_POINTER, NT_TYPE_QUALIFIER_LIST, NT_PARAMETER_TYPE_LIST,
		NT_PARAMETER_LIST, NT_PARAMETER_DECLARATION, NT_IDENTIFIER_LIST, NT_TYPE_NAME, NT_ABSTRACT_DECLARATOR,
		NT_DIRECT_ABSTRACT_DECLARATOR, NT_INITIALIZER, NT_INITIALIZER_LIST, NT_STATEMENT, NT_LABELED_STATEMENT,
		NT_COMPOUND_STATEMENT, NT_DECLARATION_LIST, NT_STATEMENT_LIST, NT_EXPRESSION_STATEMENT, NT_SELECTION_STATEMENT,
		NT_ITERATION_STATEMENT, NT_JUMP_STATEMENT, NT_TRANSLATION_UNIT, NT_EXTERNAL_DECLARATION,
		NT_FUNCTION_DEFINITION
	};

}

%error-verbose


%token IDENTIFIER CONSTANT STRING_LITERAL SIZEOF
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN TYPE_NAME

%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
%token STRUCT UNION ENUM ELLIPSIS

%token CASE DEFAULT INTERRUPT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN


%start translation_unit



/**
* |------------------------------------------|
* |                                          |
* | Grammar rules                            |
* |                                          |
* |------------------------------------------|
*/
%%

primary_expression
	: IDENTIFIER																	{ $1 = newNode(LEAF(IDENTIFIER),     0, n_linecnt, NULL); $$ = newNode(yyr1[yyn], 1, n_linecnt, $1, NULL); }
	| CONSTANT																		{ $1 = newNode(LEAF(CONSTANT),       0, n_linecnt, NULL); $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, NULL); }
	| STRING_LITERAL																{ $1 = newNode(LEAF(STRING_LITERAL), 0, n_linecnt, NULL); $$ = newNode(yyr1[yyn], 3, n_linecnt, $1, NULL); }
	| '(' expression ')'															{                                                         $$ = $2                                          }
	;

postfix_expression
	: primary_expression															{                                                   $$ = $1                                              }
	| postfix_expression '[' expression ']'											{ yyerror(ERR_SIN_MSG_00); /* FUTURE <arrays> */                                                       }
	| postfix_expression '(' ')'													{                                                   $$ = newNode(yyr1[yyn], 3, n_linecnt, $1, NULL);     }
	| postfix_expression '(' argument_expression_list ')'							{                                                   $$ = newNode(yyr1[yyn], 4, n_linecnt, $1, $3, NULL); }
	| postfix_expression '.' IDENTIFIER												{ yyerror(ERR_SIN_MSG_00); /* FUTURE <structs> */                                                      }
	| postfix_expression PTR_OP IDENTIFIER											{ yyerror(ERR_SIN_MSG_00); /* FUTURE <structs> */                                                      }
	| postfix_expression INC_OP														{                                                   $$ = newNode(yyr1[yyn], 7, n_linecnt, $1, NULL);     }
	| postfix_expression DEC_OP														{                                                   $$ = newNode(yyr1[yyn], 8, n_linecnt, $1, NULL);     }
	;

argument_expression_list
	: assignment_expression															{ $$ = newNode(yyr1[yyn], 1, n_linecnt, $1, NULL); }
	| argument_expression_list ',' assignment_expression							{ addChildren($1, $3, NULL); $$ = $1               }
	;

unary_expression
	: postfix_expression															{ $$ = $1 }
	| INC_OP unary_expression														{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $2, NULL);     }
	| DEC_OP unary_expression														{ $$ = newNode(yyr1[yyn], 3, n_linecnt, $2, NULL);     }
	| unary_operator cast_expression												{ $$ = newNode(yyr1[yyn], 4, n_linecnt, $1, $2, NULL); }
	| SIZEOF unary_expression														{ $$ = newNode(yyr1[yyn], 5, n_linecnt, $2, NULL); }
	| SIZEOF '(' type_name ')'														{ $$ = newNode(yyr1[yyn], 6, n_linecnt, $3, NULL); }
	;

unary_operator
	: '&'																			{ $$ = newNode(yyr1[yyn], 1, n_linecnt, NULL); }
	| '*'																			{ $$ = newNode(yyr1[yyn], 2, n_linecnt, NULL); }
	| '+'																			{ $$ = newNode(yyr1[yyn], 3, n_linecnt, NULL); }
	| '-'																			{ $$ = newNode(yyr1[yyn], 4, n_linecnt, NULL); }
	| '~'																			{ $$ = newNode(yyr1[yyn], 5, n_linecnt, NULL); }
	| '!'																			{ $$ = newNode(yyr1[yyn], 6, n_linecnt, NULL); }
	;

cast_expression
	: unary_expression																{ $$ = $1 }
	| '(' type_name ')' cast_expression												{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $2, $4, NULL); }
	;

multiplicative_expression
	: cast_expression																{ $$ = $1 }
	| multiplicative_expression '*' cast_expression									{ yyerror(ERR_SIN_MSG_01); /* Not supported by 6502 */ }
	| multiplicative_expression '/' cast_expression									{ yyerror(ERR_SIN_MSG_01); /* Not supported by 6502 */ }
	| multiplicative_expression '%' cast_expression									{ yyerror(ERR_SIN_MSG_01); /* Not supported by 6502 */ }
	;

additive_expression
	: multiplicative_expression														{ $$ = $1 }
	| additive_expression '+' multiplicative_expression								{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, $3, NULL); }
	| additive_expression '-' multiplicative_expression								{ $$ = newNode(yyr1[yyn], 3, n_linecnt, $1, $3, NULL); }
	;

shift_expression
	: additive_expression															{ $$ = $1 }
	| shift_expression LEFT_OP additive_expression									{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, $3, NULL); }
	| shift_expression RIGHT_OP additive_expression									{ $$ = newNode(yyr1[yyn], 3, n_linecnt, $1, $3, NULL); }
	;

relational_expression
	: shift_expression																{ $$ = $1 }
	| relational_expression '<' shift_expression									{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, $3, NULL); }
	| relational_expression '>' shift_expression									{ $$ = newNode(yyr1[yyn], 3, n_linecnt, $1, $3, NULL); }
	| relational_expression LE_OP shift_expression									{ $$ = newNode(yyr1[yyn], 4, n_linecnt, $1, $3, NULL); }
	| relational_expression GE_OP shift_expression									{ $$ = newNode(yyr1[yyn], 5, n_linecnt, $1, $3, NULL); }
	;

equality_expression
	: relational_expression															{ $$ = $1 }
	| equality_expression EQ_OP relational_expression								{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, $3, NULL); }
	| equality_expression NE_OP relational_expression								{ $$ = newNode(yyr1[yyn], 3, n_linecnt, $1, $3, NULL); }
	;

and_expression
	: equality_expression															{ $$ = $1 }
	| and_expression '&' equality_expression										{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, $3, NULL); }
	;

exclusive_or_expression
	: and_expression																{ $$ = $1 }
	| exclusive_or_expression '^' and_expression									{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, $3, NULL); }
	;

inclusive_or_expression
	: exclusive_or_expression														{ $$ = $1 }
	| inclusive_or_expression '|' exclusive_or_expression							{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, $3, NULL); }
	;

logical_and_expression
	: inclusive_or_expression														{ $$ = $1 }
	| logical_and_expression AND_OP inclusive_or_expression							{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, $3, NULL); }
	;

logical_or_expression
	: logical_and_expression														{ $$ = $1 }
	| logical_or_expression OR_OP logical_and_expression							{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, $3, NULL); }
	;

conditional_expression
	: logical_or_expression															{ $$ = $1 }
	| logical_or_expression '?' expression ':' conditional_expression				{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, $3, $5, NULL); }
	;

assignment_expression
	: conditional_expression														{ $$ = $1 }
	| unary_expression assignment_operator assignment_expression					{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, $2, $3, NULL); }
	;
	
assignment_operator
	: '='																			{ $$ = newNode(yyr1[yyn], 1,  n_linecnt, NULL); }
	| MUL_ASSIGN																	{ yyerror(ERR_SIN_MSG_01); /* Not supported by 6502 */ }
	| DIV_ASSIGN																	{ yyerror(ERR_SIN_MSG_01); /* Not supported by 6502 */ }
	| MOD_ASSIGN																	{ yyerror(ERR_SIN_MSG_01); /* Not supported by 6502 */ }
	| ADD_ASSIGN																	{ $$ = newNode(yyr1[yyn], 5,  n_linecnt, NULL); }
	| SUB_ASSIGN																	{ $$ = newNode(yyr1[yyn], 6,  n_linecnt, NULL); }
	| LEFT_ASSIGN																	{ $$ = newNode(yyr1[yyn], 7,  n_linecnt, NULL); }
	| RIGHT_ASSIGN																	{ $$ = newNode(yyr1[yyn], 8,  n_linecnt, NULL); }
	| AND_ASSIGN																	{ $$ = newNode(yyr1[yyn], 9,  n_linecnt, NULL); }
	| XOR_ASSIGN																	{ $$ = newNode(yyr1[yyn], 10, n_linecnt, NULL); }
	| OR_ASSIGN																		{ $$ = newNode(yyr1[yyn], 11, n_linecnt, NULL); }
	;

expression
	: assignment_expression															{ $$ = newNode(yyr1[yyn], 1, n_linecnt, $1, NULL); }
	| expression ',' assignment_expression											{ addChildren($1, $3, NULL); $$ = $1               }
	;

constant_expression
	: conditional_expression														{ $$ = $1 }
	;

declaration
	: declaration_specifiers ';'													{ $$ = $1 }
	| declaration_specifiers init_declarator_list ';'								{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, $2, NULL);  }
	;

declaration_specifiers
	: storage_class_specifier														{ $$ = newNode(yyr1[yyn], 1, n_linecnt, $1, NULL);         }
	| storage_class_specifier declaration_specifiers								{ addChildren($2, $1, NULL);                       $$ = $2 }
	| type_specifier																{ $$ = newNode(yyr1[yyn], 3, n_linecnt, $1, NULL);         }
	| type_specifier declaration_specifiers											{ addChildren($2, $1, NULL);                       $$ = $2 }
	| type_qualifier																{ $$ = newNode(yyr1[yyn], 5, n_linecnt, $1, NULL);         }
	| type_qualifier declaration_specifiers											{ addChildren($2, $1, NULL);                       $$ = $2 }
	;

init_declarator_list
	: init_declarator																{ $$ = newNode(yyr1[yyn], 1, n_linecnt, $1, NULL); }
	| init_declarator_list ',' init_declarator										{ addChildren($1, $3, NULL); $$ = $1 }
	;

init_declarator
	: declarator																	{ $$ = $1 }
	| declarator '=' initializer													{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, $3, NULL); }
	;

storage_class_specifier
	: TYPEDEF																		{ yyerror(ERR_SIN_MSG_00); /* FUTURE <pre_compiler> */ }
	| EXTERN																		{ yyerror(ERR_SIN_MSG_00); /* FUTURE <pre_compiler> */ }
	| STATIC																		{ yyerror(ERR_SIN_MSG_00); /* FUTURE <pre_compiler> */ /* Similar to Private */ }
	| AUTO																			{ yyerror(ERR_SIN_MSG_01); /* Not supported by 6502 */ }
	| REGISTER																		{ yyerror(ERR_SIN_MSG_01); /* FUTURE <register> */   }
	;

type_specifier
	: VOID																			{ $1 = newNode(yyr1[yyn],  1, n_linecnt, NULL); $$ = $1 }
	| CHAR																			{ $1 = newNode(yyr1[yyn],  2, n_linecnt, NULL); $$ = $1 }
	| SHORT																			{ $1 = newNode(yyr1[yyn],  3, n_linecnt, NULL); $$ = $1 }
	| INT																			{ $1 = newNode(yyr1[yyn],  4, n_linecnt, NULL); $$ = $1 }
	| LONG																			{ $1 = newNode(yyr1[yyn],  5, n_linecnt, NULL); $$ = $1 }
	| FLOAT																			{ yyerror(ERR_SIN_MSG_01); /* Not supported by 6502 */ }
	| DOUBLE																		{ yyerror(ERR_SIN_MSG_01); /* Not supported by 6502 */ }
	| SIGNED																		{ $1 = newNode(yyr1[yyn],  8, n_linecnt, NULL); $$ = $1 }
	| UNSIGNED																		{ $1 = newNode(yyr1[yyn],  9, n_linecnt, NULL); $$ = $1 }
	| struct_or_union_specifier														{ yyerror(ERR_SIN_MSG_00); /* FUTURE <structs> rule 10 */ }
	| enum_specifier																{ yyerror(ERR_SIN_MSG_00); /* FUTURE rule 11 <enum> */ }
	| TYPE_NAME																		{ $1 = newNode(yyr1[yyn], 12, n_linecnt, NULL); $$ = $1 }
	;

struct_or_union_specifier
	: struct_or_union IDENTIFIER '{' struct_declaration_list '}'					{ yyerror(ERR_SIN_MSG_00); /* FUTURE <structs> */ }
	| struct_or_union '{' struct_declaration_list '}'								{ yyerror(ERR_SIN_MSG_00); /* FUTURE <structs> */ }
	| struct_or_union IDENTIFIER													{ yyerror(ERR_SIN_MSG_00); /* FUTURE <structs> */ }
	;

struct_or_union
	: STRUCT																		{ yyerror(ERR_SIN_MSG_00); /* FUTURE <structs> */ }
	| UNION																			{ yyerror(ERR_SIN_MSG_00); /* FUTURE <structs> */ }
	;

struct_declaration_list
	: struct_declaration															{ yyerror(ERR_SIN_MSG_00); /* FUTURE <structs> */ }
	| struct_declaration_list struct_declaration									{ yyerror(ERR_SIN_MSG_00); /* FUTURE <structs> */ }
	;

struct_declaration
	: specifier_qualifier_list struct_declarator_list ';'							{ yyerror(ERR_SIN_MSG_00); /* FUTURE <structs> */ }
	;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list										{ addChildren($2, $1, NULL); $$ = $2 }
	| type_specifier																{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, NULL); }
	| type_qualifier specifier_qualifier_list										{ addChildren($2, $1, NULL); $$ = $2 }
	| type_qualifier																{ $$ = newNode(yyr1[yyn], 4, n_linecnt, $1, NULL); }
	;

struct_declarator_list
	: struct_declarator																{ yyerror(ERR_SIN_MSG_00); /* FUTURE <structs> */ }
	| struct_declarator_list ',' struct_declarator									{ yyerror(ERR_SIN_MSG_00); /* FUTURE <structs> */ }
	;

struct_declarator
	: declarator																	{ yyerror(ERR_SIN_MSG_00); /* FUTURE <structs> */ }
	| ':' constant_expression														{ yyerror(ERR_SIN_MSG_00); /* FUTURE <structs> */ }
	| declarator ':' constant_expression											{ yyerror(ERR_SIN_MSG_00); /* FUTURE <structs> */ }
	;

enum_specifier
	: ENUM '{' enumerator_list '}'													{ yyerror(ERR_SIN_MSG_00); /* FUTURE <enum> */ }
	| ENUM IDENTIFIER '{' enumerator_list '}'										{ yyerror(ERR_SIN_MSG_00); /* FUTURE <enum> */ }
	| ENUM IDENTIFIER																{ yyerror(ERR_SIN_MSG_00); /* FUTURE <enum> */ }
	;

enumerator_list
	: enumerator																	{ yyerror(ERR_SIN_MSG_00); /* FUTURE <enum> */ }
	| enumerator_list ',' enumerator												{ yyerror(ERR_SIN_MSG_00); /* FUTURE <enum> */ }
	;

enumerator
	: IDENTIFIER																	{ yyerror(ERR_SIN_MSG_00); /* FUTURE <enum> */ }
	| IDENTIFIER '=' constant_expression											{ yyerror(ERR_SIN_MSG_00); /* FUTURE <enum> */ }
	;

type_qualifier
	: CONST																			{ $$ = newNode(yyr1[yyn], 1, n_linecnt, NULL); }
	| VOLATILE																		{ $$ = newNode(yyr1[yyn], 2, n_linecnt, NULL); }
	| INTERRUPT																		{ $$ = newNode(yyr1[yyn], 3, n_linecnt, NULL); }
	;

declarator
	: pointer direct_declarator														{ $$ = newNode(yyr1[yyn], 1, n_linecnt, $1, $2, NULL); }
	| direct_declarator																{ $$ = $1 }
	;

direct_declarator
	: IDENTIFIER																	{ $$ = newNode(LEAF(IDENTIFIER), 0, n_linecnt, NULL); }
	| '(' declarator ')'															{ $$ = $2 }
	| direct_declarator '[' constant_expression ']'									{ yyerror(ERR_SIN_MSG_00); /* FUTURE <arrays> */ }
	| direct_declarator '[' ']'														{ yyerror(ERR_SIN_MSG_00); /* FUTURE <arrays> */ }
	| direct_declarator '(' parameter_type_list ')'									{ $$ = newNode(yyr1[yyn], 5, n_linecnt, $1, $3, NULL); }
	| direct_declarator '(' identifier_list ')'										{ $$ = newNode(yyr1[yyn], 6, n_linecnt, $1, $3, NULL); }
	| direct_declarator '(' ')'														{ $$ = newNode(yyr1[yyn], 7, n_linecnt, $1, NULL);     }
	;

pointer
	: '*'																			{ $$ = newNode(yyr1[yyn], 1, n_linecnt, NULL);         }
	| '*' type_qualifier_list														{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $2, NULL);     }
	| '*' pointer																	{ $$ = newNode(yyr1[yyn], 3, n_linecnt, $2, NULL);     }
	| '*' type_qualifier_list pointer												{ $$ = newNode(yyr1[yyn], 4, n_linecnt, $2, $3, NULL); }
	;

type_qualifier_list
	: type_qualifier																{                            $$ = $1 }
	| type_qualifier_list type_qualifier											{ addChildren($1, $2, NULL); $$ = $1 }
	;


parameter_type_list
	: parameter_list																{ $$ = $1 }
	| parameter_list ',' ELLIPSIS													{ yyerror(ERR_SIN_MSG_00); /* FUTURE <ellipsis> */ }
	;

parameter_list
	: parameter_declaration															{ $$ = newNode(yyr1[yyn], 1, n_linecnt, $1, NULL); }
	| parameter_list ',' parameter_declaration										{ addChildren($1, $3, NULL); $$ = $1               }
	;

parameter_declaration
	: declaration_specifiers declarator												{ $$ = newNode(yyr1[yyn], 1, n_linecnt, $1, $2, NULL); }
	| declaration_specifiers abstract_declarator									{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, $2, NULL); }
	| declaration_specifiers														{ $$ = newNode(yyr1[yyn], 3, n_linecnt, $1, NULL);     }
	;

identifier_list
	: IDENTIFIER																	{ $1 = newNode(LEAF(IDENTIFIER), 0, n_linecnt, NULL); $$ = newNode(yyr1[yyn], 1, n_linecnt, $1, NULL); }
	| identifier_list ',' IDENTIFIER												{ $3 = newNode(LEAF(IDENTIFIER), 0, n_linecnt, NULL); addChildren($1, $3, NULL); $$ = $1    }
	;

type_name
	: specifier_qualifier_list														{ $$ = $1 }
	| specifier_qualifier_list abstract_declarator									{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, $2, NULL); }
	;

abstract_declarator
	: pointer																		{ $$ = $1 }
	| direct_abstract_declarator													{ $$ = $1 }
	| pointer direct_abstract_declarator											{ $$ = newNode(yyr1[yyn], 3, n_linecnt, $1, $2, NULL); }
	;

direct_abstract_declarator
	: '(' abstract_declarator ')'													{ $$ = $2 }
	| '[' ']'																		{ yyerror(ERR_SIN_MSG_00); /* FUTURE <arrays> */ }
	| '[' constant_expression ']'													{ yyerror(ERR_SIN_MSG_00); /* FUTURE <arrays> */ }
	| direct_abstract_declarator '[' ']'											{ yyerror(ERR_SIN_MSG_00); /* FUTURE <arrays> */ }
	| direct_abstract_declarator '[' constant_expression ']'						{ yyerror(ERR_SIN_MSG_00); /* FUTURE <arrays> */ }
	| '(' ')'																		{ $$ = newNode(yyr1[yyn], 6, n_linecnt, NULL);     }
	| '(' parameter_type_list ')'													{ $$ = $2 }
	| direct_abstract_declarator '(' ')'											{ $$ = $1 }
	| direct_abstract_declarator '(' parameter_type_list ')'						{ $$ = newNode(yyr1[yyn], 9, n_linecnt, $1, $3, NULL); }
	;

initializer
	: assignment_expression															{ $$ = $1 }
	| '{' initializer_list '}'														{ yyerror(ERR_SIN_MSG_00); /* FUTURE <initializer> */ }
	| '{' initializer_list ',' '}'													{ yyerror(ERR_SIN_MSG_00); /* FUTURE <initializer> */ }
	;

initializer_list
	: initializer																	{ yyerror(ERR_SIN_MSG_00); /* FUTURE <initializer> */ }
	| initializer_list ',' initializer												{ yyerror(ERR_SIN_MSG_00); /* FUTURE <initializer> */ }
	;

statement
	: labeled_statement																{ $$ = $1 }
	| compound_statement															{ $$ = $1 }
	| expression_statement															{ $$ = $1 }
	| selection_statement															{ $$ = $1 }
	| iteration_statement															{ $$ = $1 }
	| jump_statement																{ $$ = $1 }
	| error	error_end																{ $$ = newNode(yyr1[yyn], 7, n_linecnt, NULL); yyerrok; }
	;

labeled_statement
	: IDENTIFIER ':' statement														{ $1 = newNode(LEAF(IDENTIFIER), 0, n_linecnt, NULL); $$ = newNode(yyr1[yyn], 1, n_linecnt, $1, $3, NULL); }
	| CASE constant_expression ':' statement										{                                                     $$ = newNode(yyr1[yyn], 2, n_linecnt, $2, $4, NULL); }
	| DEFAULT ':' statement															{                                                     $$ = newNode(yyr1[yyn], 3, n_linecnt, $3, NULL);     }
	;

compound_statement
	: '{' '}'																		{ $$ = newNode(yyr1[yyn], 1, n_linecnt, NULL);          }
	| '{' statement_list '}'														{ $$ = $2 }
	| '{' declaration_list '}'														{ $$ = $2 }
	| '{' declaration_list statement_list '}'										{ $$ = newNode(yyr1[yyn], 4, n_linecnt, $2, $3, NULL);  }
	| '{' error statement_list '}'													{ $$ = $3; yyerrok;                                     }
	;

declaration_list
	: declaration																	{ $$ = newNode(yyr1[yyn], 1, n_linecnt, $1, NULL);          }
	| declaration_list declaration													{ addChildren($1, $2, NULL); $$ = $1                        }
	| error declaration																{ $$ = newNode(yyr1[yyn], 1, n_linecnt, $2, NULL); yyerrok; }
	| declaration_list error declaration											{ addChildren($1, $3, NULL); $$ = $1; yyerrok;              }
	;

statement_list
	: statement																		{ $$ = newNode(yyr1[yyn], 1, n_linecnt, $1, NULL); }
	| statement_list statement														{ addChildren($1, $2, NULL); $$ = $1 }
	;

expression_statement
	: ';'																			{ $$ = newNode(yyr1[yyn], 1, n_linecnt, NULL); }
	| expression ';'																{ $$ = $1 }
	;

selection_statement
	: IF '(' expression ')' statement												{ $$ = newNode(yyr1[yyn], 1, n_linecnt, $3, $5, NULL);     }
	| IF '(' expression ')' statement ELSE statement								{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $3, $5, $7, NULL); }
	| SWITCH '(' expression ')' statement											{ $$ = newNode(yyr1[yyn], 3, n_linecnt, $3, $5, NULL);     }
	;

iteration_statement
	: WHILE '(' expression ')' statement											{ $$ = newNode(yyr1[yyn], 1, n_linecnt, $3, $5, NULL);         }
	| DO statement WHILE '(' expression ')' ';'										{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $2, $5, NULL);         }
	| FOR '(' expression_statement expression_statement ')' statement				{ $$ = newNode(yyr1[yyn], 3, n_linecnt, $3, $4, $6, NULL);     }
	| FOR '(' expression_statement expression_statement expression ')' statement	{ $$ = newNode(yyr1[yyn], 4, n_linecnt, $3, $4, $5, $7, NULL); }
	;

jump_statement
	: GOTO IDENTIFIER ';'															{ $2 = newNode(LEAF(IDENTIFIER), 0, n_linecnt, NULL); $$ = newNode(yyr1[yyn], 1, n_linecnt, $2, NULL); }
	| CONTINUE ';'																	{                                                     $$ = newNode(yyr1[yyn], 2, n_linecnt, NULL);     }
	| BREAK ';'																		{                                                     $$ = newNode(yyr1[yyn], 3, n_linecnt, NULL);     }
	| RETURN ';'																	{                                                     $$ = newNode(yyr1[yyn], 4, n_linecnt, NULL);     }
	| RETURN expression ';'															{                                                     $$ = newNode(yyr1[yyn], 5, n_linecnt, $2, NULL); }
	;

translation_unit
	: external_declaration															{ $$ = newNode(yyr1[yyn], 1, n_linecnt, $1, NULL); setAsRoot($$); }
	| translation_unit external_declaration											{ addChildren($1, $2, NULL); $$ = $1;              setAsRoot($$); }
	;

external_declaration
	: function_definition															{ $$ = $1 }
	| declaration																	{ $$ = $1 }
	| error	error_end																{ $$ = newNode(yyr1[yyn], 3, n_linecnt, NULL); yyerrok; }
	;

function_definition
	: declaration_specifiers declarator declaration_list compound_statement			{ $$ = newNode(yyr1[yyn], 1, n_linecnt, $1, $2, $3, $4, NULL); }
	| declaration_specifiers declarator compound_statement							{ $$ = newNode(yyr1[yyn], 2, n_linecnt, $1, $2, $3, NULL);     }
	| declarator declaration_list compound_statement								{ $$ = newNode(yyr1[yyn], 3, n_linecnt, $1, $2, $3, NULL);     }
	| declarator compound_statement													{ $$ = newNode(yyr1[yyn], 4, n_linecnt, $1, $2, NULL);         }
	;

error_end
	: ';'																			{ ; }
	| ')'																			{ ; }
	| '{'																			{ ; }
	| '}'																			{ ; }
	;

%%



/**
* |------------------------------------------|
* |                                          |
* | Additional C code                        |
* |                                          |
* |------------------------------------------|
*/

int getOffset(){ return yytoknum[1] - 1; }

const char** getSymbolNameTable(){ return yytname; }

int getNTokens(){ return YYNTOKENS; }
