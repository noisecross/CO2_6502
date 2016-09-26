/**
* |------------------------------------------|
* | CO2 6502, COMPILER OPTIMIZER TO 6502     |
* | File: gc.l                               |
* | v1.0, June 2012, 1985                    |
* | Source: Jeff Lee (1985)                  |
* | Author: Emilio Arango Delgado de Mendoza |
* |------------------------------------------|
*/

/* Avoid the unistd.h dependency and the 'isatty' need.
   That guarantee properly compiling under a windows
   environment. */
%option nounistd
%option never-interactive

/*
FUTURE <pre_compiler>

Add the pre-compiler directives:
	#define, #endif, #ifdef, #line, #elif, #error,
	#ifndef, #pragma, #else, #if, #include, #undef

*/





/**
* |------------------------------------------|
* |                                          |
* | Definitions                              |
* |                                          |
* |------------------------------------------|
*/

D						[0-9]
L						[a-zA-Z_]
H						[a-fA-F0-9]
E						[Ee][+-]?{D}+
FS						(f|F|l|L)
IS						(u|U|l|L)*

%{
#include <stdio.h>
#include "main.h"
#include "FrontEnd/feErrMsg.h"
#include "FrontEnd/semanticAnalyzer.h"
#include "gc.tab.h"



#define GC_NEVER_INTERACTIVE 1
#undef  YY_INPUT
#define YY_INPUT(buf,result,max) ( result = c_readsome (pInputFile, buf, max) )

#ifdef WIN32
#define snprintf _snprintf
#endif

#define COUNT(x)                              \
		if (x == '\n'){                       \
			n_chrcnt = 1;                     \
			n_linecnt++;                      \
		}else if (x == '\t'){                 \
			n_chrcnt += 4 - (n_chrcnt-1 % 4); \
		}else{                                \
			n_chrcnt++;                       \
		}


void lexErr(const char*);
int  check_type();
void count();
void comment();

extern void* pInputFile;


int   n_chrcnt  = 1;
int   n_linecnt = 1;

const char* getYytext();



%}





/**
* |------------------------------------------|
* |                                          |
* | Rules                                    |
* |                                          |
* |------------------------------------------|
*/

%%
"/*"					{ comment(); }

"auto"					{ count(); return(AUTO);      }
"break"					{ count(); return(BREAK);     }
"case"					{ count(); return(CASE);      }
"char"					{ count(); return(CHAR);      }
"const"					{ count(); return(CONST);     }
"continue"				{ count(); return(CONTINUE);  }
"default"				{ count(); return(DEFAULT);   }
"do"					{ count(); return(DO);        }
"double"				{ count(); return(DOUBLE);    }
"else"					{ count(); return(ELSE);      }
"enum"					{ count(); return(ENUM);      }
"extern"				{ count(); return(EXTERN);    }
"float"					{ count(); return(FLOAT);     }
"for"					{ count(); return(FOR);       }
"goto"					{ count(); return(GOTO);      }
"if"					{ count(); return(IF);        }
"int"					{ count(); return(INT);       }
"interrupt"				{ count(); return(INTERRUPT); }
"long"					{ count(); return(LONG);      }
"register"				{ count(); return(REGISTER);  }
"return"				{ count(); return(RETURN);    }
"short"					{ count(); return(SHORT);     }
"signed"				{ count(); return(SIGNED);    }
"sizeof"				{ count(); return(SIZEOF);    }
"static"				{ count(); return(STATIC);    }
"struct"				{ count(); return(STRUCT);    }
"switch"				{ count(); return(SWITCH);    }
"typedef"				{ count(); return(TYPEDEF);   }
"union"					{ count(); return(UNION);     }
"unsigned"				{ count(); return(UNSIGNED);  }
"void"					{ count(); return(VOID);      }
"volatile"				{ count(); return(VOLATILE);  }
"while"					{ count(); return(WHILE);     }

{L}({L}|{D})*			{ count(); return(check_type()); /* May be IDENTIFIER or TYPE_NAME */ }

0[xX]{H}+{IS}?			{ count(); c_newLiteral(yytext);    return(CONSTANT);       }
0{D}+{IS}?				{ count(); c_newLiteral(yytext);    return(CONSTANT);       }
{D}+{IS}?				{ count(); c_newLiteral(yytext);    return(CONSTANT);       }
L?'(\\.|[^\\'])+'		{ count(); c_newLiteral(yytext);    return(CONSTANT);       }

{D}+{E}{FS}?			{ count(); c_newLiteral(yytext);    return(CONSTANT);       }
{D}*"."{D}+({E})?{FS}?	{ count(); c_newLiteral(yytext);    return(CONSTANT);       }
{D}+"."{D}*({E})?{FS}?	{ count(); c_newLiteral(yytext);    return(CONSTANT);       }

L?\"(\\.|[^\\"])*\"		{ count(); c_newLiteral(yytext);    return(STRING_LITERAL); }

"..."					{ count(); return(ELLIPSIS); }
">>="					{ count(); return(RIGHT_ASSIGN); }
"<<="					{ count(); return(LEFT_ASSIGN); }
"+="					{ count(); return(ADD_ASSIGN); }
"-="					{ count(); return(SUB_ASSIGN); }
"*="					{ count(); return(MUL_ASSIGN); }
"/="					{ count(); return(DIV_ASSIGN); }
"%="					{ count(); return(MOD_ASSIGN); }
"&="					{ count(); return(AND_ASSIGN); }
"^="					{ count(); return(XOR_ASSIGN); }
"|="					{ count(); return(OR_ASSIGN); }
">>"					{ count(); return(RIGHT_OP); }
"<<"					{ count(); return(LEFT_OP); }
"++"					{ count(); return(INC_OP); }
"--"					{ count(); return(DEC_OP); }
"->"					{ count(); return(PTR_OP); }
"&&"					{ count(); return(AND_OP); }
"||"					{ count(); return(OR_OP); }
"<="					{ count(); return(LE_OP); }
">="					{ count(); return(GE_OP); }
"=="					{ count(); return(EQ_OP); }
"!="					{ count(); return(NE_OP); }
";"						{ count(); return(';'); }
("{"|"<%")				{ count(); return('{'); }
("}"|"%>")				{ count(); return('}'); }
","						{ count(); return(','); }
":"						{ count(); return(':'); }
"="						{ count(); return('='); }
"("						{ count(); return('('); }
")"						{ count(); return(')'); }
("["|"<:")				{ count(); return('['); }
("]"|":>")				{ count(); return(']'); }
"."						{ count(); return('.'); }
"&"						{ count(); return('&'); }
"!"						{ count(); return('!'); }
"~"						{ count(); return('~'); }
"-"						{ count(); return('-'); }
"+"						{ count(); return('+'); }
"*"						{ count(); return('*'); }
"/"						{ count(); return('/'); }
"%"						{ count(); return('%'); }
"<"						{ count(); return('<'); }
">"						{ count(); return('>'); }
"^"						{ count(); return('^'); }
"|"						{ count(); return('|'); }
"?"						{ count(); return('?'); }

[ \t\v\n\f\r]			{ count(); /* Ignore */   }
.						{ lexErr(ERR_LEX_MSG_00); }





%%
/**
* |------------------------------------------|
* |                                          |
* | User code                                |
* |                                          |
* |------------------------------------------|
*/


int yywrap()
{
	/* Si hay más ficheros pendientes de abrir y compilar, devuelve 0 */
	/* En otro caso, devuelve != 0 */
	return(1);
}



void lexErr(const char* message){
	char buf [255];
	snprintf(buf, sizeof(buf), "\t%.16s (%d:%d)\n\t%s %s\n", yytext, n_linecnt, n_chrcnt, "^", message);
	c_addError(buf);
}



void comment()
{
	register int c;

	n_chrcnt+=2;

	while(1){
		while ((c = input()) != '*' && c != EOF){ 
			COUNT(c)
		}

		if (c == '*'){
			n_chrcnt++;
			while ((c = input()) == '*') /* '*' symbol found */
				n_chrcnt++; 
			if (c == '/'){               /* '/' also found   */
				n_chrcnt++;
				break;
			}
		}

		if (c == EOF){
			c_addError(ERR_LEX_MSG_02);
			break;
		}

		COUNT(c)
	}
}



void count()
{
	int i;

	for (i = 0; yytext[i] != '\0'; i++)
		COUNT(yytext[i])
}



int check_type()
{
	if (c_checkType(yytext) != 0){
		c_newType(yytext); 
		return TYPE_NAME;
	}

	if (yyleng > MAX_ID_LEN) {
		fflush(stdout);
		lexErr(ERR_LEX_MSG_01);
	}

	c_newIdentifier(yytext);
	return IDENTIFIER;
}



const char* getYytext() { return yytext; }
