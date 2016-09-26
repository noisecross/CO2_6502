/* A Bison parser, made by GNU Bison 2.5.  */

/* Bison interface for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2011 Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     IDENTIFIER = 258,
     CONSTANT = 259,
     STRING_LITERAL = 260,
     SIZEOF = 261,
     PTR_OP = 262,
     INC_OP = 263,
     DEC_OP = 264,
     LEFT_OP = 265,
     RIGHT_OP = 266,
     LE_OP = 267,
     GE_OP = 268,
     EQ_OP = 269,
     NE_OP = 270,
     AND_OP = 271,
     OR_OP = 272,
     MUL_ASSIGN = 273,
     DIV_ASSIGN = 274,
     MOD_ASSIGN = 275,
     ADD_ASSIGN = 276,
     SUB_ASSIGN = 277,
     LEFT_ASSIGN = 278,
     RIGHT_ASSIGN = 279,
     AND_ASSIGN = 280,
     XOR_ASSIGN = 281,
     OR_ASSIGN = 282,
     TYPE_NAME = 283,
     TYPEDEF = 284,
     EXTERN = 285,
     STATIC = 286,
     AUTO = 287,
     REGISTER = 288,
     CHAR = 289,
     SHORT = 290,
     INT = 291,
     LONG = 292,
     SIGNED = 293,
     UNSIGNED = 294,
     FLOAT = 295,
     DOUBLE = 296,
     CONST = 297,
     VOLATILE = 298,
     VOID = 299,
     STRUCT = 300,
     UNION = 301,
     ENUM = 302,
     ELLIPSIS = 303,
     CASE = 304,
     DEFAULT = 305,
     INTERRUPT = 306,
     IF = 307,
     ELSE = 308,
     SWITCH = 309,
     WHILE = 310,
     DO = 311,
     FOR = 312,
     GOTO = 313,
     CONTINUE = 314,
     BREAK = 315,
     RETURN = 316
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE gclval;


/* "%code provides" blocks.  */

/* Line 2068 of yacc.c  */
#line 98 "FrontEnd/gc.y"


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




/* Line 2068 of yacc.c  */
#line 143 "gc.tab.h"
