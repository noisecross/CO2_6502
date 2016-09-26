/**
 * |------------------------------------------|
 * | CO2 6502, COMPILER OPTIMIZER TO 6502     |
 * | File: ocGen.h                            |
 * | v1.0, February 2012                      |
 * | Author: Emilio Arango Delgado de Mendoza |
 * |------------------------------------------|
 */

#ifndef OcGenH
#define OcGenH

#define OC_TEMP_PREFIX "t_"

#include "symbolsTable.h"
#include "../FrontEnd/semanticAnalyzer.h"

#include <list>
#include <string>

using namespace std;





/**
* LabelGenerator
*
* This class is just used to return not repeated labels
*/
class LabelGenerator{

private:
	int label;

public:
	LabelGenerator();
	~LabelGenerator();
	string getNewLabel();
};





/**
* Quad
*
* This class represent an intermediate code quad.
*/
class Quad{

public:
	enum Opcode { ERR = -1,
		          NOP , ADD , SUB , MUL , DIV , MOD , INC , DEC , 
				  LES , RIS ,
				  NEG , BR  , BEQ , BNE , BGR , BLS ,
				  EQ  , NE  , GR  , LS  , LAND, LOR ,
				  AND , OR  , XOR , NOT ,
				  MOVE, MVP , MVA , STP , STA ,
				  LABL,
				  PUSH, POP , CALL, RET , HALT };
	/*
    NOP  ---,
	ADD  xyz,  SUB  xyz,  MUL  xyz,  DIV  xyz,  MOD  xyz,  INC  xy-,  DEC  xy-,
	LES  xyz,  RIS  xyz,
	NEG  xy-,  BR   --L,  BEQ  xyL,  BNE  xyL,  BGR  xyL,  BLS  xyL,  EQ   xyz,  NE   xyz, GR   xyz,  LS   xyz,  LAND xyz,  LOR  xyz,
	AND  xyz,  OR   xyz,  XOR  xyz,  NOT  xy-,
	MOVE xy-,  MVP  xy-,  MVA  xy-,  STP  xy-,  STA  xy-,
	LABL --L,
	PUSH x--,  POP  x--,  CALL --L,  RET  x--,
	HALT ---
	*/

	enum OpType { UNDEF = -1, VAR, ADDRESS, CONST, LABEL };

	struct Operand{
		OpType opType;
		string label;
		union{
			STEntry* VAR;
			/* STEntry* ADDRESS; */
			int      CONST;
			int      LABEL;
		}      opSymbol;	
	};

	Opcode  opcode;
	Operand opX;
	Operand opY;
	Operand opZ;
	int     size;

	Quad();
	Quad(Opcode, Operand, Operand, Operand);
	~Quad();

	string toString() const;
};





/**
* OCodeGenerator
*
* This class represent the intermediate code generator engine.
*/
class OCodeGenerator{

private:
	int                  symbolsOffset;
	int                  tempVarCount;
	string               currentFramework;
	bool                 constShortcut;

	LabelGenerator*      labelGenerator;
	list <string>        breakLabel;
	list <string>        continueLabel;

	list <list <Quad>>   caseTest;
	list <string>        caseDefault;
	list <Quad::Operand> caseOperand;
	
	bool                 trueFalseLabelEnabled;
	list <bool>          trueFalseLabelDone;
	list <string>        trueLabels;
	list <string>        falseLabels;
	

	STEntry*             getTemp(QualifiedType);
	void                 releaseTemp();

	inline void          enableTrueFalse(const string &, const string &);
	inline void          disableTrueFalse();
	inline void          swapTrueFalse();

	void                 generateCode(const SyntaxTreeNode*, list<list<Quad>> *, bool);
	Quad::Operand        generateCode(const SyntaxTreeNode*, list<Quad>*, bool);

	inline Quad::Operand generatePrimaryExpression(const SyntaxTreeNode*);
	inline Quad::Operand generatePostfixExpression(const SyntaxTreeNode*, list<Quad>*, bool);
	inline Quad::Operand generateArithmeticExpression(const SyntaxTreeNode*, list<Quad>*, bool);
	inline Quad::Operand generateRelationalExpression(const SyntaxTreeNode*, list<Quad>*, bool);
	inline Quad::Operand generateLogicalExpression(const SyntaxTreeNode*, list<Quad>*, bool);
	inline Quad::Operand generateConditionalExpression(const SyntaxTreeNode*, list<Quad>*, bool);
	inline Quad::Operand generateAssignmentExpression(const SyntaxTreeNode*, list<Quad>*, bool);
	inline Quad::Operand generateExpression(const SyntaxTreeNode*, list<Quad>*, bool);

	inline void          generateLabeledStatement(const SyntaxTreeNode*, list<Quad>*);
	inline void          generateSelectionStatement(const SyntaxTreeNode*, list<Quad>*);
	inline void          generateIterationStatement(const SyntaxTreeNode*, list<Quad>*);
	inline void          generateJumpStatement(const SyntaxTreeNode*, list<Quad>*);
	inline void          generateFunctionDefinition(const SyntaxTreeNode*, list<list<Quad>>*);

	inline void          shortcutExpressionAux(const SyntaxTreeNode*, list<Quad>*, bool*, Quad::Operand*);

	void                 addQuad(list<Quad>*, Quad);

public:
	list <list <Quad>> code;

	OCodeGenerator();
	~OCodeGenerator();

	void   generate(SyntaxTree*, LabelGenerator*, bool);
	void   addToQuadList(list <Quad>);

	string toString() const;
};

#endif
