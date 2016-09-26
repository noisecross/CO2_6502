/**
 * |------------------------------------------|
 * | CO2 6502, COMPILER OPTIMIZER TO 6502     |
 * | File: optimizer.h                        |
 * | v1.0, February 2013                      |
 * | Author: Emilio Arango Delgado de Mendoza |
 * |------------------------------------------|
 */

#ifndef OptimizerH
#define OptimizerH

/* Optimizer constants */
#define MAX_OPT_ITERATIONS 3
#define OPT_END_SYMBOL     "$"				/* Must be a character unused by the ID symbols of the grammar */
#define DES_UNKNOWN        -1

#include "../CommonFiles/symbolsTable.h"
#include "../CommonFiles/ocGen.h"
#include "../BackEnd/asmGen.h"

#include <list>
#include <string>

using namespace std;





/**/


/**
* Register status descriptor
*
* This class represent a memory status descriptor
*/
class RegDescriptor{

public:
	struct Value{
		int                VAL;
		list<int>          VAR;
	};

	enum Register{ A, X, Y };

	Value regA;
	Value regX;
	Value regY;

	RegDescriptor();
	~RegDescriptor();

	void setValue  (int, const Register &);
	void setAddress(int, const Register &);
	void addAddress(int, const Register &);
	void clearReg  (const Register &);
	void delAddress(int);

	void setDescriptor(const ASM &);
};



/**
* Intermediate Code Block
*
* This class represent a block of non-branched code
*/
class OCodeBlock{

public:
	string            currentFramework;
	list<Quad>        oCode;

	list<OCodeBlock*> referencedBy;
	OCodeBlock*       referencingA;
	OCodeBlock*       referencingB;

	bool              isLeaf;

	list<STEntry*>    inUse;
	list<STEntry*>    stillAlive;
	list<STEntry*>    usedYet;
	list<STEntry*>    aliveSymbols;

	OCodeBlock();
	~OCodeBlock();

	inline bool exists(const STEntry*, const list<STEntry*>*); 

	void        addInUse(STEntry*);
	void        addStillAlive(STEntry*);
	void        addUsedYet(STEntry*);

	void        addReferencing(OCodeBlock*);
};



/**
* Final Code Block
*
* This class represent a block of non-branched assembly code
*/
class FCodeBlock{

private:
	bool                checkRedundantInst(const ASM &);
	list<ASM>::iterator killFCInstr(list<ASM>*, list<ASM>::iterator);

public:
	list<ASM>         fCode;
	string            id;
	string            framework;
	bool              isLeaf;
	RegDescriptor     regDescriptor;


	list<FCodeBlock*> referencedBy;
	list<FCodeBlock*> referencing;

	FCodeBlock();
	~FCodeBlock();

	bool                descriptorCalc();
};



/**
* Optimizer
*
* This class represent the optimization engine
*/
class Optimizer{

private:
	bool                              changeDone;
	int                               optimizationGrade;

	list<OCodeBlock> slice(list<Quad>);
	list<Quad>       join(list<OCodeBlock>);

	bool                 isJump(Quad::Opcode);
	bool                 isLabeledJump(Quad::Opcode);
	bool                 isUnconditionalJump(Quad::Opcode);
	bool                 isExpression(Quad::Opcode);
	bool                 sameOperand(Quad::Operand, Quad::Operand);

	list<Quad>::iterator killOCInstr(list<Quad>*, list<Quad>::iterator);
	void                 substituteSymbol(list<Quad>*, STEntry*, STEntry*);

	void                 firstCleaning(list<Quad> *frameworkList);
	list<Quad>::iterator algebraicChange(list<Quad>*, list<Quad>::iterator);
	void                 strengthReduction(list<Quad>::iterator);

	void                 localCommonSubexpressions(list<Quad>*);
	void                 copyPropagation(list<Quad>*);

	void                 lifecycleCalc(list<OCodeBlock>*);
	void                 jumpToJumpShortcircuit(OCodeBlock*);
	void                 deadCodeDeletion(OCodeBlock*);
	void                 tempVarRenaming(OCodeBlock*);
	bool                 movementInstructionsShortcut(const Quad &, Quad*);
	void                 shortcircuitAndUnreachableDeletion(list<OCodeBlock>*);

public:
	Optimizer();
	~Optimizer();

	void optimize(list <list <Quad>>*, int);
	void optimize(list <ASM>*);

	list<FCodeBlock> slice(list<ASM>);
	list<ASM>        join(list<FCodeBlock>);
};

#endif
