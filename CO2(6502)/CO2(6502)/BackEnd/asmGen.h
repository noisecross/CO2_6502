/**
 * |------------------------------------------|
 * | CO2 6502, COMPILER OPTIMIZER TO 6502     |
 * | File: asmGen.h                           |
 * | v1.0, December 2012                      |
 * | Author: Emilio Arango Delgado de Mendoza |
 * |------------------------------------------|
 */

#ifndef ASMGenH
#define ASMGenH

#define ASM_MEMORY_MAP_SIZE 2048
#define ASM_START_ADDRESS_S "$8000"
#define ASM_START_ADDRESS_I 0x8000

#include "../CommonFiles/symbolsTable.h"
#include "../CommonFiles/errManager.h"
#include "../CommonFiles/ocGen.h"

#include <list>
#include <string>
#include <bitset>

using namespace std;





/**
* ASM
*
* This class represent a final code instruction
*/
class ASM{

public:
	enum Opcode { ERR = -1, LAB,
		ADC, AND, ASL, BCC, BCS, BEQ, BIT,
		BMI, BNE, BPL, BRK, BVC, BVS, CLC,
		CLD, CLI, CLV, CMP, CPX, CPY, DEC,
		DEX, DEY, EOR, INC, INX, INY, JMP,
		JSR, LDA, LDX, LDY, LSR, NOP, ORA,
		PHA, PHP, PLA, PLP, ROL, ROR, RTI,
		RTS, SBC, SEC, SED, SEI, STA, STX,
		STY, TAX, TAY, TSX, TXA, TXS, TYA };

	enum Adressing {
		A_REGA,
		A_INM ,
		A_ZPG , A_ZPGX, A_ZPGY,
		A_ABS , A_ABSX, A_ABSY,
		A_IND , A_XIND, A_INDY,
		A_IMPL,
		A_REL ,
		A_LABL };
	
	struct Operand{
		Adressing opAdd;
		union{
			unsigned char A_REGA; /* Register A */
			int           A_ABS;  /* Address $HHLL (inc with x or y with carry) */
			unsigned char A_INM;  /* Byte #$BB */
			unsigned char A_IMPL; /* Implicit */
			int           A_IND;  /* ($HHLL) Operand is pointed by $HHLL */
			int           A_XIND; /* ($BB, X) Operand is pointed by $BB+x */
			int           A_INDY; /* ($LL), Y Operand is pointed by $LL and then
			                           added Y with carry */
			unsigned char A_REL;  /* $BB salto a PC + (signed)BB */
			int           A_ZPG;  /* $LL incrementado en X o Y (FUTURE research carry) */
			unsigned int  A_LABL; /* Label */
		}         relatedOp;	
	};
	
	Opcode   opcode;
	string   txtLabel;
	string   comment;
	Operand  operand;
	STEntry* symbol;

	ASM();
	ASM(Opcode, Operand);
	~ASM();

	string toString() const;
	string toBinary() const;
};





/**
* ASMGenerator
*
* This class represent an intermediate code quad
*/
class ASMGenerator{

private:

	class VarMemoryAllocator{

	public:
		/* 2Kbytes - 8 pages of 256 bytes */
		bitset<ASM_MEMORY_MAP_SIZE> memAllocated;

		VarMemoryAllocator();
		~VarMemoryAllocator();

		void mask(bitset<ASM_MEMORY_MAP_SIZE>);
		void copy(bitset<ASM_MEMORY_MAP_SIZE>);
		int  setOfZeroPage(const string &lex, const string &framework);
		int  setVar(const string &lex, const string &framework);
	};


	string                     currentFramework;
	LabelGenerator*            labelGenerator;
	unordered_map<string, int> labelAddress;
	bool                       argMemoryOverlap;

	list<Quad>::iterator       itNextInst;
	list<Quad>::iterator       itLastInst;
	int                        codeSize;

	void        memoryAllocation(const string &, list<STFramework*> *visitedFrameworks, VarMemoryAllocator *freeAllocations, bool recursiveFramework);
	void        generateCode(const Quad &, list<ASM> *);

	void        addInheritedInputSymbol(STEntry*, STFramework*, STFramework*, int);
	void        argPropAndOverlapCheck(unordered_set<string>*, unordered_multimap<string, STFramework*>*, STFramework*);
	void        overlapMemoryAllocations(VarMemoryAllocator *freeAllocations);

	void        addInstr(const ASM &, list<ASM> *);

	inline void generateInstr(ASM::Opcode, Quad::Operand, Quad::Operand, Quad::Operand, list<ASM> *);
	inline void generateInstr(ASM::Opcode, Quad::Operand, Quad::Operand, list<ASM> *);

	inline void generateNOP (const Quad &, list<ASM> *); /* NOP  --- */
	inline void generateADD (const Quad &, list<ASM> *); /* ADD  xyz */
	inline void generateSUB (const Quad &, list<ASM> *); /* SUB  xyz */ 
	inline void generateMUL (const Quad &, list<ASM> *); /* MUL  xyz */ /* Unnused */
	inline void generateDIV (const Quad &, list<ASM> *); /* DIV  xyz */ /* Unnused */
	inline void generateMOD (const Quad &, list<ASM> *); /* MOD  xyz */ /* Unnused */
	inline void generateINC (const Quad &, list<ASM> *); /* INC  xy- */ 
	inline void generateDEC (const Quad &, list<ASM> *); /* DEC  xy- */
	inline void generateLES (const Quad &, list<ASM> *); /* LES  xyz */
	inline void generateRIS (const Quad &, list<ASM> *); /* RIS  xyz */
	inline void generateNEG (const Quad &, list<ASM> *); /* NEG  xy- */
	inline void generateBEQ (const Quad &, list<ASM> *); /* BEQ  xyL */
	inline void generateBNE (const Quad &, list<ASM> *); /* BNE  xyL */
	inline void generateBGR (const Quad &, list<ASM> *); /* BGR  xyL */
	inline void generateBLS (const Quad &, list<ASM> *); /* BLS  xyL */
	inline void generateEQ  (const Quad &, list<ASM> *); /* EQ   xyz */
	inline void generateNE  (const Quad &, list<ASM> *); /* NE   xyz */
	inline void generateGR  (const Quad &, list<ASM> *); /* GR   xyz */
	inline void generateLS  (const Quad &, list<ASM> *); /* LS   xyz */
	inline void generateAND (const Quad &, list<ASM> *); /* AND  xyz */
	inline void generateOR  (const Quad &, list<ASM> *); /* OR   xyz */
	inline void generateLAND(const Quad &, list<ASM> *); /* LAND xyz */
	inline void generateLOR (const Quad &, list<ASM> *); /* LOR  xyz */
	inline void generateXOR (const Quad &, list<ASM> *); /* XOR  xyz */
	inline void generateNOT (const Quad &, list<ASM> *); /* NOT  xy- */
	inline void generateMOVE(const Quad &, list<ASM> *); /* MOVE xy- */
	inline void generateMVP (const Quad &, list<ASM> *); /* MVP  xy- */
	inline void generateMVA (const Quad &, list<ASM> *); /* MVA  xy- */
	inline void generateSTP (const Quad &, list<ASM> *); /* STP  xy- */
	inline void generateSTA (const Quad &, list<ASM> *); /* STA  xy- */
	inline void generateLABL(const Quad &, list<ASM> *); /* LABL --L */
	inline void generateBR  (const Quad &, list<ASM> *); /* BR   --L */
	inline void generatePUSH(const Quad &, list<ASM> *); /* PUSH x-- */ /* Unnused */
	inline void generatePOP (const Quad &, list<ASM> *); /* POP  x-- */ /* Unnused */
	inline void generateCALL(const Quad &, list<ASM> *); /* CALL --L */
	inline void generateRET (const Quad &, list<ASM> *); /* RET  x-- */
	inline void generateHALT(const Quad &, list<ASM> *); /* HALT --- */

	void        generateOverlappedMOVE(const Quad &, list<ASM> *);

	inline ASM  getLoInstruction(ASM::Opcode, const Quad::Operand &);
	inline ASM  getHiInstruction(ASM::Opcode, const Quad::Operand &);

	inline bool isConditionalLabel(const ASM::Opcode &);
	inline void invertJmp(ASM::Opcode*);
	inline void simplifyDoubleJumps();

public:

	list <ASM> code;

	ASMGenerator();
	~ASMGenerator();

	void generate(const list <list <Quad>> &oCode, LabelGenerator*);
	void memoryAllocation(const string &, bool);

	string toString() const;
	string toBinary();
};

#endif
