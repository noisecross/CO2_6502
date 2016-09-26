/**
 * |------------------------------------------|
 * | CO2 6502, COMPILER OPTIMIZER TO 6502     |
 * | File: asmGen.cpp                         |
 * | v1.0, March 2012                         |
 * | Author: Emilio Arango Delgado de Mendoza |
 * |------------------------------------------|
 */

#include "asmGen.h"
#include "../main.h"
#include <algorithm>
#include <iomanip>

extern SymbolsTable symbolsTable;
extern ErrManager   errManager;





/**
* |------------------------------------------|
* |                                          |
* | 6502 constants                           |
* |                                          |
* |------------------------------------------|
*/

static const int opcodeValue[] ={
/*			A_REGA, A_INM , A_ZPG , A_ZPGX, A_ZPGY, A_ABS , A_ABSX, A_ABSY, A_IND , A_XIND, A_INDY, A_IMPL, A_REL , A_LABL */
/* LAB */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 
/* ADC */	0x00  , 0x69  , 0x65  , 0x75  , 0x00  , 0x6d  , 0x7d  , 0x79  , 0x00  , 0x61  , 0x71  , 0x00  , 0x00  , 0x00  , 
/* AND */	0x00  , 0x29  , 0x25  , 0x35  , 0x00  , 0x2d  , 0x3d  , 0x39  , 0x00  , 0x21  , 0x31  , 0x00  , 0x00  , 0x00  , 
/* ASL */	0x0a  , 0x00  , 0x06  , 0x16  , 0x00  , 0x0e  , 0x1e  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 
/* BCC */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x90  , 0x00  , 
/* BCS */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0xb0  , 0x00  , 
/* BEQ */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0xf0  , 0x00  , 
/* BIT */	0x00  , 0x00  , 0x24  , 0x00  , 0x00  , 0x2c  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 
/* BMI */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x30  , 0x00  , 
/* BNE */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0xd0  , 0x00  , 
/* BPL */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x10  , 0x00  , 
/* BRK */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 
/* BVC */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x50  , 0x00  , 
/* BVS */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x70  , 0x00  , 
/* CLC */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x18  , 0x00  , 0x00  , 
/* CLD */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0xd8  , 0x00  , 0x00  , 
/* CLI */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x58  , 0x00  , 0x00  , 
/* CLV */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0xb8  , 0x00  , 0x00  , 
/* CMP */	0x00  , 0xc9  , 0xc5  , 0xd5  , 0x00  , 0xcd  , 0xdd  , 0xd9  , 0x00  , 0xc1  , 0xd1  , 0x00  , 0x00  , 0x00  , 
/* CPX */	0x00  , 0xe0  , 0xe4  , 0x00  , 0x00  , 0xec  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 
/* CPY */	0x00  , 0xc0  , 0xc4  , 0x00  , 0x00  , 0xcc  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 
/* DEC */	0x00  , 0x00  , 0xc6  , 0xd6  , 0x00  , 0xce  , 0xde  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 
/* DEX */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0xca  , 0x00  , 0x00  , 
/* DEY */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x88  , 0x00  , 0x00  , 
/* EOR */	0x00  , 0x49  , 0x45  , 0x55  , 0x00  , 0x4d  , 0x5d  , 0x59  , 0x00  , 0x41  , 0x51  , 0x00  , 0x00  , 0x00  , 
/* INC */	0x00  , 0x00  , 0xe6  , 0xf6  , 0x00  , 0xee  , 0xfe  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 
/* INX */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0xe8  , 0x00  , 0x00  , 
/* INY */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0xc8  , 0x00  , 0x00  , 
/* JMP */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x4c  , 0x00  , 0x00  , 0x6c  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 
/* JSR */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x20  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 
/* LDA */	0x00  , 0xa9  , 0xa5  , 0xb5  , 0x00  , 0xad  , 0xbd  , 0xb9  , 0x00  , 0xa1  , 0xb1  , 0x00  , 0x00  , 0x00  , 
/* LDX */	0x00  , 0xa2  , 0xa6  , 0x00  , 0xb6  , 0xae  , 0x00  , 0xbe  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 
/* LDY */	0x00  , 0xa0  , 0xa4  , 0xb4  , 0x00  , 0xac  , 0xbc  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 
/* LSR */	0x4a  , 0x00  , 0x46  , 0x56  , 0x00  , 0x4e  , 0x5e  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 
/* NOP */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0xea  , 0x00  , 0x00  , 
/* ORA */	0x00  , 0x09  , 0x05  , 0x15  , 0x00  , 0x0d  , 0x1d  , 0x19  , 0x00  , 0x01  , 0x11  , 0x00  , 0x00  , 0x00  , 
/* PHA */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x48  , 0x00  , 0x00  , 
/* PHP */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x08  , 0x00  , 0x00  , 
/* PLA */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x68  , 0x00  , 0x00  , 
/* PLP */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x28  , 0x00  , 0x00  , 
/* ROL */	0x2a  , 0x00  , 0x26  , 0x36  , 0x00  , 0x2e  , 0x3e  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 
/* ROR */	0x6a  , 0x00  , 0x66  , 0x76  , 0x00  , 0x6e  , 0x7e  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 
/* RTI */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x40  , 0x00  , 0x00  , 
/* RTS */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x60  , 0x00  , 0x00  , 
/* SBC */	0x00  , 0xe9  , 0xe5  , 0xf5  , 0x00  , 0xed  , 0xfd  , 0xf9  , 0x00  , 0xe1  , 0xf1  , 0x00  , 0x00  , 0x00  , 
/* SEC */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x38  , 0x00  , 0x00  , 
/* SED */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0xf8  , 0x00  , 0x00  , 
/* SEI */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x78  , 0x00  , 0x00  , 
/* STA */	0x00  , 0x00  , 0x85  , 0x95  , 0x00  , 0x8d  , 0x9d  , 0x99  , 0x00  , 0x81  , 0x91  , 0x00  , 0x00  , 0x00  , 
/* STX */	0x00  , 0x00  , 0x86  , 0x00  , 0x96  , 0x8e  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 
/* STY */	0x00  , 0x00  , 0x84  , 0x94  , 0x00  , 0x8c  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 
/* TAX */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0xaa  , 0x00  , 0x00  , 
/* TAY */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0xa8  , 0x00  , 0x00  , 
/* TSX */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0xba  , 0x00  , 0x00  , 
/* TXA */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x8a  , 0x00  , 0x00  , 
/* TXS */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x9a  , 0x00  , 0x00  , 
/* TYA */	0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x00  , 0x98  , 0x00  , 0x00
};

static const int instSizes[] ={
	 1, 2,-1,-1,-1, 2, 2,-1, 1, 2, 1,-1,-1, 3, 3,-1,
	 2, 2,-1,-1,-1, 2, 2,-1, 1, 3,-1,-1,-1, 3, 4,-1,
	 3, 2,-1,-1, 2, 2, 2,-1, 1, 2, 1,-1, 3, 3, 3,-1,
	 2, 2,-1,-1,-1, 2, 2,-1, 1, 3,-1,-1,-1, 3, 3,-1,
	 1, 2,-1,-1,-1, 2, 2,-1, 1, 2, 1,-1, 3, 3, 3,-1,
	 2, 2,-1,-1,-1, 2, 2,-1, 1, 3,-1,-1,-1, 3, 3,-1,
	 1, 2,-1,-1,-1, 2, 2,-1, 1, 2, 1,-1, 3, 3, 3,-1,
	 2, 2,-1,-1,-1, 2, 2,-1, 1, 3,-1,-1,-1, 3, 3,-1,
	-1, 2,-1,-1, 2, 2, 2,-1, 1,-1, 1,-1, 3, 3, 3,-1,
	 2, 2,-1,-1, 2, 2, 2,-1, 1, 3, 1,-1,-1, 3,-1,-1,
	 2, 2, 2,-1, 2, 2, 2,-1, 1, 2, 1,-1, 3, 3, 3,-1,
	 2, 2,-1,-1, 2, 2, 2,-1, 1, 3, 1,-1, 3, 3, 3,-1,
	 2, 2,-1,-1, 2, 2, 2,-1, 1, 2, 1,-1, 3, 3, 3,-1,
	 2, 2,-1,-1,-1, 2, 2,-1, 1, 3,-1,-1,-1, 3, 3,-1,
	 2, 2,-1,-1, 2, 2, 2,-1, 1, 2, 1,-1, 3, 3, 3,-1,
	 2, 2,-1,-1,-1, 2, 2,-1, 1, 3,-1,-1,-1, 3, 3,-1
};

static const int instCycles[] ={			
	 7, 6,-1,-1,-1, 3, 5,-1, 3, 2, 2,-1,-1, 4, 6,-1,
	 2, 5,-1,-1,-1, 4, 6,-1, 2, 4,-1,-1,-1, 4, 7,-1,
	 6, 6,-1,-1, 3, 3, 5,-1, 4, 2, 2,-1, 4, 4, 6,-1,
	 2, 5,-1,-1,-1, 4, 6,-1, 2, 4,-1,-1,-1, 4, 7,-1,
	 6, 6,-1,-1,-1, 3, 5,-1, 3, 2, 2,-1, 3, 4, 6,-1,
	 2, 5,-1,-1,-1, 4, 6,-1, 2, 4,-1,-1,-1, 4, 7,-1,
	 6, 6,-1,-1,-1, 3, 5,-1, 4, 2, 2,-1, 5, 4, 6,-1,
	 2, 5,-1,-1,-1, 4, 6,-1, 2, 4,-1,-1,-1, 4, 7,-1,
	-1, 6,-1,-1, 3, 3, 3,-1, 2,-1, 2,-1, 4, 4, 4,-1,
	 2, 6,-1,-1, 4, 4, 4,-1, 2, 5, 2,-1,-1, 5,-1,-1,
	 2, 6, 2,-1, 3, 3, 3,-1, 2, 2, 2,-1, 4, 4, 4,-1,
	 2, 5,-1,-1, 4, 4, 4,-1, 2, 4, 2,-1, 4, 4, 4,-1,
	 2, 6,-1,-1, 3, 3, 5,-1, 2, 2, 2,-1, 4, 4, 6,-1,
	 2, 5,-1,-1,-1, 4, 6,-1, 2, 4,-1,-1,-1, 4, 7,-1,
	 2, 6,-1,-1, 3, 3, 5,-1, 2, 2, 2,-1, 4, 4, 6,-1,
	 2, 5,-1,-1,-1, 4, 6,-1, 2, 4,-1,-1,-1, 4, 7,-1
};



/**
* operandSize
* 
* Auxiliar function. Get the size of a given argument
*
* @param op: The operand whose size is wanted
*
* @return an integer representing the size value of the input argument
*/
static int operandSize(const Quad::Operand &op){
	/* FUTURE <struct> */

	if(op.opType == Quad::VAR || op.opType == Quad::ADDRESS){
		/* Look at symbols table */
		return symbolsTable.getTypeSize(op.opSymbol.VAR->getType());
	}else{
		/* Constant. Choose smaller size */
		if(op.opSymbol.CONST >= -128 && op.opSymbol.CONST < 128)
			return 1;
		else
			return 2;
	}
}



/**
* operandIsUnsigned
* 
* Auxiliar function. Get the sinature of a given argument
*
* @param op: The operand whose sign is wanted
*
* @return true if the operand is unsigned
*/
static bool operandIsUnsigned(const Quad::Operand &op){
	if(op.opType == Quad::VAR || op.opType == Quad::ADDRESS){
		/* Look at symbols table */
		return op.opSymbol.VAR->isUnsigned();
	}else{
		return false;
	}
}



/**
* |------------------------------------------|
* |                                          |
* | Class: ASM                               |
* |                                          |
* |------------------------------------------|
*/

/**
* ASM
* 
* Class constructor.
*/
ASM::ASM(){
	opcode                   = ASM::ERR;
	operand.opAdd            = ASM::A_IMPL;
	operand.relatedOp.A_IMPL = 0;
	symbol                   = NULL;
}



/**
* ASM
* 
* Class constructor.
*/
ASM::ASM(Opcode inOpcode, Operand inOperand){
	opcode  = inOpcode;
	operand = inOperand;
	symbol  = NULL;
}



/**
* ~ASM
* 
* Class destructor.
*/
ASM::~ASM(){ }



/**
* toString
*
* @return string containing the text formated asm instruction.
*/
string ASM::toString() const{

	const string opcodeTxt[] ={ "lab ",
		"adc ", "and ", "asl ", "bcc ", "bcs ", "beq ", "bit ",
		"bmi ", "bne ", "bpl ", "brk ", "bvc ", "bvs ", "clc ",
		"cld ", "cli ", "clv ", "cmp ", "cpx ", "cpy ", "dec ",
		"dex ", "dey ", "eor ", "inc ", "inx ", "iny ", "jmp ",
		"jsr ", "lda ", "ldx ", "ldy ", "lsr ", "nop ", "ora ",
		"pha ", "php ", "pla ", "plp ", "rol ", "ror ", "rti ",
		"rts ", "sbc ", "sec ", "sed ", "sei ", "sta ", "stx ",
		"sty ", "tax ", "tay ", "tsx ", "txa ", "txs ", "tya " };

	if (opcode < LAB) {
		/* Error */
		return "";
	} else if (opcode == LAB) {
		/* Label */
		return string(txtLabel).append(":"); 
	}


	ostringstream result;
	string operandStr;

	int opSize;
	int remainingTabs;

	result.fill('\t');

	#pragma warning ( disable : 4127)
	if(JUST_ONE_TAB){
		result << "\t";
	}else{
		result << left << setw((MAX_ID_LEN + 8) / TAB_SIZE) << "";
	}
	#pragma warning ( default : 4127)

	result << opcodeTxt[opcode];

	result.fill('0');

	switch(operand.opAdd){

	case A_REGA:
		/* Register A */
		/* result << "A"; */
		result.fill('\t');
		result << left << setw((MAX_ID_LEN + 9) / TAB_SIZE) << "";
		break;

	case A_INM :
		/* Byte #$BB */
		result << right;
		result << "#$" << setw(2) << hex << (int)operand.relatedOp.A_INM;
		result.fill('\t');
		result << left << setw((MAX_ID_LEN + 6) / TAB_SIZE) << "";
		break;

	case A_ZPG :
		/* $LL */
		if (txtLabel == ""){
			result << right;
			result << "$" << setw(2) << hex << (int)operand.relatedOp.A_ZPG;
			result.fill('\t');
			result << left << setw((MAX_ID_LEN + 7) / TAB_SIZE) << "";
		}else{
			result.fill('\t');
			result << txtLabel << left << setw((MAX_ID_LEN + 10 - txtLabel.size()) / TAB_SIZE) << "";
		}
		break;

	case A_ZPGX:
		/* $LL, X */
		if (txtLabel == ""){
			result << right;
			result << "$" << setw(2) << hex << (int)operand.relatedOp.A_ZPG;
			result << ", X";
			result.fill('\t');
			result << left << setw((MAX_ID_LEN + 5) / TAB_SIZE) << "";
		}else{
			result.fill('\t');
			result << txtLabel << ",X" << left << setw((MAX_ID_LEN + 8 - txtLabel.size()) / TAB_SIZE) << "";
		}
		break;

	case A_ZPGY:
		/* $LL, Y */
		if (txtLabel == ""){
			result << right;
			result << "$" << setw(2) << hex << (int)operand.relatedOp.A_ZPG;
			result << ", Y";
			result.fill('\t');
			result << left << setw((MAX_ID_LEN + 5) / TAB_SIZE) << "";
		}else{
			result.fill('\t');
			result << txtLabel << ",Y" << left << setw((MAX_ID_LEN + 8 - txtLabel.size()) / TAB_SIZE) << "";
		}
		break;

	case A_ABS :
		/* Address $HHLL */
		if (opcode == JMP || opcode == JSR){
			opSize = (txtLabel.size() > (MAX_ID_LEN + 9)) ? (MAX_ID_LEN + 10) : txtLabel.size();
			remainingTabs = opSize + ((MAX_ID_LEN + 10) - opSize) / TAB_SIZE;
			result.fill('\t');
			result << setw(remainingTabs) << (txtLabel).substr(0, (MAX_ID_LEN + 9));
		}else{
			if (txtLabel == ""){
				result << right;
				result << "$" << setw(4) << hex << operand.relatedOp.A_ABS;
				result.fill('\t');
				result << left << setw((MAX_ID_LEN + 5) / TAB_SIZE) << "";
			}else{
				result.fill('\t');
				result << txtLabel << left << setw((MAX_ID_LEN + 10 - txtLabel.size()) / TAB_SIZE) << "";
			}
		}
		break;

	case A_ABSX:
		/* Address $HHLL, X */
		if (txtLabel == ""){
			result << right;
			result << "$" << setw(4) << hex << operand.relatedOp.A_ABS;
			result << ", X";
			result.fill('\t');
			result << left << setw((MAX_ID_LEN + 3) / TAB_SIZE) << "";
		}else{
			result.fill('\t');
			result << txtLabel << ",X" << left << setw((MAX_ID_LEN + 8 - txtLabel.size()) / TAB_SIZE) << "";
		}
		break;

	case A_ABSY:
		/* Address $HHLL, Y */
		if (txtLabel == ""){
			result << right;
			result << "$" << setw(4) << hex << operand.relatedOp.A_ABS;
			result << ", Y";
			result.fill('\t');
			result << left << setw((MAX_ID_LEN + 3) / TAB_SIZE) << "";
		}else{
			result.fill('\t');
			result << txtLabel << ",Y" << left << setw((MAX_ID_LEN + 8 - txtLabel.size()) / TAB_SIZE) << "";
		}
		break;

	case A_IND :
		/* ($HHLL) Operand is pointed by $HHLL */
		if (txtLabel == ""){
			result << right;
			result << "($" << setw(4) << hex << operand.relatedOp.A_ABS;
			result << ")";
			result.fill('\t');
			result << left << setw((MAX_ID_LEN + 3) / TAB_SIZE) << "";
		}else{
			result.fill('\t');
			result << "(" << txtLabel << ")" << left << setw((MAX_ID_LEN + 8 - txtLabel.size()) / TAB_SIZE) << "";
		}
		break;

	case A_XIND:
		/* ($BB, X) Operand is pointed by $BB+x */
		if (txtLabel == ""){
			result << right;
			result << "($" << setw(2) << hex << operand.relatedOp.A_XIND;
			result << ", X)";
			result.fill('\t');
			result << left << setw((MAX_ID_LEN + 2) / TAB_SIZE) << "";
		}else{
			result.fill('\t');
			result << "(" << txtLabel << ",X)" << left << setw((MAX_ID_LEN + 10 - txtLabel.size()) / TAB_SIZE) << "";
		}
		break;

	case A_INDY:
		/* ($LL), Y Operand is pointed by $LL and then added Y with carry */
		if (txtLabel == ""){
			result << right;
			result << "($" << setw(2) << hex << operand.relatedOp.A_INDY;
			result << "), Y";
			result.fill('\t');
			result << left << setw((MAX_ID_LEN + 2) / TAB_SIZE) << "";
		}else{
			result.fill('\t');
			result << "(" << txtLabel << "),Y" << left << setw((MAX_ID_LEN + 6 - txtLabel.size()) / TAB_SIZE) << "";
		}
		break;

	case A_IMPL:
		/* Implicit */
		result.fill('\t');
		result << left << setw((MAX_ID_LEN + 10) / TAB_SIZE) << "";
		break;

	case A_REL :
		/* $BB salto a PC + (signed)BB */
		opSize = (txtLabel.size() > (MAX_ID_LEN + 9)) ? (MAX_ID_LEN + 10) : txtLabel.size();
		remainingTabs = opSize + ((MAX_ID_LEN + 10) - opSize) / TAB_SIZE;
		result.fill('\t');
		result << setw(remainingTabs) << (txtLabel).substr(0, (MAX_ID_LEN + 9));
		/*if(operand.relatedOp.A_REL == 0){
			opSize = (txtLabel.size() > (MAX_ID_LEN + 9)) ? (MAX_ID_LEN + 10) : txtLabel.size();
			remainingTabs = opSize + ((MAX_ID_LEN + 10) - opSize) / TAB_SIZE;
			result.fill('\t');
			result << setw(remainingTabs) << (txtLabel).substr(0, (MAX_ID_LEN + 9));
		}else{
			result << right;
			result << "$" << setw(2) << hex << (operand.relatedOp.A_REL & 0xFF);
			result.fill('\t');
			result << left << setw((MAX_ID_LEN + 2) / TAB_SIZE) << "";
		}*/
		break;
	}

	if (comment != ""){
		result << "; " << toBinary() << ", " << comment;
	}else{
		result << "; " << toBinary();
	}

	return result.str();
}



/**
* toBinary
*
* @return string containing the text formated binary instruction.
*/
string ASM::toBinary() const{
	ostringstream output;
	
	if (opcode == LAB)
		return "";

	int asmOpcode = opcodeValue[(opcode * 14) + operand.opAdd];
	int nBytes    = instSizes[asmOpcode];

	output.fill('0');
	output << setw(2) << hex << asmOpcode;

	if (nBytes > 1){
		output  << " " << setw(2) << hex << (operand.relatedOp.A_ABS & 0x00FF);
		if (nBytes > 2){
			output  << " " << setw(2) << hex << ((operand.relatedOp.A_ABS & 0xFF00) >> 8);
		}
	}

	/* output << endl; */

	return output.str();
}





/**
* |------------------------------------------|
* |                                          |
* | Class: ASMGenerator                      |
* |                                          |
* |------------------------------------------|
*/

/**
* ASMGenerator
* 
* Class constructor.
*/
ASMGenerator::ASMGenerator(){
	code             = list <ASM>();
	labelGenerator   = NULL;
	labelAddress     = unordered_map<string, int>();
	currentFramework = "";
}



/**
* ~ASMGenerator
* 
* Class destructor.
*/
ASMGenerator::~ASMGenerator(){
	code.clear();
	labelAddress.clear();
}



/**
* memoryAllocation
* 
* Creates a VarMemoryAllocator object and allocates every symbol in a given framework in a free memory address.
* To do that the function calls itself recursively with every posible "dependant" framework as argument.
* The "dependant" frameworks are the frameworks of the functions that could be called from the input framework.
*
* @param framework: The name of the "main" framework.
*/
void ASMGenerator::memoryAllocation(const string &framework, bool inArgMemoryOverlap){
	list<STFramework*> visitedFrameworks = list<STFramework*>();
	VarMemoryAllocator freeAllocations   = VarMemoryAllocator();
	list<STFramework*>           frameworks = symbolsTable.getFrameworks();
	list<STFramework*>::iterator it         = frameworks.begin();

	auto f_assignInterrupts = [this, &visitedFrameworks, &freeAllocations](STFramework* x){
		if (x->getOutputType().b_interrupt)
			memoryAllocation(x->getName(), &visitedFrameworks, &freeAllocations, false);
	};

	argMemoryOverlap = inArgMemoryOverlap;

	/* Overlap memory args */
	if(inArgMemoryOverlap){
		overlapMemoryAllocations(&freeAllocations);
	}

	/* Assign memory to interruptions */
	for_each(frameworks.begin(), frameworks.end(), f_assignInterrupts);

	/* Assignm memory to (reachable and non recursive) frameworks */
	memoryAllocation(framework, &visitedFrameworks, &freeAllocations, false);

	/* Assignm memory to global variables */
	memoryAllocation(GLOBAL_FRAMEWORK, &visitedFrameworks, &freeAllocations, false);

	/* Assignm memory to recursive frameworks */
	for_each(frameworks.begin(), frameworks.end(), [this, &visitedFrameworks, &freeAllocations](STFramework* x){
		if (x->isRecursive()){
			memoryAllocation(x->getName(), &visitedFrameworks, &freeAllocations, true);
		}
	});

	/* Set the memory needed into the symbolsTable */
	symbolsTable.setMemSize(freeAllocations.memAllocated.count());
}



/**
* memoryAllocation
* 
* Allocates every symbol in a given framework in a free memory address.
* To do that the function calls itself recursively with every posible "dependant" framework as argument.
* The "dependant" frameworks are the frameworks of the functions that could be called from the input framework.
*
* @param framework: The name of a framework.
* @param visitedFrameworks: The previously visited frameworks.
* @param freeAllocations: The remaining allocations to avoid address overlap.
*/
void ASMGenerator::memoryAllocation(const string &framework, list<STFramework*> *visitedFrameworks, VarMemoryAllocator *freeAllocations, bool recursiveFramework){
	VarMemoryAllocator finalMask;
	VarMemoryAllocator childrenMask;
	list<STEntry*>     symbolsToAddress;
	list<STFramework*> referencedFrameworks;

	
	auto f_addressRegsAndPointers = [&framework, &freeAllocations](STEntry* x){
		/* FUTURE <register> */
		if (x->getPointerDepth() > 0){
			if (x->getAddress() < 0){
				if (freeAllocations->setOfZeroPage(x->lex, framework) < 0){
					if (!errManager.existsError())
						errManager.addError(ERR_ASM_MSG_00);
				}
			}else{
				freeAllocations->memAllocated[x->getAddress()]     = true;
				freeAllocations->memAllocated[x->getAddress() + 1] = true;
			}
		}
	};


	auto f_addressRefferenced = [this, &framework, &freeAllocations, &finalMask, &childrenMask, &visitedFrameworks](STFramework* x){
		bool visited = false;

		list<STFramework*>::iterator it = visitedFrameworks->begin();
		while(it != visitedFrameworks->end() && !visited)
			if ((*it++)->getName() == x->getName()) visited = true;
		
		childrenMask.copy(freeAllocations->memAllocated);

		if(!visited){
			if (!symbolsTable.getOutputType(x->getName()).b_interrupt)
				memoryAllocation(x->getName(), visitedFrameworks, &childrenMask, false);
		}else{
			if (!errManager.existsError())
				errManager.addError(ERR_ASM_MSG_00);
		}

		finalMask.mask(childrenMask.memAllocated);
	};


	auto f_addressVars = [&framework, &freeAllocations](STEntry* x){
		if (x->getAddress()<0){
			/* Get new adresses */
			if (freeAllocations->setOfZeroPage(x->lex, framework) < 0)
				if (freeAllocations->setVar(x->lex, framework) < 0)
					if (!errManager.existsError())
						errManager.addError(ERR_ASM_MSG_01);
		}else{
			/* Mark the adresses as non free */
			if (x->getAddress() < 0x0100){
				for (int i = 0 ; i < symbolsTable.getTypeSize(x->getType()) ; i ++){
					freeAllocations->memAllocated[x->getAddress() + i] = true;
				}
			}else{
				for (int i = 0 ; i < symbolsTable.getTypeSize(x->getType()) ; i ++){
					freeAllocations->memAllocated[x->getAddress() + (i * 256)] = true;
				}
			}
		}
	};


	/* 0- If this framework is recursive, nothing to do here, else assign addresses and go */
	if (symbolsTable.getFramework(framework)->isRecursive()){
		if(!recursiveFramework)
			return;
		else{
			symbolsToAddress = symbolsTable.getSymbols(framework);
			for_each(symbolsToAddress.begin(), symbolsToAddress.end(), f_addressRegsAndPointers);
			for_each(symbolsToAddress.begin(), symbolsToAddress.end(), f_addressVars);

			/* for_each non-recursive child */
			referencedFrameworks = symbolsTable.getDependencies(framework);
			for_each(referencedFrameworks.begin(), referencedFrameworks.end(), [this, &visitedFrameworks, &freeAllocations](STFramework* x){
				memoryAllocation(x->getName(), visitedFrameworks, freeAllocations, false);
			});
			return;
		}
	}

	/* 1- Set this into list */
	visitedFrameworks->push_back(symbolsTable.getFramework(framework));

	/* 2- Assign pointers & 'register' variables */
	symbolsToAddress = symbolsTable.getSymbols(framework);
	for_each(symbolsToAddress.begin(), symbolsToAddress.end(), f_addressRegsAndPointers);

	/* 3- Copy mask and free allocation */
	finalMask.copy(freeAllocations->memAllocated);
	childrenMask.copy(freeAllocations->memAllocated);

	/* 4- for_each child */
	referencedFrameworks = symbolsTable.getDependencies(framework);
	for_each(referencedFrameworks.begin(), referencedFrameworks.end(), f_addressRefferenced);

	/* 5- Mask */
	freeAllocations->mask(finalMask.memAllocated);

	/* 6- Assign the others */
	for_each(symbolsToAddress.begin(), symbolsToAddress.end(), f_addressVars);

	/* 7- Return */
	visitedFrameworks->pop_back();
}



/**
* addInheritedInputSymbol
*
* Auxiliar method to help the argPropAndOverlapCheck to perform its labour. This method calculates the diferent symbols that can be
* overlapped in an argument of a framework and check if that argument is used again as argument to call another framework. If that is
* the case, the new argument should inherit the arguments of the first one.
*
* @param entry: The symbol to check
* @param source: The source framework (the caller)
* @param destination: The destination framework (the called)
* @param position: The position of the destination where the inherency should be performed
*/
void ASMGenerator::addInheritedInputSymbol(STEntry* entry, STFramework* source, STFramework* destination, int position){
	list<string>           inArgs = source->getInputSymbols();
	list<string>::iterator it;
	int                    i;

	list<STEntry*>          symbolsToAdd;

	if (source->isRecursive() || destination->isRecursive()){
		return;
	}

	for (i = 0, it = inArgs.begin() ; it != inArgs.end() ; i++, it++){
		if(*it == entry->lex)
			break;
	}

	if(it == inArgs.end()){
		return;
	}

	symbolsToAdd = source->getInputArguments(i);

	for_each(symbolsToAdd.begin(), symbolsToAdd.end(), [&destination, &position](STEntry* x){
		destination->addInputArgument(x, position);
	});
}



/**
* argPropAndOverlapCheck
*
* Checks if an input/output argument can be avoided via memory overlap and mark the symbol as not overlappable if that is not the case.
*
* @param tested: The tested frameworks.
* @param referencedBy: The map of the frameworks referenced (called) by other frameworks.
* @param x: The framework to perform the overlappable argument check.
*/
void ASMGenerator::argPropAndOverlapCheck(unordered_set<string>* tested, unordered_multimap<string, STFramework*>* referencedBy, STFramework* x){
	list<STFramework*> dependencies = x->getDependencies();

	auto f_ensureTested = [this, &tested, &referencedBy](pair<string, STFramework*> x){
		if(tested->find(x.second->getName()) == tested->end()){
			argPropAndOverlapCheck(tested, referencedBy, x.second);
		}
	};


	/* If x is recursive, every argument is not overlappable */
	if(x->isRecursive()){
		return;
	}

	/* Ensure every referencedBy has been visited */
	auto range = referencedBy->equal_range(x->getName());
	for_each(range.first, range.second, f_ensureTested);

	/* For each input arg which is also used in a call, spread to lower levels */
	/* For each child */
	for_each(dependencies.begin(), dependencies.end(), [this, &x](STFramework* y){
		list<QualifiedType>           argTypes = y->getInputType();
		list<QualifiedType>::iterator it       = argTypes.begin();

		if(y->isRecursive())
			return;

		/* For each argument list */
		for(unsigned int i = 0 ; i < argTypes.size() ; it++, i++){
			if (it->b_const || it->pointerDepth > 0){
				/* If the list contains an argument from x, add the related to the list */
				list<STEntry*>           argInSymbols = y->getInputArguments(i);
				list<STEntry*>::iterator auxIt;

				for(auxIt = argInSymbols.begin() ; auxIt != argInSymbols.end() ; auxIt++){
					if((*auxIt)->getFramework() == x->getName()){
						/* Coincidence. Add every reference */
						addInheritedInputSymbol(*auxIt, x, y, i);
					}
				}
			}
		}
	});

	/* If a STEntry appears in two different arguments in that same framework */
	list<STEntry*>::iterator it1;
	list<STEntry*>::iterator it2;
	list<STEntry*>           argList1;
	list<STEntry*>           argList2;

	unsigned int             nArguments = x->getInputSymbols().size();

	for(int i = 0 ; i < (int)nArguments - 1 ; i++){
		argList1 = x->getInputArguments(i);

		for(unsigned int j = i + 1 ; j < nArguments ; j++){
			argList2 = x->getInputArguments(j);

			for (it1 = argList1.begin() ; it1 != argList1.end() ; it1++){
				for (it2 = argList2.begin() ; it2 != argList2.end() ; it2++){
					if(*it1 == *it2){
						/* If is an input/output arg */
						if((*it1)->getPointerDepth() > 0){
							/* No overlap is allowed to both of them */
							(*it1)->setOverlappable(false);
							(*it2)->setOverlappable(false);
							/* Uptdate its references. The children frameworks are not bounded to these arguments anymore! */
							/* FUTURE <optimize Uptdate its references> */
							/* Nothing to do here */
							break;
						/* Else if const */
						}else if((*it1)->isConst()){
							/* Delete one of them */
							it2 = argList2.erase(it2);
						}
					}
				}
			}
		}
	}

	/* Mark this framework as tested */
	tested->insert(x->getName());
}



/**
* overlapMemoryAllocations
* 
* Perform the calculus to realize which argument symbols (function inputs) can be overlaped in memory.
*
* @param framework: The name of the main framework.
* @param freeAllocations: The remaining allocations to avoid undesired address overlap.
*/
void ASMGenerator::overlapMemoryAllocations(VarMemoryAllocator* freeAllocations){
	/* PRE: Each arg in each framework contains a symbol list with the symbols to be overlapped */

	list<STFramework*>                       frameworks   = symbolsTable.getFrameworks();
	unordered_multimap<string, STFramework*> referencedBy = unordered_multimap<string, STFramework*>();
	unordered_set<string>                    tested       = unordered_set<string>();

	/* For each framework */
	for_each(frameworks.begin(), frameworks.end(), [&referencedBy](STFramework* x){
		/* Add each referenced to referencedBy */
		if (x->isRecursive())
			return;

		list<STFramework*> dependencies = x->getDependencies();

		for (auto it = dependencies.begin() ; it != dependencies.end() ; it++){
			if(!(*it)->isRecursive())
				referencedBy.insert(make_pair((*it)->getName(), x));
		}
	});

	/* For each framework */
	for_each(frameworks.begin(), frameworks.end(), [this, &tested, &referencedBy](STFramework* x){
		if (x->isRecursive())
			return;

		/* Argument propagation and overlap check */
		argPropAndOverlapCheck(&tested, &referencedBy, x);
	});

	/* Addressing memory overlap to input/output symbols and (non input/output) const symbols */
	for_each(frameworks.begin(), frameworks.end(), [this, &tested, &referencedBy, &freeAllocations](STFramework* x){
		if (x->isRecursive())
			return;

		list<string>           inputArguments = x->getInputSymbols();
		list<string>::iterator it             = inputArguments.begin();

		for(int i = 0 ; it != inputArguments.end() ; it++, i++){

			/* If argument is in/out or const and is overlapAllowance[x->getId()][i] */
			STEntry* symbol = symbolsTable.getSymbol(*it, x->getName());
			if(symbol == NULL) continue;

			if((symbol->isConst() || symbol->getPointerDepth() > 0) && symbol->isOverlappable()){
				list<STEntry*>           args    = x->getInputArguments(i);
				list<STEntry*>::iterator itAux   = args.begin();
				int                      address = -1;

				/* FUTURE <optimize this check> */
				for( ; itAux != args.end() ; itAux++){
					if((*itAux)->getAddress() != -1){
						address = (*itAux)->getAddress();
						break;
					}
				}

				/* If an address has been assigned put the same address to everyone */
				if (address != -1){
					symbol->setAddress(address);
				}else{
					/* Otherwise, get a new address and apply to every overlaped symbol */
					address = freeAllocations->setOfZeroPage(*it, x->getName());
				}

				for_each(args.begin(),args.end(),[&address](STEntry* x){
					x->setAddress(address);
				});
			}
		}
	});
}



/**
* addInstr
* 
* Aux function. Perform an adding into a code list and increment the size of the current framework code.
*
* @param quad: The quad which is being inserted.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::addInstr(const ASM &newInstruction, list<ASM> *asmCodeList){

	asmCodeList->push_back(newInstruction);

	if (newInstruction.opcode == ASM::LAB)
		return;

	int asmOpcode = opcodeValue[(newInstruction.opcode * 14) +
		                        (newInstruction.operand.opAdd)];
	int nBytes    = instSizes[asmOpcode];

	codeSize += nBytes;
}



/**
* generate
*
* Trigger the translation from intermediate code to asm code.
*
* @param oCode: The list of lists of Quad objets where the intermediate code is codified.
* @param inLabelGenerator: An external LabelGenerator object to get the auxiliary labels during the ASM generation.
*/
void ASMGenerator::generate(const list <list <Quad>> &oCode, LabelGenerator* inLabelGenerator){

    auto f_generateInstr = [this](Quad x){
        generateCode(x, &code);
    };

    auto f_generateCode = [this, f_generateInstr](list <Quad> x){
        list<Quad>::iterator it     = x.begin();
        STFramework*         framework;
      
        currentFramework = string(x.front().opZ.label).substr(2, x.front().opZ.label.size());
        framework        = symbolsTable.getFramework(currentFramework);
		itLastInst       = x.end();

        codeSize = 0;
        generateCode(*it, &code);

        if(symbolsTable.getOutputType(currentFramework).b_interrupt){
            /* 01- PHA */
            ASM::Operand operand = {ASM::A_IMPL, 0};
            ASM instr            = ASM(ASM::PHA, operand);
            addInstr(instr, &code);

            /* 02- TXA */
            instr = ASM(ASM::TXA, operand);
            addInstr(instr, &code);

            /* 03- PHA */
            instr = ASM(ASM::PHA, operand);
            addInstr(instr, &code);

            /* 04- TYA */
            instr = ASM(ASM::TYA, operand);
            addInstr(instr, &code);

            /* 05- PHA */
            instr = ASM(ASM::PHA, operand);
            addInstr(instr, &code);
        }

        it++;
        itNextInst = it;
  
        for ( ; it != x.end() ; it = itNextInst){
            /* Anticipate next instruction */
            ++itNextInst;
          
            /* Translate the current quad */
            f_generateInstr(*it);
        }
      
        framework->setCodeSize(codeSize);
    };

    labelGenerator = inLabelGenerator;
    for_each(oCode.begin(), oCode.end(), f_generateCode);
    simplifyDoubleJumps();
}



/**
* generateCode
* 
* Perform the final code generation of a quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateCode(const Quad &quad, list<ASM> *codeList){
	
	switch (quad.opcode){
    
	case Quad::NOP  : /* NOP  --- */
		generateNOP(quad, codeList);
		break;
	case Quad::ADD  : /* ADD  xyz */
		generateADD(quad, codeList);
		break;
	case Quad::SUB  : /* SUB  xyz */ 
		generateSUB(quad, codeList);
		break;
	case Quad::MUL  : /* MUL  xyz */ /* Unnused */
		generateMUL(quad, codeList);
		break;
	case Quad::DIV  : /* DIV  xyz */ /* Unnused */
		generateDIV(quad, codeList);
		break;
	case Quad::MOD  : /* MOD  xyz */ /* Unnused */
		generateMOD(quad, codeList);
		break;
	case Quad::INC  : /* INC  xy- */ 
		generateINC(quad, codeList);
		break;
	case Quad::DEC  : /* DEC  xy- */
		generateDEC(quad, codeList);
		break;
	case Quad::LES  : /* LES  xyz */
		generateLES(quad, codeList);
		break;
	case Quad::RIS  : /* RIS  xyz */
		generateRIS(quad, codeList);
		break;
	case Quad::NEG  : /* NEG  xy- */
		generateNEG(quad, codeList);
		break;
	case Quad::BR   : /* BR   --L */
		generateBR(quad, codeList);
		break;
	case Quad::BEQ  : /* BEQ  xyL */
		generateBEQ(quad, codeList);
		break;
	case Quad::BNE  : /* BRF  xyL */ 
		generateBNE(quad, codeList);
		break;
	case Quad::BGR   : /* BGR  xyL */
		generateBGR(quad, codeList);
		break;
	case Quad::BLS   : /* BLS  xyL */
		generateBLS(quad, codeList);
		break;
	case Quad::EQ   : /* EQ   xyz */
		generateEQ(quad, codeList);
		break;
	case Quad::NE   : /* NE   xyz */
		generateNE(quad, codeList);
		break;
	case Quad::GR   : /* GR   xyz */
		generateGR(quad, codeList);
		break;
	case Quad::LS   : /* LS   xyz */
		generateLS(quad, codeList);
		break;
	case Quad::LAND : /* LAND xyz */
		generateLAND(quad, codeList);
		break;
	case Quad::LOR  : /* LOR   xyz */
		generateLOR(quad, codeList);
		break;
	case Quad::AND  : /* AND  xyz */
		generateAND(quad, codeList);
		break;
	case Quad::OR   : /* OR   xyz */
		generateOR(quad, codeList);
		break;
	case Quad::XOR  : /* XOR  xyz */
		generateXOR(quad, codeList);
		break;
	case Quad::NOT  : /* NOT  xy- */
		generateNOT(quad, codeList);
		break;
	case Quad::MOVE : /* MOVE xy- */
		generateMOVE(quad, codeList);
		break;
	case Quad::MVP  : /* MVP  xy- */
		generateMVP(quad, codeList);
		break;
	case Quad::MVA  : /* MVA  xy- */
		generateMVA(quad, codeList);
		break;
	case Quad::STP  : /* STP  xy- */
		generateSTP(quad, codeList);
		break;
	case Quad::STA  : /* STA  xy- */ /* Unnused */
		generateSTA(quad, codeList);
		break;
	case Quad::LABL : /* LABL --L */
		generateLABL(quad, codeList);
		break;
	case Quad::PUSH : /* PUSH x-- */ /* Unnused */
		generatePUSH(quad, codeList);
		break;
	case Quad::POP  : /* POP  x-- */ /* Unnused */
		generatePOP(quad, codeList);
		break;
	case Quad::CALL : /* CALL --L */
		generateCALL(quad, codeList);
		break;
	case Quad::RET  : /* RET  x-- */
		if(currentFramework != MAIN_MAIN_METHOD){
			generateRET(quad, codeList);
			break;
		}
	case Quad::HALT : /* HALT --- */
		generateHALT(quad, codeList);
		break;
		
	default :
		/* FUTURE <error> */
		break;
	}
}



/**
* generateInstr
* 
* Perform the final code generation of a given opcode.
*
* @param opcode: The opcode which is being extended.
* @param opX: The x param of the quad to be translated into final code.
* @param opY: The y param of the quad to be translated into final code.
* @param opZ: The z param of the quad to be translated into final code.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateInstr(ASM::Opcode opcode, Quad::Operand opX, Quad::Operand opY, Quad::Operand opZ, list<ASM> *codeList){
	ASM instr;

	/*
	LDA LO.Y
	opc LO.Z
	STA LO.X

	[If x.size > 1]
		LDA HI.Y
		opc HI.Z
		STA HI.X
	*/

	/* LDA LO.y*/
	instr = getLoInstruction(ASM::LDA, opY);
	addInstr(instr, codeList);

	/* opcode low.z*/
	instr = getLoInstruction(opcode, opZ);
	addInstr(instr, codeList);

	/* STA LO.x */
	instr = getLoInstruction(ASM::STA, opX);
	addInstr(instr, codeList);

	if (operandSize(opX) <= 1)
		return;

	/* LDA HI.y*/
	instr = getHiInstruction(ASM::LDA, opY);
	addInstr(instr, codeList);

	/* opcode HI.z*/
	instr = getHiInstruction(opcode, opZ);
	addInstr(instr, codeList);

	/* STA HI.x */
	instr = getHiInstruction(ASM::STA, opX);
	addInstr(instr, codeList);
}



/**
* getLoInstruction
* 
* Creates an ASM instruction.
*
* @param opcode: The instruction opcode.
* @param opX: The operand to be used in the instruction.

* @return the ASM instruction related to the lower byte memory of the operand opX and the opcode.
*/
ASM ASMGenerator::getLoInstruction(ASM::Opcode opcode, const Quad::Operand &opX){
	ASM::Operand operand;
	ASM instr;
	int address;

	if(opX.opType == Quad::CONST){
		operand.opAdd           = ASM::A_INM;
		operand.relatedOp.A_INM = (opX.opSymbol.CONST & 0x00FF);
		instr                   = ASM(opcode, operand);
	}else{
		address = opX.opSymbol.VAR->getAddress();

		if (address < 0x0100){
			operand.opAdd           = ASM::A_ZPG;
			operand.relatedOp.A_ZPG = ((opX.opSymbol.VAR->getAddress()) & 0x00FF);
		}else{
			operand.opAdd           = ASM::A_ABS;
			operand.relatedOp.A_ABS = (opX.opSymbol.VAR->getAddress());
		}		

		ostringstream output;
		instr      = ASM(opcode, operand);
		output.fill('0');
		output << "_" << right << setw(2) << symbolsTable.getId(opX.opSymbol.VAR->getFramework()) << "_";
		output << opX.opSymbol.VAR->lex;

		instr.txtLabel = output.str();
		instr.symbol = opX.opSymbol.VAR;
	}

	return instr;
}



/**
* getHiInstruction
* 
* Creates an ASM instruction.
*
* @param opcode: The instruction opcode.
* @param opX: The operand to be used in the instruction.

* @return the ASM instruction related to the higher byte memory of the operand opX and the opcode.
*/
ASM ASMGenerator::getHiInstruction(ASM::Opcode opcode, const Quad::Operand &opX){
	ASM::Operand operand;
	ASM instr;
	int address;

	if(opX.opType == Quad::CONST){
		operand.opAdd           = ASM::A_INM;
		operand.relatedOp.A_INM = ((opX.opSymbol.CONST & 0xFF00) >> 8);
		instr                   = ASM(opcode, operand);
	}else{
		if (symbolsTable.getTypeSize(opX.opSymbol.VAR->getType()) <= 1){
			operand.opAdd           = ASM::A_INM;
			operand.relatedOp.A_INM = 0x0000;
			instr                   = ASM(opcode, operand);
		}else{
			address = opX.opSymbol.VAR->getAddress();

			if (address < 0x0100){
				operand.opAdd           = ASM::A_ZPG;
				operand.relatedOp.A_ZPG = (address + 0x01) & 0x00FF;
			}else{
				operand.opAdd           = ASM::A_ABS;
				operand.relatedOp.A_ABS = address + 0x0100;
			}

			ostringstream output;
			instr      = ASM(opcode, operand);
			output.fill('0');
			output << "_" << right << setw(2) << symbolsTable.getId(opX.opSymbol.VAR->getFramework()) << "_";
			output << opX.opSymbol.VAR->lex;
			if (address < 0x0100) output << "+1"; else output << "+256";

			instr.txtLabel = output.str();
			instr.symbol   = opX.opSymbol.VAR;
		}
	}

	return instr;
}



/**
* generateNOP
* 
* Aux function. Perform the final code generation of a NOP quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
#pragma warning ( disable : 4100 )
void ASMGenerator::generateNOP (const Quad &quad, list<ASM> *codeList){
	/* NOP  --- */
	/* Unused */
}



/**
* generateADD
* 
* Aux function. Perform the final code generation of a ADD quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateADD (const Quad &quad, list<ASM> *codeList){
	/* ADD  xyz */
	/* x := y + z */
	
	/*
	01- CLC
	02- LDA LO.y
	03- ADC LO.z
	04- STA LO.x
	[If X.size > 1]
		05a- LDA HI.y
		06a- ADC HI.z
		07a- STA HI.x
	*/

	/* 01- CLC */
	ASM::Operand operand = {ASM::A_IMPL, 0};
	ASM instr = ASM(ASM::CLC, operand);
	addInstr(instr, codeList);

	generateInstr(ASM::ADC, quad.opX, quad.opY, quad.opZ, codeList);
}



/**
* generateSUB
* 
* Aux function. Perform the final code generation of a SUB quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateSUB (const Quad &quad, list<ASM> *codeList){
	/* SUB  xyz */
	/* x := y - z */

	/*
	01- SEC
	02- LDA LO.y
	03- SBC LO.z
	04- STA LO.x
	[If X.size > 1]
		05b- LDA HI.y
		06b- SBC HI.z
		07b- STA HI.x
	*/
	
	/* 01- SEC */
	ASM::Operand operand = {ASM::A_IMPL, 0};
	ASM instr = ASM(ASM::SEC, operand);
	addInstr(instr, codeList);

	generateInstr(ASM::SBC, quad.opX, quad.opY, quad.opZ, codeList);
}



/**
* generateMUL
* 
* Aux function. Perform the final code generation of a MUL quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateMUL (const Quad &quad, list<ASM> *codeList){
	/* MUL  xyz */ /* Unused */
	/* Unused */
}



/**
* generateDIV
* 
* Aux function. Perform the final code generation of a DIV quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateDIV (const Quad &quad, list<ASM> *codeList){
	/* DIV  xyz */ /* Unused */
	/* Unused */
}



/**
* generateMOD
* 
* Aux function. Perform the final code generation of a MOD quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateMOD (const Quad &quad, list<ASM> *codeList){
	/* MOD  xyz */ /* Unused */
	/* Unused */
}



/**
* generateINC
* 
* Aux function. Perform the final code generation of a INC quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateINC (const Quad &quad, list<ASM> *codeList){
	/* INC  xy- */ 
	/* x := y + 1 */

	/*
	01- INC LO.x
	[If x.size > 1]
		02- BNE INC16_end
		03- INC HI.x
		04- INC16_end:		
	*/

	/* 01- INC LO.x */
	ASM instr = getLoInstruction(ASM::INC, quad.opX);
	addInstr(instr, codeList);
	
	/* [If x.size > 1] */
	if(operandSize(quad.opX) > 1){
		string newLab = string("INC16_end_").append(labelGenerator->getNewLabel());

		/* 02- BNE INC16_end */
		ASM::Operand operand = {ASM::A_REL, 0};
		instr          = ASM(ASM::BNE, operand);
		instr.txtLabel = newLab;
		addInstr(instr, codeList);

		/* 03- INC HI.x */
		instr = getHiInstruction(ASM::INC, quad.opX);
		addInstr(instr, codeList);
		
		/* 04- INC16_end: */
		operand.opAdd = ASM::A_LABL;
		instr = ASM(ASM::LAB, operand);
		instr.txtLabel = newLab;
		addInstr(instr, codeList);
	}
}



/**
* generateDEC
* 
* Aux function. Perform the final code generation of a DEC quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateDEC (const Quad &quad, list<ASM> *codeList){
	/* DEC  xy- */
	/* x := y - 1 */

	/*		
	[If x.size > 1]
		01- LDA LO.x
		02- BNE DEC16_end
		03- DEC HI.x
		04- DEC16_end:
	04- DEC LO.x
	*/
	
	ASM instr;

	/* [If x.size > 1] */
	if(operandSize(quad.opX) > 1){
		string newLab = string("DEC16_end_").append(labelGenerator->getNewLabel());

		/* 01- LDA LO.x */
		instr = getLoInstruction(ASM::LDA, quad.opX);
		addInstr(instr, codeList);
		
		/* 02- BNE DEC16_end */
		ASM::Operand operand = {ASM::A_REL, 0};
		instr          = ASM(ASM::BNE, operand);
		instr.txtLabel = newLab;
		addInstr(instr, codeList);
		
		/* 03- DEC HI.x */
		instr = getHiInstruction(ASM::DEC, quad.opX);
		addInstr(instr, codeList);
		
		/* 04- DEC16_end: */
		operand.opAdd = ASM::A_LABL;
		instr = ASM(ASM::LAB, operand);
		instr.txtLabel = newLab;
		addInstr(instr, codeList);
	}
		
	/* 04- DEC LO.x */
	instr = getLoInstruction(ASM::DEC, quad.opX);
	addInstr(instr, codeList);
}



/**
* generateLES
* 
* Aux function. Perform the final code generation of a LES quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateLES (const Quad &quad, list<ASM> *codeList){
	/* LES  xyz */
	/* x := y << z */

	/*
	[if x == y]
		[if z isConst, for i = 1 to z]
			01a- ASL LO.x
			[if x.size > 1]
				02a- ROL HI.x
		[else]
			01a- 			LDX LO.z
			02a- LES_loop:
			03a- 			ASL LO.x
			[if x.size > 1]
				04a- ROL HI.x
			05a-			DEX
			06a-			BNE LES_loop
	[else ; x!=y]
		[if !(z isConst && z < 2)]
			00-				LDA LO.y
			00-				STA LO.x
			[if x.size > 1]
				00-				LDA HI.y
				00-				STA HI.x
			01b- 			LDX LO.z
			02b- LES_loop:
		03b- [if !(z isConst && z < 2)] LDA LO.x [else] LDA LO.y
		04b- 			ASL A
		05b- 			STA LO.x
		[if x.size > 1]
			06b- [if !(z isConst && z < 2)] LDA HI.x [else] LDA HI.y
			07b-		ROL A
			08b-		STA HI.x
		[if !(z isConst && z < 2)]
			09a-			DEX
			0Aa-			BNE LES_loop
	[end if]
	*/

	ASM instr;
	string newLab = string("LES_loop_").append(labelGenerator->getNewLabel());

	/* [if x == y] */
	if(quad.opX.opSymbol.VAR == quad.opY.opSymbol.VAR){
		/* [if z isConst, for i = 1 to z] */
		if(quad.opZ.opType == Quad::CONST) for (int i = 0; i < quad.opZ.opSymbol.CONST ; i++){
			/* 01a- ASL LO.x */
			instr = getLoInstruction(ASM::ASL, quad.opX);
			addInstr(instr, codeList);
			
			/* [if x.size > 1] */
			if(operandSize(quad.opX) > 1){
				/* 02a- ROL HI.x */
				instr = getHiInstruction(ASM::ROL, quad.opX);
				addInstr(instr, codeList);
			}
		/* [else] */
		}else{
			/* 01a- 			LDX LO.z */
			instr = getLoInstruction(ASM::LDX, quad.opZ);
			addInstr(instr, codeList);
			
			/* 02a- LES_loop: */
			ASM::Operand operand = {ASM::A_LABL, 0};
			instr          = ASM(ASM::LAB, operand);
			instr.txtLabel = newLab;
			addInstr(instr, codeList);
			
			/* 03a- 			ASL LO.x */
			instr = getLoInstruction(ASM::ASL, quad.opX);
			addInstr(instr, codeList);
			
			/* [if x.size > 1] */
			if(operandSize(quad.opX) > 1){
				/* 04a- ROL HI.x */
				instr = getHiInstruction(ASM::ROL, quad.opX);
				addInstr(instr, codeList);
			}
			
			/* 05a-			DEX */
			operand.opAdd            = ASM::A_IMPL;
			operand.relatedOp.A_IMPL = 0;
			instr = ASM(ASM::DEX, operand);
			addInstr(instr, codeList);
			
			/* 06a-			BNE _beg_loop */
			operand.opAdd = ASM::A_REL;
			instr          = ASM(ASM::BNE, operand);
			instr.txtLabel = newLab;
			addInstr(instr, codeList);
		}

	/* [else ; x!=y] */
	}else{	
		/* [if !(z isConst && z < 2)] */
		if(!(quad.opZ.opType == Quad::CONST && quad.opZ.opSymbol.CONST < 2)){
			/* 00-				LDA LO.y */
			instr = getLoInstruction(ASM::LDA, quad.opY);
			addInstr(instr, codeList);

			/* 00-				STA LO.x */
			instr = getLoInstruction(ASM::STA, quad.opX);
			addInstr(instr, codeList);

			/* [if x.size > 1] */
			if(symbolsTable.getTypeSize(quad.opX.opSymbol.VAR->getType()) > 1){
				/* 00-				LDA HI.y */
				instr = getHiInstruction(ASM::LDA, quad.opY);
				addInstr(instr, codeList);

				/* 00-				STA HI.x */
				instr = getHiInstruction(ASM::STA, quad.opX);
				addInstr(instr, codeList);
			}
		
			/* 01b- 			LDX LO.z */
			instr = getLoInstruction(ASM::LDX, quad.opZ);
			addInstr(instr, codeList);
			
			/* 02b- LES_loop: */
			ASM::Operand operand = {ASM::A_LABL, 0};
			instr          = ASM(ASM::LAB, operand);
			instr.txtLabel = newLab;
			addInstr(instr, codeList);
		}
		/* 03b- [if !(z isConst && z < 2)] LDA LO.x [else] LDA LO.y */
		if(!(quad.opZ.opType == Quad::CONST && quad.opZ.opSymbol.CONST < 2)){
			instr = getLoInstruction(ASM::LDA, quad.opX);
		}else{
			instr = getLoInstruction(ASM::LDA, quad.opY);
		}
		addInstr(instr, codeList);
		
		/* 04b- 			ASL A */
		ASM::Operand operand = {ASM::A_REGA, 0};
		instr = ASM(ASM::ASL, operand);
		addInstr(instr, codeList);
		
		/* 05b- 			STA LO.x */
		instr = getLoInstruction(ASM::STA, quad.opX);
		addInstr(instr, codeList);
		
		/* [if x.size > 1] */
		if(symbolsTable.getTypeSize(quad.opX.opSymbol.VAR->getType()) > 1){
			/* 06b- [if !(z isConst && z < 2)] LDA HI.x [else] LDA HI.y */
			if(!(quad.opZ.opType == Quad::CONST && quad.opZ.opSymbol.CONST < 2)){
				instr = getHiInstruction(ASM::LDA, quad.opX);
			}else{
				instr = getHiInstruction(ASM::LDA, quad.opY);
			}
			addInstr(instr, codeList);
				
			/* 07b- ROL A */
			operand.opAdd            = ASM::A_REGA;
			operand.relatedOp.A_REGA = 0;
			instr = ASM(ASM::ROL, operand);
			addInstr(instr, codeList);
			
			/* 08b- STA HI.x */
			instr = getHiInstruction(ASM::STA, quad.opX);
			addInstr(instr, codeList);
		}
		
		/* [if !(z isConst && z < 2)] */
		if(!(quad.opZ.opType == Quad::CONST && quad.opZ.opSymbol.CONST < 2)){
			/* 09a-			DEX */
			operand.opAdd            = ASM::A_IMPL;
			operand.relatedOp.A_IMPL = 0;
			instr = ASM(ASM::DEX, operand);
			addInstr(instr, codeList);
			
			/* 0Aa-			BNE LES_loop */
			operand.opAdd = ASM::A_REL;
			instr          = ASM(ASM::BNE, operand);
			instr.txtLabel = newLab;
			addInstr(instr, codeList);
		}
	}
}



/**
* generateRIS
* 
* Aux function. Perform the final code generation of a RIS quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateRIS (const Quad &quad, list<ASM> *codeList){
	/* RIS  xyz */
	/* x := y >> z */

	/*
	[if x == y]
		[if x.size > 1]
			[if z isConst, for i = 1 to z]
				01a- LSR HI.x
				02a- ROR LO.x
			[else ; z is variable]
				01a- 			LDX z
				02a- RIS_loop:
				03a- 			LSR HI.x
				04a- 			ROR LO.x
				05a-			DEX
				06a-			BNE RIS_loop
		[else x.size = 1]
			[if z isConst, for i = 1 to z]
				01a- LSR LO.x
			[else ; z is variable]
				01a- 			LDX z
				02a- RIS_loop:
				03a- 			LSR LO.x
				04a-			DEX
				05a-			BNE RIS_loop
	[else if x!=y]
		[if !(z isConst && z < 2)]
			00-				LDA LO.y
			00-				STA LO.x
			[if x.size > 1]
				00-				LDA HI.y
				00-				STA HI.x
			01b- 			LDX z
			02b- RIS_loop:	
		[if x.size > 1]
			01b- [if !(z isConst && z < 2)] LDA HI.x [else] LDA HI.y
			02b- 			LSR A
			03b- 			STA HI.x
			04b- [if !(z isConst && z < 2)] LDA LO.x [else] LDA LO.y
			05b- 			ROR A
			06b- 			STA LO.x
		[else x.size = 1]
			01b- [if !(z isConst && z < 2)] LDA LO.x [else] LDA LO.y
			02b- 			LSR A
			03b- 			STA LO.x
		[if !(z isConst && z < 2)]
			07b-			DEX
			08b-			BNE RIS_loop
	*/
	
	string newLab = string("RIS_loop_").append(labelGenerator->getNewLabel());
	ASM instr;
	ASM::Operand operand;
	
	/* [if x == y] */
	if(quad.opX.opSymbol.VAR == quad.opY.opSymbol.VAR){
		/* [if x.size > 1] */
		if(operandSize(quad.opX) > 1){
			/* [if z isConst, for i = 1 to z] */
			if(quad.opZ.opType == Quad::CONST) for (int i = 0; i < quad.opZ.opSymbol.CONST ; i++){
				/* 01a- LSR HI.x */
				instr = getHiInstruction(ASM::LSR, quad.opX);
				addInstr(instr, codeList);
				
				/* 02a- ROR LO.x */
				instr = getLoInstruction(ASM::ROR, quad.opX);
				addInstr(instr, codeList);
			/* [else ; z is variable] */
			}else{
				/* 01a- 			LDX LO.z */
				instr = getLoInstruction(ASM::LDX, quad.opZ);
				addInstr(instr, codeList);
				
				/* 02a- RIS_loop: */
				operand.opAdd = ASM::A_LABL;
				instr      = ASM(ASM::LAB, operand);
				instr.txtLabel = newLab;
				addInstr(instr, codeList);
				
				/* 03a- 			LSR HI.x */
				instr = getHiInstruction(ASM::LSR, quad.opX);
				addInstr(instr, codeList);
				
				/* 04a- 			ROR LO.x */
				instr = getHiInstruction(ASM::ROR, quad.opX);
				addInstr(instr, codeList);
				
				/* 05a-			DEX */
				operand.opAdd            = ASM::A_IMPL;
				operand.relatedOp.A_IMPL = 0;
				instr = ASM(ASM::DEX, operand);
				addInstr(instr, codeList);
			
				/* 06a-			BNE RIS_loop */
				operand.opAdd = ASM::A_REL;
				instr      = ASM(ASM::BNE, operand);
				instr.txtLabel = newLab;
				addInstr(instr, codeList);
			}

		/* [else x.size = 1] */
		}else{
			/* [if z isConst, for i = 1 to z] */
			if(quad.opZ.opType == Quad::CONST) for (int i = 0; i < quad.opZ.opSymbol.CONST ; i++){
				/* 01a- LSR LO.x */
				instr = getLoInstruction(ASM::LSR, quad.opX);
				addInstr(instr, codeList);
				
			/* [else ; z is variable] */
			}else{
				/* 01a- 			LDX LO.z */
				instr = getLoInstruction(ASM::LDX, quad.opZ);
				addInstr(instr, codeList);
				
				/* 02a- RIS_loop: */
				operand.opAdd = ASM::A_LABL;
				instr      = ASM(ASM::LAB, operand);
				instr.txtLabel = newLab;
				addInstr(instr, codeList);
				
				/* 03a- 			LSR LO.x */
				instr = getLoInstruction(ASM::LSR, quad.opX);
				addInstr(instr, codeList);
				
				/* 04a-			DEX */
				operand.opAdd            = ASM::A_IMPL;
				operand.relatedOp.A_IMPL = 0;
				instr = ASM(ASM::DEX, operand);
				addInstr(instr, codeList);
			
				/* 05a-			BNE RIS_loop */
				operand.opAdd = ASM::A_REL;
				instr      = ASM(ASM::BNE, operand);
				instr.txtLabel = newLab;
				addInstr(instr, codeList);
			}
		}
	/* [else if x!=y] */
	}else{
		/* [if !(z isConst && z < 2)] */
		if(!(quad.opZ.opType == Quad::CONST && quad.opZ.opSymbol.CONST < 2)){
			/* 00-				LDA LO.y */
			instr = getLoInstruction(ASM::LDA, quad.opY);
			addInstr(instr, codeList);

			/* 00-				STA LO.x */
			instr = getLoInstruction(ASM::STA, quad.opX);
			addInstr(instr, codeList);

			/* [if x.size > 1] */
			if(symbolsTable.getTypeSize(quad.opX.opSymbol.VAR->getType()) > 1){
				/* 00-				LDA HI.y */
				instr = getHiInstruction(ASM::LDA, quad.opY);
				addInstr(instr, codeList);

				/* 00-				STA HI.x */
				instr = getHiInstruction(ASM::STA, quad.opX);
				addInstr(instr, codeList);
			}
			
			/* 01b- 			LDX LO.z */
			instr = getLoInstruction(ASM::LDX, quad.opZ);
			addInstr(instr, codeList);
			
			/* 02b- RIS_loop:  */
			operand.opAdd = ASM::A_LABL;
			instr      = ASM(ASM::LAB, operand);
			instr.txtLabel = newLab;
			addInstr(instr, codeList);
		}
		
		/* [if x.size > 1] */
		if(operandSize(quad.opX) > 1){
			/* 01b- [if !(z isConst && z < 2)] LDA HI.x [else] LDA HI.y */
			if(!(quad.opZ.opType == Quad::CONST && quad.opZ.opSymbol.CONST < 2)){
				instr = getHiInstruction(ASM::LDA, quad.opX);
			}else{
				instr = getHiInstruction(ASM::LDA, quad.opY);
			}
			addInstr(instr, codeList);
			
			/* 02b- LSR A */
			operand.opAdd            = ASM::A_REGA;
			operand.relatedOp.A_REGA = 0;
			instr = ASM(ASM::LSR, operand);
			addInstr(instr, codeList);
			
			/* 03b- STA HI.x */
			instr = getHiInstruction(ASM::STA, quad.opX);
			addInstr(instr, codeList);
			
			/* 04b- [if !(z isConst && z < 2)] LDA LO.x [else] LDA LO.y */
			if(!(quad.opZ.opType == Quad::CONST && quad.opZ.opSymbol.CONST < 2)){
				instr = getLoInstruction(ASM::LDA, quad.opX);
			}else{
				instr = getLoInstruction(ASM::LDA, quad.opY);
			}
			addInstr(instr, codeList);
			
			/* 05b- ROR A */
			operand.opAdd            = ASM::A_REGA;
			operand.relatedOp.A_REGA = 0;
			instr = ASM(ASM::ROR, operand);
			addInstr(instr, codeList);
			
			/* 06b- STA LO.x */
			instr = getLoInstruction(ASM::STA, quad.opX);
			addInstr(instr, codeList);
			
		/* [else x.size = 1] */
		}else{
			/* 01b- [if !(z isConst && z < 2)] LDA LO.x [else] LDA LO.y */
			if(!(quad.opZ.opType == Quad::CONST && quad.opZ.opSymbol.CONST < 2)){
				instr = getLoInstruction(ASM::LDA, quad.opX);
			}else{
				instr = getLoInstruction(ASM::LDA, quad.opY);
			}
			addInstr(instr, codeList);
			
			/* 02b- LSR A */
			operand.opAdd            = ASM::A_REGA;
			operand.relatedOp.A_REGA = 0;
			instr = ASM(ASM::LSR, operand);
			addInstr(instr, codeList);
			
			/* 03b- STA LO.x */
			instr = getLoInstruction(ASM::STA, quad.opX);
			addInstr(instr, codeList);
		}
		
		/* [if !(z isConst && z < 2)] */
		if(!(quad.opZ.opType == Quad::CONST && quad.opZ.opSymbol.CONST < 2)){
			/* 07b-			DEX */
			operand.opAdd            = ASM::A_IMPL;
			operand.relatedOp.A_IMPL = 0;
			instr = ASM(ASM::DEX, operand);
			addInstr(instr, codeList);
			
			/* 08b-			BNE RIS_loop */
			operand.opAdd = ASM::A_REL;
			instr      = ASM(ASM::BNE, operand);
			instr.txtLabel = newLab;
			addInstr(instr, codeList);
		}
	}
}



void ASMGenerator::generateNEG (const Quad &quad, list<ASM> *codeList){
	/* NEG  xy- */
	/* x := -y */

	/*
	01- LDA LO.y
	02- EOR #$FF
	03- STA LO.x
	[if x.size > 1]
		04- LDA HI.y
		05- EOR #$FF
		06- STA HI.x
	07- INC LO.x
	[If x.size > 1]
		08- BNE NEG_end
		09- INC HI.x
		0A- NEG_end:
	*/

	/* 01- LDA LO.y */
	ASM instr = getLoInstruction(ASM::LDA, quad.opY);
	addInstr(instr, codeList);
		
	/* 02- EOR #$FF */
	ASM::Operand operand = {ASM::A_INM, 0xFF};
	instr = ASM(ASM::EOR, operand);
	addInstr(instr, codeList);
		
	/* 03- STA LO.x */
	instr = getLoInstruction(ASM::STA, quad.opX);
	addInstr(instr, codeList);

	/* [if x.size > 1] */
	if (operandSize(quad.opX) > 1){
		/* 01- LDA HI.y */
		instr = getHiInstruction(ASM::LDA, quad.opY);
		addInstr(instr, codeList);
		
		/* 02- EOR #$FF */
		ASM::Operand operand = {ASM::A_INM, 0xFF};
		instr = ASM(ASM::EOR, operand);
		addInstr(instr, codeList);
		
		/* 03- STA HI.x */
		instr = getHiInstruction(ASM::STA, quad.opX);
		addInstr(instr, codeList);
	}

	/* 07- INC LO.x */
	instr = getLoInstruction(ASM::INC, quad.opX);
	addInstr(instr, codeList);
	
	/* [If x.size > 1] */
	if(operandSize(quad.opX) > 1){
		string newLab = string("NEG_end_").append(labelGenerator->getNewLabel());

		/* 08- BNE INC16_end */
		ASM::Operand operand = {ASM::A_REL, 0};
		instr          = ASM(ASM::BNE, operand);
		instr.txtLabel = newLab;
		addInstr(instr, codeList);

		/* 09- INC HI.x */
		instr = getHiInstruction(ASM::INC, quad.opX);
		addInstr(instr, codeList);
		
		/* 0A- NEG_end: */
		operand.opAdd = ASM::A_LABL;
		instr = ASM(ASM::LAB, operand);
		instr.txtLabel = newLab;
		addInstr(instr, codeList);
	}
}



/**
* generateBR
* 
* Aux function. Perform the final code generation of a BR quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateBR  (const Quad &quad, list<ASM> *codeList){
	/* BR   --L */
	/* Jump L */
	ASM::Operand operand = {ASM::A_ABS, 0};
	ASM instr      = ASM(ASM::JMP, operand);
	instr.txtLabel = quad.opZ.label;
	addInstr(instr, codeList);
}



/**
* generateBEQ
* 
* Aux function. Perform the final code generation of a BEQ quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateBEQ(const Quad &quad, list<ASM> *codeList){
	/* BEQ  xyL */
	/* IF x = y GOTO L*/

	/*
	01-				LDA LO.x
	02-				CMP LO.y
	03-				BNE BEQ_end_
	[if !(x.size == 1 && y.size == 1)]
		04b-			LDA HI.x
		05b-			CMP HI.y
		06b-			BNE BEQ_end_
	07-				JMP L
	08- BEQ_end_:
	*/

	string newLab = string("BEQ_end_").append(labelGenerator->getNewLabel());

	/* 01- LDA LO.x */
	ASM instr = getLoInstruction(ASM::LDA, quad.opX);
	addInstr(instr, codeList);
	
	/* 02- CMP LO.y */
	instr = getLoInstruction(ASM::CMP, quad.opY);
	addInstr(instr, codeList);

	/* 03- BNE BEQ_end_ */
	ASM::Operand operand = {ASM::A_REL, 0x0000};
	instr      = ASM(ASM::BNE, operand);
	instr.txtLabel = newLab;
	addInstr(instr, codeList);

	/* [if x.size == 1] */
	if(!(operandSize(quad.opX) == 1 && operandSize(quad.opY) == 1)){
		/* 04b- LDA HI.x */
		instr = getHiInstruction(ASM::LDA, quad.opX);
		addInstr(instr, codeList);
		
		/* 05b- CMP HI.y */
		instr = getHiInstruction(ASM::CMP, quad.opY);
		addInstr(instr, codeList);
		
		/* 06b- BNE BEQ_end_ */
		operand.opAdd = ASM::A_REL;
		instr      = ASM(ASM::BNE, operand);
		instr.txtLabel = newLab;
		addInstr(instr, codeList);
	}		
	/* 07- JMP L */
	operand.opAdd = ASM::A_ABS;
	instr          = ASM(ASM::JMP, operand);
	instr.txtLabel = quad.opZ.label;
	addInstr(instr, codeList);

	/* 08- BEQ_end_: */
	operand.opAdd = ASM::A_LABL;
	instr      = ASM(ASM::LAB, operand);
	instr.txtLabel = newLab;
	addInstr(instr, codeList);
}



/**
* generateBNE
* 
* Aux function. Perform the final code generation of a NOP quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateBNE(const Quad &quad, list<ASM> *codeList){
	/* BNE  xyL */
	/* IF x != y GOTO L*/

	/*
	01-				LDA LO.x
	02-				CMP LO.y  ; compare low bytes
	[if x.size > 1 || y.size > 1]
		03-				BEQ BNE_mid_
		04-				JMP L
		05- BNE_mid_:
		06-				LDA HI.x
		07-				CMP HI.y  ; compare high bytes
	08-				BEQ BNE_end_
	09-				JMP L
	0A- BNE_end_:
	*/

	string newLab  = labelGenerator->getNewLabel();
	string newLabM = string("BNE_mid_").append(newLab);
	string newLabE = string("BNE_end_").append(newLab);

	/* 01- LDA LO.x */
	ASM instr = getLoInstruction(ASM::LDA, quad.opX);
	addInstr(instr, codeList);
	
	/* 02- CMP LO.y */
	instr = getLoInstruction(ASM::CMP, quad.opY);
	addInstr(instr, codeList);

	/* [if x.size > 1 || y.size > 1] */
	if(operandSize(quad.opX) > 1 ||
	   operandSize(quad.opY) > 1){

		/* 03- BEQ BNE_mid_ */
		ASM::Operand operand = {ASM::A_REL, 0};
		instr      = ASM(ASM::BEQ, operand);
		instr.txtLabel = newLabM;
		addInstr(instr, codeList);
		
		/* 04- JMP L */
		operand.opAdd           = ASM::A_ABS;
		operand.relatedOp.A_ABS = 0x0000;
		instr          = ASM(ASM::JMP, operand);
		instr.txtLabel = quad.opZ.label;
		addInstr(instr, codeList);
		
		/* 05- BNE_mid_: */
		operand.opAdd = ASM::A_LABL;
		instr      = ASM(ASM::LAB, operand);
		instr.txtLabel = newLabM;
		addInstr(instr, codeList);

		/* 06- LDA HI.x */
		instr = getHiInstruction(ASM::LDA, quad.opX);
		addInstr(instr, codeList);
		
		/* 07- CMP HI.y */
		instr = getHiInstruction(ASM::CMP, quad.opY);
		addInstr(instr, codeList);
	}
	
	/* 08- BEQ BNE_end_ */
	ASM::Operand operand = {ASM::A_REL, 0};
	instr      = ASM(ASM::BEQ, operand);
	instr.txtLabel = newLabE;
	addInstr(instr, codeList);

	/* 09- JMP L */
	operand.opAdd           = ASM::A_ABS;
	operand.relatedOp.A_ABS = 0x0000;
	instr          = ASM(ASM::JMP, operand);
	instr.txtLabel = quad.opZ.label;
	addInstr(instr, codeList);

	/* 0A- BNE_end_: */
	operand.opAdd = ASM::A_LABL;
	instr          = ASM(ASM::LAB, operand);
	instr.txtLabel = newLabE;
	addInstr(instr, codeList);
}



/**
* generateBGR
* 
* Aux function. Perform the final code generation of a BGR quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateBGR(const Quad &quad, list<ASM> *codeList){
	/* BGR  xyL */
	/* IF x > y GOTO L */

	/*
	[if x.isUnsigned || y.isUnsigned]
		[if x.size > 1 || y.size > 1]
			1.			LDA HI.y
			2.			CMP HI.x
			3.			BCS BGR_jp1_	; if yH < xH then y < x
			4.			JMP L
			5. BGR_jp1_:
			6.			BNE BGR_end
		7.			LDA LO.y
		8.			CMP LO.x
		9.			BCS BGR_end_	; if yL < xL then y < x
	[else	; signed]
		1.			SEC
		[if x.size > 1 || y.size > 1] LDA HI.y [else] LDA LO.y
		[if x.size > 1 || y.size > 1] SBC HI.x [else] SBC LO.x
		4.			BVC BGR_jp1
		5.			EOR #$80
		6. BGR_jp1:
		[if x.size > 1 || y.size > 1] BPL BGR_jp2 [else] BPL BGR_end
		[if x.size > 1 || y.size > 1]
			8.			JMP L
			9. BGR_jp2:
			A.			BVC BGR_jp3
			B.			EOR #$80
			C. BGR_jp3:
			D.			BNE BGR_end
			E.			LDA LO.y
			F.			SBC LO.x
			0.			BCS BGR_end
	[end_if]
	0A.			JMP L
	0B. BGR_end:
	*/

	string newLab  = labelGenerator->getNewLabel();
	string newLab1 = string("BGR_jp1_").append(newLab);
	string newLab2 = string("BGR_jp2_").append(newLab);
	string newLab3 = string("BGR_jp3_").append(newLab);
	string newLabE = string("BGR_end_").append(newLab);

	bool unsignedOp = operandIsUnsigned(quad.opX) || operandIsUnsigned(quad.opY);
	bool asm16      = (operandSize(quad.opX) > 1) || (operandSize(quad.opY) > 1);

	ASM instr;

	if (unsignedOp){
		if(asm16){
			/* 1.			LDA HI.y */
			instr = getHiInstruction(ASM::LDA, quad.opY);
			addInstr(instr, codeList);
			
			/* 2.			CMP HI.x */
			instr = getHiInstruction(ASM::CMP, quad.opX);
			addInstr(instr, codeList);
			
			/* 3.			BCS BGR_jp1_	; if yH < xH then y < x */
			ASM::Operand operand = {ASM::A_REL, 0x00};
			instr      = ASM(ASM::BCS, operand);
			instr.txtLabel = newLab1;
			addInstr(instr, codeList);
			
			/* 4.			JMP L */
			operand.opAdd           = ASM::A_ABS;
			operand.relatedOp.A_ABS = 0x0000;
			instr          = ASM(ASM::JMP, operand);
			instr.txtLabel = quad.opZ.label;
			addInstr(instr, codeList);
			
			/* 5. BGR_jp1_: */
			operand.opAdd = ASM::A_LABL;
			instr      = ASM(ASM::LAB, operand);
			instr.txtLabel = newLab1;
			addInstr(instr, codeList);

			/* 6.			BNE BGR_end */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr      = ASM(ASM::BNE, operand);
			instr.txtLabel = newLabE;
			addInstr(instr, codeList);
		}
		/* 7.			LDA LO.y */
		instr = getLoInstruction(ASM::LDA, quad.opY);
		addInstr(instr, codeList);
			
		/* 8.			CMP LO.x */
		instr = getLoInstruction(ASM::CMP, quad.opX);
		addInstr(instr, codeList);
			
		/* 9.			BCS BGR_end_ */
		ASM::Operand operand = {ASM::A_REL, 0};
		instr                = ASM(ASM::BCS, operand);
		instr.txtLabel       = newLabE;
		addInstr(instr, codeList);

	/* [else	; signed] */
	}else{
		/* 1.			SEC */
		ASM::Operand operand = {ASM::A_IMPL, 0};
		instr = ASM(ASM::SEC, operand);
		addInstr(instr, codeList);

		/* [if x.size > 1 || y.size > 1] LDA HI.y [else] LDA LO.y */
		if(asm16)
			instr = getHiInstruction(ASM::LDA, quad.opY);
		else
			instr = getLoInstruction(ASM::LDA, quad.opY);
		addInstr(instr, codeList);

		/* [if x.size > 1 || y.size > 1] SBC HI.x [else] SBC LO.x */
		if(asm16)
			instr = getHiInstruction(ASM::SBC, quad.opX);
		else
			instr = getLoInstruction(ASM::SBC, quad.opX);
		addInstr(instr, codeList);

		/* 4.			BVC BGR_jp1 */
		operand.opAdd           = ASM::A_REL;
		operand.relatedOp.A_REL = 0;
		instr                   = ASM(ASM::BVC, operand);
		instr.txtLabel          = newLab1;
		addInstr(instr, codeList);

		/* 5.			EOR #$80 */
		operand.opAdd           = ASM::A_INM;
		operand.relatedOp.A_INM = 0x80;
		instr = ASM(ASM::EOR, operand);
		addInstr(instr, codeList);

		/* 6. BGR_jp1: */
		operand.opAdd = ASM::A_LABL;
		instr      = ASM(ASM::LAB, operand);
		instr.txtLabel = newLab1;
		addInstr(instr, codeList);

		/* [if x.size > 1 || y.size > 1] BPL BGR_jp2 [else] BPL BGR_end */
		operand.opAdd           = ASM::A_REL;
		operand.relatedOp.A_REL = 0;
		instr          = ASM(ASM::BPL, operand);
		instr.txtLabel = asm16 ? newLab2 : newLabE;
		addInstr(instr, codeList);

		/* [if x.size > 1 || y.size > 1] */
		if(asm16){
			/* 8.			JMP L */
			operand.opAdd           = ASM::A_ABS;
			operand.relatedOp.A_ABS = 0x0000;
			instr          = ASM(ASM::JMP, operand);
			instr.txtLabel = quad.opZ.label;
			addInstr(instr, codeList);

			/* 9. BGR_jp2: */
			operand.opAdd = ASM::A_LABL;
			instr      = ASM(ASM::LAB, operand);
			instr.txtLabel = newLab2;
			addInstr(instr, codeList);

			/* A.			BVC BGR_jp3 */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr                   = ASM(ASM::BVC, operand);
			instr.txtLabel          = newLab3;
			addInstr(instr, codeList);

			/* B.			EOR #$80 */
			operand.opAdd           = ASM::A_INM;
			operand.relatedOp.A_INM = 0x80;
			instr = ASM(ASM::EOR, operand);
			addInstr(instr, codeList);

			/* C. BGR_jp3: */
			operand.opAdd = ASM::A_LABL;
			instr      = ASM(ASM::LAB, operand);
			instr.txtLabel = newLab3;
			addInstr(instr, codeList);

			/* D.			BNE BGR_end */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr                   = ASM(ASM::BNE, operand);
			instr.txtLabel          = newLabE;
			addInstr(instr, codeList);

			/* E.			LDA LO.y */
			instr = getLoInstruction(ASM::LDA, quad.opY);
			addInstr(instr, codeList);

			/* F.			SBC LO.x */
			instr = getLoInstruction(ASM::SBC, quad.opX);
			addInstr(instr, codeList);

			/* 0.			BCS BGR_end */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr                   = ASM(ASM::BCS, operand);
			instr.txtLabel          = newLabE;
			addInstr(instr, codeList);
		}
	}

	/* 11.			JMP L */
	ASM::Operand operand = {ASM::A_ABS, 0x0000};
	instr          = ASM(ASM::JMP, operand);
	instr.txtLabel = quad.opZ.label;
	addInstr(instr, codeList);
			
	/* 12. BGR_end_: */
	operand.opAdd = ASM::A_LABL;
	instr      = ASM(ASM::LAB, operand);
	instr.txtLabel = newLabE;
	addInstr(instr, codeList);
}



/**
* generateBLS
* 
* Aux function. Perform the final code generation of a BLS quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateBLS(const Quad &quad, list<ASM> *codeList){
	/* BLS  xyL */
	/* IF x < y GOTO L */

	/*
	[if x.isUnsigned || y.isUnsigned]
		[if x.size > 1 || y.size > 1]
			1.			LDA HI.x
			2.			CMP HI.y
			3.			BCS BLS_jp1_	; if yH < xH then y < x
			4.			JMP L
			5. BLS_jp1_:
			6.			BNE BLS_end
		7.			LDA LO.x
		8.			CMP LO.y
		9.			BCS BLS_end_	; if yL < xL then y < x
	[else	; signed]
		1.			SEC
		[if x.size > 1 || y.size > 1] LDA HI.x [else] LDA LO.x
		[if x.size > 1 || y.size > 1] SBC HI.y [else] SBC LO.y
		4.			BVC BLS_jp1
		5.			EOR #$80
		6. BLS_jp1:

		[if x.size > 1 || y.size > 1] BPL BLS_jp2 [else] BPL BLS_end
		[if x.size > 1 || y.size > 1]
			8.			JMP L
			9. BLS_jp2:
			A.			BVC BLS_jp3
			B.			EOR #$80
			C. BLS_jp3:
			D.			BNE BLS_end
			E.			LDA LO.x
			F.			SBC LO.y
			0.			BCS BLS_end
	[end_if]
	0A.			JMP L
	0B. BLS_end:
	*/

	string newLab  = labelGenerator->getNewLabel();
	string newLab1 = string("BLS_jp1_").append(newLab);
	string newLab2 = string("BLS_jp2_").append(newLab);
	string newLab3 = string("BLS_jp3_").append(newLab);
	string newLabE = string("BLS_end_").append(newLab);

	bool unsignedOp = operandIsUnsigned(quad.opX) || operandIsUnsigned(quad.opY);
	bool asm16      = (operandSize(quad.opX) > 1) || (operandSize(quad.opY) > 1);

	ASM instr;

	if (unsignedOp){
		if(asm16){
			/* 1.			LDA HI.x */
			instr = getHiInstruction(ASM::LDA, quad.opX);
			addInstr(instr, codeList);
			
			/* 2.			CMP HI.y */
			instr = getHiInstruction(ASM::CMP, quad.opY);
			addInstr(instr, codeList);
			
			/* 3.			BCS BLS_jp1_	; if yH < xH then y < x */
			ASM::Operand operand = {ASM::A_REL, 0x00};
			instr      = ASM(ASM::BCS, operand);
			instr.txtLabel = newLab1;
			addInstr(instr, codeList);
			
			/* 4.			JMP L */
			operand.opAdd           = ASM::A_ABS;
			operand.relatedOp.A_ABS = 0x0000;
			instr          = ASM(ASM::JMP, operand);
			instr.txtLabel = quad.opZ.label;
			addInstr(instr, codeList);
			
			/* 5. BLS_jp1_: */
			operand.opAdd = ASM::A_LABL;
			instr      = ASM(ASM::LAB, operand);
			instr.txtLabel = newLab1;
			addInstr(instr, codeList);

			/* 6.			BNE BLS_end */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr      = ASM(ASM::BNE, operand);
			instr.txtLabel = newLabE;
			addInstr(instr, codeList);
		}
		/* 7.			LDA LO.x */
		instr = getLoInstruction(ASM::LDA, quad.opX);
		addInstr(instr, codeList);
			
		/* 8.			CMP LO.y */
		instr = getLoInstruction(ASM::CMP, quad.opY);
		addInstr(instr, codeList);
			
		/* 9.			BCS BGR_end_ */
		ASM::Operand operand = {ASM::A_REL, 0};
		instr                = ASM(ASM::BCS, operand);
		instr.txtLabel       = newLabE;
		addInstr(instr, codeList);

	/* [else	; signed] */
	}else{
		/* 1.			SEC */
		ASM::Operand operand = {ASM::A_IMPL, 0};
		instr = ASM(ASM::SEC, operand);
		addInstr(instr, codeList);

		/* [if x.size > 1 || y.size > 1] LDA HI.x [else] LDA LO.x */
		if(asm16)
			instr = getHiInstruction(ASM::LDA, quad.opX);
		else
			instr = getLoInstruction(ASM::LDA, quad.opX);
		addInstr(instr, codeList);

		/* [if x.size > 1 || y.size > 1] SBC HI.y [else] SBC LO.y */
		if(asm16)
			instr = getHiInstruction(ASM::SBC, quad.opY);
		else
			instr = getLoInstruction(ASM::SBC, quad.opY);
		addInstr(instr, codeList);

		/* 4.			BVC BLS_jp1 */
		operand.opAdd           = ASM::A_REL;
		operand.relatedOp.A_REL = 0;
		instr                   = ASM(ASM::BVC, operand);
		instr.txtLabel          = newLab1;
		addInstr(instr, codeList);

		/* 5.			EOR #$80 */
		operand.opAdd           = ASM::A_INM;
		operand.relatedOp.A_INM = 0x80;
		instr = ASM(ASM::EOR, operand);
		addInstr(instr, codeList);

		/* 6. BLS_jp1: */
		operand.opAdd = ASM::A_LABL;
		instr      = ASM(ASM::LAB, operand);
		instr.txtLabel = newLab1;
		addInstr(instr, codeList);

		/* [if x.size > 1 || y.size > 1] BPL BLS_jp2 [else] BPL BLS_end */
		operand.opAdd           = ASM::A_REL;
		operand.relatedOp.A_REL = 0;
		instr          = ASM(ASM::BPL, operand);
		instr.txtLabel = asm16 ? newLab2 : newLabE;
		addInstr(instr, codeList);

		/* [if x.size > 1 || y.size > 1] */
		if(asm16){
			/* 8.			JMP L */
			operand.opAdd           = ASM::A_ABS;
			operand.relatedOp.A_ABS = 0x0000;
			instr          = ASM(ASM::JMP, operand);
			instr.txtLabel = quad.opZ.label;
			addInstr(instr, codeList);

			/* 9. BLS_jp2: */
			operand.opAdd = ASM::A_LABL;
			instr      = ASM(ASM::LAB, operand);
			instr.txtLabel = newLab2;
			addInstr(instr, codeList);

			/* A.			BVC BLS_jp3 */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr                   = ASM(ASM::BVC, operand);
			instr.txtLabel          = newLab3;
			addInstr(instr, codeList);

			/* B.			EOR #$80 */
			operand.opAdd           = ASM::A_INM;
			operand.relatedOp.A_INM = 0x80;
			instr = ASM(ASM::EOR, operand);
			addInstr(instr, codeList);

			/* C. BLS_jp3: */
			operand.opAdd = ASM::A_LABL;
			instr      = ASM(ASM::LAB, operand);
			instr.txtLabel = newLab3;
			addInstr(instr, codeList);

			/* D.			BNE BLS_end */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr                   = ASM(ASM::BNE, operand);
			instr.txtLabel          = newLabE;
			addInstr(instr, codeList);

			/* E.			LDA LO.x */
			instr = getLoInstruction(ASM::LDA, quad.opX);
			addInstr(instr, codeList);

			/* F.			SBC LO.y */
			instr = getLoInstruction(ASM::SBC, quad.opY);
			addInstr(instr, codeList);

			/* 0.			BCS BLS_end */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr                   = ASM(ASM::BCS, operand);
			instr.txtLabel          = newLabE;
			addInstr(instr, codeList);
		}
	}

	/* 11.			JMP L */
	ASM::Operand operand = {ASM::A_ABS, 0x0000};
	instr          = ASM(ASM::JMP, operand);
	instr.txtLabel = quad.opZ.label;
	addInstr(instr, codeList);
			
	/* 12. BLS_end_: */
	operand.opAdd = ASM::A_LABL;
	instr      = ASM(ASM::LAB, operand);
	instr.txtLabel = newLabE;
	addInstr(instr, codeList);
}



/**
* generateEQ
* 
* Aux function. Perform the final code generation of a EQ quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateEQ(const Quad &quad, list<ASM> *codeList){
	/* EQ   xyz */
	/* x := y = z */

	/*
	01- 			LDA LO.y
	02- 			CMP LO.z
	03- 			BNE EQ_false
	[if y.size > 1 || z.size > 1]
		04- 			LDA HI.y
		05- 			CMP HI.z
		06- 			BNE EQ_false
	07- 			LDA #$01
	08- 			BNE EQ_end		; Branch always but is cheaper than JMP
	09- EQ_false:
	0A- 			LDA #$00
	0B- EQ_end:
	0C- 			STA LO.x
	*/

	string newLab = labelGenerator->getNewLabel();
	string newLabE = string("EQ_end_").append(newLab);
	string newLabF = string("EQ_f_").append(newLab);

	/* 01- 			LDA LO.y */
	ASM instr = getLoInstruction(ASM::LDA, quad.opY);
	addInstr(instr, codeList);
	
	/* 02- 			CMP LO.z */
	instr = getLoInstruction(ASM::CMP, quad.opZ);
	addInstr(instr, codeList);	
	
	/* 03- 			BNE EQ_false */
	ASM::Operand operand = {ASM::A_REL, 0x0000};
	instr      = ASM(ASM::BNE, operand);
	instr.txtLabel = newLabF;
	addInstr(instr, codeList);
	
	/* [if y.size > 1 || z.size > 1] */
	if(operandSize(quad.opY) > 1 ||
	   operandSize(quad.opZ) > 1){
		/* 04- 			LDA HI.y */
		instr = getHiInstruction(ASM::LDA, quad.opY);
		addInstr(instr, codeList);	
		
		/* 05- 			CMP HI.z */
		instr = getHiInstruction(ASM::CMP, quad.opZ);
		addInstr(instr, codeList);	
		
		/* 06- 			BNE EQ_false */
		operand.opAdd = ASM::A_REL;
		instr      = ASM(ASM::BNE, operand);
		instr.txtLabel = newLabF;
		addInstr(instr, codeList);
	}
	
	/* 07- 			LDA #$01 */
	operand.opAdd           = ASM::A_INM;
	operand.relatedOp.A_INM = 0x01;
	instr.opcode  = ASM::LDA;
	instr.operand = operand;
	addInstr(instr, codeList);
	
	/* 08- 			BNE EQ_end		; Branch always but is cheaper than JMP */
	operand.opAdd = ASM::A_REL;
	instr      = ASM(ASM::BNE, operand);
	instr.txtLabel = newLabE;
	addInstr(instr, codeList);
	
	/* 09- EQ_false: */
	operand.opAdd = ASM::A_LABL;
	instr      = ASM(ASM::LAB, operand);
	instr.txtLabel = newLabF;
	addInstr(instr, codeList);
	
	/* 0A- 			LDA #$00 */
	operand.opAdd           = ASM::A_INM;
	operand.relatedOp.A_INM = 0x00;
	instr.opcode  = ASM::LDA;
	instr.operand = operand;
	addInstr(instr, codeList);
	
	/* 0B- EQ_end: */
	operand.opAdd = ASM::A_LABL;
	instr      = ASM(ASM::LAB, operand);
	instr.txtLabel = newLabE;
	addInstr(instr, codeList);

	/* 0C- 			STA LO.x */
	instr = getLoInstruction(ASM::STA, quad.opX);
	addInstr(instr, codeList);	
}



/**
* generateNE
* 
* Aux function. Perform the final code generation of a NE quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateNE(const Quad &quad, list<ASM> *codeList){
	/* NE   xyz */
	/* x := y != z */

	/*
	01- 			LDA LO.y
	02- 			CMP LO.z
	03- 			BNE NE_true
	[if y.size > 1 || z.size > 1]
		04- 			LDA HI.y
		05- 			CMP HI.z
		06- 			BNE NE_true
	07- 			LDA #$00
	08- 			BEQ NE_end		; Branch always but is cheaper than JMP
	09- NE_true:
	0A- 			LDA #$01
	0B- NE_end:
	0C- 			STA LO.x
	*/

	string newLab = labelGenerator->getNewLabel();
	string newLabE = string("NE_end_").append(newLab);
	string newLabT = string("NE_t_").append(newLab);

	/* 01- 			LDA LO.y */
	ASM instr = getLoInstruction(ASM::LDA, quad.opY);
	addInstr(instr, codeList);
	
	/* 02- 			CMP LO.z */
	instr = getLoInstruction(ASM::CMP, quad.opZ);
	addInstr(instr, codeList);	
	
	/* 03- 			BNE NE_true */
	ASM::Operand operand = {ASM::A_REL, 0x0000};
	instr      = ASM(ASM::BNE, operand);
	instr.txtLabel = newLabT;
	addInstr(instr, codeList);
	
	/* [if y.size > 1 || z.size > 1] */
	if(operandSize(quad.opY) > 1 ||
	   operandSize(quad.opZ) > 1){	
		/* 04- 			LDA HI.y */
		instr = getHiInstruction(ASM::LDA, quad.opY);
		addInstr(instr, codeList);	
		
		/* 05- 			CMP HI.z */
		instr = getHiInstruction(ASM::CMP, quad.opZ);
		addInstr(instr, codeList);

		/* 06- 			BNE NE_true */
		operand.opAdd           = ASM::A_REL;
		operand.relatedOp.A_REL = 0x0000;
		instr      = ASM(ASM::BNE, operand);
		instr.txtLabel = newLabT;
		addInstr(instr, codeList);
	}

	/* 07- 			LDA #$00 */
	operand.opAdd           = ASM::A_INM;
	operand.relatedOp.A_INM = 0x00;
	instr.opcode  = ASM::LDA;
	instr.operand = operand;
	addInstr(instr, codeList);

	/* 08- 			BEQ NE_end		; Branch always but is cheaper than JMP */
	operand.opAdd           = ASM::A_REL;
	operand.relatedOp.A_REL = 0x0000;
	instr      = ASM(ASM::BEQ, operand);
	instr.txtLabel = newLabE;
	addInstr(instr, codeList);

	/* 09- NE_true: */
	operand.opAdd = ASM::A_LABL;
	instr      = ASM(ASM::LAB, operand);
	instr.txtLabel = newLabT;
	addInstr(instr, codeList);

	/* 0A- 			LDA #$01 */
	operand.opAdd           = ASM::A_INM;
	operand.relatedOp.A_INM = 0x01;
	instr.opcode  = ASM::LDA;
	instr.operand = operand;
	addInstr(instr, codeList);

	/* 0B- NE_end: */
	operand.opAdd = ASM::A_LABL;
	instr      = ASM(ASM::LAB, operand);
	instr.txtLabel = newLabE;
	addInstr(instr, codeList);
	
	/* 0C- 			STA LO.x */
	instr = getLoInstruction(ASM::STA, quad.opX);
	addInstr(instr, codeList);
}



/**
* generateGR
* 
* Aux function. Perform the final code generation of a GR quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateGR(const Quad &quad, list<ASM> *codeList){
	/* GR   xyz */
	/* x := y > z */

	/*
	[if z.isUnsigned || y.isUnsigned]
		[if z.size > 1 || y.size > 1]
			1.			LDA HI.z
			2.			CMP HI.y
			3.			BCC GR_t_	; if zH < yH then z < y
			4.			BNE GR_f
		5.			LDA LO.z
		6.			CMP LO.y
		7.			BCS GR_f_	; if zL < yL then z < y
	[else	; signed]
		1.			SEC
		[if z.size > 1 || y.size > 1] LDA HI.z [else] LDA LO.z
		[if z.size > 1 || y.size > 1] SBC HI.y [else] SBC LO.y
		4.			BVC BGR_jp1
		5.			EOR #$80
		6. GR_jp1:
		[if z.size > 1 || y.size > 1]
			7.			BMI GR_t
			8.			BVC GR_jp2
			9.			EOR #$80
			A. GR_jp2:
			B.			BNE GR_f
			C.			LDA LO.z
			D.			SBC LO.y
			E.			BCS GR_f
		[else]
			7.			BPL GR_f
	[end_if]
	11. GR_t:
	12.			LDA #$01
	13.			BNE GR_end		; Branch always
	14. GR_f:
	15.			LDA #$00
	16. GR_end:
	17.			STA LO.x
	*/

	string newLab  = labelGenerator->getNewLabel();
	string newLab1 = string("GR_jp1_").append(newLab);
	string newLab2 = string("GR_jp2_").append(newLab);
	string newLabT = string("GR_t_").append(newLab);
	string newLabF = string("GR_f_").append(newLab);
	string newLabE = string("GR_end_").append(newLab);

	bool unsignedOp = operandIsUnsigned(quad.opY) || operandIsUnsigned(quad.opZ);
	bool asm16      = (operandSize(quad.opY) > 1) || (operandSize(quad.opZ) > 1);

	ASM instr;

	if (unsignedOp){
		if(asm16){
			/* 1.			LDA HI.z */
			instr = getHiInstruction(ASM::LDA, quad.opZ);
			addInstr(instr, codeList);
			
			/* 2.			CMP HI.y */
			instr = getHiInstruction(ASM::CMP, quad.opY);
			addInstr(instr, codeList);

			/* 3.			BCC GR_t_	; if yH < xH then y < x */
			ASM::Operand operand = {ASM::A_REL, 0x00};
			instr      = ASM(ASM::BCC, operand);
			instr.txtLabel = newLabT;
			addInstr(instr, codeList);

			/* 4.			BNE GR_f */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr      = ASM(ASM::BNE, operand);
			instr.txtLabel = newLabF;
			addInstr(instr, codeList);
		}
		/* 5.			LDA LO.z */
		instr = getLoInstruction(ASM::LDA, quad.opZ);
		addInstr(instr, codeList);
			
		/* 6.			CMP LO.y */
		instr = getLoInstruction(ASM::CMP, quad.opY);
		addInstr(instr, codeList);
			
		/* 7.			BCS GR_f_ */
		ASM::Operand operand = {ASM::A_REL, 0};
		instr                = ASM(ASM::BCS, operand);
		instr.txtLabel       = newLabF;
		addInstr(instr, codeList);

	/* [else	; signed] */
	}else{
		/* 1.			SEC */
		ASM::Operand operand = {ASM::A_IMPL, 0};
		instr = ASM(ASM::SEC, operand);
		addInstr(instr, codeList);

		/* [if z.size > 1 || y.size > 1] LDA HI.z [else] LDA LO.z */
		if(asm16)
			instr = getHiInstruction(ASM::LDA, quad.opZ);
		else
			instr = getLoInstruction(ASM::LDA, quad.opZ);
		addInstr(instr, codeList);

		/* [if z.size > 1 || y.size > 1] SBC HI.y [else] SBC LO.y */
		if(asm16)
			instr = getHiInstruction(ASM::SBC, quad.opY);
		else
			instr = getLoInstruction(ASM::SBC, quad.opY);
		addInstr(instr, codeList);

		/* 4.			BVC GR_jp1 */
		operand.opAdd           = ASM::A_REL;
		operand.relatedOp.A_REL = 0;
		instr                   = ASM(ASM::BVC, operand);
		instr.txtLabel          = newLab1;
		addInstr(instr, codeList);

		/* 5.			EOR #$80 */
		operand.opAdd           = ASM::A_INM;
		operand.relatedOp.A_INM = 0x80;
		instr = ASM(ASM::EOR, operand);
		addInstr(instr, codeList);

		/* 6. GR_jp1: */
		operand.opAdd = ASM::A_LABL;
		instr      = ASM(ASM::LAB, operand);
		instr.txtLabel = newLab1;
		addInstr(instr, codeList);

		/* [if z.size > 1 || y.size > 1] */
		if(asm16){
			/* 7.			BMI GR_t */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr      = ASM(ASM::BMI, operand);
			instr.txtLabel = newLabT;
			addInstr(instr, codeList);

			/* 8.			BVC GR_jp2 */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr                   = ASM(ASM::BVC, operand);
			instr.txtLabel          = newLab2;
			addInstr(instr, codeList);

			/* 9.			EOR #$80 */
			operand.opAdd           = ASM::A_INM;
			operand.relatedOp.A_INM = 0x80;
			instr = ASM(ASM::EOR, operand);
			addInstr(instr, codeList);

			/* A. GR_jp2: */
			operand.opAdd = ASM::A_LABL;
			instr      = ASM(ASM::LAB, operand);
			instr.txtLabel = newLab2;
			addInstr(instr, codeList);

			/* B.			BNE GR_f */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr                   = ASM(ASM::BNE, operand);
			instr.txtLabel          = newLabF;
			addInstr(instr, codeList);

			/* C.			LDA LO.z */
			instr = getLoInstruction(ASM::LDA, quad.opZ);
			addInstr(instr, codeList);

			/* D.			SBC LO.y */
			instr = getLoInstruction(ASM::SBC, quad.opY);
			addInstr(instr, codeList);

			/* E.			BCS GR_f */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr                   = ASM(ASM::BCS, operand);
			instr.txtLabel          = newLabF;
			addInstr(instr, codeList);
		}else{
			/* 7.			BPL GR_f */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr      = ASM(ASM::BPL, operand);
			instr.txtLabel = newLabF;
			addInstr(instr, codeList);
		}
	}

	/* 10.			GR_t */
	ASM::Operand operand = {ASM::A_LABL, 0x0000};
	instr      = ASM(ASM::LAB, operand);
	instr.txtLabel = newLabT;
	addInstr(instr, codeList);

	/* 11.			LDA #$01 */
	operand.opAdd           = ASM::A_INM;
	operand.relatedOp.A_INM = 0x01;
	instr.opcode  = ASM::LDA;
	instr.operand = operand;
	addInstr(instr, codeList);

	/* 12.			BNE GR_end */
	operand.opAdd           = ASM::A_REL;
	operand.relatedOp.A_REL = 0;
	instr                   = ASM(ASM::BNE, operand);
	instr.txtLabel          = newLabE;
	addInstr(instr, codeList);

	/* 13. GR_f: */
	operand.opAdd = ASM::A_LABL;
	instr      = ASM(ASM::LAB, operand);
	instr.txtLabel = newLabF;
	addInstr(instr, codeList);

	/* 14.			LDA #$00 */
	operand.opAdd           = ASM::A_INM;
	operand.relatedOp.A_INM = 0x00;
	instr.opcode  = ASM::LDA;
	instr.operand = operand;
	addInstr(instr, codeList);

	/* 15. GR_end_: */
	operand.opAdd = ASM::A_LABL;
	instr      = ASM(ASM::LAB, operand);
	instr.txtLabel = newLabE;
	addInstr(instr, codeList);

	/* 16.			STA LO.x */
	instr = getLoInstruction(ASM::STA, quad.opX);
	addInstr(instr, codeList);
}



/**
* generateLS
* 
* Aux function. Perform the final code generation of a LS quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateLS(const Quad &quad, list<ASM> *codeList){
	/* LS   xyz */
	/* x := y < z */
	
	/*
	[if z.isUnsigned || y.isUnsigned]
		[if z.size > 1 || y.size > 1]
			1.			LDA HI.y
			2.			CMP HI.z
			3.			BCC LS_t_	; if yH < zH then y < z
			4.			BNE LS_f
		5.			LDA LO.y
		6.			CMP LO.z
		7.			BCS LS_f_	; if yL < zL then y < z
	[else	; signed]
		1.			SEC
		[if z.size > 1 || y.size > 1] LDA HI.y [else] LDA LO.y
		[if z.size > 1 || y.size > 1] SBC HI.z [else] SBC LO.z
		4.			BVC LS_jp1
		5.			EOR #$80
		6. LS_jp1:
		[if z.size > 1 || y.size > 1]
			7.			BMI LS_t
			8.			BVC LS_jp2
			9.			EOR #$80
			A. LS_jp2:
			B.			BNE LS_f
			C.			LDA LO.y
			D.			SBC LO.z
			E.			BCS LS_f
		[else]
			7.			BPL LS_f
	[end_if]
	11. LS_t:
	12.			LDA #$01
	13.			BNE LS_end		; Branch always
	14. LS_f:
	15.			LDA #$00
	16. LS_end:
	17.			STA LO.x
	*/

	string newLab  = labelGenerator->getNewLabel();
	string newLab1 = string("LS_jp1_").append(newLab);
	string newLab2 = string("LS_jp2_").append(newLab);
	string newLabT = string("LS_t_").append(newLab);
	string newLabF = string("LS_f_").append(newLab);
	string newLabE = string("LS_end_").append(newLab);

	bool unsignedOp = operandIsUnsigned(quad.opY) || operandIsUnsigned(quad.opZ);
	bool asm16      = (operandSize(quad.opY) > 1) || (operandSize(quad.opZ) > 1);

	ASM instr;

	if (unsignedOp){
		if(asm16){
			/* 1.			LDA HI.y */
			instr = getHiInstruction(ASM::LDA, quad.opY);
			addInstr(instr, codeList);
			
			/* 2.			CMP HI.z */
			instr = getHiInstruction(ASM::CMP, quad.opZ);
			addInstr(instr, codeList);

			/* 3.			BCC LS_t_	; if yH < zH then y < z */
			ASM::Operand operand = {ASM::A_REL, 0x00};
			instr      = ASM(ASM::BCC, operand);
			instr.txtLabel = newLabT;
			addInstr(instr, codeList);

			/* 4.			BNE LS_f */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr      = ASM(ASM::BNE, operand);
			instr.txtLabel = newLabF;
			addInstr(instr, codeList);
		}
		/* 5.			LDA LO.y */
		instr = getLoInstruction(ASM::LDA, quad.opY);
		addInstr(instr, codeList);
			
		/* 6.			CMP LO.Z */
		instr = getLoInstruction(ASM::CMP, quad.opZ);
		addInstr(instr, codeList);
			
		/* 7.			BCS LS_f_ */
		ASM::Operand operand = {ASM::A_REL, 0};
		instr                = ASM(ASM::BCS, operand);
		instr.txtLabel       = newLabF;
		addInstr(instr, codeList);

	/* [else	; signed] */
	}else{
		/* 1.			SEC */
		ASM::Operand operand = {ASM::A_IMPL, 0};
		instr = ASM(ASM::SEC, operand);
		addInstr(instr, codeList);

		/* [if z.size > 1 || y.size > 1] LDA HI.y [else] LDA LO.y */
		if(asm16)
			instr = getHiInstruction(ASM::LDA, quad.opY);
		else
			instr = getLoInstruction(ASM::LDA, quad.opY);
		addInstr(instr, codeList);

		/* [if z.size > 1 || y.size > 1] SBC HI.z [else] SBC LO.z */
		if(asm16)
			instr = getHiInstruction(ASM::SBC, quad.opZ);
		else
			instr = getLoInstruction(ASM::SBC, quad.opZ);
		addInstr(instr, codeList);

		/* 4.			BVC LS_jp1 */
		operand.opAdd           = ASM::A_REL;
		operand.relatedOp.A_REL = 0;
		instr                   = ASM(ASM::BVC, operand);
		instr.txtLabel          = newLab1;
		addInstr(instr, codeList);

		/* 5.			EOR #$80 */
		operand.opAdd           = ASM::A_INM;
		operand.relatedOp.A_INM = 0x80;
		instr = ASM(ASM::EOR, operand);
		addInstr(instr, codeList);

		/* 6. LS_jp1: */
		operand.opAdd = ASM::A_LABL;
		instr      = ASM(ASM::LAB, operand);
		instr.txtLabel = newLab1;
		addInstr(instr, codeList);

		/* [if z.size > 1 || y.size > 1] */
		if(asm16){
			/* 7.			BMI LS_t */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr      = ASM(ASM::BMI, operand);
			instr.txtLabel = newLabT;
			addInstr(instr, codeList);

			/* 8.			BVC LS_jp2 */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr                   = ASM(ASM::BVC, operand);
			instr.txtLabel          = newLab2;
			addInstr(instr, codeList);

			/* 9.			EOR #$80 */
			operand.opAdd           = ASM::A_INM;
			operand.relatedOp.A_INM = 0x80;
			instr = ASM(ASM::EOR, operand);
			addInstr(instr, codeList);

			/* A. LS_jp2: */
			operand.opAdd = ASM::A_LABL;
			instr      = ASM(ASM::LAB, operand);
			instr.txtLabel = newLab2;
			addInstr(instr, codeList);

			/* B.			BNE LS_f */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr                   = ASM(ASM::BNE, operand);
			instr.txtLabel          = newLabF;
			addInstr(instr, codeList);

			/* C.			LDA LO.y */
			instr = getLoInstruction(ASM::LDA, quad.opY);
			addInstr(instr, codeList);

			/* D.			SBC LO.z */
			instr = getLoInstruction(ASM::SBC, quad.opZ);
			addInstr(instr, codeList);

			/* E.			BCS LS_f */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr                   = ASM(ASM::BCS, operand);
			instr.txtLabel          = newLabF;
			addInstr(instr, codeList);
		}else{
			/* 7.			BPL LS_f */
			operand.opAdd           = ASM::A_REL;
			operand.relatedOp.A_REL = 0;
			instr      = ASM(ASM::BPL, operand);
			instr.txtLabel = newLabF;
			addInstr(instr, codeList);
		}
	}

	/* 10.			LS_t */
	ASM::Operand operand = {ASM::A_LABL, 0x0000};
	instr      = ASM(ASM::LAB, operand);
	instr.txtLabel = newLabT;
	addInstr(instr, codeList);

	/* 11.			LDA #$01 */
	operand.opAdd           = ASM::A_INM;
	operand.relatedOp.A_INM = 0x01;
	instr.opcode  = ASM::LDA;
	instr.operand = operand;
	addInstr(instr, codeList);

	/* 12.			BNE LS_end */
	operand.opAdd           = ASM::A_REL;
	operand.relatedOp.A_REL = 0;
	instr                   = ASM(ASM::BNE, operand);
	instr.txtLabel          = newLabE;
	addInstr(instr, codeList);

	/* 13. LS_f: */
	operand.opAdd = ASM::A_LABL;
	instr      = ASM(ASM::LAB, operand);
	instr.txtLabel = newLabF;
	addInstr(instr, codeList);

	/* 14.			LDA #$00 */
	operand.opAdd           = ASM::A_INM;
	operand.relatedOp.A_INM = 0x00;
	instr.opcode  = ASM::LDA;
	instr.operand = operand;
	addInstr(instr, codeList);

	/* 15. LS_end_: */
	operand.opAdd = ASM::A_LABL;
	instr      = ASM(ASM::LAB, operand);
	instr.txtLabel = newLabE;
	addInstr(instr, codeList);

	/* 16.			STA LO.x */
	instr = getLoInstruction(ASM::STA, quad.opX);
	addInstr(instr, codeList);

}



/**
* generateLAND
* 
* Aux function. Perform the final code generation of a LAND quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateLAND(const Quad &quad, list<ASM> *codeList){
	/* LAND xyz*/
	/* x := y && z */

	/*
	01-				LDA LO.y
	[if y.size >1]
		02a-			ORA HI.y
	03-				BEQ land_end
	04-				LDA LO.z
	[if z.size >1]
		05a-			ORA HI.z
	06- land_end:
	07-				STA LO.x
	*/

	string newLabEnd = string("LAND_end_").append(labelGenerator->getNewLabel());
	
	/* 01- LDA LO.y */
	ASM instr = getLoInstruction(ASM::LDA, quad.opY);
	addInstr(instr, codeList);
	
	/* [if y.size >1] */
	if (operandSize(quad.opY) > 1){
		/* 02a- ORA HI.y */
		instr = getHiInstruction(ASM::ORA, quad.opY);
		addInstr(instr, codeList);
	}
	
	/* 03- BEQ land_end */
	ASM::Operand operand = {ASM::A_REL, 0};
	instr      = ASM(ASM::BEQ, operand);
	instr.txtLabel = newLabEnd;
	addInstr(instr, codeList);
	
	/* 04- LDA LO.z */
	instr = getLoInstruction(ASM::LDA, quad.opZ);
	addInstr(instr, codeList);

	/* [if z.size >1] */
	if (operandSize(quad.opZ) > 1){
		/* 05a- ORA HI.z */
		instr = getHiInstruction(ASM::ORA, quad.opZ);
		addInstr(instr, codeList);
	}
		
	/* 06- land_end: */
	operand.opAdd            = ASM::A_LABL;
	operand.relatedOp.A_LABL = 0x0000;
	instr      = ASM(ASM::LAB, operand);
	instr.txtLabel = newLabEnd;
	addInstr(instr, codeList);
	
	/* 07- STA LO.x */
	instr = getLoInstruction(ASM::STA, quad.opX);
	addInstr(instr, codeList);	
}



/**
* generateLOR
* 
* Aux function. Perform the final code generation of a LOR quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateLOR(const Quad &quad, list<ASM> *codeList){
	/* LOR xyz*/
	/* x := y || z */

	/*
	01- LDA LO.y
	02- ORA LO.z
	[if y.size > 1]
		03a- ORA HI.y
	[if z.size > 1]
		03b- ORA HI.z
	04- STA LO.x
	*/

	/* 01- LDA LO.y */
	ASM instr = getLoInstruction(ASM::LDA, quad.opY);
	addInstr(instr, codeList);

	/* 02- ORA LO.z */
	instr = getLoInstruction(ASM::ORA, quad.opZ);
	addInstr(instr, codeList);

	/* [if y.size > 1] */
	if (operandSize(quad.opY) > 1){
		/* 03a- ORA HI.y */
		instr = getHiInstruction(ASM::ORA, quad.opY);
		addInstr(instr, codeList);
	}
	
	/* [if z.size > 1] */
	if (operandSize(quad.opZ) > 1){
		/* 03b- ORA HI.z */
		instr = getHiInstruction(ASM::ORA, quad.opZ);
		addInstr(instr, codeList);
	}
	
	/* 04- STA LO.x */
	instr = getLoInstruction(ASM::STA, quad.opX);
	addInstr(instr, codeList);
}



/**
* generateAND
* 
* Aux function. Perform the final code generation of a AND quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateAND (const Quad &quad, list<ASM> *codeList){
	/* AND  xyz */
	/* x := y & z */
	/*
	01- LDA LO.Y
	02- AND LO.Z
	03- STA LO.X
	[If X.size > 1]
		01- LDA HI.Y
		02- AND HI.Z
		03- STA HI.X
	*/

	generateInstr(ASM::AND, quad.opX, quad.opY, quad.opZ, codeList);
}



/**
* generateOR
* 
* Aux function. Perform the final code generation of a OR quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateOR  (const Quad &quad, list<ASM> *codeList){
	/* OR   xyz */
	/* x := y | z */
	/*
	01- LDA LO.Y
	02- ORA LO.Z
	03- STA LO.X
	[If X.size > 1]
		01- LDA HI.Y
		02- ORA HI.Z
		03- STA HI.X
	*/

	generateInstr(ASM::ORA, quad.opX, quad.opY, quad.opZ, codeList);
}



/**
* generateXOR
* 
* Aux function. Perform the final code generation of a XOR quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateXOR (const Quad &quad, list<ASM> *codeList){
	/* XOR  xyz */
	/* x := y ^ z */
	/*
	01- LDA LO.Y
	02- EOR LO.Z
	03- STA LO.X
	[If X.size > 1]
		01- LDA HI.Y
		02- EOR HI.Z
		03- STA HI.X
	*/

	generateInstr(ASM::EOR, quad.opX, quad.opY, quad.opZ, codeList);
}



/**
* generateNOT
* 
* Aux function. Perform the final code generation of a NOT quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateNOT (const Quad &quad, list<ASM> *codeList){
	/* NOT  xy- */ /* x := !y */
	/* x and y are 1 byte sized*/

	/*
	01-				LDA y
	02-				BNE NOT_Zero
	03-				LDA #$01
	04-				BNE NOT_end		; Jump always but is 1 cycle cheaper than JMP
	05- NOT_Zero:
	06-				LDA #$00
	07- NOT_end:
	08-				STA x
	*/

	ASM	         instr;
	ASM::Operand operand;
	string newLab   = labelGenerator->getNewLabel();
	string newLabZ = string("NOT_zer_").append(newLab);
	string newLabE = string("NOT_end_").append(newLab);
	
	/* 01- LDA y */
	instr = getLoInstruction(ASM::LDA, quad.opY);
	addInstr(instr, codeList);

	/* 02- BNE NOT_Zero */
	operand.opAdd           = ASM::A_REL;
	operand.relatedOp.A_REL = 0x00;
	instr      = ASM(ASM::BNE, operand);
	instr.txtLabel = newLabZ;
	addInstr(instr, codeList);

	/* 03- LDA #$01 */
	operand.opAdd           = ASM::A_INM;
	operand.relatedOp.A_INM = 0x01;
	instr.opcode  = ASM::LDA;
	instr.operand = operand;
	addInstr(instr, codeList);

	/* 04- BNE NOT_end */
	operand.opAdd           = ASM::A_REL;
	operand.relatedOp.A_REL = 0x00;
	instr      = ASM(ASM::BNE, operand);
	instr.txtLabel = newLabE;
	addInstr(instr, codeList);

	/* 05- NOT_Zero:  */
	operand.opAdd            = ASM::A_LABL;
	operand.relatedOp.A_LABL = 0x0000;
	instr      = ASM(ASM::LAB, operand);
	instr.txtLabel = newLabZ;
	addInstr(instr, codeList);

	/* 06- LDA #$00 */
	operand.opAdd           = ASM::A_INM;
	operand.relatedOp.A_INM = 0x00;
	instr.opcode  = ASM::LDA;
	instr.operand = operand;
	addInstr(instr, codeList);

	/* 07- NOT_end: */
	operand.opAdd            = ASM::A_LABL;
	operand.relatedOp.A_LABL = 0x0000;
	instr      = ASM(ASM::LAB, operand);
	instr.txtLabel = newLabE;
	addInstr(instr, codeList);

	/* 08- STA x */
	instr = getLoInstruction(ASM::STA, quad.opX);
	addInstr(instr, codeList);
}



/**
* generateMOVE
* 
* Aux function. Perform the final code generation of a MOVE quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateMOVE(const Quad &quad, list<ASM> *codeList){
	/* MOVE xy- */ /* x := y */
	/*
	LDA LO.Y
	STA LO.X
	[If X.size > 1]
		LDA HI.Y
		STA HI.X
	*/
	ASM instr;

	/* LDA low.y*/
	instr = getLoInstruction(ASM::LDA, quad.opY);
	addInstr(instr, codeList);
	/* STA low.x */
	instr = getLoInstruction(ASM::STA, quad.opX);
	addInstr(instr, codeList);

	if (operandSize(quad.opX) > 1){
		/* LDA high.y*/
		instr = getHiInstruction(ASM::LDA, quad.opY);
		addInstr(instr, codeList);
		/* STA high.x */
		instr = getHiInstruction(ASM::STA, quad.opX);
		addInstr(instr, codeList);
	}
}



/**
* generateMVP
*
* Aux function. Perform the final code generation of a MVP quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateMVP (const Quad &quad, list<ASM> *codeList){
    /* MVP  xy- */
    /* x := *y */

    /* Avoid if the y param is an overlapped argument */
	if (argMemoryOverlap && quad.opY.opType == Quad::VAR && quad.opY.opSymbol.VAR->isOverlappable()){
        quad.opY.opSymbol.VAR->setPointerDepth(quad.opY.opSymbol.VAR->getPointerDepth() - 1);
        generateOverlappedMOVE(quad, codeList);
        quad.opY.opSymbol.VAR->setPointerDepth(quad.opY.opSymbol.VAR->getPointerDepth() + 1);
        return;
    }

    /*
	[y.isConst]
	01- LDA &y
	02- STA LO.x
	[if x.size > 1]
		LDA &y(&y < 0x00FF ? +1 : +100)
		STA HI.x

	[!y.isConst]
    01- LDY #$00
    02- LDA (y), Y
    03- STA LO.x
    [if x.isPointer]
        [if y.isVolatile begin mutual exclusion]
        04a- INY
        05a- LDA (y), Y
        06a- DEY
        [if y.isVolatile end mutual exclusion]
        07a- STA HI.x
    [else if x.size > 1]
        [if y.isVolatile begin mutual exclusion]
        04b- INC y+1
        05b- LDA (y), Y
        06b- DEC y+1
        [if y.isVolatile end mutual exclusion]
        07b- STA HI.x
    */

	unsigned int address;
	bool         isVolatile = false;

	if (quad.opY.opType == Quad::CONST){
		address = quad.opY.opSymbol.CONST;

		/* 01- LDA &y */
		ASM::Operand operand;
		operand.opAdd           = ASM::A_ABS;
		operand.relatedOp.A_ABS = address;
		ASM instr               = ASM(ASM::LDA, operand);
		addInstr(instr, codeList);

		/* 02- STA LO.x */
		instr = getLoInstruction(ASM::STA, quad.opX);
		addInstr(instr, codeList);

		/* [if x.size > 1] */
		if (quad.opX.opSymbol.VAR->getType().pointerDepth > 0){
			/* LDA &y(&y < 0x00FF ? +1 : +100) */
			operand.relatedOp.A_ABS += (address < 0xFF) ? 0x0001 : 0x0100;
			instr = ASM(ASM::LDA, operand);
			addInstr(instr, codeList);

			/* STA HI.x */
			instr = getHiInstruction(ASM::STA, quad.opX);
			addInstr(instr, codeList);
		}
	}else{
		address    = quad.opY.opSymbol.VAR->getAddress();
		isVolatile = quad.opY.opSymbol.VAR->isVolatile();

		/* 01- LDY #$00 */
		ASM::Operand operand = {ASM::A_INM, 0x00};
		ASM instr            = ASM(ASM::LDY, operand);
		addInstr(instr, codeList);

		/* 02- LDA (y), Y */
		operand.opAdd            = ASM::A_INDY;
		operand.relatedOp.A_INDY = address;
		instr      = ASM(ASM::LDA, operand);

		ostringstream var;
		var.fill('0');
		var << "_" << right << setw(2) << symbolsTable.getId(quad.opY.opSymbol.VAR->getFramework()) << "_";
		var << quad.opY.opSymbol.VAR->lex;

		instr.symbol   = quad.opY.opSymbol.VAR;
		instr.txtLabel = var.str();
		instr.comment  = quad.opY.opSymbol.VAR->getFramework();
		addInstr(instr, codeList);

		/* 03- STA LO.x */
		instr = getLoInstruction(ASM::STA, quad.opX);
		addInstr(instr, codeList);

		/* [if x.isPointer] */
		if (quad.opX.opSymbol.VAR->getType().pointerDepth > 0){
			/* [if y.isVolatile begin mutual exclusion] */
			if (isVolatile){
				/* SEI */
				operand.opAdd            = ASM::A_IMPL;
				operand.relatedOp.A_IMPL = 0;
				instr = ASM(ASM::SEI, operand);
				addInstr(instr, codeList);
			}

			/* 04a- INY */
			operand.opAdd            = ASM::A_IMPL;
			operand.relatedOp.A_IMPL = 0;
			instr = ASM(ASM::INY, operand);
			addInstr(instr, codeList);

			/* 05a- LDA (y), Y */
			operand.opAdd            = ASM::A_INDY;
			operand.relatedOp.A_INDY = address;
			instr = ASM(ASM::LDA, operand);
			addInstr(instr, codeList);

			/* 06a- ; DEY */
			operand.opAdd            = ASM::A_IMPL;
			operand.relatedOp.A_IMPL = 0;
			instr = ASM(ASM::DEY, operand);
			addInstr(instr, codeList);

			/* [if y.isVolatile end mutual exclusion] */
			if (isVolatile){
				/* CLI */
				operand.opAdd            = ASM::A_IMPL;
				operand.relatedOp.A_IMPL = 0;
				instr = ASM(ASM::CLI, operand);
				addInstr(instr, codeList);
			}

			/* 07a- STA HI.x */
			instr = getHiInstruction(ASM::STA, quad.opX);
			addInstr(instr, codeList);

			/* [else if x.size > 1] */
		}else if (operandSize(quad.opX) > 1){
			/* [if y.isVolatile begin mutual exclusion] */
			if (isVolatile){
				/* SEI */
				operand.opAdd            = ASM::A_IMPL;
				operand.relatedOp.A_IMPL = 0;
				instr = ASM(ASM::SEI, operand);
				addInstr(instr, codeList);
			}

			/* 04b- INC y+1 */
			instr = getHiInstruction(ASM::INC, quad.opY);
			addInstr(instr, codeList);

			/* 05b- LDA (y), Y */
			operand.opAdd            = ASM::A_INDY;
			operand.relatedOp.A_INDY = address;
			instr = ASM(ASM::LDA, operand);
			addInstr(instr, codeList);

			/* 06b- DEC y+1 */
			instr = getHiInstruction(ASM::DEC, quad.opY);
			addInstr(instr, codeList);

			/* [if y.isVolatile end mutual exclusion] */
			if (isVolatile){
				/* CLI */
				operand.opAdd            = ASM::A_IMPL;
				operand.relatedOp.A_IMPL = 0;
				instr = ASM(ASM::CLI, operand);
				addInstr(instr, codeList);
			}

			/* 07b- STA HI.x */
			instr = getHiInstruction(ASM::STA, quad.opX);
			addInstr(instr, codeList);
		}
	}
}



/**
* generateMVA
* 
* Aux function. Perform the final code generation of a MVA quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateMVA (const Quad &quad, list<ASM> *codeList){
	/* MVA  xy- */
	/* x := &y */

	/* Avoid if x param is an overlapped argument */
	if (argMemoryOverlap && quad.opX.opSymbol.VAR->isOverlappable()){
		return;
	}


	/*
	01- LDA LO.(&y)
	02- STA LO.x
	03- LDA HI.(&y)
	04- STA HI.x
	*/
	
	unsigned int address  = quad.opY.opSymbol.VAR->getAddress();
	
	/* 01- LDA LO.&y */
	ASM::Operand operand = {ASM::A_INM, (unsigned char)(address & 0x00FF)};
	ASM instr = ASM(ASM::LDA, operand);
	ostringstream var;
	var << "_" << right << setw(2) << symbolsTable.getId(quad.opY.opSymbol.VAR->getFramework()) << "_";
	var << quad.opY.opSymbol.VAR->lex;

	instr.symbol   = quad.opY.opSymbol.VAR;
	instr.txtLabel = var.str();
	instr.comment  = quad.opY.opSymbol.VAR->lex;
	instr.comment.append(" address"); /* quad.opY.opSymbol.VAR->getFramework(); */
	addInstr(instr, codeList);
	
	/* 02- STA LO.x */
	instr = getLoInstruction(ASM::STA, quad.opX);
	addInstr(instr, codeList);

	/* 03- LDA HI.&y */
	operand.relatedOp.A_INM = (address & 0xFF00) >> 8;
	instr = ASM(ASM::LDA, operand);
	addInstr(instr, codeList);

	/* 04- STA HI.x */
	instr = getHiInstruction(ASM::STA, quad.opX);
	addInstr(instr, codeList);
}



/**
* generateSTP
*
* Aux function. Perform the final code generation of a NOP quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateSTP (const Quad &quad, list<ASM> *codeList){
    /* STP  xy- */
    /* *x := y */

    /* Avoid if x param is an overlapped argument */
    if (argMemoryOverlap && quad.opX.opType == Quad::VAR && quad.opX.opSymbol.VAR->isOverlappable()){
        quad.opX.opSymbol.VAR->setNUses(quad.opX.opSymbol.VAR->getNUses() - 1);
        quad.opX.opSymbol.VAR->setPointerDepth(quad.opX.opSymbol.VAR->getPointerDepth() - 1);
        generateOverlappedMOVE(quad, codeList);
		quad.opX.opSymbol.VAR->setPointerDepth(quad.opX.opSymbol.VAR->getPointerDepth() + 1);
        return;
    }

    /*
	[x.isConst]
	01- LDA LO.y
	02- STA &x
	[if y.size > 1]
		LDA HI.y
		STA &x(&x < 0x00FF ? +1 : +100)

	[!x.isConst]
    01- LDY #$00
    02- LDA LO.y
    [if (*x).isPointer]
        [Begin mutual exclusion]
        03- STA (x), Y
        04- INY
        05- LDA HI.y
        06- STA (x),Y
        [End mutual exclusion]
    [else if (*x).size > 1]
        [Begin mutual exclusion]
        03- STA (x), Y
        04- LDA HI.y
		05- INC HI.x
        06- STA (x),Y
		07- DEC HI.x
        [End mutual exclusion]
    [else]
        03- STA (x), Y
    */

    int  address;
    int  size;
    bool isPointer  = false;
    bool isVolatile = false;

    if (quad.opX.opType == Quad::CONST){
        address = quad.opX.opSymbol.CONST;
		size    = quad.size;

		/* 01- LDA LO.y */
		ASM instr = getLoInstruction(ASM::LDA, quad.opY);
		addInstr(instr, codeList);

		/* 02- STA &x */
		ASM::Operand operand;
		operand.opAdd           = ASM::A_ABS;
		operand.relatedOp.A_ABS = address;
		instr                   = ASM(ASM::STA, operand);
		addInstr(instr, codeList);

		/* [if y.size > 1] */
		if(size > 1){
			/* LDA HI.y */
			instr = getHiInstruction(ASM::LDA, quad.opY);
			addInstr(instr, codeList);
		
			/* STA &x(&x < 0x00FF ? +1 : +100) */
			operand.relatedOp.A_ABS += (address < 0x00FF) ? 0x0001 : 0x0100;
			instr                   = ASM(ASM::STA, operand);
			addInstr(instr, codeList);
		}
    }else{
		QualifiedType pointedType = quad.opX.opSymbol.VAR->getType();
		pointedType.pointerDepth--;
		size = symbolsTable.getTypeSize(pointedType);

        address = quad.opX.opSymbol.VAR->getAddress();
		isPointer = pointedType.pointerDepth > 0;
		isVolatile = quad.opX.opSymbol.VAR->isVolatile();

		/* 01- LDY #$00 */
		ASM::Operand operand = {ASM::A_INM, 0x00};
		ASM instr            = ASM(ASM::LDY, operand);
		addInstr(instr, codeList);

		/* 02- LDA LO.y */
		instr = getLoInstruction(ASM::LDA, quad.opY);
		addInstr(instr, codeList);

		/* [if (*x).isPointer] */
		if(isPointer){
			/* [if x.isVolatile begin mutual exclusion] */
			if (isVolatile){
				/* SEI */
				operand.opAdd            = ASM::A_IMPL;
				operand.relatedOp.A_IMPL = 0;
				instr = ASM(ASM::CLI, operand);
				addInstr(instr, codeList);
			}

			/* 03- STA (x), Y */
			operand.opAdd            = ASM::A_INDY;
			operand.relatedOp.A_INDY = address;
			instr = ASM(ASM::STA, operand);
			if(quad.opX.opType != Quad::CONST){
				ostringstream var;
				var.fill('0');
				var << "_" << right << setw(2) << symbolsTable.getId(quad.opX.opSymbol.VAR->getFramework()) << "_";
				var << quad.opX.opSymbol.VAR->lex;

				instr.symbol   = quad.opX.opSymbol.VAR;
				instr.txtLabel = var.str();
				instr.comment  = quad.opX.opSymbol.VAR->getFramework();
			}
			addInstr(instr, codeList);

			/* 04- INY */
			operand.opAdd            = ASM::A_IMPL;
			operand.relatedOp.A_IMPL = 0;
			instr = ASM(ASM::INY, operand);
			addInstr(instr, codeList);

			/* 05- LDA HI.y */
			instr = getHiInstruction(ASM::LDA, quad.opY);
			addInstr(instr, codeList);

			/* 06- STA (x),Y */
			operand.opAdd            = ASM::A_INDY;
			operand.relatedOp.A_INDY = address;
			instr = ASM(ASM::STA, operand);
			if(quad.opX.opType != Quad::CONST){
				ostringstream var;
				var.fill('0');
				var << "_" << right << setw(2) << symbolsTable.getId(quad.opX.opSymbol.VAR->getFramework()) << "_";
				var << quad.opX.opSymbol.VAR->lex;

				instr.symbol   = quad.opX.opSymbol.VAR;
				instr.txtLabel = var.str();
				instr.comment  = quad.opX.opSymbol.VAR->getFramework();
			}
			addInstr(instr, codeList);

			/* [if x.isVolatile end mutual exclusion] */
			if (isVolatile){
				/* CLI */
				operand.opAdd            = ASM::A_IMPL;
				operand.relatedOp.A_IMPL = 0;
				instr = ASM(ASM::CLI, operand);
				addInstr(instr, codeList);
			}
			/* [else if (*x).size > 1] */
		}else if(size > 1){
			/* [if x.isVolatile begin mutual exclusion] */
			if (isVolatile){
				/* SEI */
				operand.opAdd            = ASM::A_IMPL;
				operand.relatedOp.A_IMPL = 0;
				instr = ASM(ASM::SEI, operand);
				addInstr(instr, codeList);
			}

			/* 03- STA (x), Y */
			operand.opAdd            = ASM::A_INDY;
			operand.relatedOp.A_INDY = address;
			instr = ASM(ASM::STA, operand);
			if(quad.opX.opType != Quad::CONST){
				ostringstream var;
				var.fill('0');
				var << "_" << right << setw(2) << symbolsTable.getId(quad.opX.opSymbol.VAR->getFramework()) << "_";
				var << quad.opX.opSymbol.VAR->lex;

				instr.symbol   = quad.opX.opSymbol.VAR;
				instr.txtLabel = var.str();
				instr.comment  = quad.opX.opSymbol.VAR->getFramework();
			}
			addInstr(instr, codeList);

			/* 04- LDA HI.y */
			instr = getHiInstruction(ASM::LDA, quad.opY);
			addInstr(instr, codeList);

			/* 05- INC HI.x */
			instr = getHiInstruction(ASM::INC, quad.opX);
			addInstr(instr, codeList);

			/* 06- STA (x),Y */
			operand.opAdd            = ASM::A_INDY;
			operand.relatedOp.A_INDY = address;
			instr = ASM(ASM::STA, operand);
			if(quad.opX.opType != Quad::CONST){
				ostringstream var;
				var.fill('0');
				var << "_" << right << setw(2) << symbolsTable.getId(quad.opX.opSymbol.VAR->getFramework()) << "_";
				var << quad.opX.opSymbol.VAR->lex;

				instr.symbol   = quad.opX.opSymbol.VAR;
				instr.txtLabel = var.str();
				instr.comment  = quad.opX.opSymbol.VAR->getFramework();
			}
			addInstr(instr, codeList);

			/* 07- DEC HI.x */
			instr = getHiInstruction(ASM::DEC, quad.opX);
			addInstr(instr, codeList);

			/* [if x.isVolatile end mutual exclusion] */
			if (isVolatile){
				/* CLI */
				operand.opAdd            = ASM::A_IMPL;
				operand.relatedOp.A_IMPL = 0;
				instr = ASM(ASM::CLI, operand);
				addInstr(instr, codeList);
			}
			/* [else] */
		}else{
			/* 03- STA (x), Y */
			operand.opAdd            = ASM::A_INDY;
			operand.relatedOp.A_INDY = address;
			instr = ASM(ASM::STA, operand);
			if(quad.opX.opType != Quad::CONST){
				ostringstream var;
				var.fill('0');
				var << "_" << right << setw(2) << symbolsTable.getId(quad.opX.opSymbol.VAR->getFramework()) << "_";
				var << quad.opX.opSymbol.VAR->lex;

				instr.symbol   = quad.opX.opSymbol.VAR;
				instr.txtLabel = var.str();
				instr.comment  = quad.opX.opSymbol.VAR->getFramework();
			}
			addInstr(instr, codeList);
		}
	}
}



/**
* generateSTA
* 
* Aux function. Perform the final code generation of a STA quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateSTA (const Quad &quad, list<ASM> *codeList){
	/* STA  xy- */
	/* &x := y */ 
	/* unused */
}



/**
* generateLABL
* 
* Aux function. Perform the final code generation of a LABL quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateLABL(const Quad &quad, list<ASM> *codeList){
	/* LABL --L */
	/* Insert L */
	ASM::Operand operand = {ASM::A_LABL, 0};
	ASM instr            = ASM(ASM::LAB, operand);
	instr.txtLabel       = quad.opZ.label;
	addInstr(instr, codeList);
}



/**
* generatePUSH
* 
* Aux function. Perform the final code generation of a PUSH quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generatePUSH(const Quad &quad, list<ASM> *codeList){
    /* PUSH x-- */
    /* *SP := x; SP++ */
    
    /*
    [If X.size > 1]
        LDA HI.X
        PHA
    LDA LO.X
    PHA
    */
    
    ASM instr;
    ASM::Operand operand = {ASM::A_IMPL, 0};

    if (operandSize(quad.opX) > 1){
        /* LDA high.X*/
        instr = getHiInstruction(ASM::LDA, quad.opX);
        addInstr(instr, codeList);
        /* PHA */
        instr = ASM(ASM::PHA, operand);
        addInstr(instr, codeList);
    }
    
    /* LDA low.X*/
    instr = getLoInstruction(ASM::LDA, quad.opX);
    addInstr(instr, codeList);
    /* PHA */
    instr = ASM(ASM::PHA, operand);
    addInstr(instr, codeList);
}



/**
* generatePOP
* 
* Aux function. Perform the final code generation of a POP quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generatePOP (const Quad &quad, list<ASM> *codeList){
    /* POP  x-- *//* x := *SP; SP-- */
    /*
    PLA
    STA LO.X
    [If X.size > 1]
        PLA
        STA HI.X
    */
    ASM instr;
    ASM::Operand operand = {ASM::A_IMPL, 0};

    /* PLA */
    instr = ASM(ASM::PLA, operand);
    addInstr(instr, codeList);
    /* STA low.x*/
    instr = getLoInstruction(ASM::STA, quad.opX);
    addInstr(instr, codeList);

    if (operandSize(quad.opX) > 1){
        /* PLA */
        instr = ASM(ASM::PLA, operand);
        addInstr(instr, codeList);
        /* STA high.x*/
        instr = getHiInstruction(ASM::STA, quad.opX);
        addInstr(instr, codeList);
    }
}



/**
* generateCALL
* 
* Aux function. Perform the final code generation of a CALL quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateCALL(const Quad &quad, list<ASM> *codeList){
	/* CALL --L */
	/* Call L */
	ASM::Operand operand = {ASM::A_ABS, 0};
	ASM instr      = ASM(ASM::JSR, operand);
	instr.txtLabel = quad.opZ.label;
	addInstr(instr, codeList);
}



/**
* generateRET
* 
* Aux function. Perform the final code generation of a RET quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateRET (const Quad &quad, list<ASM> *codeList){
	/* RET  x-- */
	/* Return x */

	/*
	[if RTI]
		01- PLA
		02- TAY
		03- PLA
		04- TAX
		05- PLA
		06- RTI
	[else]
		[if !x.isUndef]
			01- LDA LO.x
			02- STA LO.return
			[If x.size > 1]
				03- LDA HI.x
				04- STA HI.return
		05- RTS
	*/
	
	ASM instr;

	/* [if RTI] */
	if(symbolsTable.getOutputType(currentFramework).b_interrupt){
		/* Returning from an interruption */
		/* 01- PLA */
		ASM::Operand operand = {ASM::A_IMPL, 0};
		instr                = ASM(ASM::PLA, operand);
		addInstr(instr, codeList);

		/* 02- TAY */
		instr = ASM(ASM::TAY, operand);
		addInstr(instr, codeList);

		/* 03- PLA */
		instr = ASM(ASM::PLA, operand);
		addInstr(instr, codeList);

		/* 04- TAX */
		instr = ASM(ASM::TAX, operand);
		addInstr(instr, codeList);

		/* 05- PLA */
		instr = ASM(ASM::PLA, operand);
		addInstr(instr, codeList);

		/* 06- RTI */
		instr = ASM(ASM::RTI, operand);
		addInstr(instr, codeList);
		/* [else] */
	}else{
		/* Returning from a function */
	
		/* [if !x.isUndef] */
		if (quad.opX.opType != Quad::UNDEF){

			Quad::Operand returnOp;
			returnOp.opType       = Quad::VAR;
			returnOp.opSymbol.VAR = symbolsTable.getSymbol(SEM_RETURN_SYMBOL, currentFramework);

			/* 01- LDA LO.x */
			instr = getLoInstruction(ASM::LDA, quad.opX);
			addInstr(instr, codeList);

			/* 02- STA LO.return */
			instr = getLoInstruction(ASM::STA, returnOp);
			addInstr(instr, codeList);

			/* [If x.size > 1] */
			if (operandSize(quad.opX) > 1){
				/* 03- LDA HI.x */
				instr = getHiInstruction(ASM::LDA, quad.opX);
				addInstr(instr, codeList);
				
				/* 04- STA HI.return */
				instr = getHiInstruction(ASM::STA, returnOp);
				addInstr(instr, codeList);
			}
		}
		/* 05- RTS */
		ASM::Operand operand = {ASM::A_IMPL, 0};
		instr                = ASM(ASM::RTS, operand);
		addInstr(instr, codeList);
	}
}



/**
* generateHALT
* 
* Aux function. Perform the final code generation of a HALT quad.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateHALT(const Quad &quad, list<ASM> *codeList){
	/* HALT --- */
	/* Stop the machine */
	
	/*
	01- halt:
	02- NOP			; just a halt mark
	03- JMP halt
	*/
	
	string newLab = string("HALT_").append(labelGenerator->getNewLabel());
	
	/* 01- halt: */
	ASM::Operand operand = {ASM::A_LABL, 0};
	ASM instr            = ASM(ASM::LAB, operand);
	instr.txtLabel       = newLab;
	addInstr(instr, codeList);
	
	/* 02- NOP */
	operand.opAdd  = ASM::A_IMPL;
	instr          = ASM(ASM::NOP, operand);
	instr.txtLabel = "";
	addInstr(instr, codeList);
	
	/* 03- JMP halt */
	operand.opAdd = ASM::A_ABS;
	instr      = ASM(ASM::JMP, operand);
	instr.txtLabel = newLab;
	addInstr(instr, codeList);
}



/**
* generateOverlappedMOVE
*
* Aux function. Perform the final code generation of a MVP or STP quad
* which has been transformed into MOVE because of an overlapped argument.
*
* @param quad: The quad which is being translated.
* @param codeList: The ASM list where the final code is written.
*/
void ASMGenerator::generateOverlappedMOVE(const Quad &quad, list<ASM> *codeList){
    /*
    if (x1 == y2 && x1->getNUses() == 1 && !x1->isVolatile()){
        MOVE + STP = STP
        MOVE + MVP = MVP (If Y1 is VAR)
    }else{
        MOVE + XXX = MOVE + XXX
    }
    */
  
    if (itNextInst != itLastInst){
        switch((*itNextInst).opcode){
      
		case Quad::STP:
            if ((*itNextInst).opY.opType == Quad::VAR &&
                (*itNextInst).opY.opSymbol.VAR == quad.opX.opSymbol.VAR &&
                quad.opX.opSymbol.VAR->getNUses() == 1 &&
                !(quad.opX.opSymbol.VAR->isVolatile())){

                quad.opY.opSymbol.VAR->setNUses(quad.opY.opSymbol.VAR->getNUses() + 1);
				(*itNextInst).opY.opSymbol.VAR->setNUses((*itNextInst).opY.opSymbol.VAR->getNUses() - 1);
                (*itNextInst).opY.opSymbol.VAR = quad.opY.opSymbol.VAR;
              
                Quad newQuad = Quad(*itNextInst);
                itNextInst++;
                generateSTP(newQuad, codeList);
             
                return;
            }
            break;
          
		case Quad::MVP:
            if (quad.opY.opType == Quad::VAR &&
                (*itNextInst).opY.opType == Quad::VAR &&
                (*itNextInst).opY.opSymbol.VAR == quad.opX.opSymbol.VAR &&
                quad.opX.opSymbol.VAR->getNUses() == 1 &&
                !(quad.opX.opSymbol.VAR->isVolatile())){
         
                quad.opY.opSymbol.VAR->setNUses(quad.opY.opSymbol.VAR->getNUses() + 1);
				(*itNextInst).opY.opSymbol.VAR->setNUses((*itNextInst).opY.opSymbol.VAR->getNUses() - 1);
				 (*itNextInst).opY.opSymbol.VAR = quad.opY.opSymbol.VAR;
             
                Quad newQuad = Quad(*itNextInst);
                itNextInst++;
                generateMVP(newQuad, codeList);
              
                return;
            }
            break;
          
        default:
            ;
        };
    }

    generateMOVE(quad, codeList);
}
#pragma warning ( default : 4100 )



/**
* isConditionalLabel
*
* Aux function. Checks if an opcode is a conditional label.
*
* @param opcode: The opcode to be checked.
* @return true if the opcode is a conditional label or false otherwise
*/
bool ASMGenerator::isConditionalLabel(const ASM::Opcode &opcode){
	if (opcode == ASM::BCC || opcode == ASM::BCS || opcode == ASM::BEQ || opcode == ASM::BNE ||
		opcode == ASM::BMI || opcode == ASM::BPL || opcode == ASM::BVC || opcode == ASM::BVC){
		return true;
	}else{
		return false;
	}
}



/**
* invertJmp
*
* Aux function. Perform the inversion of a jump instruction.
*
* @param opcode: The opcode to invert.
*/
void ASMGenerator::invertJmp(ASM::Opcode *opcode){

	switch(*opcode){

	case (ASM::BCC):
		*opcode = ASM::BCS;
		break;
	case (ASM::BCS):
		*opcode = ASM::BCC;
		break;
	case (ASM::BEQ):
		*opcode = ASM::BNE;
		break;
	case (ASM::BMI):
		*opcode = ASM::BPL;
		break;
	case (ASM::BNE):
		*opcode = ASM::BEQ;
		break;
	case (ASM::BPL):
		*opcode = ASM::BMI;
		break;
	case (ASM::BVC):
		*opcode = ASM::BVS;
		break;
	case (ASM::BVS):
		*opcode = ASM::BVC;
		break;
	}
}



/**
* simplifyDoubleJumps
*
* Checks where each label is currently adressed. Take advantage to transform double-instruction jumps
* into single jumps if that is possible. Stops when any new transformation could be done.
*/
void ASMGenerator::simplifyDoubleJumps(){
	int                        currentAddress;
	bool                       changeDone = true;
	unordered_map<string, int> labelUsed  = unordered_map<string, int>();
	list<ASM>::iterator        it;
	list<ASM>::iterator        auxIt;

	auto f_assignLabelAddress = [this, &currentAddress, &labelUsed](ASM const &x){
		int asmOpcode = opcodeValue[(x.opcode * 14) + x.operand.opAdd];
		int nBytes    = instSizes[asmOpcode];

		/* If label, put into labelAddress */
		if (x.opcode == ASM::LAB){
			labelAddress.insert(make_pair(x.txtLabel,currentAddress));
		}else{
			/* Increment current address */
			currentAddress += nBytes;
			
			if(x.txtLabel != ""){
				/* If not label, but operand with label, increase label usage */
				unordered_map<string, int>::const_iterator itLU;
				itLU = labelUsed.find(x.txtLabel);
				if (itLU == labelUsed.end()){
					labelUsed.insert(make_pair(x.txtLabel,1));
				}else{
					int uses = itLU->second;
					labelUsed.erase(x.txtLabel);
					labelUsed.insert(make_pair(x.txtLabel, uses + 1));
				}
			}
		}
	};


	while(changeDone){
		currentAddress = ASM_START_ADDRESS_I;
		changeDone     = false;
		labelUsed.clear();
		labelAddress.clear();

		/* Assign Addresses */
		for_each(code.begin(), code.end(), f_assignLabelAddress);


		/* Perform changes */
		currentAddress = ASM_START_ADDRESS_I;
		for(it = code.begin() ; it != code.end() ; it++){
			/* If current is conditionalLabel to labX */
			if(isConditionalLabel(it->opcode)){
				auxIt = it;
				auxIt++;

				/* And next is JMP to labY */
				if (auxIt != code.end() && auxIt->opcode == ASM::JMP){
					auxIt++;
					
					/* And next is labX */
					if (auxIt != code.end() && auxIt->opcode == ASM::LAB && auxIt->txtLabel == it->txtLabel){
						auxIt--;

						/* And difference between labY and current is > -129 and < 128 */
						int diff = labelAddress.find(auxIt->txtLabel)->second - currentAddress;
						
						if(diff < 128 && diff > -129){
							/* ---------------------------------------
							 *
							 * Is possible to kill the JMP instruction
							 *
							 * --------------------------------------- */

							/* Invert label and reference to labY */
							invertJmp(&(it->opcode));
							it->txtLabel = auxIt->txtLabel;

							/* Increment currentAddress and kill JMP labY */
							currentAddress += 3;
							code.erase(auxIt);

							/* If labX have just one reference, kill lab */
							auxIt = it;
							auxIt++;
							if(labelUsed.find(auxIt->txtLabel)->second == 1){
								code.erase(auxIt);
							}

							/* Change done = true */
							changeDone = true;
						}
					}
				}
			}

			/* Increment currentAddress */
			if (it->opcode != ASM::LAB){
				currentAddress += instSizes[opcodeValue[(it->opcode * 14) + it->operand.opAdd]];
			}

			/* Assign adresses */
			if(it->opcode != ASM::LAB && it->txtLabel != "" && !changeDone){
				if(it->operand.opAdd == ASM::A_REL){
					/* $BB jump to PC + (signed)BB */
					it->operand.relatedOp.A_REL = ((labelAddress.find(it->txtLabel)->second) - currentAddress) & 0xFF;
				}else if(it->operand.opAdd == ASM::A_ABS){
					/* $HHLL jump to $HHLL */
					it->operand.relatedOp.A_ABS = labelAddress.find(it->txtLabel)->second;
				}
			}
		}
	}
}



/**
* toString
* 
* Get the object code info and stored into a string.
*
* @return a formatted string containing the entire ASM code of the program. Variables included.
*/
string ASMGenerator::toString() const{
	ostringstream output;
	list<STFramework*> frameworks;

	/*
	auto f_concat = [&output](ASM const &x){
		output << x.toString() << endl;
	};

	auto f_concatList = [&output, f_concat](list<ASM> const &x){
		for_each(x.begin(), x.end(), f_concat);
		output << endl << endl;
	};

	for_each(code.begin(), code.end(), f_concatList);
	*/


	auto f_printSymbol = [this, &output](STEntry* const x){
		if (x->getAddress() >= 0){
			output.fill('0');
			output << "_" << right << setw(2) << hex << symbolsTable.getId(x->getFramework()) << "_";
			output << x->lex;

			output.fill('\t');
			output << left << setw((MAX_ID_LEN + 6 - x->lex.size()) / TAB_SIZE) << "";

			output << "= $" << hex << x->getAddress();
			output << endl;
		}
	};


	auto f_printSymbols = [f_printSymbol](STFramework* const x){
		list<STEntry*> symbols = x->getSymbols();
		for_each(symbols.begin(), symbols.end(), f_printSymbol);
	};


	auto f_concat = [&output](const ASM &x){
		output << x.toString() << endl;
	};


	/* Print symbol equivalence */
	frameworks = symbolsTable.getFrameworks();
	for_each(frameworks.begin(), frameworks.end(), f_printSymbols);


	/* Print code */
	output << endl << endl << endl << "\t\t\t.org " << ASM_START_ADDRESS_S << endl << endl << endl;
	for_each(code.begin(), code.end(), f_concat);

	return output.str();
}



/**
* toBinary
* 
* Get the object code info and stored into a string.
*
* @return a formatted string containing the entire ASM code of the program. Variables included.
*/
string ASMGenerator::toBinary(){
	ostringstream              output;


	auto f_concat = [&output](ASM const &x){
		output << x.toBinary();
	};


	for_each(code.begin(), code.end(), f_concat);
	
	output << endl;

	return output.str();
}





/**
* |------------------------------------------|
* |                                          |
* | Class: VarMemoryAllocator                |
* |                                          |
* |------------------------------------------|
*/

/**
* VarMemoryAllocator
* 
* Class constructor.
*/
ASMGenerator::VarMemoryAllocator::VarMemoryAllocator(){
	/* 2Kbytes - 8 pages of 256 bytes */
	memAllocated.reset();
}



/**
* ~VarMemoryAllocator
* 
* Class destructor.
*/
ASMGenerator::VarMemoryAllocator::~VarMemoryAllocator(){ }



/**
* mask
* 
* The memory allocator is masked with the mask bitset.
*
* @param maskBitset: The mask to set the bitmap.
*/
void ASMGenerator::VarMemoryAllocator::mask(bitset<ASM_MEMORY_MAP_SIZE> maskBitset){
	int i;
	for (i = 0 ; i < ASM_MEMORY_MAP_SIZE ; i++)
		memAllocated[i] = memAllocated[i] | maskBitset[i];
}



/**
* copy
* 
* The memory allocator is copied from a source.
*
* @param source: The mask to set the bitmap.
*/
void ASMGenerator::VarMemoryAllocator::copy(bitset<ASM_MEMORY_MAP_SIZE> source){
	int i;

	memAllocated.reset();

	for(i = 0 ; i < ASM_MEMORY_MAP_SIZE ; i++)
		if(source[i])
			memAllocated.set(i);
}



/**
* setOfZeroPage
* 
* Set a given symbol in a free zero page memory address.
*
* @param lex: The name of the symbol
* @param framework: The name of a framework
*
* @return The address where the value is set.
*/
int ASMGenerator::VarMemoryAllocator::setOfZeroPage(const string &lex, const string &framework){
	int  i, j;
	int  size;
	int  address = -1;
	bool set;

	size = symbolsTable.getTypeSize(symbolsTable.getType(lex, framework));
	for(i = 0 ; i < 256 ; i++){
		set = true;
		for (j = 0 ; j < size ; j++){
			if (memAllocated[i + j])
				set = false;
		}

		if(set){
			address = i;
			symbolsTable.setAddress(lex, framework, address);
			for (j = 0 ; j < size ; j++)
				memAllocated.set(address + j);
			break;
		}
	}

	return address;
}



/**
* setVar
* 
* Set a given symbol in a free memory address.
*
* @param lex: The name of the symbol
* @param framework: The name of a framework
*
* @return The address where the value is set.
*/
int ASMGenerator::VarMemoryAllocator::setVar(const string &lex, const string &framework){
	int  i, j;
	int  size;
	int  address = -1;
	bool set;

	size = symbolsTable.getTypeSize(symbolsTable.getType(lex, framework));
	for(i = 512 ; i < (ASM_MEMORY_MAP_SIZE - 256) ; i++){
		set = true;
		for (j = 0 ; j < size ; j++){
			if (memAllocated[i + (256 * j)])
				set = false;
		}

		if(set){
			address = i;
			symbolsTable.setAddress(lex, framework, address);
			for (j = 0 ; j < size ; j++)
				memAllocated.set(address + (256 * j));
			return address;
		}
	}

	if(size > 1)
		return -1;

	for( ; i < ASM_MEMORY_MAP_SIZE ; i++){
		if(!memAllocated[i]){
			address = i;
			symbolsTable.setAddress(lex, framework, address);
			memAllocated.set(address);
			return address;
		}
	}

	return address;
}
