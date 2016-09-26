/**
 * |------------------------------------------|
 * | CO2 6502, COMPILER OPTIMIZER TO 6502     |
 * | File: optimizer.cpp                      |
 * | v1.0, April 2013                         |
 * | Author: Emilio Arango Delgado de Mendoza |
 * |------------------------------------------|
 */


#include "optimizer.h"
#include "../CommonFiles/errManager.h"
#include <algorithm>
#include <iomanip>

#ifdef WIN32
#define insert   emplace   /* 'emplace' only used under WIN32.
                               Is unsupported by gnu c++0x at October 2012 */
#endif



extern SymbolsTable                      symbolsTable;
extern ErrManager                        errManager;

static unordered_map<STEntry*, STEntry*> constantPointers;





/**
* |------------------------------------------|
* |                                          |
* | Class: OCodeBlock                        |
* |                                          |
* |------------------------------------------|
*/



/**
* OCodeBlock
*
* Class constructor.
*/
OCodeBlock::OCodeBlock(){
	currentFramework = "";

	oCode            = list<Quad>();

	inUse            = list<STEntry*>();
	stillAlive       = list<STEntry*>();
	usedYet          = list<STEntry*>();
	aliveSymbols     = list<STEntry*>();

	referencedBy     = list<OCodeBlock*>();
	referencingA     = NULL;
	referencingB     = NULL;
	isLeaf           = false;
}



/**
* OCodeBlock
*
* Class destructor.
*/
OCodeBlock::~OCodeBlock(){
	referencedBy.clear();
	oCode.clear();
	inUse.clear();
	stillAlive.clear();
	usedYet.clear();
}



/**
* exists
*
* Auxiliar function. Look for a symbol into a given list
*
* @param symbol: the searched symbol
* @param inList: the symbols list to perform the check
*
* @return true if the symbol exists in the list
*/
bool OCodeBlock::exists(const STEntry* symbol, const list<STEntry*> *inList){
	return inList->end() != 
	find_if(inList->begin(), inList->end(),
		[&symbol](const STEntry* x)->bool{
			return x == symbol;
		}
	);
}



/**
* addInUse
*
* Auxiliar function. Set a symbol as "in use" (alive and used) in this block
*
* @param symbol: The symbol to add as in use
*/
void OCodeBlock::addInUse(STEntry* symbol){
	if (!exists(symbol, &inUse)){
		inUse.push_back(symbol);
	}

	/* If the symbol is a constant pointer, its pointed symbol is also in use */
	if(symbol->getPointerDepth() > 0){
		unordered_map<STEntry*,STEntry*>::iterator it = constantPointers.find(symbol);
		if(it != constantPointers.end()){
			if (!exists(it->second, &inUse)){
				inUse.push_back(it->second);
			}
		}
	}
}



/**
* addStillAlive
*
* Auxiliar function. Set a symbol as alive in this block and in each possible previous block
*
* @param symbol: The symbol to add as alive
*/
void OCodeBlock::addStillAlive(STEntry* symbol){
	/* Bottom up */
	if(symbol == NULL)
		return;

	if (!exists(symbol, &stillAlive)){
		stillAlive.push_back(symbol);

		for_each(referencedBy.begin(), referencedBy.end(),
			[this, symbol](OCodeBlock* x){ x->addStillAlive(symbol); });
	}
}



/**
* addUsedYet
*
* Auxiliar function. Set a symbol as alive in this block and in each possible next block
*
* @param symbol: The symbol to add as alive
*/
void OCodeBlock::addUsedYet(STEntry* symbol){
	/* Top down */
	if(symbol == NULL)
		return;

	if (!exists(symbol, &usedYet)){
		usedYet.push_back(symbol);

		if (referencingA != NULL)
			referencingA->addUsedYet(symbol);
		if (referencingB != NULL)
			referencingB->addUsedYet(symbol);
	}
}



/**
* addReferencing
*
* Add a new referencing to the block (another possible block to be executed after this one)
*
* @param newReference: The new reference
*/
void OCodeBlock::addReferencing(OCodeBlock* newReference){
	if (referencingA == NULL)
		referencingA = newReference;
	else
		referencingB = newReference;
}





/**
* |------------------------------------------|
* |                                          |
* | Class: Optimizer                         |
* |                                          |
* |------------------------------------------|
*/

/**
* Optimizer
* 
* Class constructor.
*/
Optimizer::Optimizer(){
	constantPointers = unordered_map<STEntry*, STEntry*>();
}



/**
* ~Optimizer
* 
* Class destructor.
*/
Optimizer::~Optimizer(){ constantPointers.clear(); }



/**
* optimize
* 
* Perform the optimization tasks.
*
* @param flags: The flags with the tasks should be performed.
*/
void Optimizer::optimize(list <list <Quad>>* oCode, int inOptimizationGrade){

	int optIterations = 0;
	optimizationGrade = inOptimizationGrade;

	auto f_isTemp = [](STEntry* symbol)->bool{
        string lex = symbol->lex;
        return (OC_TEMP_PREFIX == lex.substr(0, ((string)OC_TEMP_PREFIX).size()));
    };


	auto f_killTemp = [f_isTemp](STEntry* symbol){
		if(f_isTemp(symbol) && symbolsTable.getNUses(symbol->lex, symbol->getFramework()) == 0)
			symbolsTable.delSymbol(symbol->lex, symbol->getFramework());
	};


	auto f_blockOptimizations = [this](OCodeBlock &x){
		/* Local common subexpressions */
		localCommonSubexpressions(&(x.oCode));
	
		/* Temp var renaming  */
		tempVarRenaming(&x);

		/* Dead code deletion */
		deadCodeDeletion(&x);
	};


	auto f_performOptimizations = [this, f_blockOptimizations, f_killTemp](list <Quad> &x){
		list<OCodeBlock> oCodeBlock;

		/* Clean redundant labels and literal translations */
		firstCleaning(&x);

		/* If the optimization grade is 1, stop optimizing */
		if (optimizationGrade < 2)
			return;


		/* Codeblock slicing */
		oCodeBlock = slice(x);

		/* Jump to jumps shortcircuit and apply unreachable code deletion */
		shortcircuitAndUnreachableDeletion(&oCodeBlock);

		/* If the optimization grade is 2, stop optimizing */
		if (optimizationGrade < 3){
			x = join(oCodeBlock);
			return;
		}

		/* Symbols lifecycle */
		lifecycleCalc(&oCodeBlock);

		/*
			Local common subexpressions
			Temp var renaming
			Dead code deletion
		*/
		for_each(oCodeBlock.begin(), oCodeBlock.end(), f_blockOptimizations);
		
		/* Partial redundance deletion */
		/* FUTURE <PartialRedundanceDeletion> */
		
		/* Codeblock joining */
		x = join(oCodeBlock);

		/* Temporal symbols deletion */
		list<STEntry*> symbols = symbolsTable.getSymbols(oCodeBlock.front().currentFramework);
		for_each(symbols.begin(), symbols.end(), f_killTemp);
	};


	/* Perform optimization for every O code framework */
	do{
		changeDone = false;
		for_each(oCode->begin(), oCode->end(), f_performOptimizations);
	}while (changeDone && optIterations++ < (optimizationGrade * MAX_OPT_ITERATIONS));

	/* Perform last optimizations */

	/* Induction variables */
	/* FUTURE <InductionVariables> */

	/* Final code optimization */
	/* FUTURE <FinalCodeOptimizations> */
}



/**
* firstCleaning
*
* Optimization. Perform a block of simple code optimizations:
*	Kill unnecessary jumps (Jump to just next instruction)
*	Deletes unnecessary labels
*	Trigger unreachable code optimization
*	Trigger algebraic changes optimization
*	Trigger strength reduction optimization
*
* @param frameworkList: The block where the firsts optimizations are performed
*/
void Optimizer::firstCleaning(list<Quad>* frameworkList){
	unordered_map<string,string> labelMap;
	unordered_map<string,int>    labelCountMap;
	list<Quad>::iterator         it;
	
	/* Set the framework label as reachable */
	labelCountMap.insert(pair<string,int>(frameworkList->begin()->opZ.label,1));
	
	
	/* First step: get the <erasable, remainig> label pairs */
	for(it = frameworkList->begin() ; it != frameworkList->end() ; it++){
		
		/* But before... */

		/* Take advantage to kill unnecessary jumps */
		if (it->opcode == Quad::BR){
			string lab = it->opZ.label;
			/* Is a jump to next instruction? */
			if (++it != frameworkList->end() && it->opcode == Quad::LABL && it->opZ.label == lab){
				/* Delete that jump */
				it = frameworkList->erase(--it);
				it--;
				continue;
			}
			it--;
		}

		/* Take advantage to kill unreachable code */
		if (isUnconditionalJump(it->opcode)){
			it++;
			while(it != frameworkList->end() && it->opcode != Quad::LABL){
				it = killOCInstr(frameworkList,it);
			}
			it--;
		}

		/* Take advantage to apply algebraic changes */
		if (it != (it = algebraicChange(frameworkList, it))){
			it--;
			continue;
		}

		/* Take advantage to apply strength reduction */
		strengthReduction(it);

		/* Take advantage to get the constant pointers */
		if (it->opcode == Quad::MVA && it->opX.opType == Quad::VAR &&
			it->opY.opType == Quad::VAR && it->opX.opSymbol.VAR->isConst() &&
			it->opX.opSymbol.VAR->getPointerDepth() > 0){

			constantPointers.insert(pair<STEntry*,STEntry*>(it->opX.opSymbol.VAR, it->opY.opSymbol.VAR));
		}

		/* Finally we can get the <erasable, remainig> label pairs */
		if(it->opcode == Quad::LABL){
			string remaining = it->opZ.label;
			/* If there are consecutive labels note the remaining label and erase the others */
			++it;
			while(it != frameworkList->end() && it->opcode == Quad::LABL){
				labelMap.insert(pair<string,string>(it->opZ.label,remaining));
				it = killOCInstr(frameworkList,it);
			}
			it--;
		}

		/* And keeping track of all referenced labels */
		if (isLabeledJump(it->opcode)){
			/* If is replaced */
			if (labelMap.find(it->opZ.label) != labelMap.end()){
				/* Replace and mark the replaced one as referenced */
				it->opZ.label = (labelMap.find(it->opZ.label))->second;
			}
			if (labelCountMap.find(it->opZ.label) == labelCountMap.end()){
				labelCountMap.insert(pair<string,int>(it->opZ.label,1));
			}
		}
	}

	/* Second step: replace the label pairs and kill the unreferenced labels */
	for(it = frameworkList->begin() ; it != frameworkList->end() ; it++){

		/* Replace the label pairs */
		if (isLabeledJump(it->opcode) || it->opcode == Quad::LABL){
			if (labelMap.find(it->opZ.label) != labelMap.end()){
				it->opZ.label = (labelMap.find(it->opZ.label))->second;
				labelCountMap.insert(pair<string,int>(it->opZ.label,1));
				changeDone = true;
			}
		}
		
		/* Kill the unreferenced labels */
		if(it->opcode == Quad::LABL){
			if (labelCountMap.find(it->opZ.label) == labelCountMap.end()){
				it = killOCInstr(frameworkList,it);
				it--;
				continue;
			}			
		}
	}
}



/**
* strengthReduction
*
* Optimization. Try to transform an algebraic operation into a simpler instruction
*
* @param it: The iterator pointing to the instruction to perform this optimization
*/
void Optimizer::strengthReduction(list<Quad>::iterator it){

	switch (it->opcode){
	
	/* ADD x x 1 => INC x x - */
	/* ADD x 1 x => INC x x - */
	case Quad::ADD:
		if(it->opY.opType == Quad::CONST && it->opY.opSymbol.CONST == 1){
			if(it->opZ.opType == Quad::VAR && it->opX.opSymbol.VAR == it->opZ.opSymbol.VAR){
				it->opcode = Quad::INC;
				it->opY = it->opZ;
				it->opZ.opType = Quad::UNDEF;
				changeDone = true;
			}
		}else if (it->opZ.opType == Quad::CONST && it->opZ.opSymbol.CONST == 1){
			if(it->opY.opType == Quad::VAR && it->opX.opSymbol.VAR == it->opY.opSymbol.VAR){
				it->opcode = Quad::INC;
				it->opZ.opType = Quad::UNDEF;
				changeDone = true;
			}
		}
		break;
		
	/* SUB x x 1 => DEC x x - */
	case Quad::SUB:
		if(it->opZ.opType == Quad::CONST && it->opZ.opSymbol.CONST == 1
		   && it->opY.opType == Quad::VAR && it->opX.opSymbol.VAR == it->opY.opSymbol.VAR){
			it->opcode = Quad::DEC;
			it->opZ.opType = Quad::UNDEF;
			changeDone = true;
		}
		break;
	}
}



/**
* algebraicChange
*
* Optimization. Try to reduce or avoid an "easy" algebraic operation
*
* @param frameworkList: The code where the instruction is located
* @param it: The iterator pointing to the instruction to perform this optimization
*
* @return the new iterator place
*/
list<Quad>::iterator Optimizer::algebraicChange(list<Quad>* frameworkList, list<Quad>::iterator it){

	switch (it->opcode){
	
	/* ADD  x y 0 => MOVE x y - */
	/* ADD  x 0 y => MOVE x y - */
	case Quad::ADD:
		if(it->opY.opType == Quad::CONST && it->opY.opSymbol.CONST == 0){
			it->opcode = Quad::MOVE;
			it->opY = it->opZ;
			it->opZ.opType = Quad::UNDEF;
			changeDone = true;
		}else if(it->opZ.opType == Quad::CONST && it->opZ.opSymbol.CONST == 0){
			it->opcode = Quad::MOVE;
			it->opZ.opType = Quad::UNDEF;
			changeDone = true;
		}
		break;
		
	/* SUB  x y 0 => MOVE x y - */
	case Quad::SUB:
		if(it->opZ.opType == Quad::CONST && it->opZ.opSymbol.CONST == 0){
			it->opcode = Quad::MOVE;
			it->opZ.opType = Quad::UNDEF;
			changeDone = true;
		}
		break;
	}
	
	/* Kill: MOVE x x - */
	if(it->opcode == Quad::MOVE){
		if(it->opX.opType == Quad::VAR && it->opY.opType == Quad::VAR){
			if (it->opX.opSymbol.VAR == it->opY.opSymbol.VAR ||
			   (it->opX.opSymbol.VAR->getAddress() == it->opY.opSymbol.VAR->getAddress() &&
			    it->opX.opSymbol.VAR->getAddress() >= 0)){
				it = killOCInstr(frameworkList,it);
			}
		}
	}

	return it;
}



/**
* localCommonSubexpressions
*
* Optimization. Look for expressions performed more than one time and reuse the value (if possible)
* instead of calculating it again
*
* @param optCode: The block where the subexpression usage is performed
*/
void Optimizer::localCommonSubexpressions(list<Quad> *optCode){

	/* Iterator                              */
	/*   From it to the end of the list      */
	/*   If opX from iterator is opX or opY  */
	/*     or opZ, break                     */
	/*   If opcode, opY and opZ are equal    */
	/*     Change opcode to MOVE             */
	/*     Change opY to Quad opX            */
	/*     Change opZ to nullOperand         */
	auto f_replace = [this](list<Quad> *oClist, list<Quad>::iterator it, Quad x){
		
		for ( ; it != oClist->end() ; it++){
			if(isExpression(it->opcode) || it->opcode == Quad::MOVE || it->opcode == Quad::STP || it->opcode == Quad::MVP || it->opcode == Quad::MVA){
				/* Stop when an operand changes */
				if(sameOperand(it->opX, x.opX) || sameOperand(it->opX, x.opY) || sameOperand(it->opX, x.opZ)){
					break;
				}

				/* Stop when a pointer may change an operand */
				if (it->opcode == Quad::STP && it->opX.opType == Quad::VAR){

					/* A constant pointer may avoid this situation */
					unordered_map<STEntry*, STEntry*>::iterator symbols =
						constantPointers.find(it->opX.opSymbol.VAR);
					if (symbols == constantPointers.end()){
						/* Not a constant pointer */
						break;
					}else{
						Quad::Operand testOperand = { Quad::VAR, "", symbols->second };
						if(sameOperand(testOperand, x.opX) || sameOperand(testOperand, x.opY) || sameOperand(testOperand ,x.opZ)){
							/* Constant pointer related to an operand */
							break;
						}
					}
				}
			}

			if(isExpression(it->opcode)){
				/* Perform substitution */
				if(x.opcode == it->opcode && sameOperand(x.opY,it->opY) && sameOperand(x.opZ,it->opZ)){

					if(it->opcode == Quad::STP){
						it->opX.opSymbol.VAR->setNUses(it->opX.opSymbol.VAR->getNUses() - 1);
					}
					
					it->opcode = Quad::MOVE;

					it->opY.opSymbol.VAR->setNUses(it->opY.opSymbol.VAR->getNUses() - 1);
					it->opY = x.opX;
					x.opX.opSymbol.VAR->setNUses(x.opX.opSymbol.VAR->getNUses() + 1);

					if (it->opZ.opType == Quad::VAR)
						it->opZ.opSymbol.VAR->setNUses(it->opZ.opSymbol.VAR->getNUses() - 1);
					it->opZ.opType         = Quad::UNDEF;
				}
			}
		}
	};


	/* For each Quad in the list */
	/*   If isExpression */
	/*     Call iterator */
	list<Quad>::iterator it;

	for (it = optCode->begin() ; it != optCode->end() ; it++){
		if(isExpression(it->opcode)){
			Quad quad = *it;
			f_replace(optCode, ++it, quad);
			it--;
		}
	}
}



/**
* shortcircuitAndUnreachableDeletion
*
* Optimization. Trigger the "jump to jump" shortcircuit optimization (see below) and delete
* code blocks which never will be reached by the program flow
*
* @param oCodeBlock: The block where the optimizations are performed
*/
void Optimizer::shortcircuitAndUnreachableDeletion(list<OCodeBlock> *oCodeBlock){
	list<OCodeBlock>::iterator it;

	auto f_killOCInstr = [this](list<Quad>* oCodeList){
		list<Quad>::iterator it = oCodeList->begin();

		while(it != oCodeList->end()){
			it = killOCInstr(oCodeList, it);
		}
	};

	for(it = oCodeBlock->begin() ; it != oCodeBlock->end() ; it++){
		jumpToJumpShortcircuit(&(*it));
	}

	it = oCodeBlock->begin();
	if (it != oCodeBlock->end())
		it++;

	for( ; it != oCodeBlock->end() ; it++){
		if(it->referencedBy.size() == 0){
			/* Unreachable block
			   Kill every instruction and erase the block */
			f_killOCInstr(&(it->oCode));
			if(it->referencingA != NULL)
				it->referencingA->referencedBy.remove(&(*it));
			if(it->referencingB != NULL)
				it->referencingB->referencedBy.remove(&(*it));
			it = oCodeBlock->erase(it);
			it--;
		}
	}
}



/**
* lifecycleCalc
*
* Auxiliar function. Fill the alive symbols in each oCode block
*
* @param oCodeBlocks: The blocks to fill with the alive symbols
*/
void Optimizer::lifecycleCalc(list<OCodeBlock>* oCodeBlocks){
	/* Pre: firstCleaning */
	/*          - Unreachable code deletion  */
	/*          - Constant pointers detected */

	list<OCodeBlock*> leaves;
	list<string>      inputArgs;
	STEntry*          symbol;
	string            currentFramework = oCodeBlocks->front().currentFramework;

	/* List leaves */
	for (list<OCodeBlock>::iterator it = oCodeBlocks->begin() ; it != oCodeBlocks->end() ; it++){
		if((*it).isLeaf)
		leaves.push_back(&(*it));
	}

	/* Add "return" to every leaf as stillAlive if return exists in current framework */
	symbol = symbolsTable.getSymbol(SEM_RETURN_SYMBOL, currentFramework);
	if (symbol != NULL){
		for_each(leaves.begin(), leaves.end(), [this, &symbol](OCodeBlock* x){
			x->addStillAlive(symbol);
		});
	}

	/* Add input arguments to first block as usedYet */
	inputArgs = symbolsTable.getInputSymbols(currentFramework);
	for_each(inputArgs.begin(), inputArgs.end(), [this, &oCodeBlocks, &currentFramework](const string &x){
		oCodeBlocks->front().addUsedYet(symbolsTable.getSymbol(x,currentFramework));
	});

	/* Add const & pointer args as stillAlive to every leaves */
	for_each(inputArgs.begin(), inputArgs.end(), [this, &leaves, &currentFramework](const string &x){
		STEntry* symbol = symbolsTable.getSymbol(x,currentFramework);
		if(symbol != NULL && (symbol->isConst() || symbol->getPointerDepth() > 0)){
			for_each(leaves.begin(), leaves.end(), [&symbol](OCodeBlock* y){
				y->addStillAlive(symbol);
			});
		}
	});

	/* Run blocks and add every used symbol as usedYet & stillAlive */
	for_each(oCodeBlocks->begin(), oCodeBlocks->end(), [this, &oCodeBlocks, &currentFramework](OCodeBlock &x){

		/* A non constant pointer could set 'alive' every symbol in this block! */
		bool   pointerPanic = false;
		string currentFCopy = currentFramework;

		auto f_pointerPanic = [&x](STEntry* s){
			x.addStillAlive(s);
			x.addUsedYet(s);
		};


		for_each(x.oCode.begin(), x.oCode.end(), [&x, &currentFCopy, &pointerPanic, f_pointerPanic](Quad quad){

			/* Don't spent more time... nothing to do here */
			if(pointerPanic)
				return;

			if(quad.opX.opType == Quad::VAR)
				x.addUsedYet(quad.opX.opSymbol.VAR);
			if(quad.opY.opType == Quad::VAR)
				x.addUsedYet(quad.opY.opSymbol.VAR);
			if(quad.opZ.opType == Quad::VAR)
				x.addUsedYet(quad.opZ.opSymbol.VAR);

			if((quad.opcode >= Quad::ADD && quad.opcode <= Quad::NEG) ||
			   (quad.opcode >= Quad::EQ  && quad.opcode <= Quad::MOVE)){

				if(quad.opY.opType == Quad::VAR)
					x.addStillAlive(quad.opY.opSymbol.VAR);
				if(quad.opZ.opType == Quad::VAR)
					x.addStillAlive(quad.opZ.opSymbol.VAR);

			}else if(quad.opcode >= Quad::BEQ && quad.opcode <= Quad::BLS){
				
				if(quad.opX.opType == Quad::VAR)
					x.addStillAlive(quad.opX.opSymbol.VAR);
				if(quad.opY.opType == Quad::VAR)
					x.addStillAlive(quad.opY.opSymbol.VAR);

			}else if(quad.opcode == Quad::MVP){
				/* MVP  xy- *//* x := *y */
				if(quad.opY.opType == Quad::VAR){
					x.addStillAlive(quad.opY.opSymbol.VAR);

					/* If *y is not a constant, may be pointing to anywhere and that   */
					/* means, every variable is still alive and used yet in this block */
					/* FUTURE <pointedAddressDetection> */
					if(!quad.opY.opSymbol.VAR->isConst()){
						list<STEntry*> everyFrameworkSymbol =
							symbolsTable.getFramework(currentFCopy)->getSymbols();

						for_each(everyFrameworkSymbol.begin(), everyFrameworkSymbol.end(), f_pointerPanic);
						pointerPanic = true;
					}
				}
			}else if(quad.opcode == Quad::STP){
				/* STP  xy- *//* *x := y */
				if(quad.opY.opType == Quad::VAR)
					x.addStillAlive(quad.opY.opSymbol.VAR);

				/* If *x is not a constant, may be pointing to anywhere and that   */
				/* means, every variable is still alive and used yet in this block */
				/* FUTURE <pointedAddressDetection> */
				if(quad.opX.opType == Quad::VAR && !quad.opX.opSymbol.VAR->isConst()){
						list<STEntry*> everyFrameworkSymbol =
							symbolsTable.getFramework(currentFCopy)->getSymbols();

					for_each(everyFrameworkSymbol.begin(), everyFrameworkSymbol.end(), f_pointerPanic);
					pointerPanic = true;
				}
			}else if(quad.opcode == Quad::MVA){
				/* MVA  xy- *//* x := &y */
				if(quad.opY.opType == Quad::VAR)
					x.addStillAlive(quad.opY.opSymbol.VAR);
			}
		});
	});


	OCodeBlock* refBlock;
	auto f_addToStillAlive = [this, &refBlock](STEntry* symbol){
		if(symbol->getPointerDepth() > 0){
			unordered_map<STEntry*,STEntry*>::iterator it = constantPointers.find(symbol);
			if(it != constantPointers.end()){
				if (!refBlock->exists(it->second, &(refBlock->stillAlive))){
					refBlock->stillAlive.push_back(it->second);
				}
			}
		}
	};
	auto f_addToUsedYet = [this, &refBlock](STEntry* symbol){
		if(symbol->getPointerDepth() > 0){
			unordered_map<STEntry*,STEntry*>::iterator it = constantPointers.find(symbol);
			if(it != constantPointers.end()){
				if (!refBlock->exists(it->second, &(refBlock->usedYet))){
					refBlock->usedYet.push_back(it->second);
				}
			}
		}
	};


	/* Get aliveSymbols as intersection of usedYet and stillAlive */
	for_each(oCodeBlocks->begin(), oCodeBlocks->end(), [this, &refBlock, f_addToStillAlive, f_addToUsedYet](OCodeBlock &x){
		list<STEntry*>::iterator it;
		refBlock = &x;

		for_each(x.stillAlive.begin(), x.stillAlive.end(), f_addToStillAlive);
		for_each(x.usedYet.begin(),    x.usedYet.end(),    f_addToUsedYet);

		x.stillAlive.sort();
		x.usedYet.sort();

		x.aliveSymbols.resize(min(x.stillAlive.size(), x.usedYet.size()));

		it = set_intersection(x.stillAlive.begin(), x.stillAlive.end(),
			x.usedYet.begin(), x.usedYet.end(),
			x.aliveSymbols.begin());

		x.aliveSymbols.erase(it, x.aliveSymbols.end());
	});
}



/**
* jumpToJumpShortcircuit
*
* Optimization. Looks for branches to branches and try to bypass both jumps into once
*
* @param oCodeBlock: The block where the shortcircuit is performed
*/
void Optimizer::jumpToJumpShortcircuit(OCodeBlock* oCodeBlock){
	/* Pre firstIteration */
	/* Pre deadCodeDeletion */

	Quad*       jump1 = NULL;
	Quad*       jump2 = NULL;
	OCodeBlock* secondBlock;

	auto f_destinationIsRightJump = [this, &jump2](list<Quad> *qList, string label){
		if (qList == NULL)
			return;
		if (qList->size() != 2)
			return;
		if (qList->front().opcode != Quad::LABL)
			return;
		if (qList->front().opZ.label != label)
			return;
		if (!isJump(qList->back().opcode))
			return;

		jump2 = &(qList->back());
	};


	jump1 = &(oCodeBlock->oCode.back());

	if (isJump(jump1->opcode) && jump1->opcode != Quad::RET && jump1->opcode != Quad::CALL){
		f_destinationIsRightJump(&(oCodeBlock->referencingA->oCode), jump1->opZ.label);

		if(oCodeBlock->referencingB != NULL && jump2 == NULL){
			f_destinationIsRightJump(&(oCodeBlock->referencingB->oCode), jump1->opZ.label);
			secondBlock = oCodeBlock->referencingB;
		}else{
			secondBlock = oCodeBlock->referencingA;
		}
		
		if(jump2 == NULL)
			return;

		if(isUnconditionalJump(jump2->opcode)){
			if(isUnconditionalJump(jump1->opcode)){
				/* Jump to jump */
				*jump1 = *jump2;

				/* Put in the block the references from jump2 */
				secondBlock->referencedBy.remove(oCodeBlock);

				oCodeBlock->referencingA = secondBlock->referencingA;
				oCodeBlock->referencingB = secondBlock->referencingB;

				if(oCodeBlock->referencingA != NULL)
					oCodeBlock->referencingA->referencedBy.push_back(oCodeBlock);
				if(oCodeBlock->referencingB != NULL)
					oCodeBlock->referencingB->referencedBy.push_back(oCodeBlock);

			}else{
				if (jump2->opcode == Quad::RET || jump2->opcode == Quad::CALL)
					return;
				/* Conditional jump to jump */

				/* Put in the block the references from jump2 */
				if(oCodeBlock->referencingA->oCode.front().opcode == Quad::LABL &&
				   oCodeBlock->referencingA->oCode.front().opZ.label == jump1->opZ.label){

					oCodeBlock->referencingA->referencedBy.remove(oCodeBlock);
					oCodeBlock->referencingA = (secondBlock->referencingA != NULL) ? secondBlock->referencingA : secondBlock->referencingB;
					oCodeBlock->referencingA->referencedBy.push_back(oCodeBlock);
				}else{
					oCodeBlock->referencingB->referencedBy.remove(oCodeBlock);
					oCodeBlock->referencingB = (secondBlock->referencingA != NULL) ? secondBlock->referencingA : secondBlock->referencingB;
					oCodeBlock->referencingB->referencedBy.push_back(oCodeBlock);
				}

				jump1->opZ.label = jump2->opZ.label;
			}
		}else{
			/* Destination is conditional jump */
			/* No optimization is possible */
			return;
		}

		/* Add oCodeBlock as leaf if block is a new leaf */
		if (jump1->opcode == Quad::RET)
			oCodeBlock->isLeaf = true;
	}

}



/**
* deadCodeDeletion
*
* Optimization. Looks for instructions that store data into unused symbols and delete them
*
* @param oCodeBlock: The block where the deletion is performed
*/
void Optimizer::deadCodeDeletion(OCodeBlock* oCodeBlock){

	list<Quad>::iterator it;

	/*
	auto f_isAlive = [this, &oCodeBlock](const Quad::Operand &x)->bool{

		for (list<STEntry*>::iterator it = oCodeBlock->aliveSymbols.begin() ; it != oCodeBlock->aliveSymbols.end() ; it++ ){
			if ((*it) == (x.opSymbol.VAR))
				return true;
		}

		return false;
	};
	*/

	for (it = oCodeBlock->oCode.begin() ; it != oCodeBlock->oCode.end() ; it++){

		if (it->opX.opType == Quad::VAR &&
			/* !it->opX.opSymbol.VAR->isVolatile() && */
			/* it->opX.opSymbol.VAR->getPointerDepth() == 0 && */
			it->opX.opSymbol.VAR->getNUses() == 0){
				/* Is dead */
				it = killOCInstr(&(oCodeBlock->oCode), it);
				if (it != oCodeBlock->oCode.begin()){
					it--;
					continue;
				}
		}

		/*
		if((it->opcode >= Quad::ADD && it->opcode <= Quad::NEG) || (it->opcode >= Quad::EQ  && it->opcode <= Quad::MOVE) || it->opcode == Quad::MVA || it->opcode == Quad::MVP){
			if(it->opX.opType == Quad::VAR && !(it->opX.opSymbol.VAR->isVolatile()) && it->opX.opSymbol.VAR->getFramework() == oCodeBlock->currentFramework){
				if(!f_isAlive(it->opX)){
					// Is dead
						it = killOCInstr(&(oCodeBlock->oCode), it);
						if (it != oCodeBlock->oCode.begin()){
						it--;
					}
				}
			}
		}
		*/
	}
}



/**
* movementInstructionsShortcut
*
* Optimization. Look for a [MOVE temp varX -] + [MOVE varY temp -] to shortcut into [MOVE varY varX -]
*
* @param quad: The first quad to perform the check
* @param nextQuad: The second quad to perform the check
*
* @return true if the shortcut can be done and the symbols changes has been performed
*/
bool Optimizer::movementInstructionsShortcut(const Quad &quad, Quad *nextQuad){
    /* PRE: Any tempVarRenaming should go AFTER this one */

    /*
    if (x1 == y2 && x1->getNUses() == 1 && !x1->isVolatile()){
        MOVE + MOVE = MOVE
		MOVE + STP  = STP
        MOVE + MVP  = MVP (If Y1 is VAR)
        MVP + MOVE  = MVP
        MVA + MOVE  = MVA
    }
    */

    switch(quad.opcode){
  
    case Quad::MOVE:
        if ((nextQuad->opcode == Quad::STP || nextQuad->opcode == Quad::MOVE ||
            (nextQuad->opcode == Quad::MVP && quad.opY.opType == Quad::VAR))&&
            nextQuad->opY.opType == Quad::VAR &&
            nextQuad->opY.opSymbol.VAR == quad.opX.opSymbol.VAR &&
            quad.opX.opSymbol.VAR->getNUses() == 1){
      
            quad.opY.opSymbol.VAR->setNUses(quad.opY.opSymbol.VAR->getNUses() + 1); /* Fixme día -1. Antes era quad.opX */
            nextQuad->opY.opSymbol.VAR->setNUses(nextQuad->opY.opSymbol.VAR->getNUses() - 1);
           
            nextQuad->opY.opSymbol.VAR = quad.opY.opSymbol.VAR;
          
            return true;
        }
        break;
      
    case Quad::MVP:
    case Quad::MVA:
        if (nextQuad->opcode == Quad::MOVE &&
            nextQuad->opY.opType == Quad::VAR &&
            nextQuad->opY.opSymbol.VAR == quad.opX.opSymbol.VAR &&
			quad.opY.opType == Quad::VAR &&
            quad.opX.opSymbol.VAR->getNUses() == 1){

            quad.opY.opSymbol.VAR->setNUses(quad.opY.opSymbol.VAR->getNUses() + 1);
            nextQuad->opY.opSymbol.VAR->setNUses(nextQuad->opY.opSymbol.VAR->getNUses() - 1);
           
            nextQuad->opcode = quad.opcode;
            nextQuad->opY.opSymbol.VAR = quad.opY.opSymbol.VAR;
          
            return true;
        }
        break;  
  
    default:
        return false;
    };
  
    return false;
}



/**
* tempVarRenaming
*
* Optimization. Look for the temporal symbols in a block and try to rename them into another temporal symbol
* unused in this block
*
* @param oCodeBlock: The block where the renaming is performed
*/
void Optimizer::tempVarRenaming(OCodeBlock* oCBlock){
    unordered_set<STEntry*>      forbidTemps = unordered_set<STEntry*>();
    unordered_map<STEntry*, int> firstUsage  = unordered_map<STEntry*, int>();
    unordered_map<STEntry*, int> lastUsage   = unordered_map<STEntry*, int>();
    list<STEntry*>               sortedTemps = list<STEntry*>();

    int i = 0;


    auto isTemp = [](STEntry* symbol)->bool{
        string lex = symbol->lex;
        return (OC_TEMP_PREFIX == lex.substr(0, ((string)OC_TEMP_PREFIX).size()));
    };


    auto isStore = [](Quad::Opcode opcode)->bool{
        return (opcode == Quad::MOVE  || opcode == Quad::MVA || opcode == Quad::MVP);
    };


    auto f_checkUsage = [this, isStore, isTemp, &i, &forbidTemps, &firstUsage, &lastUsage, &sortedTemps](Quad instr){
        i++;
       
        if (instr.opX.opType == Quad::VAR &&
            isTemp(instr.opX.opSymbol.VAR) &&
            forbidTemps.count(instr.opX.opSymbol.VAR) == 0){
           
            if(isExpression(instr.opcode) || isStore(instr.opcode)){
                if(firstUsage.count(instr.opX.opSymbol.VAR) == 0){

                    firstUsage.insert(pair<STEntry*, int>(instr.opX.opSymbol.VAR, i));
                    lastUsage.insert(pair<STEntry*, int>(instr.opX.opSymbol.VAR, i));
					sortedTemps.push_back(instr.opX.opSymbol.VAR);
                }else{
                    lastUsage.at(instr.opX.opSymbol.VAR) = i;
                }
            }else{
                if(firstUsage.count(instr.opX.opSymbol.VAR) == 0){
                    forbidTemps.insert(instr.opX.opSymbol.VAR);
                }else{
                    lastUsage.at(instr.opX.opSymbol.VAR) = i;
                }
            }
        }

        if (instr.opY.opType == Quad::VAR &&
            isTemp(instr.opY.opSymbol.VAR) &&
            forbidTemps.count(instr.opY.opSymbol.VAR) == 0){

            if(firstUsage.count(instr.opY.opSymbol.VAR) == 0){
                forbidTemps.insert(instr.opY.opSymbol.VAR);
            }else{
                lastUsage.at(instr.opY.opSymbol.VAR) = i;
            }   
        }
       
        if (instr.opZ.opType == Quad::VAR &&
            isTemp(instr.opZ.opSymbol.VAR) &&
            forbidTemps.count(instr.opZ.opSymbol.VAR) == 0){

            if(firstUsage.count(instr.opZ.opSymbol.VAR) == 0){
                forbidTemps.insert(instr.opZ.opSymbol.VAR);
            }else{
                lastUsage.at(instr.opZ.opSymbol.VAR) = i;
            }   
        }
    };


    /* Every symbol alive in the children blocks is forbidden */
    if (oCBlock->referencingA != NULL && oCBlock->referencingA != oCBlock){
		for_each(oCBlock->referencingA->stillAlive.begin(),
                 oCBlock->referencingA->stillAlive.end(),
                 [&forbidTemps](STEntry* x){
            forbidTemps.insert(x);
        });
    }       
    if (oCBlock->referencingB != NULL && oCBlock->referencingB != oCBlock){
        for_each(oCBlock->referencingB->stillAlive.begin(),
			     oCBlock->referencingB->stillAlive.end(),
                 [&forbidTemps](STEntry* x){
            forbidTemps.insert(x);
        });
    }

   
    /*
    FUTURE <improvement>
    Add every parent temp symbol and not alive in this block neither in the children blocks as:
        - firstUsage = -2
        - lastUsage = -1
        - ussable as new symbol (never as old symbol) in the "substituteSymbol" function
    */

	i = i;
   
    /* Get every usable temp value to start renamings */
    for (list<Quad>::iterator it = oCBlock->oCode.begin() ; it != oCBlock->oCode.end() ; ){
        list<Quad>::iterator it2;
       
        it2 = it;

		if (it2 != oCBlock->oCode.end()){
			it2++;
		}else{
			break;
		}

        /* Take advantage to shortcircuit move instructions if possible */
        if (it2 != oCBlock->oCode.end() &&
            movementInstructionsShortcut(*it, &(*it2))){
           
            it = killOCInstr(&(oCBlock->oCode), it);
            continue;
        }
       
        f_checkUsage(*it);
        it++;
    }
   
   
    /* For each symbol */
    for (list<STEntry*>::iterator it = sortedTemps.begin() ; it != sortedTemps.end() ; it++){
        list<STEntry*>::iterator it2        = it;

        if(it2 == sortedTemps.end())
            break;
        it2++;
   
        /* Try to match with a future temp */
        it2 = find_if(it2, sortedTemps.end(), [&it, &firstUsage, &lastUsage](STEntry* x)->bool{
            return x->getType().equals((*it)->getType()) &&
                   (*it)->getNUses() > 0 &&
                   lastUsage.at(*it) <= firstUsage.at(x);
        });
       
        if(it2 == sortedTemps.end())
            continue;

        /* If a match is possible, substitute */
        substituteSymbol(&(oCBlock->oCode), *it2, *it);
    }
   
    /* And kill every dead symbol from alive symbols and from the symbols table */
    for_each(sortedTemps.begin(), sortedTemps.end(), [this, &oCBlock](STEntry* x){
        if(x->getNUses() < 1 && !(x->isVolatile())){
            oCBlock->aliveSymbols.remove(x);
        }
    });   
}



/**
* copyPropagation
*
* Optimization. FUTURE <CopyPropagation>
*
* @param optCode: oCode to optimize
*/
void Optimizer::copyPropagation(list <Quad> *optCode){
	optCode = optCode;
	/* FUTURE <CopyPropagation> */
}



/**
* substituteSymbol
*
* Optimization. Auxiliar function. Iterates through a quad list and change every ocurrence of a given symbol
* to another given symbol
*
* @param cList: The list where the changes are done
* @param newSymbol: The symbol to be changed for
* @param oldSymbol: The symbol to find
*/
void Optimizer::substituteSymbol(list<Quad>* cList, STEntry* newSymbol, STEntry* oldSymbol){

    list<Quad>::iterator it;

    for(it = cList->begin() ; it != cList->end() ; it++){
        if(((*it).opcode >= Quad::ADD && (*it).opcode <= Quad::NEG) ||
           ((*it).opcode >= Quad::EQ  && (*it).opcode <= Quad::MOVE)){
            /*
            ADD  xyz,  SUB  xyz,  MUL  xyz,  DIV  xyz,  MOD  xyz,  INC  xy-,  DEC  xy-,
            LES  xyz,  RIS  xyz,
            NEG  xy-, 
            */
            /*
            EQ   xyz,  NE   xyz,  GR   xyz,  LS   xyz,  LAND xyz,  LOR  xyz,
            AND  xyz,  OR   xyz,  XOR  xyz,  NOT  xy-,
            MOVE xy-,
            */
            if((*it).opX.opType == Quad::VAR && (*it).opX.opSymbol.VAR == oldSymbol){
                (*it).opX.opSymbol.VAR = newSymbol;
                changeDone = true;
            }

            if((*it).opY.opType == Quad::VAR && (*it).opY.opSymbol.VAR == oldSymbol){
                oldSymbol->setNUses(oldSymbol->getNUses() - 1);
                newSymbol->setNUses(newSymbol->getNUses() + 1);
                (*it).opY.opSymbol.VAR = newSymbol;
                changeDone = true;
            }

            if((*it).opZ.opType == Quad::VAR && (*it).opZ.opSymbol.VAR == oldSymbol){
                oldSymbol->setNUses(oldSymbol->getNUses() - 1);
                newSymbol->setNUses(newSymbol->getNUses() + 1);
                (*it).opZ.opSymbol.VAR = newSymbol;
                changeDone = true;
            }

        }else if((*it).opcode >= Quad::BEQ && (*it).opcode <= Quad::BLS){
            /* BEQ  xyL,  BNE  xyL,  BGR  xyL,  BLS  xyL, */
            if((*it).opX.opType == Quad::VAR && (*it).opX.opSymbol.VAR == oldSymbol){
                oldSymbol->setNUses(oldSymbol->getNUses() - 1);
                newSymbol->setNUses(newSymbol->getNUses() + 1);
                (*it).opX.opSymbol.VAR = newSymbol;
                changeDone = true;
            }
            if((*it).opY.opType == Quad::VAR && (*it).opY.opSymbol.VAR == oldSymbol){
                oldSymbol->setNUses(oldSymbol->getNUses() - 1);
                newSymbol->setNUses(newSymbol->getNUses() + 1);
                (*it).opY.opSymbol.VAR = newSymbol;
                changeDone = true;
            }

        }else if((*it).opcode == Quad::MVP){
            /* MVP  xy- */
            /* x := *y */
            if((*it).opX.opType == Quad::VAR && (*it).opX.opSymbol.VAR == oldSymbol){
                (*it).opX.opSymbol.VAR = newSymbol;
                changeDone = true;
            }

            if((*it).opY.opType == Quad::VAR && (*it).opY.opSymbol.VAR == oldSymbol){
                oldSymbol->setNUses(oldSymbol->getNUses() - 1);
                newSymbol->setNUses(newSymbol->getNUses() + 1);
                (*it).opY.opSymbol.VAR = newSymbol;
                changeDone = true;
            }

        }else if((*it).opcode == Quad::STP){
            /* STP  xy- */
            /* *x := y */
            if((*it).opX.opType == Quad::VAR && (*it).opX.opSymbol.VAR == oldSymbol){
                oldSymbol->setNUses(oldSymbol->getNUses() - 1);
                newSymbol->setNUses(newSymbol->getNUses() + 1);
                (*it).opX.opSymbol.VAR = newSymbol;
                changeDone = true;
            }

            if((*it).opY.opType == Quad::VAR && (*it).opY.opSymbol.VAR == oldSymbol){
                oldSymbol->setNUses(oldSymbol->getNUses() - 1);
                newSymbol->setNUses(newSymbol->getNUses() + 1);
                (*it).opY.opSymbol.VAR = newSymbol;
                changeDone = true;
            }
        }else if((*it).opcode == Quad::MVA){
            /* MVA  xy- */
            /* x := &y */
            if((*it).opX.opType == Quad::VAR && (*it).opX.opSymbol.VAR == oldSymbol){
                (*it).opX.opSymbol.VAR = newSymbol;
                changeDone = true;
            }

            if((*it).opY.opType == Quad::VAR && (*it).opY.opSymbol.VAR == oldSymbol){
                oldSymbol->setNUses(oldSymbol->getNUses() - 1);
                newSymbol->setNUses(newSymbol->getNUses() + 1);
                (*it).opY.opSymbol.VAR = newSymbol;
                changeDone = true;
            }
        }
    }
}



/**
* killOCInstr
*
* Deletes a quad from a list and update the usage of the involved symbols
*
* @param cList: The list where the quad is going to be deleted
* @param it: The iterator pointing the quad to delete
*
* @return the iterator after the deletion
*/
list<Quad>::iterator Optimizer::killOCInstr(list<Quad>* cList, list<Quad>::iterator it){
   
    Quad quad = *it;
   
    /* Decrement var usage */
    if((quad.opcode >= Quad::ADD && quad.opcode <= Quad::NEG) ||
       (quad.opcode >= Quad::EQ  && quad.opcode <= Quad::MOVE)){
        /*
        ADD  xyz,  SUB  xyz,  MUL  xyz,  DIV  xyz,  MOD  xyz,  INC  xy-,  DEC  xy-,
        LES  xyz,  RIS  xyz,
        NEG  xy-, 
        */
        /*
        EQ   xyz,  NE   xyz,  GR   xyz,  LS   xyz,  LAND xyz,  LOR  xyz,
        AND  xyz,  OR   xyz,  XOR  xyz,  NOT  xy-,
        MOVE xy-,
        */
        if(quad.opY.opType == Quad::VAR)
            quad.opY.opSymbol.VAR->setNUses(quad.opY.opSymbol.VAR->getNUses() - 1);

        if(quad.opZ.opType == Quad::VAR)
            quad.opZ.opSymbol.VAR->setNUses(quad.opZ.opSymbol.VAR->getNUses() - 1);

    }else if(quad.opcode >= Quad::BEQ && quad.opcode <= Quad::BLS){
        /* BEQ  xyL,  BNE  xyL,  BGR  xyL,  BLS  xyL, */
        if(quad.opX.opType == Quad::VAR)
            quad.opX.opSymbol.VAR->setNUses(quad.opX.opSymbol.VAR->getNUses() - 1);

        if(quad.opY.opType == Quad::VAR)
            quad.opY.opSymbol.VAR->setNUses(quad.opY.opSymbol.VAR->getNUses() - 1);

    }else if(quad.opcode == Quad::MVP){
        /* MVP  xy- */
        /* x := *y */
        if(quad.opY.opType == Quad::VAR)
            quad.opY.opSymbol.VAR->setNUses(quad.opY.opSymbol.VAR->getNUses() - 1);

    }else if(quad.opcode == Quad::STP){
        /* STP  xy- */
        /* *x := y */
        if(quad.opX.opType == Quad::VAR)
            quad.opX.opSymbol.VAR->setNUses(quad.opX.opSymbol.VAR->getNUses() - 1);

        if(quad.opY.opType == Quad::VAR)
            quad.opY.opSymbol.VAR->setNUses(quad.opY.opSymbol.VAR->getNUses() - 1);
           
    }else if(quad.opcode == Quad::MVA){
        /* MVA  xy- */
        /* x := &y */
        if(quad.opY.opType == Quad::VAR)
            quad.opY.opSymbol.VAR->setNUses(quad.opY.opSymbol.VAR->getNUses() - 1);
    }

    /* Kill quad */
    it = cList->erase(it);
    changeDone = true;

    return it;
}



/**
* isJump
*
* Auxiliar function. Check if an opcode is a jump instruction
*
* @param opcode: The opcode to check
*
* @return True if the opcode is a jump
*/
bool Optimizer::isJump(Quad::Opcode opcode){
	return (opcode == Quad::BR  || opcode == Quad::BEQ || opcode == Quad::BNE
	     || opcode == Quad::BGR || opcode == Quad::BLS || opcode == Quad::RET
		 || opcode == Quad::CALL);
}



/**
* isLabeledJump
*
* Auxiliar function. Check if an opcode is a labeled jump instruction
*
* @param opcode: The opcode to check
*
* @return True if the opcode is a labeed jump
*/
bool Optimizer::isLabeledJump(Quad::Opcode opcode){
	return (opcode == Quad::BR  || opcode == Quad::BEQ || opcode == Quad::BNE
	     || opcode == Quad::BGR || opcode == Quad::BLS);
}



/**
* isUnconditionalJump
*
* Auxiliar function. Check if an opcode is an unconditional jump instruction
*
* @param opcode: The opcode to check
*
* @return True if the opcode is an unconditional jump
*/
bool Optimizer::isUnconditionalJump(Quad::Opcode opcode){
	return (opcode == Quad::BR || opcode == Quad::RET);
}



/**
* isExpression
*
* Auxiliar function. Check if an opcode is a expression instruction
*
* @param opcode: The opcode to check
*
* @return True if the opcode is an expression
*/
bool Optimizer::isExpression(Quad::Opcode opcode){
	/* LES  xyz,  RIS  xyz */
	/* NEG  xy-,  EQ   xyz,  NE   xyz, GR   xyz,  LS   xyz,  LAND xyz,  LOR  xyz */
	/* AND  xyz,  OR   xyz,  XOR  xyz,  NOT  xy- */
	/* ADD  xyz,  SUB  xyz,  MUL  xyz,  DIV  xyz,  MOD  xyz */

	return (opcode == Quad::LES  || opcode == Quad::RIS  || opcode == Quad::NEG 
		 || opcode == Quad::EQ   || opcode == Quad::NE   || opcode == Quad::GR
		 || opcode == Quad::LS   || opcode == Quad::LAND || opcode == Quad::LOR
		 || opcode == Quad::AND  || opcode == Quad::OR   || opcode == Quad::XOR
		 || opcode == Quad::NOT  || opcode == Quad::ADD  || opcode == Quad::SUB
		 || opcode == Quad::MUL  || opcode == Quad::DIV  || opcode == Quad::MOD);
}



/**
* sameOperand
*
* Auxiliar function. Perform a comparison between two operands
*
* @param op1: The first operand to compare
* @param op2: The second operand to compare
*
* @return true if both operands are identical
*/
bool Optimizer::sameOperand(Quad::Operand op1, Quad::Operand op2){
	return (op1.opType == op2.opType && (void*)(op1.opSymbol.VAR) == (void*)(op2.opSymbol.VAR));
}



/**
* slice
*
* Takes quad code (a list of quad instructions) divides it into non-branched blocks
*
* @param oCode: The list of quads instructions to slice
*
* @return the list of blocks sliced by the function
*/
list<OCodeBlock> Optimizer::slice(list<Quad> oCode){
	list<OCodeBlock>     blocks   = list<OCodeBlock>();
	OCodeBlock           newBlock;

	list<Quad>::iterator it       = oCode.begin();

	unordered_map<string, OCodeBlock*>      blockLabels       = unordered_map<string, OCodeBlock*>();
	unordered_multimap<OCodeBlock*, string> referencesToBuild = unordered_multimap<OCodeBlock*, string>();
	OCodeBlock*                             referencedByPrev  = NULL;

	string                                  currentFramework = oCode.front().opZ.label;
	currentFramework = currentFramework.substr(((string)SEM_FUNCTION_PREFIX).size(), currentFramework.size()-(((string)SEM_FUNCTION_PREFIX).size()));

	/* Push first oCode inst */
	newBlock = OCodeBlock();
	newBlock.oCode.push_back(*it);

	/* Create code blocks */
	for (it++ ; it != oCode.end() ; it++){

		/* Add each quad until reach a jump or a jump destination (label) */
		while(it != oCode.end() && it->opcode != Quad::LABL){
			newBlock.oCode.push_back(*it);
			if (isJump(it->opcode))
				break;
			it++;
		}

		if (newBlock.oCode.size() != 0){
			newBlock.currentFramework = currentFramework;
			blocks.push_back(newBlock);

			/* If this block is referenced by the previous one, build reference */
			if(referencedByPrev){
				blocks.back().referencedBy.push_back(referencedByPrev);

				list<OCodeBlock>::iterator auxIt = blocks.end();
				OCodeBlock*                referencedBlock;
				
				auxIt--;
				referencedBlock = &(*auxIt);

				auxIt--;
				auxIt->addReferencing(referencedBlock);
			}

			if (blocks.back().oCode.front().opcode == Quad::LABL){
				/* Each block starting by a label is added to pairs blockLabels<string, OCodeBlock*> */
				blockLabels.insert(pair<string, OCodeBlock*>(blocks.back().oCode.front().opZ.label,&(blocks.back())));
			}

			/* Every block finishing in RET or HALT is a leaf */
			// Quad::Opcode aux = blocks.back().oCode.back().opcode;
			if (blocks.back().oCode.back().opcode == Quad::RET ||
				blocks.back().oCode.back().opcode == Quad::HALT)
				blocks.back().isLeaf = true;

			newBlock = OCodeBlock();

			if(it == oCode.end())
				break;

			/*
			Each reference remaining is noted into pairs referencesToBuild<OCodeBlock*, string>

			If a block finish in BR-						One reference, BR addressing
			If a block finish in conditional jump or CALL-	Two references, addresing and next block
			Else-											One reference, next block
			*/
			if (blocks.back().oCode.back().opcode == Quad::BR){
				referencesToBuild.insert(pair<OCodeBlock*, string>(&(blocks.back()),blocks.back().oCode.back().opZ.label));
				referencedByPrev = NULL;
			}else if(isLabeledJump(blocks.back().oCode.back().opcode) /* FUTURE */ /*|| blocks.back().oCode.back().opcode == Quad::CALL */){
				referencesToBuild.insert(pair<OCodeBlock*, string>(&(blocks.back()),blocks.back().oCode.back().opZ.label));
				referencedByPrev = &(blocks.back());
			}else{
				referencedByPrev = &(blocks.back());
			}
		}else{
			newBlock = OCodeBlock();

			if(it == oCode.end())
				break;
		}

		if(it->opcode == Quad::LABL)
			newBlock.oCode.push_back(*it);
	}


	/* Solve every reference */
	string      referencedLabel;
	OCodeBlock* referencingBlock;
	OCodeBlock* referencedBlock;

	for(unordered_multimap<OCodeBlock*, string>::iterator fIt = referencesToBuild.begin() ;
		fIt != referencesToBuild.end() ; fIt++){

		referencedLabel  = fIt->second;
		referencingBlock = fIt->first;

		referencedBlock  = blockLabels.find(referencedLabel)->second;

		referencingBlock->addReferencing(referencedBlock);
		referencedBlock->referencedBy.push_back(referencingBlock);
	}

	return blocks;
}



/**
* join
*
* Takes a list of sliced quads blocks and join them into a quads list
*
* @param blockList: The blocks of quads to join
*
* @return the complete quads code (a list of quads instructions)
*/
list<Quad> Optimizer::join(list<OCodeBlock> blocksList){
	list<Quad> oCodeList = list<Quad>();

	auto f_concatQuad = [&oCodeList](Quad x){
		oCodeList.push_back(x);
	};

	auto f_concatQuadList = [f_concatQuad](OCodeBlock x){
		for_each(x.oCode.begin(), x.oCode.end(), f_concatQuad);
	};

	for_each(blocksList.begin(), blocksList.end(), f_concatQuadList);

	return oCodeList;
}



/*
* ------------------------------------------ 
*
* Final code optimizations
*
* ------------------------------------------
*/



/**
* optimize
*
* Takes asm code (a list of asm instructions) and perform some code optimization tasks over it
*
* @param fCode: The list of asm instructions to optimizations to be performed in
*/
void Optimizer::optimize(list<ASM> *fCode){
	list<FCodeBlock> blocks = slice(*fCode);
	bool anotherIteration = true;
	int  iterationN       = 0;

	while(anotherIteration && iterationN < MAX_OPT_ITERATIONS){
		anotherIteration = false;
		iterationN++;

		for_each(blocks.begin(), blocks.end(), [&anotherIteration](FCodeBlock &x){
			anotherIteration |= x.descriptorCalc();
		});
	}


	/*
	auto f_killUnusedSymbols = [](STEntry* symbol){
		string lex = string(symbol->lex);
		lex = lex.substr(0,2);
		if(symbol->getNUses() == 0 && lex == "t_")
			symbolsTable.delSymbol(symbol->lex, symbol->getFramework());
	};

	auto f_cleanFrameworkSymbols = [f_killUnusedSymbols](STFramework* framework){
		list<STEntry*> symbols = framework->getSymbols();
		for_each(symbols.begin(), symbols.end(), f_killUnusedSymbols);
	};

	list<STFramework*> frameworks = symbolsTable.getFrameworks();
	for_each(frameworks.begin(), frameworks.end(), f_cleanFrameworkSymbols);
	*/

	*fCode = join(blocks);
}



/**
* slice
*
* Takes asm code (a list of asm instructions) divides it into non-branched blocks
*
* @param fCode: The list of asm instructions to slice
*
* @return the list of blocks sliced by the function
*/
list<FCodeBlock> Optimizer::slice(list<ASM> fCode){
	list<FCodeBlock>                   blocks            = list<FCodeBlock>();
	FCodeBlock                         newBlock;

	list<ASM>::iterator                it                = fCode.begin();

	unordered_map<string, FCodeBlock*> blockLabels       = unordered_map<string, FCodeBlock*>();
	unordered_multimap<string, string> referencesToBuild = unordered_multimap<string, string>();
	string                             referencedByPrev  = "";
	int                                auxIDGenerator    = 0;
	string                             currentId         = "";
	string                             currentFramework  = "";
	ostringstream                      ostr;

	string                             referencedLabel;
	FCodeBlock*                        referencingBlock;

	ostr.fill('0');


	auto isJumpL = [](ASM::Opcode x)->bool{
		if (x == ASM::BCC || x == ASM::BCS || x == ASM::BEQ || x == ASM::BMI ||
			x == ASM::BNE || x == ASM::BPL || x == ASM::BVC || x == ASM::BVS ||
			x == ASM::JMP || x == ASM::JSR || x == ASM::RTI || x == ASM::RTS)
			return true;
		return false;
	};


	auto isLabeledJumpL = [](ASM::Opcode x)->bool{
		if (x == ASM::BCC || x == ASM::BCS || x == ASM::BEQ || x == ASM::BMI ||
			x == ASM::BNE || x == ASM::BPL || x == ASM::BVC || x == ASM::BVS)
			return true;
		return false;
	};


	auto f_addReference = [&blockLabels, &referencingBlock](pair<string, string> x){
		FCodeBlock* referencedBlock  = blockLabels.find(x.second)->second;

		referencingBlock->referencing.push_back(referencedBlock);
		referencedBlock->referencedBy.push_back(referencingBlock);
	};


	/* Create code blocks */
	for ( ; it != fCode.end() ; it++){

		/* Create new block */
		newBlock = FCodeBlock();

		/* Each block is added to pairs blockLabels<string, FCodeBlock*> */
		if (it->opcode == ASM::LAB){
			currentId = it->txtLabel;
			/* Detect framework */
			if(string(it->txtLabel).substr(0,((string)SEM_FUNCTION_PREFIX).size()) == SEM_FUNCTION_PREFIX){
				currentFramework = it->txtLabel;
			}
			newBlock.fCode.push_back(*it);
			it++;
		}else{
			ostr.str("");
			ostr << OPT_END_SYMBOL << "Block"<< OPT_END_SYMBOL << "_" << setw(4) << hex << auxIDGenerator++;
			currentId = ostr.str();
		}

		/* Add each ASM until reach a jump or a jump destination (label) */
		while(it != fCode.end() && it->opcode != ASM::LAB){
			newBlock.fCode.push_back(*it);
			if (isJumpL(it->opcode))
				break;
			it++;
		}

		/* If this block is referenced by the previous one, build reference */
		if(referencedByPrev != ""){
			referencesToBuild.insert(pair<string, string>(referencedByPrev, currentId));
		}

		/* Every block finishing in RTS is a leaf */
		if (newBlock.fCode.back().opcode == ASM::RTS){
			newBlock.isLeaf    = true;
		}

		newBlock.id        = currentId;
		newBlock.framework = currentFramework;
		blocks.push_back(newBlock);
		blockLabels.insert(pair<string, FCodeBlock*>(currentId,&(blocks.back())));

		/*
		Each reference remaining is noted into pairs referencesToBuild<string, string>

		If a block finish in JMP-	Reference to JMP addressing
		If a block finish in JSR-	Reference to JSR addressing. One reference from leaf to next block.
		If a block finish in conditional jump	Reference to addresing, reference to next block
		Else-	Reference to next block
		*/
		if (it == fCode.end())
			it--;

		if (it->opcode == ASM::JMP){
			referencesToBuild.insert(pair<string, string>(blocks.back().id, it->txtLabel));
			referencedByPrev = "";
		}else if(it->opcode == ASM::JSR){
			referencesToBuild.insert(pair<string, string>(blocks.back().id, it->txtLabel));
			ostr.str("");
			ostr << OPT_END_SYMBOL << it->txtLabel;
			referencedByPrev =  ostr.str();
		}else if(isLabeledJumpL(it->opcode)){
			referencesToBuild.insert(pair<string, string>(blocks.back().id, it->txtLabel));
			referencedByPrev = newBlock.id;
		}else if(it->opcode == ASM::RTS || it->opcode == ASM::RTI){
			referencedByPrev = "";
		}else{
			referencedByPrev = newBlock.id;
			if (it->opcode == ASM::LAB)
				it--;
		}
	}


	/* Solve each reference */
	/*
	For each FCodeBlock b
		For each = referencesToBuild b
			Build double reference
		If b.isLeaf
			For each = referencesToBuild $currentFramework
				Build double reference
	*/
	/* For every FCodeBlock */
	for_each(blocks.begin(), blocks.end(),
		[&blocks, &referencesToBuild, &blockLabels, &referencingBlock, &ostr, f_addReference](FCodeBlock x){
			/* Build a reference from referencingBlock to x */
			referencingBlock = blockLabels.find(x.id)->second;

			/* Add every pending reference from referencingBlock */
			auto range = referencesToBuild.equal_range(referencingBlock->id);
			for_each(range.first, range.second, f_addReference);

			/* If leaf */
			if (referencingBlock->isLeaf){
				/* Add every $currentFramework references */
				ostr.str("");
				ostr << "$" << referencingBlock->framework;

				auto secRange = referencesToBuild.equal_range(ostr.str());
				for_each(secRange.first, secRange.second, f_addReference);
			}
	});

	return blocks;
}



/**
* join
*
* Takes a list of sliced asm blocks and join them into an asm list
*
* @param blocksList: The blocks of asm code to join
*
* @return the complete asm code (a list of asm instructions)
*/
list<ASM> Optimizer::join(list<FCodeBlock> blocksList){

	list<ASM> fCodeList = list<ASM>();

	auto f_concatASM = [&fCodeList](ASM x){
		fCodeList.push_back(x);
	};

	auto f_concatASMList = [f_concatASM](FCodeBlock x){
		for_each(x.fCode.begin(), x.fCode.end(), f_concatASM);
	};

	for_each(blocksList.begin(), blocksList.end(), f_concatASMList);

	return fCodeList;
}





/**
* |------------------------------------------|
* |                                          |
* | Class: FCodeBlock                        |
* |                                          |
* |------------------------------------------|
*/



/**
* FCodeBlock
*
* Class constructor.
*/
FCodeBlock::FCodeBlock(){
	fCode         = list<ASM>();
	id            = "";
	framework     = "";
	isLeaf        = false;

	referencedBy  = list<FCodeBlock*>();
	referencing   = list<FCodeBlock*>();

	regDescriptor = RegDescriptor();
}



/**
* FCodeBlock
*
* Class destructor.
*/
FCodeBlock::~FCodeBlock(){
	referencedBy.clear();
	referencing.clear();
	fCode.clear();
}



/**
* checkRedundantInst
*
* Auxiliar function. Detects if an instruction is redundant (unnecessary)
*
* @param asmInstr: The instruction to check
*
* @return true if the instruction is redundant
*/
bool FCodeBlock::checkRedundantInst(const ASM &asmInstr){
	/* Redundant means: */
	/* lda, ldy, ldx with A_INM addressing when the value is already stored */
	/* lda, ldy, ldx with A_ABS or A_ZPG addressing when the var is already stored */
	/* sta, sty, stx with A_ABS or A_ZPG addressing when the var is already stored */

	/* sta, sty, stx with A_ABS or A_ZPG addressing when symbol is not volatile and usage is 0 (dead code) */

	switch(asmInstr.opcode){

	case ASM::LDA:

		if (asmInstr.operand.opAdd == ASM::A_INM){
			return asmInstr.operand.relatedOp.A_INM == regDescriptor.regA.VAL;
		}else if(asmInstr.operand.opAdd == ASM::A_ABS || asmInstr.operand.opAdd == ASM::A_ZPG){

			return regDescriptor.regA.VAR.end() != find_if(regDescriptor.regA.VAR.begin(),
				regDescriptor.regA.VAR.end(),
				[&asmInstr](int i)->bool{
					return i == asmInstr.operand.relatedOp.A_ABS;
			});

		}
		break;

	case ASM::LDX:
		if (asmInstr.operand.opAdd == ASM::A_INM){
			return asmInstr.operand.relatedOp.A_INM == regDescriptor.regX.VAL;
		}else if(asmInstr.operand.opAdd == ASM::A_ABS || asmInstr.operand.opAdd == ASM::A_ZPG){
			return regDescriptor.regX.VAR.end() != find_if(regDescriptor.regX.VAR.begin(),
				regDescriptor.regX.VAR.end(),
				[&asmInstr](int i)->bool{
					return i == asmInstr.operand.relatedOp.A_ABS;
			});
		}
		break;

	case ASM::LDY:
		if (asmInstr.operand.opAdd == ASM::A_INM){
			return asmInstr.operand.relatedOp.A_INM == regDescriptor.regY.VAL;
		}else if(asmInstr.operand.opAdd == ASM::A_ABS || asmInstr.operand.opAdd == ASM::A_ZPG){
			return regDescriptor.regY.VAR.end() != find_if(regDescriptor.regY.VAR.begin(),
				regDescriptor.regY.VAR.end(),
				[&asmInstr](int i)->bool{
					return i == asmInstr.operand.relatedOp.A_ABS;
			});
		}
		break;

	case ASM::STA:
		if(asmInstr.operand.opAdd == ASM::A_ABS || asmInstr.operand.opAdd == ASM::A_ZPG){

			/* Dead code deletion */
			if (asmInstr.symbol != NULL && !(asmInstr.symbol->isVolatile()) && asmInstr.symbol->getNUses() < 1)
				return true;

			return regDescriptor.regA.VAR.end() != find_if(regDescriptor.regA.VAR.begin(),
				regDescriptor.regA.VAR.end(),
				[&asmInstr](int i)->bool{
					return i == asmInstr.operand.relatedOp.A_ABS;
			});
		}
		break;

	case ASM::STX:
		if(asmInstr.operand.opAdd == ASM::A_ABS || asmInstr.operand.opAdd == ASM::A_ZPG){

			/* Dead code deletion */
			if (asmInstr.symbol != NULL && !(asmInstr.symbol->isVolatile()) && asmInstr.symbol->getNUses() < 1)
				return true;

			return regDescriptor.regX.VAR.end() != find_if(regDescriptor.regX.VAR.begin(),
				regDescriptor.regX.VAR.end(),
				[&asmInstr](int i)->bool{
					return i == asmInstr.operand.relatedOp.A_ABS;
			});
		}
		break;

	case ASM::STY:
		if(asmInstr.operand.opAdd == ASM::A_ABS || asmInstr.operand.opAdd == ASM::A_ZPG){

			/* Dead code deletion */
			if (asmInstr.symbol != NULL && !(asmInstr.symbol->isVolatile()) && asmInstr.symbol->getNUses() < 1)
				return true;

			return regDescriptor.regY.VAR.end() != find_if(regDescriptor.regY.VAR.begin(),
				regDescriptor.regY.VAR.end(),
				[&asmInstr](int i)->bool{
					return i == asmInstr.operand.relatedOp.A_ABS;
			});
		}
		break;

	};

	return false;
}



/**
* killFCInstr
*
* Delete a final code instruction from a block
*
* @param fCodeList: The code where the instruction is located
* @param it: The iterator pointing to the instruction to delete
*
* @return the new iterator place
*/
list<ASM>::iterator FCodeBlock::killFCInstr(list<ASM>* fCodeList, list<ASM>::iterator it){
	return fCodeList->erase(it);
}



/**
* descriptorCalc
*
* Calculates the descriptor status before starting the block (intersection of of each known value of
* each refferencedBy regDescriptor). After that, the code block is sparing to calculate the final status
* of the descriptor. The redundant instructions detected during this check are deleted
*
* @return true if an instruction of the block has been deleted during the calculation
*/
bool FCodeBlock::descriptorCalc(){
	list<ASM>::iterator it = fCode.begin();
	bool output = false;

	/* regDescriptor is the intersection of each known value of each refferencedBy regDescriptor */
	if(referencedBy.size() < 1){
		regDescriptor.clearReg(RegDescriptor::A);
		regDescriptor.clearReg(RegDescriptor::X);
		regDescriptor.clearReg(RegDescriptor::Y);
	}else if(referencedBy.size() == 1){
		regDescriptor = referencedBy.front()->regDescriptor;
	}else{
		list<int>           tempList;
		list<int>::iterator intIt;

		regDescriptor = referencedBy.front()->regDescriptor;

		regDescriptor.regA.VAR.sort();
		regDescriptor.regX.VAR.sort();
		regDescriptor.regY.VAR.sort();

		for(list<FCodeBlock*>::iterator auxIt = referencedBy.begin() ; auxIt != referencedBy.end() ; auxIt++){
			if(regDescriptor.regA.VAL != (*auxIt)->regDescriptor.regA.VAL)
				regDescriptor.regA.VAL = DES_UNKNOWN;
			if(regDescriptor.regX.VAL != (*auxIt)->regDescriptor.regX.VAL)
				regDescriptor.regX.VAL = DES_UNKNOWN;
			if(regDescriptor.regY.VAL != (*auxIt)->regDescriptor.regY.VAL)
				regDescriptor.regY.VAL = DES_UNKNOWN;

			(*auxIt)->regDescriptor.regA.VAR.sort();
			tempList.clear();
			tempList.resize(min(regDescriptor.regA.VAR.size(), (*auxIt)->regDescriptor.regA.VAR.size())); 

			intIt = set_intersection(regDescriptor.regA.VAR.begin(), regDescriptor.regA.VAR.end(),
				(*auxIt)->regDescriptor.regA.VAR.begin(), (*auxIt)->regDescriptor.regA.VAR.end(),
				tempList.begin());

			tempList.erase(intIt, tempList.end());

			regDescriptor.regA.VAR = tempList;


			(*auxIt)->regDescriptor.regX.VAR.sort();
			tempList.clear();
			tempList.resize(min(regDescriptor.regX.VAR.size(), (*auxIt)->regDescriptor.regX.VAR.size())); 

			intIt = set_intersection(regDescriptor.regX.VAR.begin(), regDescriptor.regX.VAR.end(),
				(*auxIt)->regDescriptor.regX.VAR.begin(), (*auxIt)->regDescriptor.regX.VAR.end(),
				tempList.begin());

			tempList.erase(intIt, tempList.end());

			regDescriptor.regX.VAR = tempList;


			(*auxIt)->regDescriptor.regY.VAR.sort();
			tempList.clear();
			tempList.resize(min(regDescriptor.regY.VAR.size(), (*auxIt)->regDescriptor.regY.VAR.size())); 

			intIt = set_intersection(regDescriptor.regY.VAR.begin(), regDescriptor.regY.VAR.end(),
				(*auxIt)->regDescriptor.regY.VAR.begin(), (*auxIt)->regDescriptor.regY.VAR.end(),
				tempList.begin());

			tempList.erase(intIt, tempList.end());

			regDescriptor.regY.VAR = tempList;			
		}
	}

	/* For each ASM instruction */
	while (it != fCode.end()){
		/* If is a redundant instruction, kill it */
		if (checkRedundantInst(*it)){
			it = killFCInstr(&fCode, it);
			output = true;
		}else{
			/* Set the new regDescriptor */
			regDescriptor.setDescriptor(*it);
			it++;
		}
	}

	return output;
}



/**
* |------------------------------------------|
* |                                          |
* | Class: RegDescriptor                     |
* |                                          |
* |------------------------------------------|
*/



/**
* RegDescriptor
*
* Class constructor.
*/
RegDescriptor::RegDescriptor(){
	regA.VAL = DES_UNKNOWN;
	regX.VAL = DES_UNKNOWN;
	regY.VAL = DES_UNKNOWN;

	regA.VAR = list<int>();
	regX.VAR = list<int>();
	regY.VAR = list<int>();
}



/**
* RegDescriptor
*
* Class destructor.
*/
RegDescriptor::~RegDescriptor(){
	regA.VAR.clear();
	regX.VAR.clear();
	regY.VAR.clear();
}



/**
* setValue
*
* Auxiliar function. Set a symbol into a given value.
*
* @param value: The value to set
* @param inReg: The register to set the value in
*/
void RegDescriptor::setValue(int value, const Register &inReg){
	Value* reg;

	if (inReg == A)
		reg = &regA;
	else if (inReg == X)
		reg = &regX;
	else
		reg = &regY;

	if (reg->VAL == value)
		return;
	else{
		reg->VAL = value;
		reg->VAR.clear();
		/* FUTURE <descriptor> equalize to each other register */
	}
}



/**
* setAddress
*
* Auxiliar function. Put a symbol into a given register.
* If the symbol was not there before, kill every other symbol
*
* @param symbol: The symbol to set
* @param inReg: The register to set the symbol in
*/
void RegDescriptor::setAddress(int symbol, const Register &inReg){
	Value* reg;

	if (inReg == A)
		reg = &regA;
	else if (inReg == X)
		reg = &regX;
	else
		reg = &regY;

	if (reg->VAR.end() != find_if(reg->VAR.begin(), reg->VAR.end(), [&symbol](int i)->bool{ return i == symbol; }))
		return;
	else{
		reg->VAL = DES_UNKNOWN; /* FUTURE <descriptor> Get the new symbol value instead */
		reg->VAR.clear();
		reg->VAR.push_back(symbol);
	}
}



/**
* addAddress
*
* Auxiliar function. Add an address into a given register in the descriptor
*
* @param symbol: The symbol to add
* @param inReg: The register to insert in
*/
void RegDescriptor::addAddress(int symbol, const Register &inReg){
	Value* reg;

	if (inReg == A)
		reg = &regA;
	else if (inReg == X)
		reg = &regX;
	else
		reg = &regY;

	if (reg->VAR.end() == find_if(reg->VAR.begin(), reg->VAR.end(), [&symbol](int i)->bool{ return i == symbol; })){
		reg->VAR.push_back(symbol);
	}
}



/**
* clearReg
*
* Reset a register from the descriptor
*
* @param inReg: The register to reset
*/
void RegDescriptor::clearReg(const Register &inReg){
	if(inReg == A){
		regA.VAL = DES_UNKNOWN;
		regA.VAR.clear();
	}else if(inReg == X){
		regX.VAL = DES_UNKNOWN;
		regX.VAR.clear();
	}else{
		regY.VAL = DES_UNKNOWN;
		regY.VAR.clear();
	}
}



/**
* delAddress
*
* Auxiliar function. Delete an address from every register in the descriptor
*
* @param x: The address to delete
*/
void RegDescriptor::delAddress(int x){
	regA.VAR.remove(x);
	regX.VAR.remove(x);
	regY.VAR.remove(x);
}



/**
* setDescriptor
*
* Little virtual-machine-like of a 6502. Gets the current state of the descriptor and modify it
* with an input instruction just like a 6502 would be modified by it.
*
* @param instr: The instruction which is going to modify the descriptor state
*/
void RegDescriptor::setDescriptor(const ASM &instr){
	/* FUTURE <improveFCOptimizer> Indirect addressing improvement taking memory status into account */
	/*
	FUTURE <improveFCOptimizer> Take into account each posible addressing
	Immediate     OPR #$44
	Zero Page     OPR $44
	Zero Page,X   OPR $44,X
	Absolute      OPR $4400
	Absolute,X    OPR $4400,X
	Absolute,Y    OPR $4400,Y
	Indirect,X    OPR ($44,X)
	Indirect,Y    OPR ($44),Y
	*/

	/* FUTURE <improveFCOptimizer> */
	/*
	if(instr.operand.opAdd == ASM::A_ABSX || instr.operand.opAdd == ASM::A_ABSY ||
		instr.operand.opAdd == ASM::A_IND || instr.operand.opAdd == ASM::A_INDY ||
		instr.operand.opAdd == ASM::A_XIND || instr.operand.opAdd == ASM::A_ZPGX ||
		instr.operand.opAdd == ASM::A_ZPGY){
		regA.VAR.clear();
		regX.VAR.clear();
		regY.VAR.clear();
		return;
	}*/

	/* If ld, set. If st, set. If register affecting, do (AND). */
	switch(instr.opcode){

	case ASM::ADC:
		if (instr.operand.opAdd == ASM::A_INM){
			/* FUTURE <improveFCOptimizer>  We are not taking carry flag into account */
			clearReg(A);
		}else{
			/* FUTURE <improveFCOptimizer> */
			clearReg(A);
		}
		break;
	case ASM::AND:
		clearReg(A);
		break;
	case ASM::ASL:
		if (instr.operand.opAdd == ASM::A_REGA){
			if(regA.VAL != DES_UNKNOWN)
				setValue((regA.VAL << 1) & 0x00FF, A);
			else
				regA.VAR.clear();
		}else if(instr.operand.opAdd == ASM::A_ABS || instr.operand.opAdd == ASM::A_ZPG){
			delAddress(instr.operand.relatedOp.A_ABS);
		}else{
			regA.VAR.clear();
			regX.VAR.clear();
			regY.VAR.clear();
		}
		break;
	case ASM::DEC:
		if(instr.operand.opAdd == ASM::A_ZPG || instr.operand.opAdd == ASM::A_ABS){
			delAddress(instr.operand.relatedOp.A_ABS);
		}else{
			regA.VAR.clear();
			regX.VAR.clear();
			regY.VAR.clear();
		}
		break;
	case ASM::DEX:
		if(regX.VAL != DES_UNKNOWN){
			setValue((regX.VAL + 0x0FF) & 0x00FF, X);
		}
		break;
	case ASM::DEY:
		if(regY.VAL != DES_UNKNOWN){
			setValue((regY.VAL + 0x0FF) & 0x00FF, Y);
		}
		break;
	case ASM::EOR:
		clearReg(A);
		break;
	case ASM::INC:
		if(instr.operand.opAdd == ASM::A_ZPG || instr.operand.opAdd == ASM::A_ABS){
			delAddress(instr.operand.relatedOp.A_ABS);
		}else{
			regA.VAR.clear();
			regX.VAR.clear();
			regY.VAR.clear();
		}
		break;
	case ASM::INX:
		if(regX.VAL != DES_UNKNOWN){
			setValue((regX.VAL + 1) & 0x00FF, X);
		}
		break;
	case ASM::INY:
		if(regY.VAL != DES_UNKNOWN){
			setValue((regY.VAL + 1) & 0x00FF, Y);
		}
		break;
	case ASM::LDA:
		if (instr.operand.opAdd == ASM::A_INM){
			setValue(instr.operand.relatedOp.A_INM, A);
		}else if(instr.operand.opAdd == ASM::A_ABS || instr.operand.opAdd == ASM::A_ZPG){
			setAddress(instr.operand.relatedOp.A_ABS, A);
		}else if(instr.operand.opAdd == ASM::A_IND || instr.operand.opAdd == ASM::A_INDY){
			clearReg(A);
		}
		break;
	case ASM::LDX:
		if (instr.operand.opAdd == ASM::A_INM){
			setValue(instr.operand.relatedOp.A_INM, X);
		}else if(instr.operand.opAdd == ASM::A_ABS || instr.operand.opAdd == ASM::A_ZPG){
			setAddress(instr.operand.relatedOp.A_ABS, X);
		}else if(instr.operand.opAdd == ASM::A_IND || instr.operand.opAdd == ASM::A_INDY){
			clearReg(X);
		}
		break;
	case ASM::LDY:
		if (instr.operand.opAdd == ASM::A_INM){
			setValue(instr.operand.relatedOp.A_INM, Y);
		}else if(instr.operand.opAdd == ASM::A_ABS || instr.operand.opAdd == ASM::A_ZPG){
			setAddress(instr.operand.relatedOp.A_ABS, Y);
		}else if(instr.operand.opAdd == ASM::A_IND || instr.operand.opAdd == ASM::A_INDY){
			clearReg(Y);
		}
		break;
	case ASM::LSR:
		if (instr.operand.opAdd == ASM::A_REGA){
			if(regA.VAL != DES_UNKNOWN)
				setValue((regA.VAL >> 1) & 0x00FF, A);
			else
				regA.VAR.clear();
		}else if(instr.operand.opAdd == ASM::A_ABS || instr.operand.opAdd == ASM::A_ZPG){
			delAddress(instr.operand.relatedOp.A_ABS);
		}else{
			regA.VAR.clear();
			regX.VAR.clear();
			regY.VAR.clear();
		}
		break;
	case ASM::ORA:
		clearReg(A);
		break;
	case ASM::PHA:
		break;
	case ASM::PHP:
		break;
	case ASM::PLA:
		clearReg(A);
		break;
	case ASM::PLP:
		break;
	case ASM::ROL:
		if (instr.operand.opAdd == ASM::A_REGA){
			if(regA.VAL != DES_UNKNOWN)
				setValue((regA.VAL << 1) & 0x00FF, A);
			else
				regA.VAR.clear();
		}else if(instr.operand.opAdd == ASM::A_ABS || instr.operand.opAdd == ASM::A_ZPG){
			delAddress(instr.operand.relatedOp.A_ABS);
		}else{
			regA.VAR.clear();
			regX.VAR.clear();
			regY.VAR.clear();
		}
		break;
	case ASM::ROR:
		if (instr.operand.opAdd == ASM::A_REGA){
			if(regA.VAL != DES_UNKNOWN)
				setValue((regA.VAL >> 1) & 0x00FF, A);
			else
				regA.VAR.clear();
		}else if(instr.operand.opAdd == ASM::A_ABS || instr.operand.opAdd == ASM::A_ZPG){
			delAddress(instr.operand.relatedOp.A_ABS);
		}else{
			regA.VAR.clear();
			regX.VAR.clear();
			regY.VAR.clear();
		}
		break;
	case ASM::SBC:
		if (instr.operand.opAdd == ASM::A_INM){
			/* FUTURE <improveFCOptimizer>  We are not taking carry flag into account */
			clearReg(A);
		}else{
			/* FUTURE <improveFCOptimizer> */
			clearReg(A);
		}
		break;
	case ASM::STA:
		if(instr.operand.opAdd == ASM::A_ABS || instr.operand.opAdd == ASM::A_ZPG){
			addAddress(instr.operand.relatedOp.A_ABS, A);
		}else if(instr.operand.opAdd == ASM::A_IND || instr.operand.opAdd == ASM::A_INDY){
			regA.VAR.clear();
			regX.VAR.clear();
			regY.VAR.clear();
		}
		break;
	case ASM::STX:
		if(instr.operand.opAdd == ASM::A_ABS|| instr.operand.opAdd == ASM::A_ZPG){
			addAddress(instr.operand.relatedOp.A_ABS, X);
		}else if(instr.operand.opAdd == ASM::A_IND || instr.operand.opAdd == ASM::A_INDY){
			regA.VAR.clear();
			regX.VAR.clear();
			regY.VAR.clear();
		}
		break;
	case ASM::STY:
		if(instr.operand.opAdd == ASM::A_ABS|| instr.operand.opAdd == ASM::A_ZPG){
			addAddress(instr.operand.relatedOp.A_ABS, Y);
		}else if(instr.operand.opAdd == ASM::A_IND || instr.operand.opAdd == ASM::A_INDY){
			regA.VAR.clear();
			regX.VAR.clear();
			regY.VAR.clear();
		}
		break;
	case ASM::TAX:
		clearReg(X);
		setValue(regA.VAL, X);
		regX.VAR = regA.VAR;
		break;
	case ASM::TAY:
		clearReg(Y);
		setValue(regA.VAL, Y);
		regY.VAR = regA.VAR;
		break;
	case ASM::TSX:
		clearReg(X);
		break;
	case ASM::TXA:
		clearReg(A);
		setValue(regX.VAL, A);
		regA.VAR = regX.VAR;
		break;
	case ASM::TYA:
		clearReg(A);
		setValue(regY.VAL, A);
		regA.VAR = regY.VAR;
		break;
	};
}




