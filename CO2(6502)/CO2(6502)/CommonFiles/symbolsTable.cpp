/**
* |------------------------------------------|
* | CO2 6502, COMPILER OPTIMIZER TO 6502     |
* | File: symbolstable.cpp                   |
* | v1.0, July 2012                          |
* | Author: Emilio Arango Delgado de Mendoza |
* |------------------------------------------|
*/
 
#include "symbolsTable.h"
 
#include <assert.h>
#include <algorithm>
#include <iostream>
#include <iomanip>
 
#ifdef WIN32
#define snprintf _snprintf /* _snprintf only defined to WIN32 platforms */
#define insert   emplace   /* 'emplace' only used under WIN32.
                               Is unsupported by gnu c++0x at October 2012 */
#endif
 
 
 
/**
* |------------------------------------------|
* |                                          |
* | Class: QualifiedType                     |
* |                                          |
* |------------------------------------------|
*/
 
/**
* QualifiedType
*
* Class constructor.
*
* @param name: Name of the qualified type.
*/
QualifiedType::QualifiedType(){
    type         = "void";
    arrSize      = 1;
    pointerDepth = 0;
    b_unsigned   = false;
    b_const      = false;
    b_volatile   = false;
    b_interrupt  = false;
}
QualifiedType::QualifiedType(const string &name){
    type         = name;
    arrSize      = 1;
    pointerDepth = 0;
    b_unsigned   = false;
    b_const      = false;
    b_volatile   = false;
    b_interrupt  = false;
}
 
 
 
/**
* QualifiedType
*
* Class destructor.
*/
QualifiedType::~QualifiedType(){ }
 
 
 
/**
* sameOutput
*
* @return a true value if the qualified type is valid as output data.
*/
bool QualifiedType::sameOutput(const QualifiedType &operand){
    /* FUTURE <strings> */
    if ((operand.type == TYPE_LITERAL) && ((type == "int") || (type == "char")))
        ;
    else{
        if (type       != operand.type)       return false;
        if (b_unsigned != operand.b_unsigned) return false;
    }
 
    if (pointerDepth != operand.pointerDepth) return false;
    if (b_interrupt != operand.b_interrupt) return false;
 
    return true;
}
 
 
 
/**
* sameInput
*
* @return a true value if the qualified type is valid as input data.
*/
bool QualifiedType::sameInput(const QualifiedType &operand){
    /* FUTURE <strings> */
    if (type == TYPE_LITERAL) type = operand.type;
 
    if (type         != operand.type)         return false;
    if (pointerDepth != operand.pointerDepth) return false;
    if (b_unsigned   != operand.b_unsigned)   return false;
    if (b_volatile   && !operand.b_volatile)  return false;
    if (b_const      && !operand.b_const)     return false;
 
    return true;
}
 
 
 
/**
* equals
*
* @return a true value if both qualified type are equivalent.
*/
bool QualifiedType::equals(const QualifiedType &operand){
    /* FUTURE <strings> */
    if (type == TYPE_LITERAL) type = operand.type;
 
    if (type         != operand.type)         return false;
    if (pointerDepth != operand.pointerDepth) return false;
    if (b_unsigned   != operand.b_unsigned)   return false;
    if (b_volatile   != operand.b_volatile)   return false;
    if (b_const      != operand.b_const)      return false;
 
    return true;
}



/**
* toString
*
* @return string containing the text formated type.
*/
string QualifiedType::toString() const{
    string output = "";
 
    if (b_const) output += "const ";
 
    if (b_unsigned) output += "u ";
    output += type;
 
    for (int i=0 ; i < pointerDepth; i++)
        output += "*";
     
    /* FUTURE <arrays> arrSize; */
     
    return output;
}
 
 
 
/**
* |------------------------------------------|
* |                                          |
* | Class: STEntry                           |
* |                                          |
* |------------------------------------------|
*/
 
/**
* STEntry
*
* Class constructor.
*
* @param inLex: New symbol lexeme.
* @param inFramework: Framework where this symbol is operational.
*
* @return STEntry object.
*/
STEntry::STEntry(const string &inLex, const string &inFramework) {
    lex          = inLex;
    qType        = QualifiedType("void");
    framework    = inFramework;
    n_uses       = 0;
    trueConstant = false;
    address      = -1;
}
 
 
 
/**
* STEntry
*
* Class constructor.
*
* @param inLex: New symbol lexeme.
* @param inFramework: Framework where this symbol is operational.
* @param inType: Type of the symbol.
*
* @return STEntry object.
*/
STEntry::STEntry(const string &inLex, const string &inFramework, const QualifiedType &inType){
    lex          = inLex;
    qType        = QualifiedType(inType);
    framework    = inFramework;
    n_uses       = 0;
    trueConstant = false;
	overlappable = false;
    address      = -1;
}
 
 
 
/**
* STEntry
*
* Class destructor.
*/
STEntry::~STEntry(){ }
 
 
 
/**
* getType
*
* @return the symbol type.
*/
QualifiedType STEntry::getType() { return qType; }
 
 
 
/**
* setType
*
* @param new type of the symbol.
*/
void STEntry::setType(const string &inType) { qType.type = inType; }
 
 
 
/**
* getPointerDepth
*
* @return the pointer depth of the symbol.
*/
int STEntry::getPointerDepth() { return qType.pointerDepth; }
 
 
 
/**
* setPointerDepth
*
* @param depth: new pointer depth of the symbol.
*/
void STEntry::setPointerDepth(const int depth) { qType.pointerDepth = depth; }
 
 
 
/**
* isVolatile
*
* @return true if the symbol is volatile.
*/
bool STEntry::isVolatile() { return qType.b_volatile; }
 
 
 
/**
* setVolatile
*
* Set the symbol as volatile
*/
void STEntry::setVolatile() { qType.b_volatile = true; }
 
 
 
/**
* isConst
*
* @return true if the symbol is const.
*/
bool STEntry::isConst() { return qType.b_const; }
 
 
 
/**
* setConst
*
* Set the symbol as const.
*/
void STEntry::setConst() { qType.b_const = true; }
 
 
 
/**
* isOverlappable
*
* @return true if the symbol is a function argument with an overlappable memory address.
*/
bool STEntry::isOverlappable(){ return overlappable; }



/**
* setOverlappable
*
* Modify the overlappable property of the STEntry.
*
* @param overlappable: The new value.
*/
void STEntry::setOverlappable(bool inOverlappable){ overlappable = inOverlappable; }



/**
* isLiteral
*
* @return true if the symbol is a literal.
*/
bool STEntry::isLiteral() { return trueConstant; }
 
 
 
/**
* setLiteral
*
* Set the symbol as literal.
*/
void STEntry::setLiteral() { trueConstant = true; }
 
 
 
/**
* isUnsigned
*
* @return true if the symbol is unsigned.
*/
bool STEntry::isUnsigned() { return qType.b_unsigned; }
 
 
 
/**
* setUnsigned
*
* Set the symbol as unsigned.
*/
void STEntry::setUnsigned() { qType.b_unsigned = true; }
 
 
 
/**
* getNUses
*
* @return the number of uses stored in the symbol.
*/
int STEntry::getNUses(){ return n_uses; }
 
 
 
/**
* setNUses
*
* Store a name of uses in the symbol.
*
* @param inNUses: the number of uses to store in the symbol.
*/
void STEntry::setNUses(int inNUses){
	assert(inNUses >= 0);
	n_uses = inNUses;
}
 
 
 
/**
* getAddress
*
* Set the symbol address value.
*/
int STEntry::getAddress() { return address; }
 
 
 
/**
* setAddress
*
* Set the symbol address value.
*/
void STEntry::setAddress(int inAddress) { address = inAddress; }
 
 
 
/**
* getFramework
*
* Get the symbol framework.
*/
string STEntry::getFramework() { return framework; }



/**
* toString
*
* @return string containing the text formated symbol.
*/
string STEntry::toString() const {
   ostringstream output;
    char   c_v;
    string outputType;
    string framework_b;
 
    c_v = qType.b_volatile? 'Y' : 'N';
    framework_b = framework == ""? "GLOBAL VAR" : framework;
    outputType = "";
 
    if (qType.b_unsigned)
        outputType += "u ";
    outputType += qType.type;
    if (qType.pointerDepth > 0)
        for (int i=0 ; i < qType.pointerDepth ; i++) outputType += "*";
 
    output << left;
    output.fill(' ');
    output << "| "  << setw(16) << lex.substr(0, 16);
    output << " | " << setw(12) << outputType.substr(0, 12);
    output.fill('0');
    output << right;
    output << " | " << setw(3) << hex << qType.arrSize;
    output.fill(' ');
    output << left;
    output << " | " << setw(16) << framework_b.substr(0, 16);
    output << " | " << c_v;
    output.fill('0');
    output << right;
    output << " | " << setw(3)  << n_uses;
    if (trueConstant)
        output << " | Const ";
    else if (address < 0)
        output << " | Undef ";
    else
        output << " | " << "0x" << setw(4) << hex << address;
    output << " |"  << endl;
 
    return output.str();
}
 
 
 
/**
* |------------------------------------------|
* |                                          |
* | Class: STFramework                       |
* |                                          |
* |------------------------------------------|
*/
 
/**
* STFramework
*
* Class constructor.
*
* @param inName: New framework name
*
* @return STFramework object.
*/
STFramework::STFramework(const string &inName, int inId){
    name           = inName;
	id             = inId;
    outputType     = QualifiedType("void");
    inputType      = list<QualifiedType>();
	inputSymbols   = list<string>();
	inputArguments = list<list<STEntry*>>();
    symbols        = unordered_map <string, STEntry*>();
    labels         = unordered_set <string>();
    dependencies   = list <STFramework*>();
    codeSize       = 0;
	symbolsSize    = 0;
	recursive      = false;
	defined        = false;
}
 
 
 
/**
* STFramework
*
* Class destructor.
*/
STFramework::~STFramework(){
 
    auto f_delSymbolPointers = [](pair<string, STEntry*> const &x){
        if (x.second != NULL) delete x.second;
    };
     
     
    for_each(symbols.begin(), symbols.end(), f_delSymbolPointers);
    symbols.clear();
 
    dependencies.clear();
    inputType.clear();
	inputSymbols.clear();
    labels.clear();
}
 
 
 
/**
* getName
*
* Get the framework name.
*
* @return the name of the framework.
*/
string STFramework::getName(){ return name; }
 
 
 
/**
* getId
*
* Get the framework id.
*
* @return the id of the framework.
*/
int STFramework::getId(){ return id; }



/**
* setOutputType
*
* Set the output type to the framework.
*
* @param inName: Type to set
*
* @return true if the type wasn't set before.
*/
bool STFramework::setOutputType(const QualifiedType &inOutType){
    if (outputType.type == "void"){
        outputType = inOutType;
        return true;
    }
    return false;
}
 
 
 
/**
* getOutputType
*
* Get the output type fromo the framework.
*
* @return the type.
*/
QualifiedType STFramework::getOutputType() { return outputType; }
 
 
 
/**
* addInputType
*
* Add an input type to the framework.
*
* @param inName: Type to add
*
* @return true if the types wasn't set before.
*/
bool STFramework::addInputType(const QualifiedType &inType){
    inputType.push_back(inType);
	inputArguments.push_back(list<STEntry*>());
    return true;
}
 
 
 
/**
* addInputType
*
* Add an input type to the framework.
*
* @param inName: Type to add
* @param inSymbol: Symbol to add
*
* @return true if the types wasn't set before.
*/
bool STFramework::addInputType(const QualifiedType &inType, const string &inSymbol){
    inputType.push_back(inType);
	inputSymbols.push_back(inSymbol);
	inputArguments.push_back(list<STEntry*>());
    return true;
}



/**
* addInputArgument
*
* Add an argument in a given position to perform future optimizations
*
* @param argument: The pointer to the STEntry argument
* @param position: The place in the argument list where the symbol is allocated
*
* @return true if the operation was performed without failures.
*/
bool STFramework::addInputArgument(STEntry* argument, int position){

	list<list<STEntry*>>::iterator it = inputArguments.begin();

	for(int i = 0 ; i < position ; i++){
		if (it == inputArguments.end()) return false;
		it++;
	}

	(*it).push_back(argument);
	return true;
}



/**
* getInputArguments
*
* Get the symbol list related with the argument in a given position to perform future optimizations
*
* @param position: The argument place in the argument list
*
* @return the list of symbols related to that argument
*/
list<STEntry*> STFramework::getInputArguments(int position){

	list<list<STEntry*>>::iterator it = inputArguments.begin();

	for(int i = 0 ; i < position ; i++){
		if (it == inputArguments.end()) return list<STEntry*>();
		it++;
	}

	if(it != inputArguments.end())
		return *it;
	else
		return list<STEntry*>();
}



/**
* getInputType
*
* Get the input types from the framework.
*
* @return a list containing the input types.
*/
list<QualifiedType> STFramework::getInputType(){ return inputType; }
 
 
 
/**
* getInputSymbols
*
* Get the input symbols from the framework.
*
* @return a list containing the input symbols.
*/
list<string> STFramework::getInputSymbols(){ return inputSymbols; }



/**
* resetInputType
*
* Reset the input types of the framework.
*/
void STFramework::resetInputType(){
	inputType.clear();
	inputArguments.clear();
}
 
 
 
/**
* calculateSize
*
* Calculates the memory needed by the framework to store all symbols
*
* @return the number of bytes used to store all symbols
*/
int  STFramework::calculateSize(SymbolsTable *st){
	unordered_set<int> usedLocations = unordered_set<int>();

	symbolsSize = 0;	

	for_each(symbols.begin(), symbols.end(), [this, &st, &usedLocations](pair<string, STEntry*> const x){
		int symSize = st->getTypeSize(x.second->getType());
		int address = x.second->getAddress();

		if (address < 0)
			return;

		/* Add 1 byte to 1 byte symbols */
		if (!usedLocations.count(address)){
			usedLocations.insert(address);
			symbolsSize++;
		}

		/* Add more bytes to biger symbols */
		for(int i = 1 ; i < symSize ; i++){	
			if (address < 0x0100){
				address += 0x0001;
			}else{
				address += 0x0100;
			}

			if (!usedLocations.count(address)){
				usedLocations.insert(address);
				symbolsSize++;
			}
		}
	});

	return symbolsSize;
}
 
 
 
/**
* setCodeSize
*
* Set the size of the code related with the framework.
*
* @param inSize: The size to set
*/
void STFramework::setCodeSize(int inSize){ codeSize = inSize; }



/**
* getCodeSize
*
* Get the size of the code related with the framework.
*
* @return the size.
*/
int  STFramework::getCodeSize(){ return codeSize; }



/**
* addDependency
*
* Stores a new dependency in the framework.
*
* @param newDependency: Pointer to add to the dependencies list.
*
* @return true if the operation was performed without failures.
*/
bool STFramework::addDependency(STFramework* newDependency){
    bool exists = false;
    list<STFramework*>::iterator it = dependencies.begin();
 
    while(it != dependencies.end() && !exists)
        if ((*it++)->getName() == newDependency->getName()) exists = true;
 
    if (exists) return false;
 
    dependencies.push_back(newDependency);
 
    return true;
}
 
 
 
/**
* getDependencies
*
* Return the dependencies from the framework.
*
* @return dependencies list.
*/
list<STFramework*> STFramework::getDependencies(){ return dependencies; }



/**
* setRecursive
*
* Set the framework as recursive.
*/
void STFramework::setRecursive(){
	recursive = true;
}



/**
* isRecursive
*
* Return true if the framework has been detected as recursive
*/
bool STFramework::isRecursive(){
	return recursive;
}



/**
* setAsDefined
*
* Set the framework as defined.
*/
void STFramework::setAsDefined(){ defined = true; }



/**
* isDefined
*
* @return if the framework is defined.
*/
bool STFramework::isDefined(){ return defined; }



/**
* addSymbol
*
* Add a new symbol to the framework.
*
* @param lex: the name of the symbol.
*
* @return true if the operation was performed without failures.
*/
bool STFramework::addSymbol(const string &lex){
    if (symbols.count(lex) > 0) return false;
 
    STEntry* newSymbol = new STEntry(lex, name);
    pair <string, STEntry*> newEntry = make_pair (lex, newSymbol);
 
    symbols.insert(newEntry);
 
    return true;
}
 
 
 
/**
* addSymbol
*
* Add a new symbol to the framework.
*
* @param lex: the name of the symbol.
* @param inType: the type of the symbol
*
* @return true if the operation was performed without failures.
*/
bool STFramework::addSymbol(const string &lex, const QualifiedType &inType){
    if (symbols.count(lex) > 0) return false;
 
    STEntry* newSymbol = new STEntry(lex, name, inType);
    pair <string, STEntry*> newEntry = make_pair (lex, newSymbol);
 
    symbols.insert(newEntry);
 
    return true;
}
 
 
 
/**
* getSymbol
*
* Get a pointer to a given symbol of the framework.
*
* @param lex: the name of the symbol.
*
* @return the symbol if the operation was performed without failures.
*/
STEntry* STFramework::getSymbol(const string &lex){
    unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
 
    if (entry != symbols.end()) return entry->second;
 
	return NULL;
 }



/**
* getSymbols
*
* Get a pointer list to the symbol in the framework.
*
* @return the symbol list.
*/
list <STEntry*> STFramework::getSymbols(){
	list <STEntry*> output;

	auto f_addToList = [&output](pair<string, STEntry*> const x){
		output.push_back(x.second);
    };

	for_each(symbols.begin(), symbols.end(), f_addToList);

	return output;
}



/**
* delSymbol
*
* Delete a symbol from the framework.
*
* @param lex: the name of the symbol.
*
* @return true if the operation was performed without failures.
*/
bool STFramework::delSymbol(const string &lex){
unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
 
    if (entry == symbols.end()) return false;

	symbols.erase(entry);

	return true;
}



/**
* existsSymbol
*
* Check the existence of a symbol with a certain name in the current framework.
*
* @param lex: the name of the symbol.
*
* @return true if a symbol with the given name exists.
*/
bool STFramework::existsSymbol(const string &lex){ return symbols.count(lex) > 0; }
 
 
 
/**
* setType
*
* Set the type of a certain symbol.
*
* @param lex: the name of the symbol.
* @param type: the name of the type to set.
*
* @return true if the operation was performed without failures.
*/
bool STFramework::setType(const string &lex, const string &type){
    unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
    if (entry == symbols.end()) return false;
 
    entry->second->setType(type);
    return true;
}
 
 
 
/**
* getType
*
* Get the type of a certain symbol.
*
* @param lex: the name of the symbol.
*
* @return the type of the symbol.
*/
QualifiedType STFramework::getType(const string &lex){
    unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
 
    if (entry != symbols.end()) return entry->second->getType();
 
    return QualifiedType("");
}
 
 
 
/**
* setPointerDepth
*
* Set the pointer depth of a certain symbol.
*
* @param lex: the name of the symbol.
* @param depth: the depth to set.
*
* @return true if the operation was performed without failures.
*/
bool STFramework::setPointerDepth(const string &lex, const int depth){
    unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
    if (entry == symbols.end()) return false;
 
    entry->second->setPointerDepth(depth);
    return true;
}
 
 
 
/**
* getPointerDepth
*
* Get the pointer depth of a certain symbol.
*
* @param lex: the name of the symbol.
*
* @return the pointer depth of the symbol or -1 if the symbol doesn't exist.
*/
int STFramework::getPointerDepth(const string &lex){
    unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
 
    if (entry != symbols.end()) return entry->second->getPointerDepth();
 
    return -1;
}
 
 
 
/**
* isVolatile
*
* Check if a certain symbol is volatile.
*
* @param lex: the name of the symbol.
*
* @return the volatile flag of the symbol.
*/
bool STFramework::isVolatile(const string &lex){
    unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
 
    if (entry != symbols.end()) return entry->second->isVolatile();
 
    return false;
}
 
 
 
/**
* setVolatile
*
* Set a certain symbol as volatile.
*
* @param lex: the name of the symbol.
*
* @return true if the operation was performed without failures.
*/
bool STFramework::setVolatile(const string &lex){
    unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
 
    if (entry != symbols.end()) {
        entry->second->setVolatile();
        return true;
    }
 
    return false;
}
 
 
 
/**
* isConst
*
* Check if a certain symbol is const.
*
* @param lex: the name of the symbol.
*
* @return the const flag of the symbol.
*/
bool STFramework::isConst(const string &lex){
    unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
 
    if (entry != symbols.end()) return entry->second->isConst();
 
    return false;
}
 
 
 
/**
* setConst
*
* Set a certain symbol as const.
*
* @param lex: the name of the symbol.
*
* @return true if the operation was performed without failures.
*/
bool STFramework::setConst(const string &lex){
    unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
 
    if (entry != symbols.end()) {
        entry->second->setConst();
        return true;
    }
 
    return false;
}
 
 
 
/**
* isLiteral
*
* Check if a certain symbol is literal.
*
* @param lex: the name of the symbol.
*
* @return the literal flag of the symbol.
*/
bool STFramework::isLiteral(const string &lex){
    unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
 
    if (entry != symbols.end()) return entry->second->isLiteral();
 
    return false;
}
 
 
 
/**
* setLiteral
*
* Set a certain symbol as literal.
*
* @param lex: the name of the symbol.
*
* @return true if the operation was performed without failures.
*/
bool STFramework::setLiteral(const string &lex){
    unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
 
    if (entry != symbols.end()) {
        entry->second->setLiteral();
        return true;
    }
 
    return false;
}
 
 
 
/**
* isUnsigned
*
* Check if a certain symbol is unsigned.
*
* @param lex: the name of the symbol.
*
* @return the unsigned flag of the symbol.
*/
bool STFramework::isUnsigned(const string &lex){
    unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
 
    if (entry != symbols.end()) return entry->second->isUnsigned();
 
    return false;
}
 
 
 
/**
* setUnsigned
*
* Set a certain symbol as unsigned.
*
* @param lex: the name of the symbol.
*
* @return true if the operation was performed without failures.
*/
bool STFramework::setUnsigned(const string &lex){
    unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
 
    if (entry != symbols.end()) {
        entry->second->setUnsigned();
        return true;
    }
 
    return false;
}
 
 
 
/**
* getNUses
*
* Get the number of uses from a certain symbol.
*
* @param lex: the name of the symbol.
*
* @return the number of uses or -1 if the operation was performed with failures.
*/
int STFramework::getNUses(const string &lex){
    unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
 
    if (entry != symbols.end())
        return entry->second->getNUses();
 
    return -1;
}
 
 
 
/**
* setNUses
*
* Set the number of uses from a certain symbol.
*
* @param lex: the name of the symbol.
* @param inNUses: the number of uses to set.
*
* @return true if the operation was performed without failures.
*/
bool STFramework::setNUses(const string &lex, int inNUses){
    unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
 
    if (entry != symbols.end()){
        entry->second->setNUses(inNUses);
        return true;
    }
 
    return false;
}
 
 
 
/**
* getAddress
*
* Get the current proposed memory address of a certain symbol.
*
* @param lex: the name of the symbol.
*
* @return the current proposed memory address of the symbol.
*/
int STFramework::getAddress(const string &lex){
    unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
 
    if (entry != symbols.end()) return entry->second->getAddress();
 
    return -1;
}
 
 
 
/**
* setAddress
*
* Set a proposed memory address to a certain symbol.
*
* @param lex: the name of the symbol.
* @param int: the new address to set.
*
* @return true if the operation was performed without failures.
*/
bool STFramework::setAddress(const string &lex, int address){
    unordered_map<string, STEntry*>::const_iterator entry;
    entry = symbols.find(lex);
 
    if (entry != symbols.end()) {
        entry->second->setAddress(address);
        return true;
    }
 
    return false;
}
 
 
 
/**
* addLabel
*
* Add a new label to the framework namespace
*
* @param newLabel: the name of the new label.
*
* @return true if the operation was performed without failures.
*/
bool STFramework::addLabel(const string &newLabel){
    if (existsLabel(newLabel)) return false;
    labels.insert(newLabel);
    return true;
}
 
 
 
/**
* existsLabel
*
* Check if a label exists in the framework namespace
*
* @param label: the name of the label to check.
*
* @return true if the label exists.
*/
bool STFramework::existsLabel(const string &label){
    return labels.find(label) != labels.end();
}
 
 
 
/**
* toString
*
* @return string containing the text formated framework
*/
string STFramework::toString() const{
    /* <future> print labels */
    ostringstream output;
    string        inputType_string = "";
 
    auto f_concat = [&output](pair<string, STEntry*> const x){
        output << x.second->toString();
    };
 
    auto f_printInput = [&output, &inputType_string](const QualifiedType &x){
        inputType_string += x.toString() + ", ";
    };
 
 
	if (name == GLOBAL_FRAMEWORK && symbols.size() == 0){
		output << endl;
		return output.str();
	}	

	output << "+-------------------------------------------";
    output << "----------------------------------+" << endl;
	output << "|" << setw(78) << right << "|" << endl;
	if(!recursive){
		output << "| Framework: " << left << setw(64) << name << " |" << endl;
	}else{
		output << "| Framework: (R)" << left << setw(61) << name << " |" << endl;
	}
    output << "|" << setw(78) << right << "|" << endl;
     
    for_each(inputType.begin(), inputType.end(), f_printInput);
    if (inputType_string == "")
        inputType_string = "void";
    else
        inputType_string.erase(inputType_string.length() - 2, 2);
 
    output << "| Input:     " << left << setw(65) << inputType_string.substr(0, 64) << right << "|" << endl;
    output << "| Output:    " << left << setw(64) << outputType.toString() << " |" << endl;
 
	output << right;
	output << "| Var Mem:   " << setw(5) << symbolsSize << " bytes" << right << setw(55) << " |" << endl;
	output << "| Code Mem:  " << setw(5) << codeSize    << " bytes" << right << setw(55) << " |" << endl;

	output << "|" << setw(78) << right << "|" << endl;
 
    output << "+------------------+--------------+-----+---";
    output << "---------------+---+-----+--------+" << endl;
 
    output << "| Lexeme           | Type         | Arr | Fr";
    output << "amework        | V | #Us | MemAdd |" << endl;
 
    output << "+------------------+--------------+-----+---";
    output << "---------------+---+-----+--------+" << endl;
 
    for_each(symbols.begin(), symbols.end(), f_concat);
 
    output << "+------------------+--------------+-----+---";
    output << "---------------+---+-----+--------+" << endl;
    output << endl;
 
    return output.str();
}
 
 
 
/**
* |------------------------------------------|
* |                                          |
* | Class: Type                              |
* |                                          |
* |------------------------------------------|
*/
 
/**
* Type
*
* Class constructor.
*/
Type::Type(const string &inType, int inSize){ type = inType; size = inSize; }
 
 
 
/**
* Type
*
* Class destructor.
*/
Type::~Type() { }
 
 
 
/**
* |------------------------------------------|
* |                                          |
* | Class: SymbolsTable                      |
* |                                          |
* |------------------------------------------|
*/
 
/**
* SymbolsTable
*
* Class constructor.
*/
SymbolsTable::SymbolsTable() {
    frameworks      = unordered_map <string, STFramework*>();
    constants       = unordered_map <string, string>();
    types           = list <Type>();
    tempIdentifiers = list <string>();
    tempTypes       = list <string>();
    tempLiterals    = list <string>();
	idCounter       = 0;
}
 
 
 
/**
* SymbolsTable
*
* Class destructor.
*/
SymbolsTable::~SymbolsTable() {
    auto f_delSymbolPointers = [](pair<string, STFramework*> const &x){ delete x.second; };
    for_each(frameworks.begin(), frameworks.end(), f_delSymbolPointers);
    frameworks.clear();
    constants.clear();
    types.clear();
    tempIdentifiers.clear();
    tempTypes.clear();
    tempLiterals.clear();
}
 
 
 
/**
* addFramework
*
* Add a new framework to the symbols table
*
* @param framework: The name of a framework
*
* @return true if the operation was performed without failures
*/
bool SymbolsTable::addFramework(const string &framework) {
    if (frameworks.count(framework) > 0) return false;
 
    STFramework* newSymbol = new STFramework(framework, idCounter++);
    pair <string, STFramework*> newEntry = make_pair (framework, newSymbol);
 
    frameworks.insert(newEntry);
 
    return true;
}
 
 
 
/**
* existsFramework
*
* Determines if exists a framework with a given name
*
* @param framework: The name of a framework
*
* @return true if the framework exists
*/
bool SymbolsTable::existsFramework(const string& framework) {
    return frameworks.count(framework) > 0;
}
 
 
 
/**
* getFramework
*
* Return a pointer to a STFramework object with a given name
*
* @param framework: The name of a framework
*
* @return the pointer to the STFramework object
*/
STFramework* SymbolsTable::getFramework(const string &framework) {
    unordered_map<string, STFramework*>::const_iterator entry;
    entry = frameworks.find(framework);
 
    return entry->second;
}
 
 
 
/**
* getFrameworks
*
* Return the STFramework pointer list.
*
* @return the pointer to the list<STFramework*> object
*/
list <STFramework*> SymbolsTable::getFrameworks(){
	list <STFramework*> output;

    auto f_addToList = [&output](pair<string, STFramework*> const x){
		output.push_back(x.second);
    };

	for_each(frameworks.begin(), frameworks.end(), f_addToList);
 
	return output;
}



/**
* addConstant
*
* Adds a constant to the symbols table.
*
* @param name: The name of a constant.
* @param constant: the literal string related to the constant.
*
* @return true if the operation was performed without failures.
*/
bool SymbolsTable::addConstant(const string &name, const string &constant){
    if (constants.count(name) > 0) return false;
 
    pair <string, string> newEntry = make_pair (name, constant);
 
    constants.insert(newEntry);
 
    return true;
}
 
 
 
/**
* existsConstant
*
* Check the existence of a certain constant.
*
* @param name: The name of a constant.
*
* @return true if the constant does exist.
*/
bool SymbolsTable::existsConstant(const string &name){
    return constants.count(name) > 0;
}
 
 
 
/**
* getConstant
*
* @return the literal string related to a given constant.
*
* @param name: The name of a constant.
*
* @return the literal string related to the constant.
*/
string SymbolsTable::getConstant(const string &name){
    unordered_map<string, string>::const_iterator entry;
    entry = constants.find(name);
 
    return entry->second;
}
 
 
 
/**
* addType
*
* Adds a new type to the symbols table.
*
* @param name: The name of a type.
*
* @return true if the operation was performed without failures.
*/
bool SymbolsTable::addType(const string &name, int size){
    types.push_back(Type(name, size));
    return true;
}
 
 
 
/**
* existsType
*
* Check if a given type exists.
*
* @param name: The name of a type.
*
* @return the literal string related to the constant.
*/
bool SymbolsTable::existsType(const string &name){
    bool exists = false;
    list<Type>::iterator it = types.begin();
 
    while(it != types.end() && !exists)
        if ((*it++).type == name) exists = true;
 
    return exists;
}
 
 
 
/**
* getTypeSize
*
* @return the size value in bytes of a certain type or -1 if the type doesn't exist.
*
* @param name: The name of a type.
*
* @return the size value
*/
int SymbolsTable::getTypeSize(const string &name){
    int output = -255;
    list<Type>::iterator it = types.begin();
 
    while(it != types.end() && output < -254)
        if ((*it++).type == name) output = (*--it).size;
 
    return output;
}
 
 
 
/**
* getTypeSize
*
* @return the size value in bytes of a certain object type or -1 if the type doesn't exist.
*
* @param inType: The type.
*
* @return the size value
*/
int SymbolsTable::getTypeSize(const QualifiedType &inType){
    int output = -255;
    list<Type>::iterator it = types.begin();
 
    if (inType.pointerDepth == 0){
        while(it != types.end() && output <= -255)
            if ((*it++).type == inType.type) output = (*--it).size;
    }else{
        /* The size of a pointer */
        output = 2;
    }
 
    output *= inType.arrSize;
 
    return output;
}
 
 
 
/**
* pushTempIdentifier
*
* Store a identifier name on the stack to be given later to the syntax analyzer
*
* @param name: The name of the new identifier
*/
void SymbolsTable::pushTempIdentifier(const string &name){
    tempIdentifiers.push_back(name);
}
 
 
 
/**
* popTempIdentifier
*
* Get the last identifier name set on the temporal stack
*
* @return the given name
*/
string SymbolsTable::popTempIdentifier(){
    string output = tempIdentifiers.back();
    tempIdentifiers.pop_back();
    return output;
}
 
 
 
/**
* pushTempLiteral
*
* Store a literal on the stack to be given later to the syntax analyzer
*
* @param name: The literal
*/
void SymbolsTable::pushTempLiteral(const string &literalValue){
    tempLiterals.push_back(literalValue);
}
 
 
 
/**
* popTempLiteral
*
* Get the last literal value set on the temporal stack
*
* @return the given literal
*/
string SymbolsTable::popTempLiteral(){
    string output = tempLiterals.back();
    tempLiterals.pop_back();
    return output;
}
 
 
 
/**
* pushTempType
*
* Store a type name on the stack to be given later to the syntax analyzer
*
* @param name: The name of the new type
*/
void SymbolsTable::pushTempType(const string &name){
    tempIdentifiers.push_back(name);
}
 
 
 
/**
* popTempType
*
* Get the last type name set on the temporal stack
*
* @return the given name
*/
string SymbolsTable::popTempType(){
    string output = tempTypes.back();
    tempTypes.pop_back();
    return output;
}
 
 
 
/**
* resetNUses
*
* Set to 0 the nUses of every symbol
*/
void SymbolsTable::resetNUses(){

	auto f_resetNUses = [](pair<string, STFramework*> x){
		list <STEntry*> symbolsList = x.second->getSymbols();
		for_each(symbolsList.begin(), symbolsList.end(),
			[](STEntry* x){ x->setNUses(0); }
		);
	};


	for_each(frameworks.begin(), frameworks.end(), f_resetNUses);
}



/**
* getId
*
* Get the ID from an existing framework
*
* @param framework: The name of a framework
*
* @return the ID if the operation was performed without failures. Otherwise return -1
*/
int SymbolsTable::getId(const string &framework){
    if (!existsFramework(framework)) return -1;
    STFramework* framework_ptr = getFramework(framework);
 
	return framework_ptr->getId();
}



/**
* getOutputType
*
* Get the output type from an existing framework
*
* @param framework: The name of a framework
*
* @return the type if the operation was performed without failures. Otherwise return ""
*/
QualifiedType SymbolsTable::getOutputType(const string &framework){
    if (!existsFramework(framework)) return QualifiedType("");
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->getOutputType();
}
 
 
 
/**
* setOutputType
*
* Set the output type on an existing framework
*
* @param framework: The name of a framework
* @param type: The type to set
*
* @return true if the operation was performed without failures.
*/
bool SymbolsTable::setOutputType(const string &framework, const QualifiedType &type){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->setOutputType(type);
}
 
 
 
/**
* getInputType
*
* Get the input type from an existing framework
*
* @param framework: The name of a framework
*
* @return the type list if the operation was performed without failures. Otherwise return an empty list
*/
list<QualifiedType> SymbolsTable::getInputType(const string &framework){
    if (!existsFramework(framework)) return list<QualifiedType>();
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->getInputType();
}
 
 
 
/**
* getInputSymbols
*
* Get the input symbol from an existing framework
*
* @param framework: The name of a framework
*
* @return the symbol list if the operation was performed without failures. Otherwise return an empty list
*/
list<string> SymbolsTable::getInputSymbols(const string &framework){
    if (!existsFramework(framework)) return list<string>();
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->getInputSymbols();
}



/**
* addInputType
*
* Add an input type on an existing framework
*
* @param framework: The name of a framework
* @param inTypes: The type string to set
*
* @return true if the operation was performed without failures.
*/
bool SymbolsTable::addInputType(const string &framework, const QualifiedType &inType){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->addInputType(inType);
}
 
 
 
/**
* addInputType
*
* Add an input type and its symbol lex on an existing framework
*
* @param framework: The name of a framework
* @param inTypes: The type string to set
* @param inSymbol: The symbol string to set
*
* @return true if the operation was performed without failures.
*/
bool SymbolsTable::addInputType(const string &framework, const QualifiedType &inType, const string &inSymbol){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->addInputType(inType, inSymbol);
}



/**
* addInputArgument
*
* Add an argument in a given position on an existing framework to perform future optimizations
*
* @param framework: The name of a framework
* @param argument: The pointer to the STEntry argument
* @param position: The place in the argument list where the symbol is allocated
*
* @return true if the operation was performed without failures.
*/
bool SymbolsTable::addInputArgument(const string &framework, STEntry* argument, int position){
	if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
	return framework_ptr->addInputArgument(argument, position);
}



/**
* getInputArguments
*
* Get the symbol list from the argument in a given position on an existing framework to perform future optimizations
*
* @param framework: The name of a framework
* @param position: The place in the argument list where the symbol is allocated
*
* @return the STEntry list of Symbols related with that argument
*/
list<STEntry*> SymbolsTable::getInputArguments(const string &framework, int position){
	if (!existsFramework(framework)) return list<STEntry*>();
    STFramework* framework_ptr = getFramework(framework);
 
	return framework_ptr->getInputArguments(position);
}



/**
* resetInputType
*
* Reset the input types on an existing framework
*
* @param framework: The name of a framework
*/
void SymbolsTable::resetInputType(const string &framework){
    STFramework* framework_ptr = getFramework(framework);
 
    framework_ptr->resetInputType();
}
 
 
 
/**
* calculateSize
*
* Perform an operation to calculate the memory size required to store the variables
* of a given framework. Store that value on the framework and returns it.
*
* @param framework: The name of a framework.
*
* @return the memory size given after the operation.
*/
int SymbolsTable::calculateSize(const string &framework){
    if (!existsFramework(framework)) return -1;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->calculateSize(this);
}
 
 
 
/**
* addDependency
*
* Add a new dependency to a given framework.
*
* @param framework: The name of a framework.
* @param dependency: The pointer to the dependency.
*
* @return true if the operation was performed without failures.
*/
bool SymbolsTable::addDependency(const string &framework, STFramework* dependency){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->addDependency(dependency);
}
 
 
 
/**
* addDependency
*
* Add a new dependency to a given framework.
*
* @param framework: The name of a framework.
* @param dependency: The name of the dependency.
*
* @return true if the operation was performed without failures.
*/
bool SymbolsTable::addDependency(const string &framework, const string &dependency){
    if (!existsFramework(framework)) return false;
    if (!existsFramework(dependency)) return false;
 
    STFramework* framework_ptr = getFramework(framework);
    STFramework* dependency_ptr = getFramework(dependency);
 
    return framework_ptr->addDependency(dependency_ptr);
}
 
 
 
/**
* getDependencies
*
* Get the dependency list from a given framework.
*
* @param framework: The name of a framework.
*
* @return the framework dependency list.
*/
list<STFramework*> SymbolsTable::getDependencies(const string &framework){
    if (!existsFramework(framework)) return list<STFramework*>();
    STFramework* framework_ptr = getFramework(framework);
 
	return framework_ptr->getDependencies();
}



/**
* addLabel
*
* Add a new label to the framework namespace
*
* @param framework: The name of a framework.
* @param newLabel: the name of the new label.
*
* @return true if the operation was performed without failures.
*/
bool SymbolsTable::addLabel(const string &framework, const string &newLabel){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->addLabel(newLabel);
}
 
 
 
/**
* existsLabel
*
* Check if a label exists in the framework namespace
*
* @param framework: The name of a framework.
* @param label: the name of the label to check.
*
* @return true if the label exists.
*/
bool SymbolsTable::existsLabel(const string &framework, const string &label){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->existsLabel(label);
}
 
 
 
/**
* setRecursive
*
* Set a given framework as recursive.
*
* @param framework: The name of a framework.
*/
void SymbolsTable::setRecursive(const string &framework){
    if (!existsFramework(framework)) return;
    STFramework* framework_ptr = getFramework(framework);
 
	return framework_ptr->setRecursive();
}



/**
* isRecursive
*
* Check if a given framework is recursive.
*
* @param framework: The name of a framework.
*
* @return true if the framework is recursive.
*/
bool SymbolsTable::isRecursive(const string &framework){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
	return framework_ptr->isRecursive();
}



/**
* setAsDefined
*
* Set a given framework as defined.
*
* @param framework: The name of a framework.
*/
void SymbolsTable::setAsDefined(const string &framework){
    if (!existsFramework(framework)) return;
    STFramework* framework_ptr = getFramework(framework);
 
	return framework_ptr->setAsDefined();
}



/**
* isDefined
*
* Return true if the given framework is defined.
*
* @param framework: The name of a framework.
*
* @return true if the framework is defined.
*/
bool SymbolsTable::isDefined(const string &framework){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
	return framework_ptr->isDefined();
}



/**
* addSymbol
*
* Adds a new symbol to a framework with given names
*
* @param lex: The name of the symbol
* @param framework: The name of a framework
*
* @return true if the operation was performed without failures
*/
bool SymbolsTable::addSymbol(const string &lex, const string &framework) {
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->addSymbol(lex);
}
 
 
 
/**
* addSymbol
*
* Adds a new symbol with a type to a framework with given names
*
* @param lex: The name of the symbol
* @param type: The name of the type
* @param framework: The name of a framework
*
* @return true if the operation was performed without failures
*/
bool SymbolsTable::addSymbol(const string &lex, const string &type, const string &framework) {
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    if (!framework_ptr->addSymbol(lex)) return false;
 
    return framework_ptr->setType(lex, type);
}
 
 
 
/**
* addSymbol
*
* Adds a new symbol with a type to a framework with given names
*
* @param lex: The name of the symbol
* @param type: The name of the type
* @param framework: The name of a framework
*
* @return true if the operation was performed without failures
*/
bool SymbolsTable::addSymbol(const string &lex, const QualifiedType &inType, const string &framework) {
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->addSymbol(lex, inType);
}
 
 
 
/**
* getSymbol
*
* Gets a pointer to symbol with given framework and name.
*
* @param lex: The name of the symbol.
* @param framework: The name of a framework.
*
* @return the symbol if the operation was performed without failures.
*/
STEntry* SymbolsTable::getSymbol(const string &lex, const string &framework){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->getSymbol(lex);
}



/**
* getSymbols
*
* Gets a pointer to the symbols with given framework.
*
* @param framework: The name of a framework.
*
* @return the symbol list if the operation was performed without failures.
*/
list <STEntry*> SymbolsTable::getSymbols(const string &framework){
    if (!existsFramework(framework)) return list<STEntry*>();
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->getSymbols();
}



/**
* delSymbol
*
* Deletes a symbol in a given framework.
*
* @param lex: The name of the symbol
* @param framework: The name of a framework.
*
* @return true if the operation was performed without failures.
*/
bool SymbolsTable::delSymbol(const string &lex, const string &framework){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
	return framework_ptr->delSymbol(lex);
}



/**
* existsSymbol
*
* Determines if exists a symbol in a framework with given names
*
* @param lex: The name of the symbol
* @param framework: The name of a framework
*
* @return true if exists a symbol in the framework with given names
*/
bool SymbolsTable::existsSymbol(const string &lex, const string &framework) {
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->existsSymbol(lex);
}
 
 
 
/**
* setType
*
* Set a type to an existing symbol in a framework with given names
*
* @param lex: The name of the symbol
* @param type: The name of the type
* @param framework: The name of a framework
*
* @return true if the operation was performed without failures
*/
bool SymbolsTable::setType(const string &lex, const string &type, const string &framework) {
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->setType(lex, type);
}
 
 
 
/**
* getType
*
* Get a type from an existing symbol in a framework with given names
*
* @param lex: The name of the symbol
* @param framework: The name of a framework
*
* @return the type if the operation was performed without failures. Otherwise return ""
*/
QualifiedType SymbolsTable::getType(const string &lex, const string &framework){
    if (!existsFramework(framework)) return QualifiedType("");
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->getType(lex);
}
 
 
 
/**
* setPointerDepth
*
* Set the pointer depth to an existing symbol in a framework with given names
*
* @param lex: The name of the symbol
* @param type: The name of the type
* @param framework: The name of a framework
*
* @return true if the operation was performed without failures
*/
bool SymbolsTable::setPointerDepth(const string &lex, const string &framework, const int depth) {
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->setPointerDepth(lex, depth);
}
 
 
 
/**
* getPointerDepth
*
* Get the pointer depth from an existing symbol in a framework with given names
*
* @param lex: The name of the symbol
* @param framework: The name of a framework
*
* @return the pointer depth if the operation was performed without failures. Otherwise return -2
*/
int SymbolsTable::getPointerDepth(const string &lex, const string &framework){
    if (!existsFramework(framework)) return -2;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->getPointerDepth(lex);
}
 
 
 
/**
* isVolatile
*
* Check if a certain symbol in a certain framework is volatile.
*
* @param lex: The name of the symbol.
* @param framework: The name of a framework.
*
* @return true if the symbol in the framework is volatile
*/
bool SymbolsTable::isVolatile(const string &lex, const string &framework){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->isVolatile(lex);
}
 
 
 
/**
* setVolatile
*
* Set a certain symbol in a certain framework as volatile.
*
* @param lex: The name of the symbol.
* @param framework: The name of a framework.
*
* @return true if the operation was performed without failures.
*/
bool SymbolsTable::setVolatile(const string &lex, const string &framework){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->setVolatile(lex);
}
 
 
 
/**
* isConst
*
* Check if a certain symbol in a certain framework is const.
*
* @param lex: The name of the symbol.
* @param framework: The name of a framework.
*
* @return true if the symbol in the framework is const
*/
bool SymbolsTable::isConst(const string &lex, const string &framework){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->isConst(lex);
}
 
 
 
/**
* setConst
*
* Set a certain symbol in a certain framework as const.
*
* @param lex: The name of the symbol.
* @param framework: The name of a framework.
*
* @return true if the operation was performed without failures.
*/
bool SymbolsTable::setConst(const string &lex, const string &framework){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->setConst(lex);
}
 
 
 
/**
* isLiteral
*
* Check if a certain symbol in a certain framework is literal.
*
* @param lex: The name of the symbol.
* @param framework: The name of a framework.
*
* @return true if the symbol in the framework is literal
*/
bool SymbolsTable::isLiteral(const string &lex, const string &framework){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->isLiteral(lex);
}
 
 
 
/**
* setLiteral
*
* Set a certain symbol in a certain framework as literal.
*
* @param lex: The name of the symbol.
* @param framework: The name of a framework.
*
* @return true if the operation was performed without failures.
*/
bool SymbolsTable::setLiteral(const string &lex, const string &framework){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->setLiteral(lex);
}
 
 
 
/**
* isUnsigned
*
* Check if a certain symbol in a certain framework is unsigned.
*
* @param lex: The name of the symbol.
* @param framework: The name of a framework.
*
* @return true if the symbol in the framework is unsigned
*/
bool SymbolsTable::isUnsigned(const string &lex, const string &framework){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->isUnsigned(lex);
}
 
 
 
/**
* setUnsigned
*
* Set a certain symbol in a certain framework as unsigned.
*
* @param lex: The name of the symbol.
* @param framework: The name of a framework.
*
* @return true if the operation was performed without failures.
*/
bool SymbolsTable::setUnsigned(const string &lex, const string &framework){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->setUnsigned(lex);
}
 
 
 
/**
* getNUses
*
* Get the number of uses of a certain symbol in a certain framework.
*
* @param lex: The name of the symbol.
* @param framework: The name of a framework.
*
* @return the current uses if the operation was performed without failures or -1 otherwise.
*/
int SymbolsTable::getNUses(const string &lex, const string &framework){
    if (!existsFramework(framework)) return -1;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->getNUses(lex);
}
 
 
 
/**
* setNUses
*
* Set the number of uses of a certain symbol in a certain framework.
*
* @param lex: The name of the symbol.
* @param framework: The name of a framework.
* @param inNUses: The number of uses to set.
*
* @return true if the operation was performed without failures.
*/
bool SymbolsTable::setNUses(const string &lex, const string &framework, int inNUses){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->setNUses(lex, inNUses);
}
 
 
 
/**
* incNUses
*
* Increment the number of uses of a certain symbol in a certain framework with a certain value.
*
* @param lex: The name of the symbol.
* @param framework: The name of a framework.
* @param increment: The value of the increment.
*
* @return true if the operation was performed without failures.
*/
bool SymbolsTable::incNUses(const string &lex, const string &framework, int increment){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    int oldValue = framework_ptr->getNUses(lex);
    if (oldValue < 0) return false;
 
    return framework_ptr->setNUses(lex, oldValue + increment);
}
 
 
 
/**
* getAddress
*
* Get the current proposed memory address of a certain symbol in a certain
* framework.
*
* @param lex: The name of the symbol.
* @param framework: The name of a framework.
*
* @return the current proposed memory address value if the operation was performed
*        without failures or -1 otherwise.
*/
int SymbolsTable::getAddress(const string &lex, const string &framework){
    if (!existsFramework(framework)) return -1;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->getAddress(lex);
}
 
 
 
/**
* setAddress
*
* Set the current proposed memory address of a certain symbol in a certain
* framework.
*
* @param lex: The name of the symbol.
* @param framework: The name of a framework.
* @param address: The address to set on.
*
* @return true if the operation was performed without failures.
*/
bool SymbolsTable::setAddress(const string &lex, const string &framework, int address){
    if (!existsFramework(framework)) return false;
    STFramework* framework_ptr = getFramework(framework);
 
    return framework_ptr->setAddress(lex, address);
}



/**
* setMemSize
*
* Store the total quantity value of memory used by the (overlapped) symbols in bytes
*
* @param inSize: 
*/
void SymbolsTable::setMemSize(int inSize){ memSize = inSize; }



/**
* printSymbolsTable
*
* Prety printer.
*
* @return string containing the text formated symbols table.
*/
string SymbolsTable::toString() {
 
    int codeSize = 0;
	ostringstream output;
	const string  boxLine =
		"+--------------------------------------"\
		"---------------------------------------+";


	for_each(frameworks.begin(), frameworks.end(),
		[&codeSize](pair<string, STFramework*> const x){
		codeSize += x.second->getCodeSize();
	});

    output << boxLine << endl;
    output << "|" << setw(78) << right << "|" << endl;
    output << "| SYMBOLS TABLE" << right << setw(64) << " |" << endl;
    output << "|" << setw(78) << right << "|" << endl;
	output << "| # of frameworks: " << setw(5) << frameworks.size() << right << setw(55) << " |" << endl;
    output << "| RAM variables:   " << setw(5) << memSize  << " bytes" << right << setw(49) << " |" << endl;
    output << "| RAM code size:   " << setw(5) << codeSize << " bytes" << right << setw(49) << " |" << endl;
    output << "|" << setw(78) << right << "|" << endl;
    output << boxLine << endl << endl;

    for_each(frameworks.begin(), frameworks.end(),
		[this, &output](pair<string, STFramework*> const x){
		x.second->calculateSize(this);
        output << x.second->toString();
    });
 
    output << boxLine << endl;
    output << "|" << setw(78) << right << "|" << endl;
    output << "| \\SYMBOLS TABLE" << right << setw(63) << " |" << endl;
    output << "|" << setw(78) << right << "|" << endl;
    output << boxLine << endl << endl;
 
    return output.str();
}
