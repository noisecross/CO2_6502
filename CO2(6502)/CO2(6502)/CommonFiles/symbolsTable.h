/**
* |------------------------------------------|
* | CO2 6502, COMPILER OPTIMIZER TO 6502     |
* | File: symbolstable.h                     |
* | v1.0, July 2012                          |
* | Author: Emilio Arango Delgado de Mendoza |
* |------------------------------------------|
*/

#ifdef __cplusplus

#ifndef SymbolsTableH
#define SymbolsTableH



#include <list>
#include <string>
#include <sstream>
#include <unordered_map>
#include <unordered_set>

using namespace std;

/* Symbols table constants */
#define TYPE_ERR         "$type_err"
#define TYPE_OK          "$type_ok"
#define TYPE_LITERAL     "$numeric_literal"
#define GLOBAL_FRAMEWORK "$GLOBAL"



/**
* QualifiedType
*
* This class represent one qualified type and all its relevant data
*/
class QualifiedType{

public:
	string type;
	int    arrSize;
    int    pointerDepth;
	bool   b_unsigned;
	bool   b_const;
	bool   b_volatile;
	bool   b_interrupt;

	QualifiedType();
	QualifiedType(const string &);
	~QualifiedType();

	bool   sameOutput(const QualifiedType &);
	bool   sameInput(const QualifiedType &);
	bool   equals(const QualifiedType &);

	string toString() const;
};



/**
* STEntry
*
* This class represent one language symbol and all its related info
*/
class STEntry{

private:
	QualifiedType qType;
	string        framework;
	int           n_uses;
	bool          trueConstant;
	bool          overlappable;
	int           address;

public:
	string        lex;

	STEntry(const string &, const string &);
	STEntry(const string &, const string &, const QualifiedType &);
	~STEntry();

	QualifiedType getType();
	void          setType(const string &);
	int           getPointerDepth();
	void          setPointerDepth(int);
	bool          isVolatile();
	void          setVolatile();
	bool          isUnsigned();
	void          setUnsigned();
	bool          isConst();
	void          setConst();
	bool          isOverlappable();
	void          setOverlappable(bool);
	bool          isLiteral();
	void          setLiteral();
	int           getNUses();
	void          setNUses(int);
	int           getAddress();
	void          setAddress(int);
	string        getFramework();

	string        toString() const;
};



/**
* STFramework
*
* This class represent one function framework and stores its symbols and all its related info
*/
class SymbolsTable;

class STFramework{

private:
	int                             id;
	string                          name;
	QualifiedType                   outputType;
	list<QualifiedType>             inputType;
	list<string>                    inputSymbols;
	list<list<STEntry*>>            inputArguments;
	unordered_map<string, STEntry*> symbols;
	unordered_set<string>           labels;
	list<STFramework*>              dependencies;
	int                             codeSize;
	int                             symbolsSize;
	bool                            defined;
	bool                            recursive;

public:
	STFramework(const string &, const int);
	~STFramework();

	string              getName();
	int                 getId();
	bool                setOutputType(const QualifiedType &);
	QualifiedType       getOutputType();
	bool                addInputType(const QualifiedType &);
	bool                addInputType(const QualifiedType &, const string&);
	bool                addInputArgument(STEntry* argument, int position);
	list<STEntry*>      getInputArguments(int position);
	list<QualifiedType> getInputType();
	list<string>        getInputSymbols();
	void                resetInputType();
	int                 calculateSize(SymbolsTable *);
	void                setCodeSize(int);
	int                 getCodeSize();
	bool                addDependency(STFramework*);
	list<STFramework*>  getDependencies();
	void                setRecursive();
	bool                isRecursive();
	void                setAsDefined();
	bool                isDefined();

	bool                addSymbol(const string &);
	bool                addSymbol(const string &, const QualifiedType &);
	STEntry*            getSymbol(const string &);
	list<STEntry*>      getSymbols();
	bool                delSymbol(const string &);
	bool                existsSymbol(const string &);
	QualifiedType       getType(const string &);
	bool                setType(const string &, const string &);
	int                 getPointerDepth(const string &);
	bool                setPointerDepth(const string &, int);
	bool		        isVolatile(const string &);
	bool		        setVolatile(const string &);
	bool                isConst(const string &);
	bool                setConst(const string &);
	bool                isLiteral(const string &);
	bool                setLiteral(const string &);
	bool                isUnsigned(const string &);
	bool                setUnsigned(const string &);
	int                 getNUses(const string &);
	bool                setNUses(const string &, int);
	int                 getAddress(const string &);
	bool                setAddress(const string &, int);
	bool                addLabel(const string &);
	bool                existsLabel(const string &);

	string              toString() const;
};



/**
* Type
*
* This class contains a pair with a type name and its size in bytes.
*/
class Type {

public:
	string type;
	int    size;
	Type(const string &, int);
	~Type();
};

	
	
/**
* SymbolsTable
*
* This class represent the symbols table and stores all the frameworks and symbols
*/
class SymbolsTable {

private:
	unordered_map <string, STFramework*> frameworks;
	unordered_map <string, string>       constants;
	list <Type>                          types;
	list <string>                        tempIdentifiers;
	list <string>                        tempTypes;
	list <string>                        tempLiterals;
	int                                  idCounter;
	int                                  memSize;

public:
	SymbolsTable();
	~SymbolsTable();

	string              currentFramework;

	bool                addFramework(const string &framework);
	bool                existsFramework(const string &framework);
	STFramework*        getFramework(const string &framework);
	list <STFramework*> getFrameworks();
	bool                addConstant(const string &name, const string &constant);
	bool                existsConstant(const string &name);
	string              getConstant(const string &name);
	bool                addType(const string &name, int size);
	bool                existsType(const string &name);
	int                 getTypeSize(const string &name);
	int                 getTypeSize(const QualifiedType &name);
	void                pushTempIdentifier(const string &name);
	string              popTempIdentifier();
	void                pushTempLiteral(const string &name);
	string              popTempLiteral();
	void                pushTempType(const string &name);
	string              popTempType();
	void                resetNUses();
	void                setMemSize(int);

	int                 getId(const string &framework);
	QualifiedType       getOutputType(const string &framework);
	bool                setOutputType(const string &framework, const QualifiedType &type);
	list<QualifiedType> getInputType(const string &framework);
	list<string>        getInputSymbols(const string &framework);
	bool                addInputType(const string &framework, const QualifiedType &);
	bool                addInputType(const string &framework, const QualifiedType &, const string &);
	bool                addInputArgument(const string &framework, STEntry* argument, int position);
	list<STEntry*>      getInputArguments(const string &framework, int position);
	void                resetInputType(const string &framework);
	int                 calculateSize(const string &framework);
	bool                addDependency(const string &framework, STFramework* dependency);
	bool                addDependency(const string &framework, const string &dependency);
	list<STFramework*>  getDependencies(const string &framework);
	bool                addLabel(const string &framework, const string &);
	bool                existsLabel(const string &framework, const string &);
	void                setRecursive(const string &framework);
	bool                isRecursive(const string &framework);
	void                setAsDefined(const string &framework);
	bool                isDefined(const string &framework);

	bool                addSymbol(const string &lex, const string &framework);
	bool                addSymbol(const string &lex, const string &type, const string &framework);
	bool                addSymbol(const string &lex, const QualifiedType &, const string &framework);
	STEntry*            getSymbol(const string &lex, const string &framework);
	list <STEntry*>     getSymbols(const string &framework);
	bool                delSymbol(const string &lex, const string &framework);
	bool                existsSymbol(const string &lex, const string &framework);
	QualifiedType       getType(const string &lex, const string &framework);
	bool                setType(const string &lex, const string &type, const string &framework);
	int                 getPointerDepth(const string &lex, const string &);
	bool                setPointerDepth(const string &lex, const string &, int);
	bool                isVolatile(const string &lex, const string &framework);
	bool                setVolatile(const string &lex, const string &framework);
	bool                isConst(const string &lex, const string &framework);
	bool                setConst(const string &lex, const string &framework);
	bool                isLiteral(const string &lex, const string &framework);
	bool                setLiteral(const string &lex, const string &framework);
	bool                isUnsigned(const string &lex, const string &framework);
	bool                setUnsigned(const string &lex, const string &framework);
	int                 getNUses(const string &lex, const string &);
	bool                setNUses(const string &lex, const string &, int);
	bool                incNUses(const string &lex, const string &, int);
	int                 getAddress(const string &lex, const string &framework);
	bool                setAddress(const string &lex, const string &framework, int address);

	string              toString();
};

#endif
#endif
