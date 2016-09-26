/**
* |------------------------------------------|
* | CO2 6502, COMPILER OPTIMIZER TO 6502     |
* | File: errManager.h                       |
* | v1.0, May 2012                           |
* | Author: Emilio Arango Delgado de Mendoza |
* |------------------------------------------|
*/

#ifdef __cplusplus

#ifndef ErrManagerH
#define ErrManagerH



#include "../BackEnd/beErrMsg.h"
#include "../FrontEnd/feErrMsg.h"

#include <list>
#include <string>
using namespace std;



/**
* ErrManager
*
* This class stores the messages produced during the analysis phase.
*/
class ErrManager {

private:
	bool          bWarnings;
	bool          bInfos;
	list <string> errList;
	list <string> warList;
	list <string> infList;

public:
	ErrManager();
	ErrManager(bool, bool);
	~ErrManager();
	bool   existsError();
	void   addError(const string &message);
	void   addWarning(const string &message);
	void   addInfo(const string &message);
	string toString();
};



#endif
#endif
