/**
* |------------------------------------------|
* | CO2 6502, COMPILER OPTIMIZER TO 6502     |
* | File: errmanager.cpp                     |
* | v1.0, May 2012                           |
* | Author: Emilio Arango Delgado de Mendoza |
* |------------------------------------------|
*/

#include "errManager.h"
#include <algorithm>
#include <sstream>



/**
* ErrManager
* 
* Class constructor.
*
* @return ErrManager object.
*/
ErrManager::ErrManager(){
	bWarnings = true;
	bInfos = true;
}



/**
* ErrManager
* 
* Class constructor.
*
* @param bW: Warning messages are (not) managed.
* @param bI: Info messages are (not) managed.
*
* @return ErrManager object.
*/
ErrManager::ErrManager(bool bW, bool bI){
	bWarnings = bW;
	bInfos = bI;
}



/**
* ErrManager
* 
* Class destructor.
*/
ErrManager::~ErrManager(){
	errList.clear();
	warList.clear();
	infList.clear();
}



/**
* existsError
* 
* @return value which indicates if an error have been found.
*/
bool ErrManager::existsError(){
	return !errList.empty();
}



/**
* addError
* 
* An error message is stored into the manager.
*/
void ErrManager::addError(const string &message){
	errList.push_back("Error: " + message);
}



/**
* addWarning
* 
* If warnings allowed, a warning message is stored into the manager.
*/
void ErrManager::addWarning(const string &message){
	if (bWarnings) warList.push_back("Warn: " + message);
}



/**
* addInfo
* 
* If info messages allowed, a warning message is stored into the manager.
*/
void ErrManager::addInfo(const string &message){
	if (bInfos) infList.push_back("Info: " + message);
}



/**
* toString
* 
* Get all the erroneous info founded and stored into the manager.
*
* @return a formatted string containing all the relevant stored messages
*/
string ErrManager::toString(){
	ostringstream output;

	auto f_concat = [&output](const string &x){ output << x << endl; };

	for_each(errList.begin(), errList.end(), f_concat);
	if (!errList.empty()) output << endl;

	for_each(warList.begin(), warList.end(), f_concat);
	if (!warList.empty()) output << endl;

	for_each(infList.begin(), infList.end(), f_concat);
	if (!infList.empty()) output << endl;

	return output.str();
}
