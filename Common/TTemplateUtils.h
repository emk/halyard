// -*- Mode: C++; tab-width: 4; -*-

#ifndef TTemplateUtils_H
#define TTemplateUtils_H

#include <string>

#include "TCommon.h"

BEGIN_NAMESPACE_FIVEL

//////////
// Convert an STL string to lowercase.
//
// [in] inString - the string to convert
// [out] return - the input string, as lowercase
//
extern std::string MakeStringLowercase(std::string inString);


//////////
// Convert nested quotes within an STL string into 
// escape-character quotes.
//
// [in] inString - the string to convert
// [out] return - the input string, with escaped quotes
//
extern std::string MakeQuotedString(const std::string &inString); 

//////////
// You can use this class to save and restore a value in an exception-safe
// fashion.  To use:
//
//   int i = 0;
//   StValueRestorer<int> restore_i(i);
//
// When restore_i goes out of scope, i will be reset to its original value.
//
template<class Type>
class StValueRestorer
{
	Type &mLocation;
	Type mSaved;

	DISABLE_COPY_AND_ASSIGN_TMPL(StValueRestorer,StValueRestorer<Type>);

public:
	explicit StValueRestorer(Type &inVariable)
		: mLocation(inVariable), mSaved(inVariable) { }
	~StValueRestorer() { mLocation = mSaved; }
};

END_NAMESPACE_FIVEL

#endif // TTemplateUtils_H
