// -*- Mode: C++; tab-width: 4; -*-

#ifndef TTemplateUtils_H
#define TTemplateUtils_H

#include <string>
#include <boost/checked_delete.hpp>

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
// This functor can be used to delete all the pointers in a container
// as follows:
//
//   for_each(container.begin, container.end(), DeletePointer())
//
// This code is adapted from Meyer's "Effective STL", and modified
// to use the appropriate boost deletion operators.
//
struct DeletePointer {
	template <typename T>
	void operator() (T* ptr) const
	{
		boost::checked_delete<T>(ptr);
	}
};

//////////
// The same as DeletePointer, but for use with containers of pointers
// TO ARRAYS.
//
struct DeleteArray {
	template <typename T>
	void operator() (T* ptr) const
	{
		boost::checked_array_delete<T>(ptr);
	}
};

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
