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

END_NAMESPACE_FIVEL

#endif // TTemplateUtils_H
