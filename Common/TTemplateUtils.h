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

END_NAMESPACE_FIVEL

#endif TTemplateUtils_H
