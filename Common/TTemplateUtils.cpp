// -*- Mode: C++; tab-width: 4; -*-

 #include <algorithm>
#include <ctype.h>

#include "TTemplateUtils.h"

USING_NAMESPACE_FIVEL

std::string FIVEL_NS MakeStringLowercase(std::string inString)
{
    std::transform(inString.begin(), inString.end(), inString.begin(),
				   tolower);
	return inString;
}


std::string FIVEL_NS MakeQuotedString(const std::string& inString)
{
	std::string result = "\"";
	for (std::string::size_type pos = 0; pos < inString.length(); pos++)
	{
		char c = inString[pos];
		if (c == '\"')
			result += "\\\"";
		else if (c == '\\')
			result += "\\\\";
		else
			result += c;
	}
	result += "\"";
	return result;
}
