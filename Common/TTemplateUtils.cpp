#include <algorithm>
#include <ctype.h>

#include "TTemplateUtils.h"

USING_NAMESPACE_FIVEL

std::string MakeStringLowercase(std::string inString)
{
    std::transform(inString.begin(), inString.end(), inString.begin(),
		   tolower);
	return inString;
}
