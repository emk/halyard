//
//	CMacroManager.h
//

#ifndef _H_CMACROMANAGER
#define _H_CMACROMANAGER

#include "THeader.h"

#include "TIndex.h"

BEGIN_NAMESPACE_FIVEL

class CMacroManager : public TIndexManager 
{
	public:

		virtual const char	*GetScript(const char *macroname);
		virtual void	ProcessTopLevelForm(TIndexFile *inFile, const char *inName  = NULL,
								            int32 inStart = 0, int32 inEnd = 0);
};

extern CMacroManager gMacroManager;

END_NAMESPACE_FIVEL

#endif
