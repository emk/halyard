//
//	CMacroManager.h
//

#ifndef _H_CMACROMANAGER
#define _H_CMACROMANAGER

#include "KHeader.h"

#include "CIndex.h"

class CMacroManager : public CIndexManager 
{
	public:

		virtual const char	*GetScript(const char *macroname);
		virtual void	MakeNewIndex(CIndexFile *inFile, const char *inName  = NULL,
								int32 inStart = 0, int32 inEnd = 0);
};

extern CMacroManager gMacroManager;

#endif
