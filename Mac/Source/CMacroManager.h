//
//	CMacroManager.h
//

#ifndef _H_CMACROMANAGER
#define _H_CMACROMANAGER

#include "Mac5L.h"

#include "CIndex.h"

class CMacroManager : public CIndexManager 
{
	public:

		virtual char	*GetScript(const char *macroname);
		OVERRIDE void	MakeNewIndex(char *name, int32 start, int32 end);
};

extern CMacroManager gMacroManager;

#endif
