//
//	MacUtils.h
//

#include "KCommon.h"
#include "KString.h"

bool 	PathToFSSpec(KString &inPath, FSSpec *inSpec);
KString	NameFromFSSpec(FSSpec *inSpec);
KString	PathFromFSSpec(FSSpec *inSpec);