//
//	MacUtils.h
//

#include "TCommon.h"
#include "TString.h"

bool 			PathToFSSpec(FiveL::TString &inPath, FSSpec *inSpec);
FiveL::TString	NameFromFSSpec(FSSpec *inSpec);
FiveL::TString	PathFromFSSpec(FSSpec *inSpec);