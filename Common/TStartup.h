// -*- Mode: C++; tab-width: 4; -*-

#if !defined (_TStartup_h_)
#define _TStartup_h_

#include "TCommon.h"

BEGIN_NAMESPACE_FIVEL

//////////
// Initialize the various modules of the Common/ library.
// If you want to call FileSystem::SetBaseDirectory, do it
// before calling this function.
//
extern void InitializeCommonCode();

END_NAMESPACE_FIVEL

#endif // TStartup
