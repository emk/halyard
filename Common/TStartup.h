// -*- Mode: C++; tab-width: 4; -*-

#if !defined (_TStartup_h_)
#define _TStartup_h_

#include "TCommon.h"
#include "TInterpreter.h"

BEGIN_NAMESPACE_FIVEL

//////////
// Initialize the various modules of the Common/ library.
// If you want to call FileSystem::SetBaseDirectory, do it
// before calling this function.
//
extern void InitializeCommonCode();

//////////
// If the current program looks like a Scheme script, attempt
// to start a Scheme interpreter.  Otherwise, return NULL.
//
extern TInterpreterManager *
MaybeGetSchemeInterpreterManager(TInterpreter::SystemIdleProc inIdleProc);

END_NAMESPACE_FIVEL

#endif // TStartup
