// -*- Mode: C++; tab-width: 4; -*-

#if !defined (_TStartup_h_)
#define _TStartup_h_

#include "TInterpreter.h"

// On the Macintosh, we need to manually set the stack base so the
// Boehm GC doesn't get confused (it doesn't know how to detect it
// correctly).
#ifdef FIVEL_PLATFORM_MACINTOSH
#   include <scheme.h>
#   define FIVEL_SET_STACK_BASE() \
	    do { \
            int dummy; \
	        scheme_set_stack_base(&dummy, 0); \
        }  while (0)
#else // !FIVEL_PLATFORM_MACINTOSH
#   define FIVEL_SET_STACK_BASE() ((void) 0)
#endif // !FIVEL_PLATFORM_MACINTOSH

BEGIN_NAMESPACE_FIVEL

//////////
// Initialize the various modules of the Common/ library.
// If you want to call FileSystem::SetBaseDirectory, do it
// before calling this function.
//
extern void InitializeCommonCode();

//////////
// Create a Scheme interpreter manager.
//
extern TInterpreterManager *
GetSchemeInterpreterManager(TInterpreter::SystemIdleProc inIdleProc);

//////////
// If the current program looks like a Scheme script, attempt
// to start a Scheme interpreter.  Otherwise, return NULL.  This function
// can be used to support two languages in a single engine.
//
extern TInterpreterManager *
MaybeGetSchemeInterpreterManager(TInterpreter::SystemIdleProc inIdleProc);

END_NAMESPACE_FIVEL

#endif // TStartup
