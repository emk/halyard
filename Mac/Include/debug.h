//
//	debug.h
//

#ifndef _H_DEBUG
#define _H_DEBUG

// This is something that PowerPlant defines for debugging.
#ifdef Debug_Throw

#define	DEBUG_5L
#define DEBUG_5L_SCRIPT

#define ASSERT(x)	if (!(x)) prerror("Assertion failed in file %s, line %d", __FILE__, __LINE__);

#else

#undef DEBUG_5L
#undef DEBUG_5L_SCRIPT

#define ASSERT(x)

#endif

#endif
