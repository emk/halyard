// -*- Mode: C++; tab-width: 4; -*-

#if !defined (_TInterpreter_h_)
#define _TInterpreter_h_

#include "TCommon.h"

BEGIN_NAMESPACE_FIVEL

//////////
// TInterpreter provides an abstract interface to a programming language
// interpreter used by the 5L engine.  In theory, it should be possible
// to change 5L's scripting language by providing a new implementation
// of this class.
//
class TInterpreter
{
public:
	TInterpreter() {}
	virtual ~TInterpreter() {}
};

END_NAMESPACE_FIVEL

#endif // TInterpreter
