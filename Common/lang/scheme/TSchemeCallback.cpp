// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "CommonHeaders.h"
#include "TSchemeInterpreter.h"
#include "TSchemeConv.h"

USING_NAMESPACE_FIVEL


//=========================================================================
//	TSchemeCallback Methods
//=========================================================================

void TSchemeCallback::Run(const TValueList &inArguments)
{
	// Make sure we have a Scheme interpreter and that it isn't stopped.
	ASSERT(TSchemeInterpreter::HaveInstance());
	ASSERT(!TSchemeInterpreter::GetInstance()->IsStopped());
	
	// TODO - I wish we could do this without consing.
	Scheme_Object *args[2];
	args[0] = mCallback;
	args[1] = TValueToScheme(inArguments);
	TSchemeInterpreter::CallScheme("%kernel-run-callback", 2, args);
}
