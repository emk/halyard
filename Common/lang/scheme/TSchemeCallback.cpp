// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "CommonHeaders.h"
#include "TSchemeInterpreter.h"

USING_NAMESPACE_FIVEL


//=========================================================================
//	TSchemeCallbackArgumentList Methods
//=========================================================================

TSchemeCallbackArgumentList::TSchemeCallbackArgumentList()
	: mIsFrozen(false), mArguments(scheme_null), mListArgument(NULL)
{
	// All done.
}

void TSchemeCallbackArgumentList::AddArg(Scheme_Object *inArg)
{
	ASSERT(!mIsFrozen);

	// Push the argument onto the appropriate list.
	if (mListArgument)
		mListArgument = scheme_make_pair(inArg, mListArgument);
	else
		mArguments = scheme_make_pair(inArg, mArguments);		
}

void TSchemeCallbackArgumentList::AddStringArg(const std::string &inArg)
{
	AddArg(scheme_make_string(inArg.c_str()));
}

void TSchemeCallbackArgumentList::AddSymbolArg(const std::string &inArg)
{
	AddArg(scheme_intern_symbol(inArg.c_str()));
}

void TSchemeCallbackArgumentList::AddInt32Arg(int inArg)
{
	AddArg(scheme_make_integer_value(inArg));
}

void TSchemeCallbackArgumentList::AddDoubleArg(double inArg)
{
	AddArg(scheme_make_double(inArg));
}

void TSchemeCallbackArgumentList::AddBoolArg(bool inArg)
{
	AddArg(inArg ? scheme_true : scheme_false);
}

void TSchemeCallbackArgumentList::BeginListArg()
{
	ASSERT(!mIsFrozen);
	ASSERT(mListArgument == NULL);
	mListArgument = scheme_null;
}

void TSchemeCallbackArgumentList::EndListArg()
{
	ASSERT(!mIsFrozen);
	ASSERT(mListArgument != NULL);

	// Reverse the list argument.
	Scheme_Object *args[1];
	args[0] = mListArgument;
	mListArgument = NULL;
	AddArg(TSchemeInterpreter::CallScheme("%kernel-reverse!", 1, args));
}

Scheme_Object *TSchemeCallbackArgumentList::GetArgs()
{
	ASSERT(mListArgument == NULL);

	// If we haven't frozen the list yet, freeze it and reverse it.
	if (!mIsFrozen)
	{
		Scheme_Object *args[1];
		args[0] = mArguments;
		mArguments =
			TSchemeInterpreter::CallScheme("%kernel-reverse!", 1, args);
		mIsFrozen = true;
	}

	return mArguments;	
}


//=========================================================================
//	TSchemeCallback Methods
//=========================================================================

TCallbackArgumentList *TSchemeCallback::MakeArgumentList()
{
	return new TSchemeCallbackArgumentList();
}

void TSchemeCallback::Run(TCallbackArgumentList *inArguments)
{
	// Make sure we have a Scheme interpreter and that it isn't stopped.
	ASSERT(TSchemeInterpreter::HaveInstance());
	ASSERT(!TSchemeInterpreter::GetInstance()->IsStopped());
	
	TSchemeCallbackArgumentList *callback_args =
		dynamic_cast<TSchemeCallbackArgumentList*>(inArguments);

	Scheme_Object *args[2];
	args[0] = mCallback;
	args[1] = callback_args ? callback_args->GetArgs() : scheme_null;
	TSchemeInterpreter::CallScheme("%kernel-run-callback", 2, args);
}
