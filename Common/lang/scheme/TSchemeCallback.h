// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef TSchemeCallback_H
#define TSchemeCallback_H

BEGIN_NAMESPACE_FIVEL

//////////
// A C++ wrapper which builds lists of arguments to pass to callbacks.
//
class TSchemeCallbackArgumentList : public TCallbackArgumentList
{
	//////////
	// The first time this argument list is passed to a callback,
	// we freeze the argument list and REVERSE! it destructively.
	//
	bool mIsFrozen;
	
	//////////
	// The list of arguments we'll pass to the next invocation of Run(),
	// stored in reverse order.
	//
	TSchemePtr<Scheme_Object> mArguments;

	//////////
	// We use this variable to store partially constructed lists (in
	// reverse order) between calls to BeginListArg and EndListArg.
	//
	TSchemePtr<Scheme_Object> mListArgument;

	//////////
	// Internal function to add a single argument.
	//
	void AddArg(Scheme_Object *inArg);

public:
	TSchemeCallbackArgumentList();

	// For documentation of these virtual methods, see TInterpreter.h.
	virtual void AddStringArg(const std::string &inArg);
	virtual void AddSymbolArg(const std::string &inArg);
	virtual void AddInt32Arg(int inArg);
	virtual void AddDoubleArg(double inArg);
	virtual void AddBoolArg(bool inArg);
	virtual void BeginListArg();
	virtual void EndListArg();

	Scheme_Object *GetArgs();
};

//////////
// A C++ wrapper for a zero-argument Scheme callback function (a "thunk").
//
class TSchemeCallback : public TCallback
{
	//////////
	// Our callback object.  Note that we need to use a TSchemePtr to
	// prevent mCallback from being garbage-collected, because we're
	// a heap-based object.
	//
	TSchemePtr<Scheme_Object> mCallback;

public:
	TSchemeCallback(Scheme_Object *inCallback) : mCallback(inCallback) {}

	// For documentation of these virtual methods, see TInterpreter.h.
	virtual TCallbackArgumentList *MakeArgumentList();
	virtual void Run(TCallbackArgumentList *inArguments = NULL);
	virtual std::string PrintableRepresentation() { return "#<thunk>"; }
};

END_NAMESPACE_FIVEL

#endif // TSchemeCallback_H
