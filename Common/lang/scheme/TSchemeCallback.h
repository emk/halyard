// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef TSchemeCallback_H
#define TSchemeCallback_H

BEGIN_NAMESPACE_FIVEL

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
	virtual TValue Run(const TValueList &inArguments = TValueList());
	virtual std::string PrintableRepresentation() { return "#<thunk>"; }
};

END_NAMESPACE_FIVEL

#endif // TSchemeCallback_H
