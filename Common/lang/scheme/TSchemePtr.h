// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef TSchemePtr_H
#define TSchemePtr_H

#include "scheme.h"

BEGIN_NAMESPACE_FIVEL

//////////
// A smart-pointer class which can point to a Scheme object and prevent
// it from being garbage-collected.  You must use this class to point to
// a Scheme_Object stored anywhere except the stack (which the Scheme GC
// automatically scans for us).  So global and heap objects *must* use
// this class to refer to anything in the Scheme heap.
//
template <class Type>
class TSchemePtr
{
	Type *mPtr;
	
	void Set(Type *inPtr)
	{
		if (mPtr == inPtr)
			return;
		if (mPtr)
			scheme_gc_ptr_ok(mPtr);
		mPtr = inPtr;
		if (inPtr)
			scheme_dont_gc_ptr(mPtr);
	}

public:
	TSchemePtr() : mPtr(NULL) {}
	TSchemePtr(Type *inPtr) : mPtr(NULL) { Set(inPtr); }
	TSchemePtr(const TSchemePtr &inSchemePtr) : mPtr(NULL)
		{ Set(inSchemePtr.mPtr); }
	operator Type*() { return mPtr; }
	operator const Type*() const { return mPtr; }
	TSchemePtr<Type> &operator=(Type *inPtr) { Set(inPtr); return *this; }
	TSchemePtr<Type> &operator=(const TSchemePtr &inPtr)
		{ Set(inSchemePtr.mPtr); return *this; }
    bool operator<(const TSchemePtr<Type> &inRight) const
        { return mPtr < inRight.mPtr; }
};

END_NAMESPACE_FIVEL

#endif // TSchemePtr_H
