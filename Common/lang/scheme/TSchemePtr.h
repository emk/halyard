// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

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
