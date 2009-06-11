// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

BEGIN_NAMESPACE_HALYARD

//////////
/// The number of TSchemePtr objects (of all types) currently in existence.
/// We use this to detect leaked TSchemePtrs that might keep old Scheme
/// heaps from being garbage collected.
///
/// This is not a member variable, because TSchemePtr is a template class,
/// and will therefore be instantiated multiple times.
///
extern int gTSchemePointerCount;

//////////
/// A smart-pointer class which can point to a Scheme object and prevent
/// it from being garbage-collected.  You must use this class to point to
/// a Scheme_Object stored anywhere except the stack (which the Scheme GC
/// automatically scans for us).  So global and heap objects *must* use
/// this class to refer to anything in the Scheme heap.
///
/// This class works like most C++ smart pointer classes, _except_ that we
/// choose to define implicit conversion operators rather than the
/// dereferencing operators ('->' or 'get()') used by most other smart
/// pointer classes.  This is because we almost never point to anything
/// with use member variables, but we do get passed to a lot of API
/// functions that take pointers as arguments.
///
template <class Type>
class TSchemePtr
{
    // mBox points to an immobile, non-collectable Scheme object (the
    // "box").  The box, in turn, contains a single pointer.  The pointer
    // in the box will be updated as needed by the GC.
    //
    // It's important to remember that the pointer, once remove from the
    // box, has a very short shelf life--any function which allocates
    // memory may cause the GC to move the underlying object.
    Type **mBox;
    
    void CreateBox(const Type *inPtr) {
        // Make sure we register inPtr locally before attempting to store
        // it in our box, in case allocating the box moves the pointer.
        // It's entirely possible that scheme_malloc_immobile_box does this
        // for us internally, but since the manual doesn't guarantee it,
        // I'm choosing the defensive approach.
        MZ_GC_DECL_REG(1);
        MZ_GC_VAR_IN_REG(0, inPtr);
        MZ_GC_REG();
        ++gTSchemePointerCount;
        void **box = scheme_malloc_immobile_box(const_cast<Type*>(inPtr));
        mBox = reinterpret_cast<Type**>(box);
        MZ_GC_UNREG();
    }

    void DestroyBox() {
        ASSERT(gTSchemePointerCount > 0);
        scheme_free_immobile_box(reinterpret_cast<void**>(mBox));
        --gTSchemePointerCount;
    }

    void Set(Type *inPtr) { *mBox = inPtr; }
    Type *Get() const { return *mBox; }

public:
    TSchemePtr() { CreateBox(NULL); }

    // Create a TSchemePtr.
    //
    // MANUAL GC PROOF REQUIRED - This code is only safe if
    // scheme_malloc_immobile_box is unable to trigger a garbage collection
    // (which _should be_ true in the current GC, because it allocates
    // boxes on the C heap, but go check this if you're reviewing the
    // code).  If scheme_malloc_immobile_box can trigger a GC, then mark
    // this constructor 'explict', and go find all the callsites which look
    // like this:
    //
    //   Scheme_Object *obj1 = NULL, obj2 = NULL;
    //   // Register obj1 and obj2.
    //   fn_with_tschemeptr_args(obj1, obj2);
    //
    // This callsite is actually an abbreviation for:
    //
    //   fn_with_tschemeptr_args(TSchemePtr(obj1), TSchemePtr(obj2));
    //
    // ...which may not be safe if TSchemePtr(...) triggers a collection.
    TSchemePtr(Type *inPtr) { CreateBox(inPtr); }

    TSchemePtr(const TSchemePtr &inSchemePtr) { CreateBox(inSchemePtr); }
    ~TSchemePtr() { DestroyBox(); }

    /// Convert a non-const TSchemePtr<Type> to Type *.
    operator Type*() { return Get(); }
    /// Convert a const TSchemePtr<Type> to const Type *.
    operator const Type*() const { return Get(); }
    TSchemePtr<Type> &operator=(Type *inPtr) { Set(inPtr); return *this; }
    TSchemePtr<Type> &operator=(const TSchemePtr<Type> &inSchemePtr)
        { Set(inSchemePtr); return *this; }
};

END_NAMESPACE_HALYARD

#endif // TSchemePtr_H
