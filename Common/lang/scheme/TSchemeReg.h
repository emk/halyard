// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
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

#ifndef TSchemeReg_H
#define TSchemeReg_H

#include "scheme.h"

BEGIN_NAMESPACE_HALYARD

//////////
/// This is an exception-safe replacement for MZ_GC_REG and friends.  It is
/// used as follows:
///
///   void foo(Scheme_Object *obj, int argc, Scheme_Object **argv) {
///       // First, declare locals and set them to NULL.
///       Scheme_Object *local = NULL;
///
///       // Next, register our parameters and local variables.  Here,
///       // 2 is the number of calls to 'var', and 1 is the number of calls
///       // to 'array'.  In debug mode, these values will be checked.
///       TSchemeReg<2,1> reg;
///       reg.param(obj);
///       reg.param_array(argv, argc);
///       reg.var(local);
///       reg.done();
///
///       // Cleanup is automatic.
///   }
///
/// Note that we actually look _slightly_ below the covers of PLT's
/// abstraction APIs.
///
template <size_t VARS, size_t ARRAYS = 0>
class TSchemeReg : boost::noncopyable {
    // Layout:
    //   0:   pointer to previous stack entry.
    //   1:   number of entries following this one
    //   2..: Variables: One entry, a pointer to the variable.
    //        Arrays: Three entries: A zero, pointer to the array, length.
    void *mReg[2+VARS+(3*ARRAYS)];
    size_t mOffset;

    /// Equivalent to MZ_GC_ARRAY_VAR_IN_REG.
    template <typename T>
    void array_helper(T *array, size_t length) throw () {
        mReg[mOffset++] = reinterpret_cast<void*>(0);
        mReg[mOffset++] = reinterpret_cast<void*>(&array);
        mReg[mOffset++] = reinterpret_cast<void*>(length);
    }

public:
    /// Equivalent to MZ_GC_DECL_REG.
	TSchemeReg() throw () {
        mReg[0] = NULL;
        mReg[1] = reinterpret_cast<void*>(VARS + 3*ARRAYS);
        mOffset = 2;
    }

    /// Equivalent to MZ_GC_VAR_IN_REG.  Intended for use with parameters.
    /// We assert that 'ptr' is non-NULL, because that isn't a sensible
    /// Scheme value, as far as we know, and we might catch an error or
    /// two.
    template <typename T>
    void param(T *ptr) throw () {
        ASSERT(NULL != ptr);
        mReg[mOffset++] = reinterpret_cast<void*>(&ptr);
    }

    /// Equivalent to MZ_GC_VAR_IN_REG.  Intended for use with local
    /// variables.  We initialize 'ptr' to NULL for gc safety.
    template <typename T>
    void local(T *ptr) throw () {
        ptr = NULL;
        mReg[mOffset++] = reinterpret_cast<void*>(&ptr);
    }

    /// Equivalent to MZ_GC_ARRAY_VAR_IN_REG.  Intended for use with
    /// parameter arrays.  We assert that the pointers in 'arry' are
    /// non-NULL, because that isn't a sensible Scheme value, as far as we
    /// know, and we might catch an error or two.
    template <typename T>
    void param_array(T *array, size_t length) throw () {
        for (size_t i = 0; i < length; i++)
            ASSERT(NULL != array[i]);
        array_helper(array, length);
    }

    /// Equivalent to MZ_GC_ARRAY_VAR_IN_REG.  Intended for use with local
    /// arrays.  We initialize all the entries in the array to 0.
    template <typename T>
    void local_array(T *array, size_t length) throw () {
        for (size_t i = 0; i < length; i++)
            array[i] = NULL;
        array_helper(array, length);
    }

    /// Register a TSchemeArgs object as a regular array.
    template <size_t LENGTH>
    void args(TSchemeArgs<LENGTH> &args) throw () {
        // These are properly NULL'd by the TSchemeArgs constructor, so we
        // don't worry about them.
        array_helper(args.get(), args.size());
    }

    /// Equivalent to MZ_GC_REG.
    void done() throw () {
        ASSERT(mOffset-2 == VARS + 3*ARRAYS);
        mReg[0] = reinterpret_cast<void*>(GC_variable_stack);
        GC_variable_stack = mReg;
    }

    /// Equivalent to MZ_GC_UNREG.
    ~TSchemeReg() throw () {
        ASSERT(GC_variable_stack == reinterpret_cast<void**>(mReg));
        GC_variable_stack = reinterpret_cast<void**>(mReg[0]);
    }
};

END_NAMESPACE_HALYARD

#endif // TSchemeReg_H
