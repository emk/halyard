// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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

#ifndef StackBase_H
#define StackBase_H

// We pass a second argument of 1 to scheme_set_stack_base to avoid
// scanning random DLL memory segments for Scheme pointers.  This is
// especially necessary because we often link against QuickTime, which maps
// huge areas of graphics card memory into our address space, causing Boehm
// to run very... very... very... slowly.
//
// Note that you are not allowed to throw an exception through
// HALYARD_BEGIN_STACK_BASE / HALYARD_END_STACK_BASE.
#include <scheme.h>
#define HALYARD_BEGIN_STACK_BASE() \
    MZ_GC_DECL_REG(0); \
    MZ_GC_REG(); \
    scheme_set_stack_base(&__gc_var_stack__, 1);

#define HALYARD_END_STACK_BASE() \
    MZ_GC_UNREG();

#endif // StackBase_H
