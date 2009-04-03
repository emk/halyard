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

#if !defined (_TCommon_h_)
#define _TCommon_h_

#include "TPlatform.h"
#include "TWarnings.h"


///////////////////////////////////////////////////////////////////////////
/// \defgroup macros TCommon Macro Definitions
///
/// These are a group of widely-used macro definitions.  Most of these
/// have been around nearly forever
///
/// \author Chuck Officer
/// \author ...and others
///@{

//////////
/// We have our own, portable assertion-checking routine because
/// we want to log assertion failures (because the users *never*
/// write them down).
///
/// This routine is defined in TLogger.cpp.
/// 
/// \param inTest  If this value is zero, trigger the assertion.
/// \param inDescription  Text describing the assertion.
/// \param inFile  The file in which the assertion appears.
/// \param inLine  The line number of the assertion.
///
extern void HalyardCheckAssertion(bool inTest, const char *inDescription,
								  const char *inFile, int inLine);

#ifdef DEBUG
#	define ASSERT(test) \
		HalyardCheckAssertion((bool) (test), #test, __FILE__, __LINE__);
#else // DEBUG
#	define ASSERT(test) ((void) 0)
#endif // DEBUG

#define Max(x, y)   ((x) > (y) ? (x) : (y))
#define Min(x, y)   ((x) < (y) ? (x) : (y))
#define Absolute(x) (((x) > 0)? (x): ((x) * -1))

// Allow our test suites to access some private and protected methods.
// At the top of a test file, write '#define WANT_HALYARD_TEST_INTERFACES 1'.
#ifdef WANT_HALYARD_TEST_INTERFACES
#	define TESTABLE_PRIVATE    public
#	define TESTABLE_PROTECTED  public
#else
#	define TESTABLE_PRIVATE    private
#	define TESTABLE_PROTECTED  protected
#endif

BEGIN_NAMESPACE_HALYARD

//////////
enum TriState
{
	TriStateOff,
	TriStateOn,
	TriStateLatent
};

//////////
enum Alignment 
{
    AlignLeft,
    AlignCenter,
    AlignRight
};

typedef char   int8;
typedef short  int16;
typedef long   int32;

typedef unsigned char   uint8;
typedef unsigned short  uint16;
typedef unsigned long   uint32;

// The MIN_INTn values must be written as "-MAX - 1" because some
// compilers can't handle the smallest possible 32-bit integer as
// a literal value in the source code.
#define MAX_INT8  ((int8)  0x007F)
#define MIN_INT8  (-(MAX_INT8) - 1)
#define MAX_INT16 ((int16) 0x7FFF)
#define MIN_INT16 (-(MAX_INT16) - 1)
#define MAX_INT32 ((int32) 0x7FFFFFFF)
#define MIN_INT32 (-(MAX_INT32) - 1)

#define MAX_UINT8 ((uint8) 0x00FF)
#define MAX_UINT16 ((uint16) 0xFFFF)
#define MAX_UINT32 ((uint32) 0xFFFFFFFF)

END_NAMESPACE_HALYARD

///@}

#endif // _TCommon_h_
