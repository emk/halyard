//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999, Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// KCommon.h : 
//

#if !defined (_KCommon_h_)
#define _KCommon_h_

#define not		!
#define and		&&
#define or		|| 

#ifndef NULL
#ifdef __cplusplus
#define NULL 0
#else
#define NULL ((void *) 0)
#endif
#endif 

#define int8	char
#define int16	short
#define int32	long

#define uint8	unsigned char
#define uint16	unsigned short
#define uint32	unsigned long

#define MAX_INT8  ((int8)  0x007F)
#define MAX_INT16 ((int16) 0x7FFF)
#define MAX_INT32 ((int32) 0x7FFFFFFF)

#define MAX_UINT8 ((uint8) 0x00FF)
#define MAX_UINT16 ((uint16) 0xFFFF)
#define MAX_UINT32 ((uint32) 0xFFFFFFFF)

#define Max(x, y)   ((x) > (y) ? (x) : (y))
#define Min(x, y)   ((x) < (y) ? (x) : (y))
#define Absolute(x) (((x) > 0)? (x): ((x) * -1))

enum TriState
{
	TriStateOff,
	TriStateOn,
	TriStateLatent
};

enum Alignment 
{
    AlignLeft,
    AlignCenter,
    AlignRight
};

// include platform specific header files here

// look at what is defined to define some internal things
#if defined (__QTML__)
#define _5L_QUICKTIME_
#endif

// Mac part
#if defined (MACOS)

#define NEWLINE_CHAR	'\r'
#define RETURN_CHAR		'\n'

#ifdef Debug_Throw
#define DEBUG

#define ASSERT(x)	if (!(x)) gLog.Error("Assertion failed in file %s, line %d", __FILE__, __LINE__);

// backwards compatible defines
//#define DEBUG_5L
//#define DEBUG_5L_SCRIPT

#else

#define ASSERT(x)

#endif	// Debug_Throw

#endif	// TARGET_OS_MAC

// WIN32 part
#if defined (WIN32)
#define _5L_WIN32_

#if defined (_DEBUG)
#define DEBUG
#endif

#define ASSERT(x) _ASSERTE(x)

#endif	// WIN32

#endif // _KCommon_h_

/*
 $Log$
 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
