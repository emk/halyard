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

#if !defined (_TCommon_h_)
#define _TCommon_h_


/*-----------------------------------------------------------------

LIBRARY
    Common

OVERVIEW
	The Common library contains a set of utility classes that are
	used throughout the FiveL codebase.

TCOMMON MACRO DEFINITIONS
	#define not		!<br>
	#define and		&&<br>
	#define or		||<br> 

	#ifndef NULL<br>
	#ifdef __cplusplus<br>
	#define NULL 0<br>
	#else<br>
	#define NULL ((void *) 0)<br>
	#endif<br>
	#endif<br>

	#define int8	char<br>
	#define int16	short<br>
	#define int32	long<br>

	#define uint8	unsigned char<br>
	#define uint16	unsigned short<br>
	#define uint32	unsigned long<br>

	#define MAX_INT8  ((int8)  0x007F)<br>
	#define MAX_INT16 ((int16) 0x7FFF)<br>
	#define MAX_INT32 ((int32) 0x7FFFFFFF)<br>

	#define MAX_UINT8 ((uint8) 0x00FF)<br>
	#define MAX_UINT16 ((uint16) 0xFFFF)<br>
	#define MAX_UINT32 ((uint32) 0xFFFFFFFF)<br>

	#define Max(x, y)   ((x) > (y) ? (x) : (y))<br>
	#define Min(x, y)   ((x) < (y) ? (x) : (y))<br>
	#define Absolute(x) (((x) > 0)? (x): ((x) * -1))<br>

	#if defined (__QTML__)<br>
	#define _5L_QUICKTIME_<br>
	#endif<br>

	#if defined (WIN32)<br>
	#define _5L_WIN32_<br>

	#if defined (_DEBUG)<br>
	#define DEBUG<br>
	#endif<br>

	#define ASSERT(x) _ASSERTE(x)<br>

AUTHOR
    Chuck Officer<br>

-----------------------------------------------------------------*/

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

// include platform specific header files here

// look at what is defined to define some internal things
#if defined (__QTML__)
#define _5L_QUICKTIME_
#endif

#if defined (WIN32)
#define _5L_WIN32_

#if defined (_DEBUG)
#define DEBUG
#endif

#define ASSERT(x) _ASSERTE(x)

#endif	// WIN32

#endif // _TCommon_h_

/*
 $Log$
 Revision 1.1  2001/09/24 15:11:00  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.1  2000/04/06 17:06:10  chuck
 Initial check-in

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
