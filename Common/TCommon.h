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


//=========================================================================
//  QuickTime Configuration
//=========================================================================

#if defined (__QTML__)
#define _5L_QUICKTIME_
#endif


//=========================================================================
//  Win32 Configuration
//=========================================================================

#if defined (WIN32)
#define _5L_WIN32_

#if defined (_DEBUG)
#define DEBUG
#endif

#define ASSERT(x) _ASSERTE(x)

// For now, the Windows engine uses some non-standard string functions
// instead of using our own, equivalent code.
#define HAVE__STRLWR 1
#define HAVE__STRUPR 1
#define HAVE__STRICMP 1


//=========================================================================
//  Macintosh Configuration
//=========================================================================

#elif defined (macintosh)

#define NEWLINE_CHAR	'\r'
#define RETURN_CHAR		'\n'

#ifdef Debug_Throw
#define DEBUG

#define ASSERT(x) \
	if (!(x)) \
		gLog.Error("Assertion failed in file %s, line %d", __FILE__, __LINE__);

// backwards compatible defines
//#define DEBUG_5L
//#define DEBUG_5L_SCRIPT

#else

#define ASSERT(x)

#endif	// Debug_Throw


//=========================================================================
//  Other Platform Configuration
//=========================================================================

#else

#define ASSERT(expr) assert(expr)

#endif


#endif // _TCommon_h_

/*
 $Log$
 Revision 1.2  2002/02/27 16:38:21  emk
 Cross-platform code merge!

 * Merged back in support for the Macintosh platform.  This is an ongoing
   process, and we'll need to do more work.

 * Separated out platform specific configuration with big block comments.

 * Merged in a few changes from KBTree which appeared to fix bugs.

 * Merged in IntToString, UIntToString, DoubleToString changes from the
   Macintosh engine, and improved the error handling.  NOTE: doubles now
   print using "%f" (the way the Mac engine always did it).  This means
   that "tstr = 0.0" will set 'tstr' to "0.000000", not "0" (as it
   did in the Win32 engine).

 This code might not build on Windows.  As soon as I hear from ssharp
 that he's updated the project files, I'll test it myself.

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
