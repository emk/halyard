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
// LCommon.h : 
//

#if !defined (_LCommon_h_)
#define _LCommon_h_

/*-----------------------------------------------------------------

LIBRARY
    FiveL

LCOMMON MACRO DEFINITIONS
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

#endif // _LCommon_h_

/*
 $Log$
 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
