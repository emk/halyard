// -*- Mode: C++; tab-width: 4; -*-
//========================================================================
//  Carbon/PPC Portability Glue
//========================================================================
//  This file contains a number of glue routines which allow us to
//  compile FiveL for either the old MacOS 8/9 API, or for Apple's
//  Carbon API. 
//
//  TODO - Look for non-Carbon MacOS calls in libfivel.

#ifndef MacCarbonGlue_H
#define MacCarbonGlue_H


//========================================================================
//  Configure Preprocessor Macros
//========================================================================
//  Set up all the options we need for Carbon or non-Carbon builds.  This
//  must be done before we include any MacOS headers.

#if FIVEL_PLATFORM_MACINTOSH_CARBON
#	define PP_Target_Carbon		1
#	define PP_StdDialogs_Option	PP_StdDialogs_NavServicesOnly
#else
#	define PP_Target_Carbon		0
#	define PP_StdDialogs_Option	PP_StdDialogs_ClassicOnly
#	define ACCESSOR_CALLS_ARE_FUNCTIONS 1
#endif

#define PP_Target_Classic		(!PP_Target_Carbon)
#define TARGET_API_MAC_CARBON	PP_Target_Carbon
#define TARGET_API_MAC_OS8		PP_Target_Classic


//========================================================================
//  MacOS Headers
//========================================================================
//  These headers are needed to compile our various glue routines.

#include <TextUtils.h>


//========================================================================
//  Carbon Glue
//========================================================================
//  These routines emulate old-style MacOS functions when running under
//  Carbon.

#if TARGET_API_MAC_CARBON

inline unsigned char *c2pstr(char *ioString)
{
	::CopyCStringToPascal(ioString, (unsigned char *) ioString);
	return (unsigned char *) ioString;
}

inline char *p2cstr(unsigned char *ioString)
{
	::CopyPascalStringToC(ioString, (char *) ioString);
	return (char *) ioString;
}

#endif // TARGET_API_MAC_CARBON

#endif // MacCarbonGlue_H
