//
//	Mac5L.h
//
//		Some general definitions for Mac5L.
//

#ifndef MAC5L_H
#define MAC5L_H

// These two change from the PowerPC compiler to the 68K compiler.
//#ifdef __powerc
//#define NEWLINE_CHAR	'\n'
//#define RETURN_CHAR		'\r'
//#else
#define NEWLINE_CHAR	'\r'
#define RETURN_CHAR		'\n'
//#endif

#define not		!
#define and		&&
#define or		||

#ifndef FALSE
#define FALSE	0
#endif

#ifndef TRUE
#define TRUE	1
#endif

#define int8	char
#define int16	short
#define int32	long

#define uint8	unsigned char
#define uint16	unsigned short
#define uint32	unsigned long

#define MAX_INT8	((int8) 0x007F)
#define MAX_INT16	((int16) 0x7FFF)
#define MAX_INT32	((int32) 0x7FFFFFFF)

#define MAX_UINT8	((int8) 0x00FF)
#define MAX_UINT16	((int16) 0xFFFF)
#define MAX_UINT32	((int32) 0xFFFFFFFF)

#endif
