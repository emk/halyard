/*  util.h

    Prototypes for util.c

*/

#ifndef _H_UTIL
#define _H_UTIL

#include "Mac5L.h"

#define Max(x, y)   ((x) > (y) ? (x) : (y))
#define Min(x, y)   ((x) < (y) ? (x) : (y))

#define TIMERID_NAP 13

#define BEVEL 0 //originally 3, removed to center text in buttons

enum Alignment 
{
    Left,
    Center,
    Right
};

int16   PinRange(int16 num, int16 minimum, int16 maximum);
void    napClear(void);
void    prerror(char *,...);
void    prcaution(char *,...);
void    nap(long msecs, int allowesc);
void    trim(char *str);
void    fixstring(void);
void 	strlower(char *str);
int16 	strnicmp(const char *str1, const char *str2, int32 count);
int16 	stricmp(const char *str1, const char *str2);

#ifdef DEBUG_5L
void prinfo(char *cs,...);
void prmem(void);
void pronecommand(char *inStr, uint32 inPos);
void open_debug_file(void);
void close_debug_file(void);
#endif


#endif
