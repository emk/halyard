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


#if !defined (_LUtil_h_)
#define _LUtil_h_

#include "TCommon.h" 
#include "TString.h"

//#define Max(x, y)   ((x) > (y) ? (x) : (y))
//#define Min(x, y)   ((x) < (y) ? (x) : (y))
//#define Absolute(x) (((x) > 0)? (x): ((x) * -1))

#define OK_VOLUME		1
#define NO_VOLUME		2
#define WRONG_VOLUME	3

//////////
// [in] errormsg - alert message
// [in] isError - if true, use error icon
//
void AlertMsg(const char *msg, bool isError);

//////////
// Check to see if a volume is mounted in the CD drive
//
// [in] inCDPath - path to the CD drive
// [in] inVolName - volume name expected
// [out] return - error code<br><br>
//			OK_VOLUME - correct volume is in the drive
//			NO_VOLUME - no volume is in the drive
//			WRONG_VOLUME - the wrong volume was found in the drive	
//
int32	VolIsMounted(char *inCDPath, TString &inVolName);

#endif // _LUtil_h_

/*
 $Log$
 Revision 1.4  2002/07/29 22:02:46  zeb
   * zeb: Moved some primitives from TMacPrimitives.cpp and TWinPrimitives.cpp
     to TCommonPrimitives.cpp and T5LPrimitives.cpp.
   * zeb: Added a GET primitive.
   * zeb: Cleaned date stuff out of LUtil.{h,cpp}.

 Revision 1.3  2002/01/24 19:22:41  tvw
 Fixed bug (#531) in -D command-line option causing
 system registry read error.

 Revision 1.2  2002/01/23 20:39:20  tvw
 A group of changes to support a new stable build.

 (1) Only a single instance of the FiveL executable may run.

 (2) New command-line option "-D" used to lookup the installation directory in the system registry.
     Note: Underscores will be parsed as spaces(" ").
     Ex: FiveL -D HIV_Prevention_Counseling

 (3) Slow down the flash on buttpcx so it can be seen on
     fast machines.  A 200 mS pause was added.

 (4) Several bugfixes to prevent possible crashes when error
     conditions occur.

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.3  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
