//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999, 2000 Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// TObject.h : Base class for everything.
//

#if !defined (_TObject_h_)
#define _TObject_h_

BEGIN_NAMESPACE_FIVEL

/*-----------------------------------------------------------------

CLASS
    TObject

	Base class for everything.   

AUTHOR
    Chuck Officer<br>

-----------------------------------------------------------------*/
class TObject 
{
	public:

	 	TObject(void);
		virtual ~TObject(void);
};

//////////
// Pointer to a TObject.
//
typedef TObject *TObjectPtr;

END_NAMESPACE_FIVEL

#endif // _TObject_h_

/*
 $Log$
 Revision 1.2  2002/03/04 15:16:03  hamon
 Added support for compiler's namespaces. Namespaces are only enabled on macintosh.

Moved OS specific configuration to TPlatform.h

Changes by Elizabeth and Eric, okayed by Eric.

 Revision 1.1  2001/09/24 15:11:00  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.1  2000/04/06 17:06:10  chuck
 Initial check-in

*/
