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

#endif // _TObject_h_

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

*/
