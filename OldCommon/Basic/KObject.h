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
// KObject.h : Base class for everything.
//

#if !defined (_KObject_h_)
#define _KObject_h_

class KObject 
{
	public:

	 			KObject(void);
		virtual ~KObject(void);
};

typedef KObject *KObjectPtr;

#endif // _KObject_h_

/*
 $Log$
 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
