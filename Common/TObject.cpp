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
// TObject.cpp : 
//

#include "CommonHeaders.h"
#include "THeader.h"
#include "TObject.h"

USING_NAMESPACE_FIVEL

//
//	Default constructor does nothing.
//
TObject::TObject()
{
}

//
//	Destructor does nothing.
//
TObject::~TObject()
{
}

/*
 $Log$
 Revision 1.3  2003/06/13 10:57:30  emk
 Further use of precompiled headers; pruning of various inappropriate
 includes.

 Revision 1.2  2002/03/04 15:16:01  hamon
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
