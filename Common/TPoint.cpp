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
// TPoint.cpp : 
//

#include "THeader.h"
#include "TPoint.h"

USING_NAMESPACE_FIVEL

TPoint::TPoint(int32 inX /* = 0 */, int32 inY /* = 0 */) 
{ 
	m_X = inX; 
	m_Y = inY; 
}

TPoint::TPoint(TPoint &inPt) 
{ 
	m_X = inPt.X(); 
	m_Y = inPt.Y(); 
}

TPoint &TPoint::operator=(const TPoint &inPt)
{
	Set(inPt);

	return (*this);
}

// 
// equality operator
//
bool TPoint::operator==(TPoint &inPt)
{
	if ((inPt.X() == m_X) and
		(inPt.Y() == m_Y))
	{
		return (true);
	}
	return (false);
}

void TPoint::Set(int32 inX, int32 inY) 
{ 
	m_X = inX; 
	m_Y = inY; 
}

void TPoint::Set (const TPoint &inPt) 
{ 
	m_X = inPt.X();
	m_Y = inPt.Y(); 
}

void TPoint::Offset(TPoint &inPt)
{
	m_X += inPt.X();
	m_Y += inPt.Y();
}

#if defined (_5L_QUICKTIME_)
Point TPoint::GetPoint(void)
{
	Point	retPt;

	retPt.h = (short) m_X;
	retPt.v = (short) m_Y;

	return (retPt);
}

void TPoint::Set(Point &inPt) 
{ 
	m_X = inPt.h; 
	m_Y = inPt.v; 
}
#endif

#if defined (_5L_WIN32_)
POINT TPoint::GetPOINT(void) 
{		
	POINT retPt; 

	retPt.x = m_X;
	retPt.y = m_Y;

	return (retPt);
}

void TPoint::Set(POINT &inPt) 
{ 
	m_X = inPt.x; 
	m_Y = inPt.y; 
}
#endif

/*
 $Log$
 Revision 1.2  2002/03/04 15:16:05  hamon
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
