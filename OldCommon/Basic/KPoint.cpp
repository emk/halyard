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
// KPoint.cpp : 
//

#include "KHeader.h"
#include "KPoint.h"

KPoint::KPoint(int32 inX /* = 0 */, int32 inY /* = 0 */) 
{ 
	m_X = inX; 
	m_Y = inY; 
}

KPoint::KPoint(const KPoint &inPt) 
{ 
	m_X = inPt.X(); 
	m_Y = inPt.Y(); 
}

KPoint &KPoint::operator=(const KPoint &inPt)
{
	if (this != &inPt)
		Set(inPt);

	return (*this);
}

// 
// equality operator
//
bool KPoint::operator==(KPoint &inPt)
{
	if ((inPt.X() == m_X) and
		(inPt.Y() == m_Y))
	{
		return (true);
	}
	return (false);
}

void KPoint::Set(int32 inX, int32 inY) 
{ 
	m_X = inX; 
	m_Y = inY; 
}

void KPoint::Set(const KPoint &inPt) 
{ 
	m_X = inPt.X();
	m_Y = inPt.Y(); 
}

void KPoint::Offset(KPoint &inPt)
{
	m_X += inPt.X();
	m_Y += inPt.Y();
}

#if defined (MACOS)
Point KPoint::GetPoint(void) const
{
	Point	retPt;

	retPt.h = (short) m_X;
	retPt.v = (short) m_Y;

	return (retPt);
}

void KPoint::Set(Point &inPt) 
{ 
	m_X = inPt.h; 
	m_Y = inPt.v; 
}
#endif

#if defined (WIN32)
POINT KPoint::GetPOINT(void) const
{		
	POINT retPt; 

	retPt.x = m_X;
	retPt.y = m_Y;

	return (retPt);
}

void KPoint::Set(POINT &inPt) 
{ 
	m_X = inPt.x; 
	m_Y = inPt.y; 
}
#endif

/*
 $Log$
 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
