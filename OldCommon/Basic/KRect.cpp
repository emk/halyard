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
// KRect.cpp : 
//

#include "KHeader.h"
#include "KRect.h"

KRect::KRect(int32 inTop /* = 0 */, int32 inLeft /* = 0 */, 
			 int32 inBottom /* = 0 */, int32 inRight /* = 0 */)
{ 
	m_Top = inTop;
	m_Left = inLeft;
	m_Bottom = inBottom; 
	m_Right = inRight;
}

KRect::KRect(const KRect &inRect)
{
	m_Top = inRect.m_Top;
	m_Left = inRect.m_Left;
	m_Bottom = inRect.m_Bottom;
	m_Right = inRect.m_Right;
}

void KRect::Set(int32 inTop, int32 inLeft, int32 inBottom, int32 inRight)
{  
	m_Top = inTop; 
	m_Left = inLeft; 
	m_Bottom = inBottom; 
	m_Right = inRight; 
}

void KRect::Set(const KRect &inRect)
{
	m_Top = inRect.Top();
	m_Left = inRect.Left();
	m_Bottom = inRect.Bottom();
	m_Right = inRect.Right();
}

KRect &KRect::operator=(const KRect &inRect)
{
	if (this != &inRect)
		Set(inRect);

	return (*this);
}

// 
// equality operator
//
bool KRect::operator==(KRect &inRect)
{
	if ((inRect.Top() == m_Top) and
		(inRect.Left() == m_Left) and
		(inRect.Bottom() == m_Bottom) and
		(inRect.Right() == m_Right))
	{
		return (true);
	}
	return (false);
}

bool KRect::Contains(KPoint &inPt)
{
	if ((inPt.X() >= m_Left) and 
		(inPt.X() <= m_Right) and
		(inPt.Y() >= m_Top) and 
		(inPt.Y() <= m_Bottom))
	{
		return (true);
	}
	return (false);
}

void KRect::Offset(KPoint &inPt)
{
	m_Top += inPt.Y();
	m_Bottom += inPt.Y();
	m_Left += inPt.X();
	m_Right += inPt.X();
}

KPoint KRect::TopLeft(void)
{
	KPoint	retPt(m_Left, m_Top);
	
	return (retPt);
}

#if defined (MACOS)
Rect KRect::GetRect(void) const
{
	Rect	retRect;
	
	retRect.top = (short) m_Top;
	retRect.left = (short) m_Left;
	retRect.bottom = (short) m_Bottom;
	retRect.right = (short) m_Right;

	return (retRect);
}

void KRect::Set(Rect &inRect)
{
	m_Top = inRect.top;
	m_Left = inRect.left;
	m_Bottom = inRect.bottom;
	m_Right = inRect.right;
}
#endif

#if defined (WIN32)
RECT KRect::GetRect(void) const
{
	RECT	retRect;

	reKRect.top = m_Top;
	reKRect.left = m_Left;
	reKRect.bottom = m_Bottom;
	reKRect.right = m_Right;

	return (reKRect);
}
void KRect::Set(RECT &inRect)
{
	m_Top = inRect.top;
	m_Left = inRect.left;
	m_Bottom = inRect.bottom;
	m_Right = inRect.right;
}
#endif
/*
 $Log$
 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
