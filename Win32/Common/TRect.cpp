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
// TRect.cpp : 
//

#include "THeader.h"
#include "TRect.h"

TRect::TRect(int32 inTop /* = 0 */, int32 inLeft /* = 0 */, 
			 int32 inBottom /* = 0 */, int32 inRight /* = 0 */)
{ 
	m_Top = inTop;
	m_Left = inLeft;
	m_Bottom = inBottom; 
	m_Right = inRight;
}

TRect::TRect(TRect &inRect)
{
	m_Top = inRect.m_Top;
	m_Left = inRect.m_Left;
	m_Bottom = inRect.m_Bottom;
	m_Right = inRect.m_Right;
}

void TRect::Set(int32 inTop, int32 inLeft, int32 inBottom, int32 inRight)
{  
	m_Top = inTop; 
	m_Left = inLeft; 
	m_Bottom = inBottom; 
	m_Right = inRight; 
}

void TRect::Set(TRect &inRect)
{
	m_Top = inRect.Top();
	m_Left = inRect.Left();
	m_Bottom = inRect.Bottom();
	m_Right = inRect.Right();
}

TRect &TRect::operator=(TRect &inRect)
{
	Set(inRect);

	return (*this);
}

// 
// equality operator
//
bool TRect::operator==(TRect &inRect)
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

bool TRect::Contains(TPoint &inPt)
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

void TRect::Offset(TPoint &inPt)
{
	m_Top += inPt.Y();
	m_Bottom += inPt.Y();
	m_Left += inPt.X();
	m_Right += inPt.X();
}

#if defined (_5L_QUICKTIME_)
Rect TRect::GetRect(void)
{
	Rect	retRect;
	
	retRect.top = (short) m_Top;
	retRect.left = (short) m_Left;
	retRect.bottom = (short) m_Bottom;
	retRect.right = (short) m_Right;

	return (retRect);
}

void TRect::Set(Rect &inRect)
{
	m_Top = inRect.top;
	m_Left = inRect.left;
	m_Bottom = inRect.bottom;
	m_Right = inRect.right;
}
#endif

#if defined (_5L_WIN32_)
RECT TRect::GetRECT(void)
{
	RECT	retRect;

	retRect.top = m_Top;
	retRect.left = m_Left;
	retRect.bottom = m_Bottom;
	retRect.right = m_Right;

	return (retRect);
}
void TRect::Set(RECT &inRect)
{
	m_Top = inRect.top;
	m_Left = inRect.left;
	m_Bottom = inRect.bottom;
	m_Right = inRect.right;
}
#endif
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
