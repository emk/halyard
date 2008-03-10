// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

#include "CommonHeaders.h"
#include "TRect.h"

USING_NAMESPACE_FIVEL

TRect::TRect(int32 inLeft, int32 inTop,
	     int32 inRight, int32 inBottom)
{ 
	m_Top = inTop;
	m_Left = inLeft;
	m_Bottom = inBottom; 
	m_Right = inRight;
}

TRect::TRect(const TRect &inRect)
{
	m_Top = inRect.m_Top;
	m_Left = inRect.m_Left;
	m_Bottom = inRect.m_Bottom;
	m_Right = inRect.m_Right;
}

void TRect::Set(int32 inLeft, int32 inTop, int32 inRight, int32 inBottom)
{  
	m_Top = inTop; 
	m_Left = inLeft; 
	m_Bottom = inBottom; 
	m_Right = inRight; 
}

void TRect::Set(const TRect &inRect)
{
	m_Top = inRect.Top();
	m_Left = inRect.Left();
	m_Bottom = inRect.Bottom();
	m_Right = inRect.Right();
}

TPoint TRect::TopLeft(void) const
{
	TPoint retPt(m_Left, m_Top);
	return retPt;
}

TRect &TRect::operator=(const TRect &inRect)
{
	Set(inRect);

	return (*this);
}

// 
// equality operator
//
bool TRect::operator==(const TRect &inRect) const
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

void TRect::Offset(const TPoint &inPt)
{
	m_Top += inPt.Y();
	m_Bottom += inPt.Y();
	m_Left += inPt.X();
	m_Right += inPt.X();
}
