// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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
#include "TPoint.h"

using namespace Halyard;

TPoint::TPoint(int32 inX /* = 0 */, int32 inY /* = 0 */) {
    m_X = inX; 
    m_Y = inY; 
}

TPoint::TPoint(const TPoint &inPt) {
    m_X = inPt.X(); 
    m_Y = inPt.Y(); 
}

TPoint &TPoint::operator=(const TPoint &inPt) {
    Set(inPt);

    return (*this);
}

// 
// equality operator
//
bool TPoint::operator==(const TPoint &inPt) const {
    if ((inPt.X() == m_X) &&
        (inPt.Y() == m_Y))
    {
        return (true);
    }
    return (false);
}

void TPoint::Set(int32 inX, int32 inY) {
    m_X = inX; 
    m_Y = inY; 
}

void TPoint::Set (const TPoint &inPt) {
    m_X = inPt.X();
    m_Y = inPt.Y(); 
}

void TPoint::Offset(TPoint &inPt) {
    m_X += inPt.X();
    m_Y += inPt.Y();
}
