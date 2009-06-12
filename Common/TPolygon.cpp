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

#include <math.h>

using namespace Halyard;
using namespace std;

TPolygon::TPolygon(const std::vector<TPoint> &inVertices)
  : mVertices(inVertices)
{
  int top = 0, 
      left = 0, 
      bottom = 0, 
      right = 0;

  if (mVertices.size() == 0)
    return;

  top = mVertices[0].Y();
  left = mVertices[0].X();
  bottom = mVertices[0].Y();
  right = mVertices[0].X();

  std::vector<TPoint>::iterator i;
  for (i = mVertices.begin(); i != mVertices.end(); ++i) {
    TPoint pt = *i;
    if (pt.X() < left)
      left = pt.X();
    if (pt.Y() < top)
      top = pt.Y();
    if (pt.X() > right)
      right = pt.X();
    if (pt.Y() > bottom)
      bottom = pt.Y();
  }
  
  mBounds = TRect(left, top, right, bottom);
}

// NB: I think I confused up and down in all of these comments (the
// code is fine, it's just the comments that might be wrong).
// Interperet "up" as positive Y, and "down" as negative Y.
// ...
// OK, now I've switched up and down and left and right enough that
// I'm not sure which comments are right and which are not. Caveat
// emptor!
bool TPolygon::Contains(const TPoint &inPt) {
  // First check to see if point is within bounding box, since that's
  // quick and simple.
  if (inPt.X() < mBounds.Left()
      || inPt.Y() < mBounds.Top()
      || inPt.X() > mBounds.Right()
      || inPt.Y() > mBounds.Bottom())
    return false;

  // Now the real check. We use the basic odd-parity rule for
  // determining if a point is within the polygon (Foley, van Dam, et
  // al. 1990). The basic idea is that if you have a ray from the
  // point in any arbitrary direction (in our case, to the left),
  // then the point is in the polygon if the ray intersects the
  // polygon an odd number of times, and outside if it intersects and
  // even number of times. There is a slight issue with the case where
  // the ray intersects one of the vertices. In that case, we count
  // each segment of which that vertex is the uppermost vertex. Think
  // about it, and you'll see why. In order to count the
  // intersections, we use a modified version of the sweep line
  // algorithm for segment intersections (see CLRS 2001 or google
  // search).
  
  std::vector<TPoint>::iterator i;
  TPoint begin, end, tmp;
  float slope, rightVal;
  int count = 0;
  for (i = mVertices.begin(); i != mVertices.end(); ++i) {
      // Get the endpoints of the current segment.
      begin = *i;
      end = (i+1) == mVertices.end() ? *(mVertices.begin()) : *(i+1);
    
      // Make sure our endpoints are in order from right to left
      if (begin.X() < end.X()) {
          tmp = begin;
          begin = end;
          end = tmp;
      }

      // If the whole segment is not ever to the left of the start
      // point, it certainly can't intersect
      if (end.X() > inPt.X())
          continue;
          
      // If our beginning point is to the right of the ray, compute
      // it's value at the start of the ray. 
      if (begin.X() > inPt.X()) {
          // Note: we can't be vertical, so this is OK.
          slope = (float) (end.Y() -  begin.Y()) 
              / (float) (end.X() - begin.X());
          rightVal = slope * (inPt.X() - begin.X()) + begin.Y();
      } else {
          rightVal = (float) begin.Y();
      }

      // If the segment is horizontal, it doesn't affect our count
      if (begin.Y() == end.Y())
          continue;
      
      // We count the intersection when 
      // a) The end point is on the ray and the end point is the top
      //    of the segment. (remember, up is actually lower Y value)
      // b) The beginning point is on the ray and the beginning point 
      //    is the top of the segment.
      // c) The Y value of the segment at the start of the ray is
      //    greater than the Y value of the ray, and the Y value at
      //    the end point of the segment is lesser.
      // d) The opposite of (c) (Y val of segment lesser and Y value
      //    of end of segment greater).
      if ((end.Y() == inPt.Y() && begin.Y() > end.Y())           // a
          || (begin.Y() == inPt.Y() && begin.X() <= inPt.X()     // b
              && begin.Y() < end.Y())
          || (rightVal > (float) inPt.Y() && end.Y() < inPt.Y())  // c
          || (rightVal < (float) inPt.Y() && end.Y() > inPt.Y())) // d
      {
          count++;
      }
  }
  
  // If the count is odd, we're in.
  return count % 2 == 1;
}

void TPolygon::Offset(const TPoint &inPt) {
    std::vector<TPoint>::iterator i;
    
    for (i = mVertices.begin(); i != mVertices.end(); ++i)
        i->Set(i->X() + inPt.X(), i->Y() + inPt.Y());
    
    mBounds.Offset(inPt);
}

void TPolygon::MoveTo(const TPoint &inOrigin) {
    TPoint offsetDelta(inOrigin.X() - Origin().X(), inOrigin.Y() - Origin().Y());
    Offset(offsetDelta);
}

TPoint TPolygon::Origin() const {
    return TPoint(mBounds.Left(), mBounds.Top()); 
}

bool TPolygon::operator==(const TPolygon &inPoly) const {
    return mVertices == inPoly.Vertices();
}

std::ostream &Halyard::operator<<(std::ostream &out, const TPolygon &poly) {
    out << "(polygon";
    std::vector<TPoint> vertices(poly.Vertices());
    std::vector<TPoint>::iterator i = vertices.begin();
    for (; i != vertices.end(); ++i)
        out << " " << *i;
    out << ")";
    return out;
}
