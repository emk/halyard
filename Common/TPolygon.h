// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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

#if !defined (_TPolygon_h_)
#define _TPolygon_h_

BEGIN_NAMESPACE_FIVEL

//////////
/// A class representing a closed polygon.
///
class TPolygon
{
	std::vector<TPoint> mVertices;
	TRect mBounds;

public:
	//////////
	/// Create a new polygon.
	///
	/// \param inVertices  The vertices of the polygon.
	/// 
	TPolygon(const std::vector<TPoint> &inVertices);
	TPolygon() : mVertices(), mBounds() {}

	//////////
	/// Determine if a point is in the polygon.
	/// 
	/// \param inPt  The point to check.
	/// 
	bool Contains(TPoint &inPt);
	
	//////////
	/// Offset the polygon.
	/// 
	/// \param inPt  Point to be used as the offset.
	/// 
	void Offset(const TPoint &inPt);

	//////////
	/// Get a rectangle which completely contains the polygon.
	/// 
	const TRect Bounds() const { return mBounds; }
	
	//////////
	/// Get the number of points in the polygon.
	///
	size_t GetPointCount() { return mVertices.size(); }

	//////////
	/// Get the specified point.
	///
	TPoint GetPoint(size_t inIndex)
	    { ASSERT(inIndex < mVertices.size()); return mVertices[inIndex]; }

	//////////
	/// Get the points which define the polygon.
	/// 
	const std::vector<TPoint> &Vertices() const { return mVertices; }

	//////////
	/// Test if two polygons are equal.
	/// 
	bool operator==(const TPolygon &inPoly) const;
};

extern std::ostream &operator<<(std::ostream &out, const TPolygon &poly);

END_NAMESPACE_FIVEL

#endif // _TPolygon_h_
