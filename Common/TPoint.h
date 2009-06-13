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

#if !defined (_TPoint_h_)
#define _TPoint_h_

BEGIN_NAMESPACE_HALYARD

/// A class to represent a point in 2D space.
///
/// \author Chuck Officer
/// \author ...and others
class TPoint {
    public:
        /// Constructor.  Initialize X and Y to zero.
        TPoint() : m_X(0), m_Y(0)  {}

        /// Constructor.
        ///
        /// [in] inX - set the X-coordinate
        /// [in] inY - set the y-coordinate
        TPoint(int32 inX, int32 inY);

        /// Copy Constructor.
        ///
        /// \param inPt  a TPoint to copy from
        TPoint(const TPoint &inPt);

        /// Set values for the point.
        ///
        /// \param inX  set the X-coordinate
        /// \param inY  set the y-coordinate
        void            Set(int32 inX, int32 inY);

        /// Set values for the point using another TPoint.
        ///
        /// \param inPt  TPoint to copy values from
        void            Set(const TPoint &inPt);

        /// Offset the point using another TPoint.
        ///
        /// \param inPt  TPoint used to offset values for this point
        void            Offset(TPoint &inPt);

        /// Set the X-coordinate.
        ///
        /// \param inX  the X-coordinate
        inline void     SetX(int32 inX) { m_X = inX; }
        
        /// Set the Y-coordinate.
        ///
        /// \param inY  the Y-coordinate
        inline void     SetY(int32 inY) { m_Y = inY; }

        /// Offset the X-coordinate.
        ///
        /// \param inXOffset  offset for the X-coordinate
        inline void     OffsetX(int32 inXOffset) { m_X += inXOffset; }
        
        /// Offset the Y-coordinate.
        ///
        /// \param inYOffset  offset for the Y-coordinate
        inline void     OffsetY(int32 inYOffset) { m_Y += inYOffset; }
        
        /// Get the X-coordinate
        ///
        /// \return  the X-coordinate
        inline int32    X(void) const { return (m_X); }
        
        /// Get the Y-coordinate
        ///
        /// \return  the Y-coordinate
        inline int32    Y(void) const { return (m_Y); }

        /// Set values for this point using another TPoint.  Same as Set().
        ///
        /// \param inPt  TPoint to copy values from (r-value)
        /// \return  l-value with coordinates set to r-value
        TPoint          &operator=(const TPoint &inPt);
        
        /// Equality check.
        ///
        /// \param inPt  a TPoint to check against for equality
        /// \return  true if the two points are equal, false otherwise
        bool            operator==(const TPoint &inPt) const;

    protected:
        /// X-Coordinate
        int32       m_X;
        
        /// Y-Coordinate
        int32       m_Y;
};

inline std::ostream &operator<<(std::ostream &out, const TPoint &p) {
    out << "(point " << p.X() << " " << p.Y() << ")";
    return out;
}

END_NAMESPACE_HALYARD

#endif // _TPoint_h_
