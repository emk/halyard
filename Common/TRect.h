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

#if !defined (_TRect_h_)
#define _TRect_h_

BEGIN_NAMESPACE_HALYARD

//////////
/// A class to represent a rectangular region.
///
/// \author Chuck Officer
/// \author ...and others
///
class TRect 
{  
    public:
        TRect() : m_Left(0), m_Top(0), m_Right(0), m_Bottom(0) {}

        TRect(int32 inLeft, int32 inTop, 
              int32 inRight, int32 inBottom);

        TRect(const TRect &rect);
        
        void Set(int32 inLeft, int32 inTop, 
                 int32 inRight, int32 inBottom);
        
        void Set(const TRect &inRect);

        void Offset(const TPoint &inPt);

        inline void SetTop(int32 inTop) { m_Top = inTop; }
        
        inline void SetLeft(int32 inLeft) { m_Left = inLeft; }
        
        inline void SetBottom(int32 inBottom) { m_Bottom = inBottom; }
        
        inline void SetRight(int32 inRight) { m_Right = inRight; }

        inline void OffsetLeft(int32 inLeftOffset) { m_Left += inLeftOffset; }
        
        inline void OffsetBottom(int32 inBottomOffset) { 
            m_Bottom += inBottomOffset; 
        }
        
        inline void OffsetRight(int32 inRightOffset) { 
            m_Right += inRightOffset; 
        }

        inline int32 Top() const { return (m_Top); }
        
        inline int32 Left() const { return (m_Left); }
        
        inline int32 Bottom() const { return (m_Bottom); }
        
        inline int32 Right() const { return (m_Right); }

        TPoint TopLeft() const;

        int32 Width() { return (m_Right - m_Left); }
        
        int32 Height() { return (m_Bottom - m_Top); }

        TRect &operator=(const TRect &inRect);
        
        bool operator==(const TRect &inRect) const;

        // Does TRect contain TPoint?
        bool Contains(TPoint &inPt);
        
    protected:
        int32 m_Left;
        int32 m_Top;
        int32 m_Right;
        int32 m_Bottom;
};

inline std::ostream &operator<<(std::ostream &out, const TRect &r) {
    out << "(rect " << r.Left() << " " << r.Top() << " " << r.Right() << " "
        << r.Bottom() << ")";
    return out;
}

END_NAMESPACE_HALYARD

#endif // _TRect_h_
