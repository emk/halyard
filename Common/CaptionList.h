// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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

#ifndef CaptionList_H
#define CaptionList_H

#include <typeinfo>

BEGIN_NAMESPACE_FIVEL

/// A single media caption.
class Caption {
    double mTime;
    std::string mText;
    
public:
    Caption() : mTime(-1) {}
    Caption(double time, const std::string &text) : mTime(time), mText(text) {}
    
    /// Return the time at which this caption should appear, in seconds.
    double time() const { return mTime; }

    /// Return the text of this caption.  The is an XML fragment, possibly
    /// containing things like "&lt;" and "text with <i>emphasis</i>".
    std::string text() const { return mText; }
};

/// A list of media captions, with associated time points.
class CaptionList {
    typedef std::vector<Caption> CaptionVector;
    typedef std::map<double,CaptionVector::difference_type> TimeIndexMap;

    CaptionVector mList;
    TimeIndexMap mTimeIndexMap;

    // The index of the last caption returned, or -1 if none.
    CaptionVector::difference_type mLastCaptionReturned;

public:
    CaptionList(const std::string &file);

    CaptionVector::size_type size() const { return mList.size(); }
  
    Caption operator[](CaptionVector::size_type i) const;
    
    bool getCaptionIfChanged(double time, Caption &outCaption);
};

END_NAMESPACE_FIVEL

#endif // CaptionList_H
