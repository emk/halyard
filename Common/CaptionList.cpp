// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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
#include <stdlib.h>
#include "TestCase.h"
#include "CaptionList.h"
#include "XmlUtils.h"

using namespace Halyard;

REGISTER_TEST_CASE_FILE(CaptionList);


//=========================================================================
//  CaptionList Methods
//=========================================================================

CaptionList::CaptionList(const std::string &file)
    : mLastCaptionReturned(-1)
{
    xml_doc doc(file);
    xml_node root(doc.root());
    
    for (xml_node::iterator i = root.begin(); i != root.end(); ++i) {
        std::string at_str((*i).attribute("at"));

        // Sanity-check our time-string and do a cheap parse.
        if (at_str.size() < sizeof("hh:mm:ss")
            || at_str[2] != ':' || at_str[5] != ':')
            THROW("Malformed time code <" + at_str + "> in " + file);
        at_str[2] = '\0';
        at_str[5] = '\0';
        int hours = atoi(&at_str[0]);
        int minutes = atoi(&at_str[3]);
        double seconds = atof(&at_str[6]);
        double time = 3600 * hours + 60 * minutes + seconds;

        mList.push_back(Caption(time, (*i).contentAsXml()));
        mTimeIndexMap[time] = mList.size()-1;
    }
}

Caption CaptionList::operator[](CaptionVector::size_type i) const {
    ASSERT(0 <= i && i < size());
    return mList[i];
}

bool CaptionList::getCaptionIfChanged(double time, Caption &outCaption) {
    // Figure out what caption we desire to display at the this time.
    TimeIndexMap::iterator bound = mTimeIndexMap.lower_bound(time);
    CaptionVector::difference_type desired_caption;
    if (bound != mTimeIndexMap.end() && bound->first == time) {
        // We got an exact match.
        desired_caption = bound->second;
    } else if (bound == mTimeIndexMap.begin()) {
        // We overshot, but there's nothing to go back to.
        desired_caption = -1;
    } else {
        // Go back and get the previous caption.
        --bound;
        desired_caption = bound->second;
    }

    // If we've already returned this caption, or if we have no caption to
    // return, then return false.
    if (desired_caption == mLastCaptionReturned || desired_caption == -1)
        return false;
    
    // Return the caption we found.
    mLastCaptionReturned = desired_caption;
    outCaption = mList[desired_caption];
    return true;
}


//=========================================================================
//  Tests
//=========================================================================

#if BUILD_TEST_CASES

BEGIN_TEST_CASE(TestCaptionList, TestCase) {
    CaptionList caps("fixtures/captions.xml");
    CHECK_EQ(size_t(3), caps.size());
    CHECK_EQ(3.25, caps[1].time());
    CHECK_EQ("SPEAKER 2: Let's get started!", caps[1].text());
    CHECK_EQ(1*3600+2*60+6, caps[2].time());
    CHECK_EQ("SPEAKER 1: We begin with an\noverview of the clinic and\n"
             "the available rooms.", caps[2].text());

    // Embedded XML in captions.
    CHECK_EQ("SPEAKER 1: There are <i>lots</i> of things to\n"
             "do in this program. &amp;&lt;", caps[0].text());

    // Fetching captions as the movie plays.
    Caption cap;
    CHECK_EQ(false, caps.getCaptionIfChanged(0.0, cap));
    CHECK_EQ(true,  caps.getCaptionIfChanged(1.0, cap));
    CHECK_EQ(caps[0].text(), cap.text());
    CHECK_EQ(false, caps.getCaptionIfChanged(1.5, cap));
    CHECK_EQ(false, caps.getCaptionIfChanged(2.0, cap));
    CHECK_EQ(true,  caps.getCaptionIfChanged(3.5, cap));
    CHECK_EQ(caps[1].text(), cap.text());
    CHECK_EQ(false, caps.getCaptionIfChanged(3.5, cap));

    // What happens when the user rewinds?  What we do at 0.0 is only one
    // option; we could also choose to return an empty caption.
    CHECK_EQ(true,  caps.getCaptionIfChanged(2.0, cap));
    CHECK_EQ(caps[0].text(), cap.text());
    CHECK_EQ(false, caps.getCaptionIfChanged(0.0, cap));
} END_TEST_CASE(TestCaptionList);

#endif // BUILD_TEST_CASES
