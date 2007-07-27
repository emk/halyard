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

#include "TamaleHeaders.h"
#include "CaptionList.h"
#include "Element.h"
#include "MediaElement.h"

USING_NAMESPACE_FIVEL

MediaElement::MediaElement()
    : mEndPlaybackWasCalled(false), mHaveSentMediaFinishedEvent(false)
{
    // Do nothing.
}

void MediaElement::AttachCaptionFile(const std::string &inCaptionFile) {
    mCaptions = shared_ptr<CaptionList>(new CaptionList(inCaptionFile));
}

void MediaElement::EndPlayback() {
	mEndPlaybackWasCalled = true;
}

bool MediaElement::HasReachedFrame(MovieFrame inFrame) {
	if (mEndPlaybackWasCalled)
		return true;
	else if (inFrame == LAST_FRAME)
		return IsDone();
	else
		return IsDone() || (CurrentFrame() >= inFrame);
}

void MediaElement::MediaElementIdle() {
    // We know this class is always mixed into the Element hierarchy, so
    // get a pointer to Element that we can use to access our event
    // dispatcher.
    //
    // In theory, we should use C++ virtual inheritence to make
    // MediaElement a subclass of Element, but then classes like
    // MovieElement would inherit from Element along two different routes.
    // This doesn't work unless we use virtual inheritence, and virtual
    // inheritence is notoriously broken (go see Google).  So we're just
    // going to hack it.
    Element *thisAsElement = dynamic_cast<Element*>(this);
    ASSERT(thisAsElement);

    // Decide what frame we're on.  If we're finished with playback, assume
    // that we're on LAST_FRAME.
    // TODO - Code duplication with HasReachedFrame.
    MovieFrame current_frame;
    if (mEndPlaybackWasCalled || IsDone())
        current_frame = LAST_FRAME;
    else
        current_frame = CurrentFrame();

    // See if we have any captions to display, and if so, send an event.
    double current_time = current_frame / FRAMES_PER_SECOND;
    Caption cap;
    if (mCaptions && mCaptions->getCaptionIfChanged(current_time, cap))
        thisAsElement->GetEventDispatcher()->DoEventMediaCaption(cap.text());

    // If we've reached the end of the movie, send a MediaFinished event.
    if (!mHaveSentMediaFinishedEvent && HasReachedFrame(LAST_FRAME)) {
        mHaveSentMediaFinishedEvent = true;

        // Send the actual event.
        thisAsElement->GetEventDispatcher()->DoEventMediaFinished();
    }
}
