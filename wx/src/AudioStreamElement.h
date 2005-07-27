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

#ifndef AudioStreamElement_H
#define AudioStreamElement_H

#include "Stage.h"
#include "InvisibleElement.h"
#include "MediaElement.h"

class AudioStream;

//////////
/// A widget represents a full-fledged wxWindow object hanging around on
/// our stage.  It does its own event processing.
///
class AudioStreamElement : public InvisibleElement, public MediaElement
{
	AudioStream *mStream;
    bool mEndPlaybackWasCalled;

public:
	AudioStreamElement(Stage *inStage, const wxString &inName,
					   AudioStream *inStream,
                       FIVEL_NS TCallbackPtr inDispatcher);
	virtual ~AudioStreamElement();

	AudioStream *GetAudioStream() { return mStream; }

    virtual bool HasReachedFrame(MovieFrame inFrame);
    virtual void Idle();
    virtual bool IsLooping();
	virtual void EndPlayback();
    virtual void Pause();
    virtual void Resume();
	
	virtual void SetVolume(const std::string &inChannel, double inVolume);
};

#endif // AudioStreamElement_H
