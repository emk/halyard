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

#include "AppHeaders.h"

#include "AudioStreamElement.h"
#include "AudioStream.h"


//=========================================================================
//  AudioStreamElement Methods
//=========================================================================

AudioStreamElement::AudioStreamElement(Stage *inStage, const wxString &inName,
									   AudioStream *inStream,
                                       Halyard::TCallbackPtr inDispatcher)
    : InvisibleElement(inStage, inName, inDispatcher), mStream(inStream)
{
    mStream->Start();
}

AudioStreamElement::~AudioStreamElement()
{
    mStream->Stop();
    mStream->Delete();
}

bool AudioStreamElement::IsDone() {
    return mStream->IsDone();
}

MovieFrame AudioStreamElement::CurrentFrame() {
    return mStream->GetTime() * FRAMES_PER_SECOND;
}

void AudioStreamElement::Idle() {
    MediaElementIdle();
}

bool AudioStreamElement::IsLooping()
{
    return mStream->IsLooping();
}

void AudioStreamElement::EndPlayback()
{
    MediaElement::EndPlayback();
	mStream->Stop();
}

void AudioStreamElement::Pause()
{
    mStream->Stop();
}

void AudioStreamElement::Resume()
{
    mStream->Start();
}

void AudioStreamElement::SetVolume(const std::string &inChannel,
								   double inVolume)
{
    mStream->SetChannelVolume(inChannel, inVolume);
}
