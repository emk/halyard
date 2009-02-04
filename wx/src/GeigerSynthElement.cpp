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

#include "AppHeaders.h"
#include <boost/mem_fn.hpp>
#include "GeigerSynthElement.h"
#include "GeigerAudioStream.h"
#include "VorbisAudioStream.h"

GeigerSynthElement::GeigerSynthElement(Stage *inStage, const wxString &inName,
                                       const std::string &inStatePath,
                                       const char *inChirpLocation,
                                       size_t inBufferSize,
                                       double inVolume)
    : InvisibleElement(inStage, inName),
      mStatePath(inStatePath),
      mCurrentLoopCps(0), mBufferSize(inBufferSize),
      mInitialVolume(inVolume)
{
    // We need to pass a custom cleanup function to shared_ptr because we're
    // not allowed to simply 'delete' AudioStream elements.
    mGeigerAudioStream =
        GeigerAudioStreamPtr(new GeigerAudioStream(inChirpLocation, inVolume),
                             boost::mem_fn(&GeigerAudioStream::Delete));
    NotifyStateChanged();
}

GeigerSynthElement::~GeigerSynthElement() {
    mGeigerAudioStream->Stop();
}

void GeigerSynthElement::AddLoop(double inLoopCps, const char *inLoopLocation)
{
    // We need to pass a custom cleanup function to shared_ptr because we're
    // not allowed to simply 'delete' AudioStream elements.
    VorbisAudioStreamPtr stream(new VorbisAudioStream(inLoopLocation,
                                                      mBufferSize,
                                                      true,
                                                      mInitialVolume),
                                boost::mem_fn(&VorbisAudioStream::Delete));
    mLoopStreams.insert(LoopMap::value_type(inLoopCps, stream));
}

void GeigerSynthElement::DoneAddingLoops() {
    mGeigerAudioStream->Start();
}

void GeigerSynthElement::SetChirpsPerSecond(double inCPS) {
    mGeigerAudioStream->SetChirpsPerSecond(inCPS);
}

void GeigerSynthElement::NotifyStateChanged() {
    double cps(tvalue_cast<double>(gStateDB.Get(this, mStatePath)));
    SetChirpsPerSecond(cps);
}

bool GeigerSynthElement::IsDone() {
    return mGeigerAudioStream->IsDone();
}

MovieFrame GeigerSynthElement::CurrentFrame() {
    return mGeigerAudioStream->GetTime() * FRAMES_PER_SECOND;
}

void GeigerSynthElement::Idle() {
    MediaElementIdle();
}

void GeigerSynthElement::EndPlayback()
{
    MediaElement::EndPlayback();
    mGeigerAudioStream->Stop();
}

void GeigerSynthElement::Pause()
{
    mGeigerAudioStream->Stop();
}

void GeigerSynthElement::Resume()
{
    mGeigerAudioStream->Start();
}

void GeigerSynthElement::SetVolume(const std::string &inChannel,
                                   double inVolume)
{
    mGeigerAudioStream->SetChannelVolume(inChannel, inVolume);
}
