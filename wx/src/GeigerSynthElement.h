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

#ifndef GeigerSynthElement_H
#define GeigerSynthElement_H

#include "InvisibleElement.h"
#include "MediaElement.h"
#include "TStateDB.h"

USING_NAMESPACE_FIVEL

class GeigerAudioStream;
class VorbisAudioStream;

//////////
/// A GeigerSynthElement manages a number of separate audio streams to
/// acheive a complete Geiger-counter-like effect.
///
class GeigerSynthElement : public InvisibleElement, public MediaElement,
                           public TStateListener
{
    typedef shared_ptr<VorbisAudioStream> VorbisAudioStreamPtr;
    typedef shared_ptr<GeigerAudioStream> GeigerAudioStreamPtr;
    typedef std::map<double,VorbisAudioStreamPtr> LoopMap;

    std::string mStatePath;
    GeigerAudioStreamPtr mGeigerAudioStream;
    LoopMap mLoopStreams;
    double mCurrentLoopCps;
    size_t mBufferSize;
    double mInitialVolume;

    void SetChirpsPerSecond(double inCPS);

protected:
    virtual bool IsDone();
    virtual MovieFrame CurrentFrame();

public:
	GeigerSynthElement(Stage *inStage, const wxString &inName,
                       const std::string &inStatePath,
                       const char *inChirpLocation,
                       size_t inBufferSize,
                       double inVolume = 1.0);
    ~GeigerSynthElement();

    void AddLoop(double inLoopCps, const char *inLoopLocation);
    void DoneAddingLoops();
    
    virtual void NotifyStateChanged();

    virtual void Idle();
    virtual bool IsLooping() { return true; }
	virtual void EndPlayback();
    virtual void Pause();
    virtual void Resume();
	virtual void SetVolume(const std::string &inChannel, double inVolume);
};

#endif // GeigerSynthElement_H
