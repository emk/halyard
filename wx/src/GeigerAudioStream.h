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

#ifndef GeigerAudioStream_H
#define GeigerAudioStream_H

#include "AudioStream.h"

//////////
/// An audio stream which synthesizes the noise of a chirping Geiger counter.
/// Note that the technically correct term for "chirp" is "count", but "count"
/// is too confusing to use in an API, where it already has a conventional
/// meaning.
///
class GeigerAudioStream : public AudioStream
{
    enum { MAX_CHIRP_CURSORS = 10 };

    volatile float mChirpsPerSecond;

    float *mChirpBegin;
    float *mChirpEnd;
    float *mChirpCursors[MAX_CHIRP_CURSORS];

    int mChirpsPlayed;
    double mFrameEndTime;
	int mClipCount;

    void ZeroBuffer(float *outBuffer, unsigned long inFrames);
	void ClipBuffer(float *ioBuffer, unsigned long inFrames);
    bool DoesEventOccurGivenProbability(float inProbability);
    bool ShouldChirpDuringInterval(size_t inSamplesPerInterval);
    size_t FindCursorIndexForNewChirp();
    void UpdateChirpStateForInterval(unsigned long inFrames);
    void MixChirpIntoBuffer(size_t inCursor, float *ioBuffer,
							unsigned long inFrames);

public:
    GeigerAudioStream(const char *inFileName, float inVolume = 1.0f);
    ~GeigerAudioStream();
    
    void SetChirpsPerSecond(float inChirpsPerSecond)
		{ mChirpsPerSecond = inChirpsPerSecond; }

    virtual bool IsLooping() { return true; }

protected:
    bool FillBuffer(void *outBuffer, unsigned long inFrames,
					PaTimestamp inTime);	
};

#endif // GeigerAudioStream_H
