// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef GeigerAudioStream_H
#define GeigerAudioStream_H

#include "AudioStream.h"

#include <boost/shared_ptr.hpp>
#include <boost/shared_array.hpp>

//////////
// An audio stream which synthesizes the noise of a chirping Geiger counter.
// Note that the technically correct term for "chirp" is "count", but "count"
// is too confusing to use in an API, where it already has a conventional
// meaning.
//
class GeigerAudioStream : public AudioStream
{
    enum { MAX_CHIRP_CURSORS = 10 };

    volatile float mChirpsPerSecond;

    float *mChirpBegin;
    float *mChirpEnd;
    float *mChirpCursors[MAX_CHIRP_CURSORS];

    int mChirpsPlayed;
    double mFrameEndTime;

    void ZeroBuffer(float *outBuffer, unsigned long inFrames);
    bool DoesEventOccurGivenProbability(float inProbability);
    bool ShouldChirpDuringInterval(size_t inSamplesPerInterval);
    size_t FindCursorIndexForNewChirp();
    void UpdateChirpStateForInterval(unsigned long inFrames);
    void MixChirpIntoBuffer(size_t inCursor, float *ioBuffer,
			    unsigned long inFrames);

public:
    GeigerAudioStream(const char *inFileName);
    ~GeigerAudioStream();
    
    void SetChirpsPerSecond(float inChirpsPerSecond)
	{ mChirpsPerSecond = inChirpsPerSecond; }

protected:
    bool FillBuffer(void *outBuffer, unsigned long inFrames,
		    PaTimestamp inTime);	
};

#endif // GeigerAudioStream_H
