// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"

#include <stdlib.h>

#include "GeigerAudioStream.h"
#include "VorbisFile.h"


//=========================================================================
//  GeigerAudioStream Methods
//=========================================================================

GeigerAudioStream::GeigerAudioStream(const char *inFileName)
    : AudioStream(FLOAT32_STREAM)
{
	// Read our data from the file.
    VorbisFile file(inFileName, SAMPLES_PER_SECOND, GetChannelCount());
	std::vector<int16> *data = file.ReadAll();

	// Set up our member variables.
	mChirpsPerSecond = 0.0;
	size_t len = data->size();
	mChirpBegin  = new float[len];
	mChirpEnd    = mChirpBegin + len;
	for (size_t i = 0; i < MAX_CHIRP_CURSORS; i++)
		mChirpCursors[i] = NULL;

	// Convert data to floating point.
	for (size_t j = 0; j < len; j++)
		mChirpBegin[j] = int16_pcm_to_float((*data)[j]);

	// Delete our read data buffer.
	delete data;

	// Prepare for logging.
	mChirpsPlayed = 0;
	mFrameEndTime = 0;
	mClipCount = 0;
}

GeigerAudioStream::~GeigerAudioStream()
{
	gDebugLog.Log("Played %d geiger counts in %.1f seconds, %d samples "
				  "clipped",
				  mChirpsPlayed, mFrameEndTime / SAMPLES_PER_SECOND,
				  mClipCount);
	delete [] mChirpBegin;
}

void GeigerAudioStream::ZeroBuffer(float *outBuffer, unsigned long inFrames)
{
	ASSERT(GetChannelCount() == 2);
	float *cursor = outBuffer;
    for (int i = 0; i < inFrames; i++)
	{
		*cursor++ = 0.0f;
		*cursor++ = 0.0f;
	}
}

void GeigerAudioStream::ClipBuffer(float *ioBuffer, unsigned long inFrames)
{
	size_t sample_count = inFrames * GetChannelCount();
	float *cursor = ioBuffer;
    for (int i = 0; i < sample_count; i++)
	{
		float sample = *cursor;
		if (sample > 1.0)
		{
			*cursor = 1.0;
			mClipCount++;
		}
		if (sample < -1.0)
		{
			*cursor = -1.0;
			mClipCount++;
		}
		cursor++;
	}
}

bool GeigerAudioStream::DoesEventOccurGivenProbability(float inProbability)
{
	// NOTE - rand() sucks.  It only generates about 15 bits of entropy,
	// and the low order bits show highly regular patterns.
	//
	// THREAD - XXX - Technically, we shouldn't call rand() from a
	// multimedia timer; it might scramble the generator state.  But
	// it works well enough in practice.
	int scaled_probability = inProbability * RAND_MAX;
	return (rand() < scaled_probability);
}

bool GeigerAudioStream::ShouldChirpDuringInterval(size_t inSamplesPerInterval)
{
	float chirps_per_sample = mChirpsPerSecond / SAMPLES_PER_SECOND;
	float chirps_per_interval = chirps_per_sample * inSamplesPerInterval;
	ASSERT(chirps_per_interval <= 1.0);
	return DoesEventOccurGivenProbability(chirps_per_interval);
}

size_t GeigerAudioStream::FindCursorIndexForNewChirp()
{
	size_t best_candidate = 0;
	for (size_t i = 0; i < MAX_CHIRP_CURSORS; i++)
	{
		if (mChirpCursors[i] == NULL)
			return i;
		else if (mChirpCursors[i] > mChirpCursors[best_candidate])
			best_candidate = i;
	}
	return best_candidate;
}

void GeigerAudioStream::UpdateChirpStateForInterval(unsigned long inFrames)
{
	if (ShouldChirpDuringInterval(inFrames))
	{
		mChirpCursors[FindCursorIndexForNewChirp()] = mChirpBegin;
		mChirpsPlayed++;
	}
}

void GeigerAudioStream::MixChirpIntoBuffer(size_t inCursor,
										   float *ioBuffer,
										   unsigned long inFrames)
{
	float *cursor = ioBuffer;
    for (int i = 0; mChirpCursors[inCursor] && i < inFrames; i++)
    {
		*cursor++ += *(mChirpCursors[inCursor])++;
		ASSERT(mChirpCursors[inCursor] < mChirpEnd);
		*cursor++ += *(mChirpCursors[inCursor])++;
		
		// We're done with this one.
		if (mChirpCursors[inCursor] == mChirpEnd)
			mChirpCursors[inCursor] = NULL;
    }
}

bool GeigerAudioStream::FillBuffer(void *outBuffer, unsigned long inFrames,
								   PaTimestamp inTime)
{
	ASSERT(GetChannelCount() == 2);
	float *buffer = (float *) outBuffer;

	ZeroBuffer(buffer, inFrames);
	UpdateChirpStateForInterval(inFrames);
	for (size_t i = 0; i < MAX_CHIRP_CURSORS; i++)
		MixChirpIntoBuffer(i, buffer, inFrames);
	ClipBuffer(buffer, inFrames);

	mFrameEndTime = inTime + inFrames;
    return false;
}
