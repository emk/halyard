// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"
#include "TLogger.h"
#include "VorbisAudioStream.h"
#include "VorbisFile.h"


//=========================================================================
//  Utility Functions
//=========================================================================

static inline float int16_to_float(int16 inSample)
{
	return (inSample / 32768.0f);
}


//=========================================================================
//  VorbisAudioStream Methods
//=========================================================================

VorbisAudioStream::VorbisAudioStream(const char *inFileName,
									 size_t inBufferSize)
	: mFile(new VorbisFile(inFileName)), mBufferSize(inBufferSize),
	  mBuffer(new int16[inBufferSize+2])
{
	ASSERT(mBufferSize % 2 == 0); // Buffer size must be even.
	mDataBegin = 0;
	mDataEnd = 0;
	mDone = false;

	// Fill our buffer.
	Idle();
}

void VorbisAudioStream::Idle()
{
	int16 *data;
	size_t size;
	int frequency, channels;

	// Add as much data to the buffer as we can.
	while (!mDone && ((mDataEnd + 2) % mBufferSize != mDataBegin))
	{
		if (!mFile->ReadChunk(&data, &size, &frequency, &channels))
			mDone = true;
		else if (frequency != SAMPLES_PER_SECOND || channels != 2)
		{
			// TODO - Handle a few of the more useful formats instead
			// of giving up.
			gLog.Caution("Vorbis file wasn't 44.1kHz stero");
			mDone = true;
		}
		else
		{
			for (size_t i = 0;
				 i < size && ((mDataEnd + 2) % mBufferSize != mDataBegin);
				 i+=2)
			{
				mBuffer[mDataEnd] = data[i];
				mBuffer[mDataEnd+1] = data[i+1];
				mDataEnd = (mDataEnd + 2) % mBufferSize;
				ASSERT(0 <= mDataEnd && mDataEnd < mBufferSize);
			}
		}
	}
}

bool VorbisAudioStream::FillBuffer(float *outBuffer, unsigned long inFrames,
								   PaTimestamp inTime)
{
	float *buffer = (float *) outBuffer;
	size_t begin = mDataBegin;
	size_t end = mDataEnd;

	// We don't have any data, and we're not getting more.  Quit.
	if (begin == end && mDone)
		return true;

	// Write our data to the output buffer, and pad with zeros if
	// necessary.
	for (int i = 0; i < inFrames; i++)
	{
		if (begin != end)
		{
			// Copy samples into the synth output buffer.
			*buffer++ = int16_to_float(mBuffer[begin++]);
			ASSERT(begin != end && begin != mBufferSize);
			*buffer++ = int16_to_float(mBuffer[begin++]);
			begin %= mBufferSize;
			ASSERT(0 <= begin && begin < mBufferSize);
		}
		else
		{
			// We have no data with which to fill the buffer.  (We
			// may get more later if mDone is false.)
			*buffer++ = 0.0;
			*buffer++ = 0.0;
		}
	}
	mDataBegin = begin;
	return false;
}

