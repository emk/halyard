// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"
#include "TLogger.h"
#include "VorbisAudioStream.h"
#include "VorbisFile.h"


//=========================================================================
//  VorbisAudioStream Methods
//=========================================================================

VorbisAudioStream::VorbisAudioStream(const char *inFileName,
									 size_t inBufferSize)
	: AudioStream(INT16_PCM_STREAM),
	  mFile(new VorbisFile(inFileName, SAMPLES_PER_SECOND, CHANNELS)),
	  // Pad the buffer with an extra frame which we'll never use.
	  // See IsBufferFull() for details.
	  mBufferSize(inBufferSize+CHANNELS),
	  mBuffer(new int16[inBufferSize+CHANNELS])
{
	// Buffer size must multiple of number of channels.
	ASSERT(mBufferSize % CHANNELS == 0);

	mDataBegin = 0;
	mDataEnd = 0;
	mDone = false;

	// Fill our buffer.
	Idle();
}

bool VorbisAudioStream::IsBufferFull()
{
	// We always leave at least one empty frame somewhere in the buffer.
	// This means that an empty buffer can be represented as
	// mDataBegin == mDataEnd, and never be confused with a full buffer.
	return (mDataEnd + CHANNELS) % mBufferSize == mDataBegin;
}

void VorbisAudioStream::GetFreeBlocks(int16 **outSpace1, size_t *outLength1,
									  int16 **outSpace2, size_t *outLength2)
{
	// Cache local copies of these variables to prevent interesting
	// things from happening when the other thread updates mDataBegin.
	int begin = mDataBegin;
	int end = mDataEnd;

	// Figure out where the empty space is.  (We must leave one frame
	// empty.  See IsBufferFull() for details.)
	if ((end + CHANNELS) % mBufferSize == begin)
	{
		// The buffer is already full, so there's no free space.
		*outSpace1  = NULL;
		*outLength1 = 0;
		*outSpace2  = NULL;
		*outLength2 = 0;
	}
	else if (end < begin)
	{
		// Simple contiguous free space.
		*outSpace1  = &mBuffer[end];
		*outLength1 = begin - end - CHANNELS;
		*outSpace2  = NULL;
		*outLength2 = 0;
	}
	else if (begin == 0)
	{
		// Sneaky contiguous free space.  Handle this case separately to
		// avoid complicating the case below.
		*outSpace1  = &mBuffer[end];
		*outLength1 = mBufferSize - end - CHANNELS;
		*outSpace2  = NULL;
		*outLength2 = 0;
	}
	else
	{
		// True wraparound!
		ASSERT(begin >= CHANNELS);
		*outSpace1  = &mBuffer[end];
		*outLength1 = mBufferSize - end;
		*outSpace2  = &mBuffer[0];
		*outLength2 = begin - CHANNELS;
	}

	// Make sure we're returning entire frames.
	ASSERT(*outLength1 % CHANNELS == 0);
	ASSERT(*outLength2 % CHANNELS == 0);
}

void VorbisAudioStream::MarkAsWritten(size_t inSize)
{
	mDataEnd = (mDataEnd + inSize) % mBufferSize;
	ASSERT(0 <= mDataEnd && mDataEnd < mBufferSize);
}

void VorbisAudioStream::Idle()
{
	// Add as much data to the buffer as we can.
	while (!mDone && !IsBufferFull())
	{
		// Figure out how much free space we need to fill.
		int16 *space1, *space2;
		size_t length1, length2, written, total_written;
		GetFreeBlocks(&space1, &length1, &space2, &length2);

		// Fill as much space as we can.
		total_written = 0;
		if (space1)
		{
			if (!mFile->Read(space1, length1, &written))
				mDone = true;
			else
				total_written += written;
		}
		if (space2)
		{
			if (!mFile->Read(space2, length2, &written))
				mDone = true;
			else
				total_written += written;
		}
		MarkAsWritten(total_written);
	}
}

bool VorbisAudioStream::FillBuffer(void *outBuffer, unsigned long inFrames,
								   PaTimestamp inTime)
{
	// This routine assumes 2 channels.  If you allow different numbers
	// of channels, you'll need to fix it.
	ASSERT(CHANNELS == 2);

	int16 *buffer = (int16 *) outBuffer;
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
			*buffer++ = mBuffer[begin++];
			ASSERT(begin != end && begin != mBufferSize);
			*buffer++ = mBuffer[begin++];
			begin %= mBufferSize;
			ASSERT(0 <= begin && begin < mBufferSize);
		}
		else
		{
			// We have no data with which to fill the buffer.  (We
			// may get more later if mDone is false.)
			*buffer++ = 0;
			*buffer++ = 0;
		}
	}
	mDataBegin = begin;
	return false;
}
