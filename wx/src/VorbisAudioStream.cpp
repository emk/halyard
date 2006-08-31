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
#include "VorbisAudioStream.h"
#include "VorbisFile.h"

USING_NAMESPACE_FIVEL


//=========================================================================
//  IntuitiveVolatile<T> Methods
//=========================================================================
//  See the comments in the header and "The JSR-133 Cookbook" for details:
//    http://gee.cs.oswego.edu/dl/jmm/cookbook.html

#ifdef FIVEL_PLATFORM_WIN32

#include <windows.h>

template <typename T>
void IntuitiveVolatile<T>::FullBarrier() const {
    // Fail if T isn't a single-word value, or isn't aligned on a word
    // boundary.  No sense in pushing our luck.
    //
    // PORTABILITY - On a 64-bit processor, you'll need to read the manuals
    // to figure out what happens with writes to larger values.
    // XXX - Figure out if this is actually safe to use with values smaller
    // than a word.
    ASSERT(sizeof(volatile T) <= 4);
    ASSERT((reinterpret_cast<size_t>(&mValue) % sizeof(void*)) == 0);

    // According to the MSDN API docs, InterlockedExchange guarantees a
    // full memory barrier.
    volatile LONG junk;
    (void) ::InterlockedExchange(&junk, 0);
}

#else
#error "Need to port memory barrier support to new platform."
#endif

template <typename T>
T IntuitiveVolatile<T>::read() const {
    T temp = mValue;
    // Make sure that any future loads or stores see values consistent with
    // what we read from mValue, and not random stale data in our read
    // cache.  This doesn't affect current generation x86 processors, but
    // there's apparently an "x86-SPO" proposal floating around which will
    // require this--and several non-x86 chips already need it.
    LoadLoadAndLoadStoreBarriers();
    return temp;
}

template <typename T>
void IntuitiveVolatile<T>::write(T inValue) {
    // Make sure that all previous stores are complete.  Not needed on
    // current-generation x86 chips, but mandatory on several other
    // architectures.
    StoreStoreBarrier();
    mValue = inValue;
    // Make sure that this store occurs before any following loads.
    // Required on current-generation x86 chips.
    StoreLoadBarrier();
}


//=========================================================================
//  VorbisAudioStream Methods
//=========================================================================
//  Because FillBuffer is called from an unknown (and possibly weird)
//  multithreaded context, this class does not use semaphores, critical
//  sections, or any other high-level synchronization mechanisms.  Instead,
//  we use a "circular buffer".
//
//  The basic idea: We have a single wraparound buffer (with one extra
//  frame's worth of unused storage, so we can tell the different between
//  an empty buffer and a full one), and two threads which access it.
//
//  1) The producer thread, which reads data from disk and stores it in
//     the buffer.  Only the producer thread can move mDataEnd forward,
//     and only once the data is in place.
//
//  2) The consumer thread, which reads data from the circular buffer and
//     copies it to portaudio.  Only the consumer thread can update
//     mDataBegin, and only once it has used the data.
//
//  It's important that the updates to mDataBegin and mDataEnd occur at
//  the appropriate times, even on a multiprocessor system with weird cache
//  behavior.  This is a very subtle problem to solve correctly.
//
//  This code was initially written under the assumption that "volatile"
//  did something useful in a multithreaded environment (no such luck).
//  The volatile variables have been replaced with an IntuitiveVolatile
//  type that implements Java 1.5 JSR-133 "volatile" semantics, which
//  should do something much more useful.

VorbisAudioStream::VorbisAudioStream(const char *inFileName,
									 size_t inBufferSize,
									 bool inShouldLoop,
                                     float inVolume)
	: AudioStream(INT16_PCM_STREAM, inVolume),
	  mFileName(inFileName), mShouldLoop(inShouldLoop),
	  // Pad the buffer with an extra frame which we'll never use.
	  // See IsBufferFull() for details.
	  mBufferSize(inBufferSize+CHANNELS),
	  mBuffer(new int16[inBufferSize+CHANNELS]),
      mDataBegin(0), mDataEnd(0), mDoneWithFile(false)
{
	// Buffer size must be a multiple of the number of channels.
	ASSERT(mBufferSize % CHANNELS == 0);

    mSamplesLoaded = 0;
	mUnderrunCount = 0;

	InitializeFile();

    // Allow background thread to start calling Idle().
    InitializationDone();
}

void VorbisAudioStream::LogFinalStreamInfo() {
	gDebugLog.Log("Stopped Vorbis stream, %d samples underrun",
				  mUnderrunCount);
}

void VorbisAudioStream::InitializeFile()
{
	mDoneWithFile.write(false);
	VorbisFile *file =
		new VorbisFile(mFileName.c_str(), SAMPLES_PER_SECOND, CHANNELS);
	mFile = shared_ptr<VorbisFile>(file);
}

void VorbisAudioStream::RestartFileIfLoopingAndDone()
{
	if (mDoneWithFile.read() && mShouldLoop)
		InitializeFile();
}

size_t VorbisAudioStream::ReadIntoBlock(int16 *inSpace, size_t inLength)
{
	size_t written;
	if (!mFile->Read(inSpace, inLength, &written))
		mDoneWithFile.write(true);
	ASSERT(!mDoneWithFile.read() || written == 0);
    mSamplesLoaded += written / CHANNELS;
	return written;
}

inline bool VorbisAudioStream::DoneReadingData() const
{
	return mDoneWithFile.read() && !mShouldLoop;
}

bool VorbisAudioStream::IsBufferFull() const
{
	// We always leave at least one empty frame somewhere in the buffer.
	// This means that an empty buffer can be represented as
	// mDataBegin == mDataEnd, and never be confused with a full buffer.
	return (mDataEnd.read() + CHANNELS) % mBufferSize == mDataBegin.read();
}

void VorbisAudioStream::GetFreeBlocks(int16 **outSpace1, size_t *outLength1,
									  int16 **outSpace2, size_t *outLength2)
{
	// Cache local copies of these variables to prevent interesting
	// things from happening when the other thread updates mDataBegin.
	int begin = mDataBegin.read();
	int end = mDataEnd.read();

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
	mDataEnd.write((mDataEnd.read() + inSize) % mBufferSize);
	ASSERT(0 <= mDataEnd.read() && mDataEnd.read() < mBufferSize);
}

bool VorbisAudioStream::IsDone() const {
    return (DoneReadingData() &&
            (GetSamplesPlayed() > mSamplesLoaded));
}

void VorbisAudioStream::Idle()
{
	// Add as much data to the buffer as we can.
	RestartFileIfLoopingAndDone();
	while (!DoneReadingData() && !IsBufferFull())
	{
		// Figure out how much free space we need to fill.
		int16 *space1, *space2;
		size_t length1, length2;
		GetFreeBlocks(&space1, &length1, &space2, &length2);

		// Fill as much space as we can.
		size_t total_written = 0;
		if (space1)
			total_written += ReadIntoBlock(space1, length1);
		if (space2)
			total_written += ReadIntoBlock(space2, length2);
		MarkAsWritten(total_written);

		RestartFileIfLoopingAndDone();
	}
}

double VorbisAudioStream::GetSamplesPlayed() const {
    // This is only approximate.  We may be substracting unplayed underrun
    // from the played sample count, so this may temporarily jump back in
    // time if we've just queued a bunch of empty buffers.  We could get
    // more accurate results at the cost of a lot more work.
    return AudioStream::GetSamplesPlayed() - mUnderrunCount;
}

bool VorbisAudioStream::FillBuffer(void *outBuffer, unsigned long inFrames,
								   PaTimestamp inTime)
{
	// This routine assumes 2 channels.  If you allow different numbers
	// of channels, you'll need to fix it.
	ASSERT(CHANNELS == 2);

	int16 *buffer = (int16 *) outBuffer;
	size_t begin = mDataBegin.read();
	size_t end = mDataEnd.read();

	// We don't have any data, and we're not getting more.  Quit.
	if (begin == end && DoneReadingData())
		return true;

	// Write our data to the output buffer, and pad with zeros if
	// necessary.
	for (size_t i = 0; i < inFrames; i++)
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
			/// \todo Although we inline this conditional, is it still too
			/// expensive?
            if (!DoneReadingData()) 
                mUnderrunCount++;
		}
	}
	mDataBegin.write(begin);
	return false;
}
