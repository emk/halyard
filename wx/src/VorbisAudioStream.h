// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef VorbisAudioStream_H
#define VorbisAudioStream_H

#include "AudioStream.h"

#include <boost/scoped_ptr.hpp>
#include <boost/scoped_array.hpp>

class VorbisFile;

class VorbisAudioStream : public AudioStream
{
	boost::scoped_ptr<VorbisFile> mFile;
	boost::scoped_array<int16> mBuffer;
	size_t mBufferSize;
	volatile size_t mDataBegin;
	volatile size_t mDataEnd;
	volatile bool mDone;

	enum {
		CHANNELS = 2
	};

	//////////
	// Return true if the circular buffer is full.
	//
	bool IsBufferFull();

	//////////
	// Get pointers to the free space in the buffer.  Because the buffer is
	// circular, the free space may be discontiguous.  If the space is
	// contiguous, *outSpace2 will be NULL.  If no space is available, both
	// *outSpace1 and *outSpace2 will be NULL.
	//
	// This API is inspired by some interfaces in DirectSound.
	//
	void GetFreeBlocks(int16 **outSpace1, size_t *outLength1,
					   int16 **outSpace2, size_t *outLength2);

	//////////
	// Mark the specified number of samples as written.
	//
	void MarkAsWritten(size_t inSize);

public:
	VorbisAudioStream(const char *inFileName, size_t inBufferSize);

	virtual void Idle();
	
protected:
	bool FillBuffer(void *outBuffer, unsigned long inFrames,
					PaTimestamp inTime);	
};


#endif // VorbisAudioStream_H

