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

public:
	VorbisAudioStream(const char *inFileName, size_t inBufferSize);

	virtual void Idle();
	
protected:
	bool FillBuffer(float *outBuffer, unsigned long inFrames,
					PaTimestamp inTime);	
};


#endif // VorbisAudioStream_H

