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

#ifndef VorbisAudioStream_H
#define VorbisAudioStream_H

#include "AudioStream.h"

class VorbisFile;

/// An AudioStream which reads from a VorbisFile.
class VorbisAudioStream : public AudioStream
{
	std::string mFileName;
	bool mShouldLoop;
	shared_ptr<VorbisFile> mFile;
	shared_array<int16> mBuffer;
	size_t mBufferSize;
	volatile size_t mDataBegin;
	volatile size_t mDataEnd;
	volatile bool mDoneWithFile;
    /// \todo Big enough data type?
    size_t mSamplesLoaded;
    /// \todo Big enough data type?
	size_t mUnderrunCount;

	enum {
		CHANNELS = 2
	};

	void InitializeFile();

	//////////
	/// If we're supposed to be looping, and we're out of data, re-open
	/// our file and read it from the beginning.
	///
	void RestartFileIfLoopingAndDone();

	size_t ReadIntoBlock(int16 *inSpace, size_t inLength);
	bool DoneReadingData() const;

	//////////
	/// Return true if the circular buffer is full.
	///
	bool IsBufferFull() const;

	//////////
	/// Get pointers to the free space in the buffer.  Because the buffer is
	/// circular, the free space may be discontiguous.  If the space is
	/// contiguous, *outSpace2 will be NULL.  If no space is available, both
	/// *outSpace1 and *outSpace2 will be NULL.
	///
	/// This API is inspired by some interfaces in DirectSound.
	///
	void GetFreeBlocks(int16 **outSpace1, size_t *outLength1,
					   int16 **outSpace2, size_t *outLength2);

	//////////
	/// Mark the specified number of samples as written.
	///
	void MarkAsWritten(size_t inSize);

public:
	VorbisAudioStream(const char *inFileName, size_t inBufferSize,
					  bool inShouldLoop);
	~VorbisAudioStream();

    virtual bool IsDone() const;
    virtual bool IsLooping() { return mShouldLoop; }
	virtual void Idle();

protected:
    virtual double GetSamplesPlayed() const;
	virtual bool FillBuffer(void *outBuffer, unsigned long inFrames,
                            PaTimestamp inTime);	
};


#endif // VorbisAudioStream_H

