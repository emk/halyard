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

#ifndef VorbisFile_H
#define VorbisFile_H

#include "ivorbisfile.h"

enum {
	/// The decoder documentation claims this is a good size.
	VORBIS_BUFFER_BYTES = 4096,
	VORBIS_BUFFER_SIZE = VORBIS_BUFFER_BYTES / sizeof(Halyard::int16)
};

/// Reads and decodes a file containing Ogg Vorbis audio data.
class VorbisFile
{
	OggVorbis_File mVF;
	
	int mWantedFrequency;
	int mWantedChannels;

	/// We use a small buffer for reading data from the Vorbis codec, and
	/// storing it until Read() is called.  mBufferBegin points to the
	/// first valid data in the buffer, and mBufferEnd points one past
	/// the last valid data.
	Halyard::int16 mBuffer[VORBIS_BUFFER_SIZE];
	Halyard::int16 *mBufferBegin, *mBufferEnd;
	int mBufferFrequency;
	int mBufferChannels;
	bool mDoneReading;

	/// Read the next chunk from this file, and update the mBuffer
	/// variables accordingly.
	bool ReadChunk();

	void TryToRefillBufferIfEmpty();
	bool MoreDataIsAvailable();
	void CheckBufferFrequency();
	int CheckBufferChannelCountAndGetStretchFactor();
	size_t GetBufferedSampleCount();
	void GetSamplesFromBuffer(Halyard::int16 *outOutputBuffer,
							  size_t inOutputSampleCount,
							  int inStretchFactor);

public:
	//////////
	/// Open an Ogg Vorbis audio file for reading.  You must specify
	/// the data format you wish to receive; this class has limited
	/// conversion capabilities.
	///
	/// \param inFileName  The name of the file to open
	/// \param inWantedFrequency  The frequency of the data we want to read
	/// \param inWantedChannels  The number of channels we want to read
	///
	VorbisFile(const char *inFileName, int inWantedFrequency,
			   int inWantedChannels);
	~VorbisFile();

	//////////
	/// Read the specified amount of data into a buffer, using the
	/// specified format.
	///
	/// \param outData  (out) The buffer to fill
	/// \param inMaxSize  The maximum size of the buffer
	/// \param outSizeUsed  (out) The amount of data actually read
	/// \param result  (out) true if the read succeeded, false if we've reached
	///                the end of the file
	///
	bool Read(Halyard::int16 *outData, size_t inMaxSize, size_t *outSizeUsed);

	//////////
	/// Read all remaining data from the file and return it in an
	/// appropriately-sized vector.
	///
	/// \return  The buffer.  The caller must delete this.
	///
	std::vector<Halyard::int16> *ReadAll();
};

#endif // VorbisFile_H
