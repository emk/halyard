// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef VorbisFile_H
#define VorbisFile_H

#include "TCommon.h"
#include "ivorbisfile.h"

enum {
	// The decoder documentation claims this is a good size.
	VORBIS_BUFFER_BYTES = 4096,
	VORBIS_BUFFER_SIZE = VORBIS_BUFFER_BYTES / sizeof(int16)
};

class VorbisFile
{
	OggVorbis_File mVF;
	int16 mReadBuffer[VORBIS_BUFFER_SIZE];

public:
	VorbisFile(const char *inFileName);
	~VorbisFile();

	//////////
	// Read the next chunk from this file.  All output values remain
	// valid until the next call to ReadChunk.
	//
	// [out] outData - A pointer to the data.
	// [out] outSize - The size of the output data, in 16-bit words.
	// [out] outFrequency - The frequency of this data block, in hertz.
	//                 (Yes, this can change from block to block.)
	// [out] outChannels - The frequency of this data block, in hertz.
	//                 (Yes, this can change from block to block.)
	// [out] return -  true if a chunk was read, false if the file is done
	//
	bool ReadChunk(int16 **outData, size_t *outSize, int *outFrequency,
				   int *outChannels);
};

#endif // VorbisFile_H
