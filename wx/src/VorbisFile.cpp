// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TException.h"

#include "VorbisFile.h"

// libvorbis supposedly wants these.
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "ivorbiscodec.h"

VorbisFile::VorbisFile(const char *inFileName)
{
	// Open the underlying file.
	FILE *file = fopen(inFileName, "rb");

	// Ask Ogg Vorbis to start the decoding process.
	if (ov_open(file, &mVF, NULL, 0) < 0)
		THROW("Bitstream is not a Vorbis bitstream");
}

VorbisFile::~VorbisFile()
{
	// Shut down our decoding stream.
	ov_clear(&mVF);
}

bool VorbisFile::ReadChunk(int16 **outData, size_t *outSize,
						   int *outFrequency, int *outChannels)
{
	int current_section;
	long ret = ov_read(&mVF, (char *) mReadBuffer, VORBIS_BUFFER_SIZE,
					   &current_section);
	if (ret == 0)
		return false;
	else if (ret < 0)
		THROW("Error reading from Vorbis audio file");
	else
	{
		// Fetch some more information.
		vorbis_info *info = ov_info(&mVF, current_section);
		ASSERT(info);

		// Return all the information to our caller.
		*outData = mReadBuffer;
		*outSize = ret / sizeof(int16);
		*outFrequency = info->rate;
		*outChannels = info->channels;
		return true;
	}
}
