// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"
#include "TException.h"

#include "VorbisFile.h"

// libvorbis supposedly wants these.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "ivorbiscodec.h"

VorbisFile::VorbisFile(const char *inFileName, int inWantedFrequency,
					   int inWantedChannels)
	: mWantedFrequency(inWantedFrequency), mWantedChannels(inWantedChannels),
	  mBufferBegin(mBuffer), mBufferEnd(mBuffer), mDoneReading(false)
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

bool VorbisFile::ReadChunk()
{
	ASSERT(mBufferBegin == mBufferEnd);

	int current_section;
	long ret = ov_read(&mVF, (char *) mBuffer, VORBIS_BUFFER_SIZE,
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

		// Store the information for future use.
		mBufferBegin = mBuffer;
		mBufferEnd = mBufferBegin + (ret / sizeof(int16));
		mBufferFrequency = info->rate;
		mBufferChannels = info->channels;
		return true;
	}
}

bool VorbisFile::Read(int16 *outData, size_t inMaxSize, size_t *outSizeUsed)
{
	int16 *remaining_space_ptr  = outData;
	size_t remaining_space_size = inMaxSize;

	*outSizeUsed = 0;
	while (!mDoneReading && *outSizeUsed < inMaxSize)
	{
		// Refill our read buffer, if needed.
		if (mBufferBegin == mBufferEnd)
		{
			mDoneReading = !ReadChunk();
			if (mDoneReading)
				break;
		}

		// Check to see whether our buffer format is OK.
		if (mWantedFrequency != mBufferFrequency)
			THROW("Vorbis file has wrong frequency");
		if (mWantedChannels != mBufferChannels)
			THROW("Vorbis file has wrong number of channels");
		
		// Figure out how much data we can use right now.
		int available_data_size = mBufferEnd - mBufferBegin;
		int copy_size = Min(available_data_size, remaining_space_size);

		// Copy the data, and update our pointers.
		memcpy(remaining_space_ptr, mBufferBegin, copy_size * sizeof(int16));
		*outSizeUsed += copy_size;
		mBufferBegin += copy_size;

		// Locally update these values for another trip around the loop.
		remaining_space_ptr += copy_size;
		remaining_space_size -= copy_size;
	}

	// Tell the caller whether or not we succeeded.
	if (mDoneReading && *outSizeUsed == 0)
		return false;
	else
		return true;
}

