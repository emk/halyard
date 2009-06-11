// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

#include "AppHeaders.h"
#include "VorbisFile.h"

// libvorbis supposedly wants these.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "ivorbiscodec.h"

using namespace Halyard;

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

void VorbisFile::TryToRefillBufferIfEmpty()
{
    if (mBufferBegin == mBufferEnd && !mDoneReading)
        mDoneReading = !ReadChunk();
}

bool VorbisFile::MoreDataIsAvailable()
{
    TryToRefillBufferIfEmpty();
    return (mBufferBegin < mBufferEnd) || !mDoneReading;
}

void VorbisFile::CheckBufferFrequency()
{
    // We only support 44.1 audio for now.
    if (mWantedFrequency != mBufferFrequency)
        THROW("Vorbis file has wrong frequency");
}

int VorbisFile::CheckBufferChannelCountAndGetStretchFactor()
{
    if (mWantedChannels == mBufferChannels)
        // If we have the right number of channels, we can use the data as is.
        return 1;
    else if (mWantedChannels == 2 && mBufferChannels == 1)
        // If we have a mono file, and we want stero data, we can stretch it.
        return 2;
    else
        THROW("Vorbis file has wrong number of channels");
}

size_t VorbisFile::GetBufferedSampleCount()
{
    return mBufferEnd - mBufferBegin;
}

void VorbisFile::GetSamplesFromBuffer(int16 *outOutputBuffer,
                                      size_t inOutputSampleCount,
                                      int inStretchFactor)
{
    // Copy the data, and update our pointers.
    if (inStretchFactor == 1)
    {
        // The formats match exactly.  We can do a fast copy.
        memcpy(outOutputBuffer, mBufferBegin,
               inOutputSampleCount * sizeof(int16));
    }
    else
    {
        // Duplicate each sample for the left and right channel.
        ASSERT(inStretchFactor == 2);
        ASSERT(inOutputSampleCount % inStretchFactor == 0);
        size_t out_idx = 0, in_idx = 0;
        while (out_idx < inOutputSampleCount)
        {
            int16 sample = mBufferBegin[in_idx++];
            outOutputBuffer[out_idx++] = sample;
            outOutputBuffer[out_idx++] = sample;
        }
    }
    ASSERT(inOutputSampleCount % inStretchFactor == 0);
    mBufferBegin += inOutputSampleCount / inStretchFactor;
}

bool VorbisFile::Read(int16 *outData, size_t inMaxSize, size_t *outSizeUsed)
{
    int16 *remaining_space_ptr  = outData;

    *outSizeUsed = 0;
    while (*outSizeUsed < inMaxSize && MoreDataIsAvailable())
    {
        CheckBufferFrequency();
        int stretch = CheckBufferChannelCountAndGetStretchFactor();

        // Figure out how much data we can use right now.  These numbers
        // are calculated in terms of our output format, not our input
        // format.
        size_t available_sample_count = GetBufferedSampleCount() * stretch;
        size_t copy_sample_count = Min(available_sample_count,
                                       inMaxSize - *outSizeUsed);

        GetSamplesFromBuffer(remaining_space_ptr, copy_sample_count, stretch);
        *outSizeUsed += copy_sample_count;
        remaining_space_ptr += copy_sample_count;
    }

    // Tell the caller whether or not we succeeded.
    if (*outSizeUsed == 0 && !MoreDataIsAvailable())
        return false;
    else
        return true;
}

std::vector<int16> *VorbisFile::ReadAll()
{
    // Load our data from a file.
    std::vector<int16> *data = new std::vector<int16>(VORBIS_BUFFER_SIZE);
    size_t space_used = 0;
    while (MoreDataIsAvailable())
    {
        // If we've run out of space to put the data in, double our vector
        // size (doubling gives us amortitized O(n) time).
        if (data->size() == space_used)
            data->resize(data->size() * 2, 0);
        
        // Read the data.
        size_t space_available = data->size() - space_used;
        size_t samples_read;
        if (Read(&(*data)[space_used], space_available, &samples_read))
            space_used += samples_read;
    }
    data->resize(space_used);
    return data;
}
