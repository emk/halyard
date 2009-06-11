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

#ifndef VorbisAudioStream_H
#define VorbisAudioStream_H

#include "AudioStream.h"

class VorbisFile;

/// Intuitive "volatile" variables which work correctly in a
/// multi-processor environment.  (Regular C++ volatiles only affect the
/// behavior of a single CPU, and they don't do anything useful on an SMP
/// system, surprisingly enough.)
///
/// The semantics of IntuitiveVolatile are based on Java 1.5 volatiles as
/// described in JSR-133.  I've adopted the terminology from "The JSR-133
/// Cookbook", which you can find here:
///
///   http://gee.cs.oswego.edu/dl/jmm/cookbook.html
///
/// This class implements a version of "volatile" that works the way you
/// might expect in the presence of multiple CPUs.
///
/// PORTABILITY - There's a pretty good chance this class works on current
/// and near-future CPUs.  But it's worth keeping an eye on this class, and
/// not doing anything tricky with it--such as using it to store values
/// bigger than the native word size.
template <typename T>
class IntuitiveVolatile {
    volatile T mValue;

    /// Make sure that any preceding stores are completed before any
    /// following stores, from the perspective of other CPUs.
    void StoreStoreBarrier() const { FullBarrier(); }

    /// Make sure that any preceding stores are completed before any
    /// following loads, from the perspective of other CPUs.
    void StoreLoadBarrier() const { FullBarrier(); }

    /// Make sure that any preceding loads are completed before any
    /// following loads or stores, from the perspective of other CPUs.
    void LoadLoadAndLoadStoreBarriers() const { FullBarrier(); }

    /// The most expensive (and most conservative) form of barrier.
    void FullBarrier() const;

public:
    /// Create and initialize a new IntuitiveVolatile object.
    IntuitiveVolatile(T inValue) : mValue(inValue) {}

    /// Read the value stored in this object.
    T read() const;

    /// Write a new value to this object.
    void write(T inValue);
};

/// An AudioStream which reads from a VorbisFile.
class VorbisAudioStream : public AudioStream
{
    std::string mFileName;
    bool mShouldLoop;
    shared_ptr<VorbisFile> mFile;
    shared_array<Halyard::int16> mBuffer;
    size_t mBufferSize;
    IntuitiveVolatile<size_t> mDataBegin;
    IntuitiveVolatile<size_t> mDataEnd;
    IntuitiveVolatile<bool> mDoneWithFile;
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

    size_t ReadIntoBlock(Halyard::int16 *inSpace, size_t inLength);
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
    void GetFreeBlocks(Halyard::int16 **outSpace1, size_t *outLength1,
                       Halyard::int16 **outSpace2, size_t *outLength2);

    //////////
    /// Mark the specified number of samples as written.
    ///
    void MarkAsWritten(size_t inSize);

public:
    VorbisAudioStream(const char *inFileName, size_t inBufferSize,
                      bool inShouldLoop, float inVolume = 1.0f);
    virtual void LogFinalStreamInfo();

    virtual bool IsDone() const;
    virtual bool IsLooping() { return mShouldLoop; }
    virtual void Idle();

protected:
    virtual double GetSamplesPlayed() const;
    virtual bool FillBuffer(void *outBuffer, unsigned long inFrames,
                            PaTimestamp inTime);    
};


#endif // VorbisAudioStream_H

