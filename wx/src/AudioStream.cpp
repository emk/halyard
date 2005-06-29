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

#include "TamaleHeaders.h"

#include <math.h>

#include "AudioStream.h"

USING_NAMESPACE_FIVEL


//=========================================================================
//  Utility Functions
//=========================================================================

#ifndef M_2_PI
#   define M_2_PI (3.14159265358979 * 2)
#endif

#define RADIANS_PER_CYCLE    (M_2_PI)
#define CYCLES_PER_SECOND_A  (440)

static inline float note_amplitude(PaTimestamp sample, int cycles_per_second)
{
	return (float) sin((1.0 * sample / AudioStream::SAMPLES_PER_SECOND) *
					   cycles_per_second * RADIANS_PER_CYCLE);
}


//=========================================================================
//  AudioStream Methods
//=========================================================================
//  AudioStream has tricky thread-safety issues.  In particular, there
//  are *three* threads of execution involved:
//
//    1) The main application thread.
//    2) The FillBuffer() interrupt task.
//    3) The background Idle() thread.
//
//  Most of our public interface takes place in thread (1).  Thread (2) is
//  managed internally by PortAudio, and calls FillBuffer between calls to
//  Start() and Stop().  Thread (3) is directly managed by AudioStream, and
//  calls Idle() from after the first call to InitializationDone() until
//  some undetermined time *after* Delete().
//
//  We have two critical sections: One for the mStreamState variable, and
//  one for the sStreamList variable. These variables may only be read or
//  written from inside the appropriate critical section.
//
//  We use critical sections to synchronize between (1) and (3).  We
//  haven't tried using any wxWidgets functions to synchronize with (2),
//  and we don't know if they'd work--instead, we rely on
//  carefully-designed circular buffers in our subclass VorbisAudioStream.

AudioStream::AudioStream(Format inFormat, float inVolume)
	: mIsRunning(false), mStreamState(INITIALIZING)
{
	for (int i = 0; i < MAX_CHANNELS; i++)
		mChannelVolumes[i] = inVolume;

	mFormat = (inFormat == INT16_PCM_STREAM) ? paInt16 : paFloat32;
	PaError err = Pa_OpenDefaultStream(&mStream,
									   0,         // no input
									   2,         // stereo output
									   mFormat,
									   SAMPLES_PER_SECOND,
									   256,       // frames/buffer
									   16,         // default number of buffers
									   &AudioCallback,
									   this);     // user data for callback
    if (err != paNoError)
		gLog.FatalError("Cannot open audio output stream.");
}

AudioStream::~AudioStream() {
    // We are called from the Idle() thread, not the main application thread,
    // so any destructor code needs to be thread-safe.
    //
    // For now, we just automatically destroy our member objects.
}

void AudioStream::InitializationDone() {
    ASSERT(GetStreamState() == INITIALIZING);
    SetStreamState(INITIALIZED);
	RegisterStream(this);
}

void AudioStream::Delete() {
    ASSERT(GetStreamState() == INITIALIZED);
	ASSERT(!mIsRunning);

    // Release our PortAudio resources. These are only required by
    // FillBuffer--which was shut down by the last call to Stop()--and
    // not by Idle(), which will continue to run for a while.
    //
    // We call this from here, not the destructor, because we're too lazy
    // to find out whether PortAudio calls are thread safe.
	Pa_CloseStream(mStream);

    // Allow our subclasses to log any final stream-related data.
    LogFinalStreamInfo();
    
    // Ask the background thread to stop sending us Idle() messages and to
    // unregister and delete us at the earliest possible opportunity.
    SetStreamState(DELETING);
}

void AudioStream::SetStreamState(State state) {
    wxCriticalSectionLocker lock(sCriticalSection);
    ASSERT((mStreamState == INITIALIZING && state == INITIALIZED) ||
           (mStreamState == INITIALIZED && state == DELETING));
    mStreamState = state;
}

AudioStream::State AudioStream::GetStreamState() const {
    wxCriticalSectionLocker lock(sCriticalSection);
    return mStreamState;
}

int AudioStream::AudioCallback(void *inInputBuffer,
							   void *outOutputBuffer,
							   unsigned long inFramesPerBuffer,
							   PaTimestamp inOutTime,
							   void *inUserData)
{
	// Recover our object, and delegate all the work to it.
	AudioStream *obj = (AudioStream *) inUserData;
	bool should_stop = obj->FillBuffer((float *) outOutputBuffer,
									   inFramesPerBuffer, inOutTime);
	obj->ApplyChannelVolumes(outOutputBuffer, inFramesPerBuffer);
	return should_stop ? 1 : 0;
}

void AudioStream::ApplyChannelVolumes(void *ioOutputBuffer,
									  unsigned long inFramesPerBuffer)
{
    ASSERT(GetStreamState() == INITIALIZED);
	ASSERT(GetChannelCount() == 2);
	ASSERT(mFormat == paInt16 || mFormat == paFloat32);

	// If the volume is 1.0, we don't need to do anything.
	if (mChannelVolumes[0] == 1.0 && mChannelVolumes[1] == 1.0)
		return;

	if (mFormat == paInt16)
	{
		int16 *buffer = (int16 *) ioOutputBuffer;
		for (unsigned long i = 0; i < inFramesPerBuffer; i++)
		{
			*buffer++ *= mChannelVolumes[(size_t) LEFT_CHANNEL];
			*buffer++ *= mChannelVolumes[(size_t) RIGHT_CHANNEL];
		}
	}
	else if (mFormat == paFloat32)
	{
		float *buffer = (float *) ioOutputBuffer;
		for (unsigned long i = 0; i < inFramesPerBuffer; i++)
		{
			*buffer++ *= mChannelVolumes[(size_t) LEFT_CHANNEL];
			*buffer++ *= mChannelVolumes[(size_t) RIGHT_CHANNEL];
		}		
	}
}

void AudioStream::SetChannelVolume(int inChannel, float inVolume)
{
    ASSERT(GetStreamState() != DELETING);
	ASSERT(0 <= inChannel && inChannel < GetChannelCount());
	mChannelVolumes[inChannel] = inVolume;
}

void AudioStream::SetChannelVolume(const std::string &inChannel,
                                   float inVolume)
{
    ASSERT(GetStreamState() != DELETING);
	int channels = GetChannelCount();
	if (inChannel == "left" && channels >= 2)
		SetChannelVolume(AudioStream::LEFT_CHANNEL, inVolume);
	else if (inChannel == "right" && channels >= 2)
		SetChannelVolume(AudioStream::RIGHT_CHANNEL, inVolume);
	else if (inChannel == "all")
		SetVolume(inVolume);
	else
		THROW("Tried to set volume on an unknown channel");
}

void AudioStream::SetVolume(float inVolume)
{
    ASSERT(GetStreamState() != DELETING);
	for (int i = 0; i < GetChannelCount(); i++)
		SetChannelVolume(i, inVolume);
}

bool AudioStream::IsDone() const {
    ASSERT(GetStreamState() != DELETING);
    return false;
}

double AudioStream::GetTime() const {
    ASSERT(GetStreamState() != DELETING);
    return GetSamplesPlayed() / SAMPLES_PER_SECOND;
}

double AudioStream::GetSamplesPlayed() const {
    ASSERT(GetStreamState() != DELETING);
    return Pa_StreamTime(mStream);
}

void AudioStream::Start()
{
    ASSERT(GetStreamState() == INITIALIZED);
	if (!mIsRunning)
	{
		PaError err = Pa_StartStream(mStream);
		if (err != paNoError)
			gLog.Error("Cannot start audio output stream.");
		else
			mIsRunning = true;
	}
}

void AudioStream::Stop()
{
    ASSERT(GetStreamState() == INITIALIZED);
	if (mIsRunning)
	{
		Pa_StopStream(mStream);
		mIsRunning = false;
	}
}


//=========================================================================
//  AudioStreamThread Methods
//=========================================================================

/// An internal timer called from the idle loop to trigger stream processing.
class AudioStreamThread : public wxThread {
public:
	enum {
		TIMER_INTERVAL = 500 // in milliseconds
	};

	AudioStreamThread();
	virtual ExitCode Entry();
};

AudioStreamThread::AudioStreamThread()
    // We want a detached thread so wxThead::Delete will automatically
    // delete this thread object when we're shutting down.
    : wxThread(wxTHREAD_DETACHED)
{
}

wxThread::ExitCode AudioStreamThread::Entry() {
    // TestDestroy() will return true if wxThread::Delete has been called
    // on us.  Our responsibility is to exit quickly at that point.  We can't
    // exit, though, until IdleAllStreams() has destroyed all streams.
	while (!TestDestroy() || AudioStream::StreamsAreRunning()) {
        AudioStream::IdleAllStreams();
		if (!TestDestroy())
			Sleep(TIMER_INTERVAL);
    }
	return 0;
}


//=========================================================================
//  AudioStream Static Methods
//=========================================================================

AudioStreamThread *AudioStream::sThread = NULL;
wxCriticalSection AudioStream::sCriticalSection;
AudioStream::AudioStreamList AudioStream::sStreams;

void AudioStream::InitializeStreams() {
    PaError err = Pa_Initialize();
    if( err != paNoError )
		gLog.FatalError("Cannot initialize audio library.");

	sThread = new AudioStreamThread();
	sThread->Create();
    sThread->Run();
    // TODO - Should we suspend the thread when there are no streams?
}

void AudioStream::ShutDownStreams() {
	sThread->Delete();
    Pa_Terminate();
}

void AudioStream::RegisterStream(AudioStream *inStream) {
    wxCriticalSectionLocker lock(sCriticalSection);
	sStreams.push_front(inStream);
}

void AudioStream::UnregisterStream(AudioStream *inStream) {
    wxCriticalSectionLocker lock(sCriticalSection);
	sStreams.remove(inStream);
}

bool AudioStream::StreamsAreRunning() {
    wxCriticalSectionLocker lock(sCriticalSection);
    return sStreams.empty();
}

// Called from background thread.
void AudioStream::IdleAllStreams() {
    // Make a copy of sStreams, in case some streams get unregistered while
    // we're iterating.
    //
    // PORTABILITY - This will fail if std::vector uses copy-on-write
    // semantics.
    AudioStreamList copy;
    {
        wxCriticalSectionLocker lock(sCriticalSection);
        copy = sStreams;
    }

	AudioStreamList::iterator i = copy.begin();
	for (; i != copy.end(); ++i) {
        // Get the stream state, and process the stream appropriately.
        // It's possible for the stream state to advance one or more
        // values between the call to GetStreamState() and the time
        // we reach a 'case' label, so it must always be safe to apply
        // an earlier 'case' label's code to any later state.
        //
        // We call 'delete' from here, because that's the easiest way
        // to make sure nobody else deletes streams at some
        // random point in this function.  (We can't use critical sections
        // to keep someone else from deleting streams while we're running,
        // because the call to Idle() below is quite expensive.)
        switch ((*i)->GetStreamState()) {
            case INITIALIZING:
                // We should never get here--we currently aren't registered
                // until we leave this state.
                ASSERT(false);
                break;

            case INITIALIZED:
                (*i)->Idle();
                break;
                
            case DELETING:
                UnregisterStream(*i);
                delete *i;
                break;
                
            default:
                ASSERT(false);
        }
    }
}


//=========================================================================
//  SineAudioStream
//=========================================================================

SineAudioStream::SineAudioStream(int inFrequency, float inVolume)
    : AudioStream(FLOAT32_STREAM, inVolume), mFrequency(inFrequency)
{
    InitializationDone();
}

bool SineAudioStream::FillBuffer(void *outBuffer,
								 unsigned long inFrames,
								 PaTimestamp inTime)
{
	float *buffer = (float *) outBuffer;
	PaTimestamp time = inTime;
	for (unsigned long i = 0; i < inFrames; i++)
	{
		float sample = note_amplitude(time++, mFrequency) * 0.5;
		*buffer++ = sample;
		*buffer++ = sample;
	}
	return false;	
}
