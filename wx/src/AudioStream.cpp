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
//  AudioStreamThread Methods
//=========================================================================
//  See the AudioStream block comment for a discussion of how
//  synchronization works.

/// An internal timer called from the idle loop to trigger stream processing.
class AudioStreamThread : public wxThread {
    wxMutex mSleepMutex;
    wxCondition mSleepCondition;

    // These should arguably be static members of AudioStream, but
    // I'm not sure whether it's safe to make them global variables
    // initialized at startup time.  This is the next best place to
    // put them.
    //
    // TODO - Try to move these into AudioStream.
    wxMutex mUnregisterMutex;
    wxCondition mUnregisterCondition;

    void SleepUntilWakeupOrTimeout();

public:
	enum {
		TIMER_INTERVAL = 500 // in milliseconds
	};

	AudioStreamThread();
    void WakeUp();

    wxMutex &GetUnregisterMutex();
    void WaitUntilUnregistrationFinished();
    void UnregistrationFinished();

	virtual ExitCode Entry();
};

AudioStreamThread::AudioStreamThread()
    // We want a detached thread so wxThead::Delete will automatically
    // delete this thread object when we're shutting down.
    : wxThread(wxTHREAD_DETACHED),
      mSleepCondition(mSleepMutex), mUnregisterCondition(mUnregisterMutex)
{
}

void AudioStreamThread::WakeUp() {
    // If we're not sleeping, this will simply be ignored.  We don't need
    // to lock mSleepMutex here, because we're not trying to synchronize
    // with the other thread, just get its attention.
    mSleepCondition.Signal();
}

void AudioStreamThread::SleepUntilWakeupOrTimeout() {
    // We need to lock mSleepMutex before calling WaitTimeout(), which is
    // basically a formality in this case--see the wxCondition
    // documentation for details.  WaitTimeout() will wake us when
    // TIMER_INTERVAL expires or someone calls WakeUp().
    wxMutexLocker lock(mSleepMutex);
    mSleepCondition.WaitTimeout(TIMER_INTERVAL);
}

/// Called from the foreground thread: This mutex should be locked before
/// calling WaitUntilUnregistrationFinished(), and unlocked afterwards.
wxMutex &AudioStreamThread::GetUnregisterMutex() {
    return mUnregisterMutex;
}

/// Called from the foreground thread: Wait() for the background thread
/// to call UnregistrationFinished().  The call to Wait() automically unlocks
/// mUnregisterMutex *while* waiting, and locks it afterwards.
void AudioStreamThread::WaitUntilUnregistrationFinished() {
    mUnregisterCondition.Wait();
}

/// Called from this thread: Wait until the foreground thread calls
/// WaitUntilUnregistrationFinished(), if it hasn't already, and wake it up.
void AudioStreamThread::UnregistrationFinished() {
    wxMutexLocker lock(mUnregisterMutex);
    mUnregisterCondition.Signal();
}

wxThread::ExitCode AudioStreamThread::Entry() {
    // TestDestroy() will return true if wxThread::Delete has been called
    // on us.  Our responsibility is to exit reasonably quickly at that
    // point.  We can't exit, though, until IdleAllStreams() has destroyed
    // all streams.
	while (!TestDestroy()) {
        AudioStream::IdleAllStreams();
		if (!TestDestroy())
			SleepUntilWakeupOrTimeout();
    }
    ASSERT(!AudioStream::StreamsAreRunning());
	return 0;
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
//  some undetermined time during Delete().
//
//  We have two critical sections:
//    
//    - sCriticalSection: For the mStreamState, mIsRunning and sStreams
//      variables.  We never hold this critical section for more than an
//      instant; doing so can block the main application thread.
//    - sPortAudioCriticalSection: For all PortAudio API calls.  We're too
//      lazy to figure out whether PortAudio is thread safe.  If you need
//      to hold sPortAudioCriticalSection *and* sCriticalSection, then
//      acquire sPortAudioCriticalSection *second* to prevent deadlocks.
//      During stream deletion, it may be necessary to hold this lock for
//      a while, because some of the PortAudio API functions take a while
//      to compelete.
//
//  We also have two wxConditions (with associated wxMutexes).  These
//  live in AudioStreamThread:
//
//    - mSleepCondition: This is used to wake the background thread
//      up from its normal TIMER_INTERVAL naps, when we have something
//      we want it to handle immediately.  We don't maintain strict
//      synchronization on this condition; it's just an optimization.
//    - mUnregisterCondition: The background thread signals this condition
//      when it has finished unregistering a stream and the foreground
//      thread can wake up and delete the stream object.
//
//  We use sCriticalSection, mSleepCondition and mUnregisterCondition to
//  synchronize between (1) and (3).  We haven't tried using any wxWidgets
//  functions to synchronize with (2), and we don't know if they'd
//  work--instead, we rely on carefully-designed circular buffers in our
//  subclass VorbisAudioStream.
//
//  Now go have a a look at AudioStreamThread, InitializationDone, Preload,
//  Delete, RegisterStream, UnregisterStream and IdleAllStreams.

AudioStream::AudioStream(Format inFormat, float inVolume)
	: mIsRunning(false), mStreamState(INITIALIZING)
{
	for (int i = 0; i < MAX_CHANNELS; i++)
		mChannelVolumes[i] = inVolume;

	mFormat = (inFormat == INT16_PCM_STREAM) ? paInt16 : paFloat32;
    
    wxCriticalSectionLocker lock(sPortAudioCriticalSection);
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
    // Release our PortAudio resources.  We're called from the foreground
    // thread, so we don't need to worry *too* much during cleanup.
    wxCriticalSectionLocker lock(sPortAudioCriticalSection);
    Pa_CloseStream(mStream);
}

void AudioStream::InitializationDone() {
    ASSERT(GetStreamState() == INITIALIZING);
    SetStreamState(PRELOADING);
	RegisterStream(this);
}

void AudioStream::Preload() {
    // At any point during this function, the stream state may transition
    // from PRELOADING to DELETING because the application thread called
    // Delete().
    ASSERT(IsStreamStatePreloadingOrDeleting());
    
    // Allow the stream to run any idle tasks for the first time.  This
    // tends to be expensive, so we can't hold any critical sections.
    Idle();

    // Transition from PRELOADING to ACTIVE, unless the application
    // thread has already transitioned us from PRELOADING to DELETING.
    // We access mStreamState directly here so we don't need to hold
    // sCriticalSection recursively.
    wxCriticalSectionLocker lock(sCriticalSection);
    ASSERT(mStreamState == PRELOADING || mStreamState == DELETING);
    if (mStreamState == PRELOADING) {
        mStreamState = ACTIVE;
        StartIfStartDelayedByPreload();
    }
}

void AudioStream::Delete() {
    ASSERT(IsStreamStatePreloadingOrActive());
	ASSERT(!mIsRunning);

    {
        // TODO - Refactor this into a support function, and
        // the copy in Stop(), too.
        wxCriticalSectionLocker lock(sPortAudioCriticalSection);
        ASSERT(!Pa_StreamActive(mStream));
    }

    // Before we can actually delete this object, we need to make sure
    // the background thread is done with this object.
    {
        // Lock mUnregisterMutex *before* notifying the background thread,
        // just it case the background thread finishes the deletion before
        // we call WaitUntilUnregistrationFinished().
        wxMutexLocker lock(sThread->GetUnregisterMutex());

        // Ask the background thread to stop sending us Preload() and
        // Idle() messages and to call UnregisterStream().
        //
        // DO NOT release resources before this call, or you'll surprise
        // Preload() and Idle().
        SetStreamState(DELETING);

        // Wait until the background thread actually deletes us.
        sThread->WaitUntilUnregistrationFinished();
    }

    // Allow our subclasses to log any final stream-related data.
    LogFinalStreamInfo();

    // Destroy this object.
    delete this;
}

void AudioStream::SetStreamState(State state) {
    wxCriticalSectionLocker lock(sCriticalSection);
    // These are the only allowable state transitions.
    ASSERT((mStreamState == INITIALIZING && state == PRELOADING) ||
           // This transition is handled directly by Preload().
           //(mStreamState == PRELOADING && state == ACTIVE) ||
           (mStreamState == PRELOADING && state == DELETING) ||
           (mStreamState == ACTIVE && state == DELETING));
    mStreamState = state;

    // Wake up the background thread so it can service this stream soon
    // (unless we just transitioned to PRELOADING, in which case we'll let
    // RegisterStream() wake up the background thread as soon as
    // RegisterStream() adds it to sStreams--waking it up now would be
    // useless, and would only increase the amount of time before the first
    // call to Preload()).
    if (mStreamState != PRELOADING)
        sThread->WakeUp();
}

AudioStream::State AudioStream::GetStreamState() const {
    wxCriticalSectionLocker lock(sCriticalSection);
    return mStreamState;
}

bool AudioStream::IsStreamStatePreloadingOrDeleting() const {
    // This is one atomic function so that we don't need to worry about
    // state transitions while we're doing the check.
    wxCriticalSectionLocker lock(sCriticalSection);
    return (mStreamState == PRELOADING) || (mStreamState == DELETING);
}

bool AudioStream::IsStreamStatePreloadingOrActive() const {
    // This is one atomic function so that we don't need to worry about
    // state transitions while we're doing the check.
    wxCriticalSectionLocker lock(sCriticalSection);
    return (mStreamState == PRELOADING) || (mStreamState == ACTIVE);
}

int AudioStream::AudioCallback(void *inInputBuffer,
							   void *outOutputBuffer,
							   unsigned long inFramesPerBuffer,
							   PaTimestamp inOutTime,
							   void *inUserData)
{
	// Recover our object, and delegate all the work to it.
	AudioStream *obj = (AudioStream *) inUserData;
    // We have to access mStreamState directly, because the multimedia
    // timer thread cannot access sCriticalSection without causing a
    // deadlock.  Be warned.  As long as we check PRELOADING and ACTIVE
    // in the right order, there shouldn't be any race conditions with
    // other mutators of mStreamState.
    ASSERT(obj->mStreamState == PRELOADING || obj->mStreamState == ACTIVE);
	bool should_stop = obj->FillBuffer((float *) outOutputBuffer,
									   inFramesPerBuffer, inOutTime);
	obj->ApplyChannelVolumes(outOutputBuffer, inFramesPerBuffer);
    ASSERT(obj->mStreamState == PRELOADING || obj->mStreamState == ACTIVE);
	return should_stop ? 1 : 0;
}

void AudioStream::ApplyChannelVolumes(void *ioOutputBuffer,
									  unsigned long inFramesPerBuffer)
{
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
    wxCriticalSectionLocker lock(sPortAudioCriticalSection);
    return Pa_StreamTime(mStream);
}

void AudioStream::StartInternal(bool isInBackground) {
    // This assertion should be here, but there's no IsLocked() function.
    //ASSERT(sCriticalSection->IsLocked());
    ASSERT(mStreamState == PRELOADING || mStreamState == ACTIVE);
    wxCriticalSectionLocker lock(sPortAudioCriticalSection);
    PaError err = Pa_StartStream(mStream);
    if (err != paNoError) {
        mIsRunning = false;
        // We want to log an error here, but we may be in the background
        // thread, which isn't allowed to call the logger (yet).
        // TODO - Make logger thread safe.
        if (!isInBackground)
            gLog.Error("Cannot start audio output stream.");
    } else {
        mIsRunning = true;
    }
}

void AudioStream::StartIfStartDelayedByPreload() {
    // This assertion should be here, but there's no IsLocked() function.
    //ASSERT(sCriticalSection->IsLocked());
    ASSERT(mStreamState == ACTIVE);
    //if (mIsRunning && BACKGROUND_START_AFTER_PRELOAD)
    //    StartInternal(true);
}

void AudioStream::Start() {
    wxCriticalSectionLocker lock(sCriticalSection);
    ASSERT(mStreamState == PRELOADING || mStreamState == ACTIVE);
	if (!mIsRunning) {
        if (mStreamState == PRELOADING && BACKGROUND_START_AFTER_PRELOAD) {
            // We can't actually start this stream, because Idle() hasn't
            // been called yet and we don't want lots of buffer underruns.
            // But we can mark this stream as started and rely on Preload()
            // to do the right thing once the data is available.
            mIsRunning = true;
        } else {
            // We can actually start this stream for real.
            StartInternal(false);
        }
    }
}

void AudioStream::Stop() {
    wxCriticalSectionLocker lock(sCriticalSection);
    ASSERT(mStreamState == PRELOADING || mStreamState == ACTIVE);
	if (mIsRunning) {
        wxCriticalSectionLocker lock2(sPortAudioCriticalSection);
		Pa_StopStream(mStream);
		mIsRunning = false;
	}
 
    wxCriticalSectionLocker lock3(sPortAudioCriticalSection);
    ASSERT(!Pa_StreamActive(mStream));
}


//=========================================================================
//  AudioStream Static Methods
//=========================================================================

AudioStreamThread *AudioStream::sThread = NULL;
wxCriticalSection AudioStream::sCriticalSection;
wxCriticalSection AudioStream::sPortAudioCriticalSection;
AudioStream::AudioStreamList AudioStream::sStreams;

void AudioStream::InitializeStreams() {
    {
        wxCriticalSectionLocker lock(sPortAudioCriticalSection);
        PaError err = Pa_Initialize();
        if( err != paNoError )
            gLog.FatalError("Cannot initialize audio library.");
    }

	sThread = new AudioStreamThread();
	sThread->Create();
    sThread->Run();
    // TODO - Should we suspend the thread when there are no streams?
}

void AudioStream::ShutDownStreams() {
	sThread->Delete();
    // Since our background thread is guaranteed to be dead, we don't
    // actually need this lock. (Actually, Pa_Terminate() takes a while
    // to run--longer than we'd normally hold a critical section for,
    // but nobody else is waiting on it.)
    wxCriticalSectionLocker lock(sPortAudioCriticalSection);
    Pa_Terminate();
}

void AudioStream::RegisterStream(AudioStream *inStream) {
    wxCriticalSectionLocker lock(sCriticalSection);
	sStreams.push_front(inStream);
    // Get this new stream services as soon as possible.
    sThread->WakeUp();
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

    // Walk the stream list back-to-front, so service newly-created
    // streams before "topping off" existing streams.
	AudioStreamList::reverse_iterator i = copy.rbegin();
	for (; i != copy.rend(); ++i) {
        // Get the stream state, and process the stream appropriately.
        // It's possible for the stream state to advance from PRELOADING or
        // ACTIVE to DELETING between the call to GetStreamState() and the
        // time we reach a 'case' label, so it must always be safe to apply
        // the PRELOADING or ACTIVE code to a stream in the DELETING state.
        switch ((*i)->GetStreamState()) {
            case INITIALIZING:
                // We should never get here--we aren't registered until we
                // leave this state.
                ASSERT(false);
                break;

            case PRELOADING:
                // Preload() will call Idle() and then transition from
                // PRELOADING to ACTIVE.
                (*i)->Preload();
                break;

            case ACTIVE:
                (*i)->Idle();
                break;
                
            case DELETING:
                // Unregister the stream from our list, and hand it back to
                // the foreground thread (see Delete() for details).  We
                // can't call delete from here, because the destructors for
                // AudioStream may not be thread safe.
                //
                // We handle unregistration here, so nobody else can can
                // unregister a stream we're working on.
                UnregisterStream(*i);
                sThread->UnregistrationFinished();
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
