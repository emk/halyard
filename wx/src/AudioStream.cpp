// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"

#include <math.h>

#include "AudioStream.h"


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

AudioStream::AudioStream(Format inFormat)
	: mIsRunning(false)
{
	for (int i = 0; i < MAX_CHANNELS; i++)
		mChannelVolumes[i] = 1.0f;

	mFormat = (inFormat == INT16_PCM_STREAM) ? paInt16 : paFloat32;
	PaError err = Pa_OpenDefaultStream(&mStream,
									   0,         // no input
									   2,         // stereo output
									   mFormat,
									   SAMPLES_PER_SECOND,
									   256,       // frames/buffer
									   0,         // default number of buffers
									   &AudioCallback,
									   this);     // user data for callback
    if (err != paNoError)
		gLog.FatalError("Cannot open audio output stream.");
	RegisterStream(this);
}

AudioStream::~AudioStream()
{
	ASSERT(!mIsRunning);
	UnregisterStream(this);
	Pa_CloseStream(mStream);	
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
	ASSERT(GetChannelCount() == 2);
	ASSERT(mFormat == paInt16 || mFormat == paFloat32);

	// If the volume is 1.0, we don't need to do anything.
	if (mChannelVolumes[0] == 1.0 && mChannelVolumes[1] == 1.0)
		return;

	if (mFormat == paInt16)
	{
		int16 *buffer = (int16 *) ioOutputBuffer;
		for (int i = 0; i < inFramesPerBuffer; i++)
		{
			*buffer++ *= mChannelVolumes[(size_t) LEFT_CHANNEL];
			*buffer++ *= mChannelVolumes[(size_t) RIGHT_CHANNEL];
		}
	}
	else if (mFormat == paFloat32)
	{
		float *buffer = (float *) ioOutputBuffer;
		for (int i = 0; i < inFramesPerBuffer; i++)
		{
			*buffer++ *= mChannelVolumes[(size_t) LEFT_CHANNEL];
			*buffer++ *= mChannelVolumes[(size_t) RIGHT_CHANNEL];
		}		
	}
}

void AudioStream::SetChannelVolume(int inChannel, float inVolume)
{
	ASSERT(0 <= inChannel && inChannel < GetChannelCount());
	mChannelVolumes[inChannel] = inVolume;
}

void AudioStream::SetVolume(float inVolume)
{
	for (int i = 0; i < GetChannelCount(); i++)
		SetChannelVolume(i, inVolume);
}

void AudioStream::Start()
{
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
	if (mIsRunning)
	{
		Pa_StopStream(mStream);
		mIsRunning = false;
	}
}


//=========================================================================
//  AudioStreamTimer Methods
//=========================================================================

class AudioStreamTimer : public wxTimer
{
public:
	enum {
		TIMER_INTERVAL = 500 // in milliseconds
	};

	AudioStreamTimer();
	virtual void Notify();
};

AudioStreamTimer::AudioStreamTimer()
{
}

void AudioStreamTimer::Notify()
{
	AudioStream::IdleAllStreams();
}


//=========================================================================
//  AudioStream Static Methods
//=========================================================================

AudioStreamTimer *AudioStream::sTimer = NULL;
AudioStream::AudioStreamList AudioStream::sStreams;

void AudioStream::InitializeStreams()
{
    PaError err = Pa_Initialize();
    if( err != paNoError )
		gLog.FatalError("Cannot initialize audio library.");

	sTimer = new AudioStreamTimer();
}

void AudioStream::ShutDownStreams()
{
	delete sTimer;
    Pa_Terminate();
}

void AudioStream::RegisterStream(AudioStream *inStream)
{
	sStreams.push_front(inStream);
	if (!sTimer->IsRunning())
		sTimer->Start(AudioStreamTimer::TIMER_INTERVAL);
}

void AudioStream::UnregisterStream(AudioStream *inStream)
{
	sStreams.remove(inStream);
	if (sStreams.empty())
		sTimer->Stop();
}

void AudioStream::IdleAllStreams()
{
	AudioStreamList::iterator i = sStreams.begin();
	for (; i != sStreams.end(); ++i)
		(*i)->Idle();
}


//=========================================================================
//  SineAudioStream
//=========================================================================

bool SineAudioStream::FillBuffer(void *outBuffer,
								 unsigned long inFrames,
								 PaTimestamp inTime)
{
	float *buffer = (float *) outBuffer;
	PaTimestamp time = inTime;
	for (int i = 0; i < inFrames; i++)
	{
		float sample = note_amplitude(time++, mFrequency) * 0.5;
		*buffer++ = sample;
		*buffer++ = sample;
	}
	return false;	
}
