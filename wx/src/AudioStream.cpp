// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include <math.h>

#include "TLogger.h"
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

static inline float int16_to_float(int16 inSample)
{
	return (inSample / 32768.0f);
}


//=========================================================================
//  AudioStream Methods
//=========================================================================

AudioStream::AudioStream()
	: mIsRunning(false)
{
	PaError err = Pa_OpenDefaultStream(&mStream,
									   0,         // no input
									   2,         // stereo output
									   paFloat32, // floating-point samples
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
	return should_stop ? 1 : 0;
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
	enum {
		TIMER_INTERVAL = 500 // in milliseconds
	};

public:
	AudioStreamTimer();
	virtual void Notify();
};

AudioStreamTimer::AudioStreamTimer()
{
	Start(TIMER_INTERVAL);
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
}

void AudioStream::UnregisterStream(AudioStream *inStream)
{
	sStreams.remove(inStream);
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

bool SineAudioStream::FillBuffer(float *outBuffer,
								 unsigned long inFrames,
								 PaTimestamp inTime)
{
	float *left_buffer = (float *) outBuffer;
	float *right_buffer = left_buffer + 1;
	PaTimestamp time = inTime;
	for (int i = 0; i < inFrames; i++)
	{
		*left_buffer = note_amplitude(time, mFrequency) * 0.5;
		*right_buffer = 0; //note_amplitude(time, 900);

		left_buffer += 2;
		right_buffer += 2;
		time++;
	}
	return false;	
}
