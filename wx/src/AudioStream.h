// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef AudioStream_H
#define AudioStream_H

#include <list>
#include <portaudio.h>


//////////
// AudioStreams are used for continuous, real-time playback of custom
// audio.
//
class AudioStream
{
	PortAudioStream *mStream;
	bool mIsRunning;

	static int AudioCallback(void *inInputBuffer,
							 void *outOutputBuffer,
							 unsigned long inFramesPerBuffer,
							 PaTimestamp inOutTime,
							 void *inUserData);

public:
	enum Format {
		INT16_PCM_STREAM,
		FLOAT32_STREAM
	};

	enum {
		SAMPLES_PER_SECOND = 44100
	};

	//////////
	// Create a new AudioStream.  You must call Start() afterwards if
	// you want to play any audio.
	//
	AudioStream(Format inFormat);

	//////////
	// Destroy an AudioStream.  The stream must be stopped before
	// destroying it.
	//
	virtual ~AudioStream();

	//////////
	// Is the stream currently running?
	//
	bool IsRunning() { return mIsRunning; }

	//////////
	// Start the stream running.  It's OK to call this if the stream
	// is already running.
	//
	// This must be separate from the constructor because it indirectly
	// relies upon a virtual function.
	//
	void Start();

	//////////
	// Stop the stream.  It's OK to call this if the stream is already
	// stopped.
	//
	// This must be separate from the destructor because it indirectly
	// relies upon a virtual function.
	//
	void Stop();

protected:
	//////////
	// Our callback function.  This code runs in a separate thread (or
	// perhaps at interrupt level), and isn't allowed to do much of
	// anything besides filling the buffer.
	//
	// [out] outBuffer - 2*inFrames values in the specified
	//	                 format, representing left and right channels (I'm
	//                   not sure which is which, actually).
	// [in] inFrames   - The number of left/right pairs of samples
	//                   to generate.
	// [in] inTime     - The number of frames already played through
	//                   this channel.
	// [out] return    - Should we stop this stream now?
	//
	virtual bool FillBuffer(void *outBuffer, unsigned long inFrames,
							PaTimestamp inTime) = 0;

	//////////
	// Give some non-interrupt processing time to the stream.  This
	// gets called automatically.  Don't spend *too* long in this method;
	// there may be other media streams playing, and they need time, too.
	//
	virtual void Idle() {}

private: // static stuff
	friend class AudioStreamTimer;

	typedef std::list<AudioStream*> AudioStreamList;
	
	static AudioStreamTimer *sTimer;
	static AudioStreamList sStreams;

    static void IdleAllStreams();
	static void RegisterStream(AudioStream *inStream);
	static void UnregisterStream(AudioStream *inStream);

public: // static stuff
    //////////
    // Initialize the audio synthesis subsystem.
    //
    static void InitializeStreams();

    //////////
    // Shut down the audio synthesis subsystem.
    //
    static void ShutDownStreams();
};


//////////
// Sine wave synthesizer.  This is a simple demo class.
// 
class SineAudioStream : public AudioStream
{
	int mFrequency;

public:
	SineAudioStream(int inFrequency)
		: AudioStream(FLOAT32_STREAM), mFrequency(inFrequency) {}

protected:
	bool FillBuffer(void *outBuffer, unsigned long inFrames,
					PaTimestamp inTime);

};

#endif // AudioStream_H
