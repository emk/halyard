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

#ifndef AudioStream_H
#define AudioStream_H

#include <portaudio.h>


//////////
// AudioStreams are used for continuous, real-time playback of custom
// audio.
//
class AudioStream
{
public:
	enum Format {
		INT16_PCM_STREAM,
		FLOAT32_STREAM
	};

	enum {
		SAMPLES_PER_SECOND = 44100,
		LEFT_CHANNEL = 0,
		RIGHT_CHANNEL = 1,
		MAX_CHANNELS = 2
	};

private:
	PortAudioStream *mStream;
	PaSampleFormat mFormat;
	bool mIsRunning;
	float mChannelVolumes[MAX_CHANNELS];

	static int AudioCallback(void *inInputBuffer,
							 void *outOutputBuffer,
							 unsigned long inFramesPerBuffer,
							 PaTimestamp inOutTime,
							 void *inUserData);

	void ApplyChannelVolumes(void *ioOutputBuffer,
							 unsigned long inFramesPerBuffer);

public:
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
	// How many audio channels does this stream have?
	//
	int GetChannelCount() { return MAX_CHANNELS; }

	//////////
	// Set the volume of all channels.
	//
	void SetVolume(float inVolume);

	//////////
	// Set the volume of the specified channel.
	//
	void SetChannelVolume(int inChannel, float inVolume);

	//////////
	// Set the volume of the specified channel.
	//
	void SetChannelVolume(const std::string &inChannel, float inVolume);

	//////////
	// Is the stream currently running?
	//
	bool IsRunning() { return mIsRunning; }

    //////////
    // Returns true if the audio stream is looping (or some other form
    // of infinitely long, repetitive stream).
    //
    virtual bool IsLooping() = 0;

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

    virtual bool IsLooping() { return true; }

protected:
	bool FillBuffer(void *outBuffer, unsigned long inFrames,
					PaTimestamp inTime);

};

//////////
// Convert a 16-signed integer PCM sample into float-point format.
//
inline float int16_pcm_to_float(int16 sample)
{
	return sample / 32768.0f;
}

#endif // AudioStream_H
