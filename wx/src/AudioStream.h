// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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
/// AudioStreams are used for continuous, real-time playback of custom
/// audio.
///
/// AudioStream relies on two background threads--one to call FillBuffer(),
/// and one to call Idle().  See the description of each function for
/// more details.
///
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
    enum {
        // Set this variable to 1 to delay calls to Pa_StartStream until
        // after we've actually buffered some data.  Doing so is highly
        // desirable--it prevents underruns during stream startup--but it
        // requires us to make PortAudio calls from a background thread,
        // which causes weird crashes during application shutdown.
        //
        // XXX - Decide which way we're going to live with, and delete
        // the other code path.
        BACKGROUND_START_AFTER_PRELOAD = 0
    };

    /// Execution state for an AudioStream.
    enum State {
        /// This is the initial state for each thread.  An INITIALIZING
        /// stream does not receive automatic Idle() calls.
        INITIALIZING,
        /// A PRELOADING stream is buffering enough data to start playback
        /// for the first time.  It receives one call to Idle(), and then
        /// becomes an ACTIVE stream automatically. It's possible to
        /// Start() a preloading stream, but audio playback may not actually
        /// start until the stream is ACTIVE (depending on the value of
        /// BACKGROUND_START_AFTER_PRELOAD).
        PRELOADING,
        /// An ACTIVE stream will receive automatic Idle() calls.
        ACTIVE,
        /// A DELETING stream no longer receives Idle calls, and will
        /// be deleted automatically sometime soon.
        DELETING
    };  

    volatile State mStreamState;
	PortAudioStream *mStream;
	PaSampleFormat mFormat;
	float mChannelVolumes[MAX_CHANNELS];

    /// Is the stream currently running?  This is true if Start() has
    /// been called more recently than Stop().  If mIsRunning is true
    /// *and* mStreamState is PRELOADING, then the stream is "officially"
    /// running, though no actual audio will play until Preload() calls
    /// StartIfStartDelayedByPreload().
    ///
    /// This member variable is volatile so we don't need a critical
    /// section to read it.
	volatile bool mIsRunning;

	static int AudioCallback(void *inInputBuffer,
							 void *outOutputBuffer,
							 unsigned long inFramesPerBuffer,
							 PaTimestamp inOutTime,
							 void *inUserData);

    void SetStreamState(State state);
    State GetStreamState() const;
    bool IsStreamStatePreloadingOrDeleting() const;
    bool IsStreamStatePreloadingOrActive() const;
    /// Is our underlying PortAudio object currently playing audio?
    bool IsPortAudioStreamRunning() const;
	void ApplyChannelVolumes(void *ioOutputBuffer,
							 unsigned long inFramesPerBuffer);

    //////////
    /// An internal version of the Start() function, called by Start()
    /// and StartIfStartDelayedByPreload().
    ///
    /// Make sure you're holding sCriticalSection before you enter this
    /// function!
    ///
    void StartInternal(bool isInBackground);

    //////////
    /// If a previous call to Start() was supposed to start audio
    /// playing, but was unable to do so because the stream was
    /// PRELOADING, then perform the actual start.  Called at the
    /// end of Preload().
    ///
    /// Make sure you're holding sCriticalSection before you enter this
    /// function!
    ///
    void StartIfStartDelayedByPreload();

public:
	//////////
	/// Create a new AudioStream.  You must call Start() afterwards if
	/// you want to play any audio.
	///
	AudioStream(Format inFormat, float inVolume = 1.0f);

	//////////
	/// Destroy an AudioStream.  The stream must be stopped before
	/// destroying it.
	///
	void Delete();

	//////////
	/// How many audio channels does this stream have?
	///
	int GetChannelCount() { return MAX_CHANNELS; }

	//////////
	/// Set the volume of all channels.
	///
	void SetVolume(float inVolume);

	//////////
	/// Set the volume of the specified channel.
	///
	void SetChannelVolume(int inChannel, float inVolume);

	//////////
	/// Set the volume of the specified channel.
	///
	void SetChannelVolume(const std::string &inChannel, float inVolume);

    //////////
    /// Is this stream done playing?  Never returns true for a looping
    /// stream.
    ///
    virtual bool IsDone() const;

    //////////
    /// How many seconds of audio have been played?  For some more
    /// advanced stream types, this will generally try not to count
    /// buffer underrun time, although this may make the result
    /// occasionally jump backwards in time by a small fraction of a
    /// second.
    ///
    double GetTime() const;

	//////////
	/// Is the stream currently running?
	///
	bool IsRunning() { return mIsRunning; }

    //////////
    /// Returns true if the audio stream is looping (or some other form
    /// of infinitely long, repetitive stream).
    ///
    virtual bool IsLooping() = 0;

	//////////
	/// Start the stream running.  It's OK to call this if the stream
	/// is already running.
	///
	/// This must be separate from the constructor because it indirectly
	/// relies upon a virtual function.
	///
	void Start();

	//////////
	/// Stop the stream.  It's OK to call this if the stream is already
	/// stopped.
	///
	/// This must be separate from the destructor because it indirectly
	/// relies upon a virtual function.
	///
	void Stop();

protected:
    //////////
    /// This destructor SHOULD NOT BE CALLED DIRECTLY.  Use Delete()
    /// instead.
    ///
    /// Destructors are now called from the foreground thread.  We used
    /// to call them from the background thread, but it lead to too many
    /// thread-safety headaches.
    ///
	virtual ~AudioStream();

    //////////
    /// Change thread state from INITIALIZING to PRELOADING.  This must be
    /// called at the end of each subclass's constructor, and will cause
    /// the background thread to start sending Preload() and Idle()
    /// messages.
    /// 
    void InitializationDone();

    //////////
    /// Makes the first call to Idle(), and changes state from INITIALIZING
    /// to PRELOADING.  Called automatically from the background thread.
    ///
    void Preload();

    //////////
    /// Get the number of samples played.  The default implementation
    /// reports solely the number of raw samples actually played, but
    /// subclasses may attempt to fudge this number to account for
    /// underruns.
    ///
    virtual double GetSamplesPlayed() const;

	//////////
	/// Our callback function.  This code runs in a separate PortAudio
	/// thread (or perhaps at interrupt level), and isn't allowed to do
	/// much of anything besides filling the buffer.
    ///
    /// FillBuffer() is called periodically after you call Start() until
    /// the time you call Stop().
	///
	/// \param outBuffer  (out) 2*inFrames values in the specified
	///	                 format, representing left and right channels (I'm
	///                   not sure which is which, actually).
	/// \param inFrames  The number of left/right pairs of samples
	///                   to generate.
	/// \param inTime  The number of frames already played through
	///                   this channel.
	/// \return  Should we stop this stream now?
	///
	virtual bool FillBuffer(void *outBuffer, unsigned long inFrames,
							PaTimestamp inTime) = 0;

	//////////
	/// Give some non-interrupt processing time to the stream.  This
	/// gets called automatically.  Don't spend *too* long in this method;
	/// there may be other media streams playing, and they need time, too.
    ///
    /// Idle() is called from a background thread, starting any time after
    /// you call InitializationDone() and ending some time *after* you call
    /// Delete().  This means you can do disk I/O and other things not
    /// allowed in FillBuffer, but you still have to think about thread
    /// safety a bit.
	///
	virtual void Idle() {}

    //////////
    /// Log any useful information about this stream.  This is called once,
    /// after the last call to FillBuffer() but possibly before (or during)
    /// the last call to Idle().
    ///
    virtual void LogFinalStreamInfo() {}

private: // static stuff
	friend class AudioStreamThread;

	typedef std::list<AudioStream*> AudioStreamList;
	
	static AudioStreamThread *sThread;
    static wxCriticalSection sCriticalSection;
    static wxCriticalSection sPortAudioCriticalSection;
    static wxMutex sUnregisterMutex;
    static wxCondition sUnregisterCondition;
    /// XXX - I don't whether this needs to be volatile or not.  I'd
    /// make it volatile just to be safe, but the STL doesn't like
    /// volatile std::list objects.
	static AudioStreamList sStreams;

    static void IdleAllStreams();
    static bool StreamsAreRunning();
	static void RegisterStream(AudioStream *inStream);
	static void UnregisterStream(AudioStream *inStream);

    //////////
    /// Called from the foreground thread: Wait() for the background thread
    /// to call UnregistrationFinished().  The call to Wait() automically
    /// unlocks mUnregisterMutex *while* waiting, and locks it afterwards.
    /// You must lock sUnregisterMutex before calling this function!
    ///
    static void WaitUntilUnregistrationFinished();

    //////////
    /// Called from this thread: Wait until the foreground thread calls
    /// WaitUntilUnregistrationFinished(), if it hasn't already, and wake
    /// it up.
    ///
    static void UnregistrationFinished();

public: // static stuff
    //////////
    /// Initialize the audio synthesis subsystem.
    ///
    static void InitializeStreams();

    //////////
    /// Shut down the audio synthesis subsystem.
    ///
    static void ShutDownStreams();
};


//////////
/// Sine wave synthesizer.  This is a simple demo class.
/// 
class SineAudioStream : public AudioStream
{
	int mFrequency;

public:
	SineAudioStream(int inFrequency, float inVolume = 1.0f);

    virtual bool IsLooping() { return true; }

protected:
	bool FillBuffer(void *outBuffer, unsigned long inFrames,
					PaTimestamp inTime);

};

//////////
/// Convert a 16-signed integer PCM sample into float-point format.
///
inline float int16_pcm_to_float(int16 sample)
{
	return sample / 32768.0f;
}

#endif // AudioStream_H
