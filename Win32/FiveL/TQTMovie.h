// -*- Mode: C++; tab-width: 4; -*-

#ifndef TQTMovie_H
#define TQTMovie_H

#include <windows.h>

#include <string>
#include <stdexcept>
#include <time.h>

#include <Movies.h>

// TODO - 
//   Preload support
//   Don't show movie until ready to play
//   Preloading
//   Auto-centering
// / Center movie in screen
// / Hide controller bar
// / Disable all interaction
//   Audio-only movies
//   Port to 3.2.0.x
//   Port to 3.3.x


//BEGIN_NAMESPACE_FIVEL

//////////
// This exception class represents a MacOS error.  Note that QuickTime
// for Windows uses MacOS-style errors.
//
// TODO - Port this code to be a subclass of TException.
//
class TMacError : public std::runtime_error
{
	std::string mFile;
	int mLine;
	OSErr mErrorCode;

public:
	//////////
	// Create a Macintosh exception from an OSErr.
	//
	TMacError(const char *inFile, int inLine, OSErr inErrorCode);
	
	//////////
	// If inErrCode does not equal noErr, raise an error.
	//
	static void Check(const char *inFile, int inLine, OSErr inErrorCode);
};

//////////
// Check the result of a MacOS function for an error.
//
// [in] expr - A value of type OSErr.
//
#define CHECK_MAC_ERROR(expr) \
	/*FIVEL_NS*/ TMacError::Check(__FILE__, __LINE__, (expr))

//////////
// A stack-based resource management class for the current Macintosh
// QuickDraw port.  Look up "resource aquisition is initialization"
// for a summary of why we do this.
//
class StMacDrawingContext
{
	GrafPtr mSavedPort;

public:
	StMacDrawingContext(CGrafPtr inPort)
	{
		GrafPtr port = reinterpret_cast<GrafPtr>(inPort);
		::GetPort(&mSavedPort);
		::MacSetPort(port);
	}

	~StMacDrawingContext()
	{
		::MacSetPort(mSavedPort);
	}
};

//////////
// This class is intended to be a portable QuickTime movie wrapper
// containing all our low-level interfaces to QuickTime.  We're
// implementing this because we're tired of non-portable,
// duplicate QuickTime code that breaks every time sombody modifies it,
// and every time that Apple updates QuickTime.  Cross your fingers.
//
// Please design this class for maximum paranoia.  Use *lots* of
// assertions, and code defensively.
//
class TQTMovie
{
private:
	static bool sIsQuickTimeInitialized;
	static GWorldPtr sDummyGWorld;

public:
	static void InitializeMovies();
	static void ShutDownMovies();
	static void RegisterWindowForMovies(HWND inWindow);
	static void UnregisterWindowForMovies(HWND inWindow);
	static CGrafPtr GetPortFromHWND(HWND inWindow);

	typedef unsigned long PlaybackOptions;
	enum /* PlaybackOptions */ {
		kDefaultOptions = 0,
		kCenterMovie = 1,
		kAudioOnly = 2,
		kEnableMovieController = 4,
		kEnableInteraction = 8,
		kPlayEveryFrame = 16,
		kLoopMovie = 32
	};

private:
	//////////
	// We use non-blocking versions of all our QuickTime calls in an
	// effort to allow as much movie setup to run in the background
	// as possible.
	//
	// This means our object is basically a state machine--it
	// advances from one state to another as the movie opens.
	//
	enum MovieState {
		MOVIE_BROKEN,         // Something went wrong; this object is dead

		// This sequence is guaranteed to be in this order.
		MOVIE_UNINITIALIZED,  // We're in the constructor
		MOVIE_INCOMPLETE,     // We're waiting for newMovieAsyncOK
		MOVIE_READY,          // Our movie is fully loaded, with a controller
		MOVIE_STARTED         // Our movie has been started
	};

	//////////
	// The state of our object (and the attached Movie).  Set this using
	// UpdateMovieState, which may take various actions for certain state
	// transitions.
	//
	MovieState mState;

	//////////
	// The port into which we're supposed to draw our movie.
	//
	CGrafPtr mPort;

    //////////
    // The movie we're playing.
    //
    Movie mMovie;

    //////////
    // We always wrap our movie in a MovieController, even if
    // we chose not to show the controller bar.  The
    // MovieController does a ton of work for us.  Furthermore,
    // it *appears* that MovieController-based applications are
    // more reliable (especially in the face of QuickTime updates)
    // than applications which roll all their own QuickTime code.
    // This is, however, merely a hypothesis.
    //
    MovieController mMovieController;

	//////////
	// Should we start this movie immediately once it's ready to
	// play?
	//
	bool mShouldStartWhenReady;

	//////////
	// Our playback options.
	//
	PlaybackOptions mOptions;

	//////////
	// Where we should draw our movie.
	//
	Point mPosition;

    //////////
    // Should our timeout be active right now?
    //
    bool mTimeoutStarted;

    //////////
    // The last time something interesting happened, for various values
    // of "interesting".  This is used as the base number for calculating
    // timeouts.
    //
    time_t mTimeoutBase;

    //////////
    // The TimeValue the movie was at when we lasted updated TimeoutBase.
    //
    TimeValue mLastSeenTimeValue;

public:
	//////////
	// Create a new movie object, and begin the preloading process.
	//
	// [in] inPort -      The Macintosh GrafPort which will eventually
	//                    be used to display the movie.  Windows systems
	//                    can use PrepareWindowForMovies and
	//                    GetPortFromHWND to get a value for this parameter.
	// [in] inMoviePath - The URL or local filename of the movie.
	//
	TQTMovie(CGrafPtr inPort, const std::string &inMoviePath);

	//////////
	// Stop movie playback, and release all resources.
	//
	virtual ~TQTMovie() throw ();

	//////////
	// Allow QuickTime some time to process this movie.  QuickTime
	// need these periodic idle calls to get work done (but feel free
	// to call HandleMovieEvent below, instead).
	// 
	void Idle() throw ();

	//////////
	// Tell QuickTime to start the movie as soon as it's playable.  This
	// could be immediately, or in several seconds.
	//
	// [in] inOptions -   PlaybackOptions value to control playback.
	// [in] inPosition -  The location at which we should draw the movie.
	//                    This is either the upper-left corner, or the
	//                    center (if kCenterMovie is specified).
	//
	void StartWhenReady(PlaybackOptions inOptions, Point inPosition);

	//////////
	// Tell QuickTime to start the movie immediately.  Don't call this
	// unless IsReady returns true!  (Arguments are the same as the
	// arguments to StartWhenReady.)
	//
	void Start(PlaybackOptions inOptions, Point inPosition);

	//////////
	// Did a problem occur either loading or playing this movie?  If
	// this function returns true, the object is essentially scrap.
	//
	bool IsBroken() throw () { return mState == MOVIE_BROKEN; }

	//////////
	// Is the movie ready to play?
	// 
	bool IsReady() throw () { return mState == MOVIE_READY; }

	//////////
	// Has the movie been started?
	//
	bool IsStarted() throw () { return mState == MOVIE_STARTED; }

	//////////
	// Is the movie done playing?
	//
	bool IsDone() throw ();

	//////////
	// Has the movie been paused?  Don't call this function
	// unless IsStated returns true.
	//
	bool IsPaused();

	//////////
	// Pause the movie, if it isn't paused.  Don't call this function
	// unless IsStated returns true.  It's safe to call this on a
	// paused movie.
	//
	void Pause();

	//////////
	// Unpause the movie, if it's paused.  Don't call this function
	// unless IsStated returns true.  It's safe to call this on an
	// unpaused movie.
	//
	void Unpause();

	TimeValue GetMovieTime();
	void SetMovieVolume(short inVolume);
	TimeScale GetTimeScale();
	TimeValue GetDuration();
	void ThrowIfBroken();

    //////////
    // How much time has ellapsed towards a timeout?  (We don't actually
    // worry about how long the timeout is; that's our caller's job.)
    //
    int GetTimeoutEllapsed();

	//////////
	// Fill out a Win32 MSG object based on the parameters to this
	// function and the per-thread message state.
	//
	void FillOutMSG(HWND inHWND, UINT inMessage, WPARAM inWParam,
					LPARAM inLParam, MSG *outMessage);

	//////////
	// Fill out a QuickTime event object based on the parameters to this
	// function and the per-thread message state.
	//
	void FillOutEvent(HWND inHWND, UINT inMessage, WPARAM inWParam,
					  LPARAM inLParam, EventRecord *outEvent);

	//////////
	// Allow the TQTMovie object first crack at processing window
	// events.  If Windows is generating idle messages for your
	// window, you can call this instead of 
	//
	// [out] return - Theoretically, if this value is true,
	//                your application should assume that QuickTime
	//                took care of this event.  At least for update
	//                events, though, you'll need to process them
	//                anyway, and it doesn't hurt to process other
	//                kinds of events either.  Experimentation needed!
	//
	bool HandleMovieEvent(HWND hWnd, UINT message, WPARAM wParam,
						  LPARAM lParam) throw ();

	//////////
	// Notify the movie that the window has been redrawn.  If you
	// don't call this, the Sorenson 2 codec will often fail to
	// display video.  It's safe to call this function on broken
	// or unready movies (so you don't need to overcomplicate your
	// event loop).
	//
	void Redraw(HWND hWnd) throw ();

	//////////
	// Notify the movie of window activation and deactivation.
	//
	void Activate(HWND hWnd, bool inIsActivating) throw ();

	//////////
	// Notify the movie of a mouse click.
	//
	void Click(HWND hWnd, Point inWhere, long inWhen, long inModifiers)
		throw ();

	//////////
	// Notify the movie of a key press.
	//
	void Key(HWND hWnd, SInt8 inKey, long inModifiers)
		throw ();

protected:
	virtual bool ActionFilter(short inAction, void* inParams);

private:
	void ProcessAsyncLoad();
	void AsyncLoadComplete();
	
	//////////
	// Call MCDoAction with the specified command and parameter.
	//
	// [in] inAction - The action to perform.  There's about a zillion
	//                 of these, and they're not all centrally
	//                 documented.
	// [in] inParam -  The parameter value to use.
	//
	void DoAction(mcAction inAction, void *inParam);

	//////////
	// Release all resources held by this object.  (This call may
	// safely be made on half-constructed objects.)
	//
	void ReleaseResources() throw ();

	//////////
	// Advance to the next state in our state machine.
	//
	void UpdateMovieState(MovieState inNewState);

    //////////
    // Update the timeout base with the current time.
    //
    void UpdateTimeout(bool inStart = false);

	//////////
	// We use this function to process movie controller events.  This allows
	// us to (1) receive events from the controller and (2) intercept
	// the controller's own events.
	//
	// [in] inController - The controller for this event.
	// [in] inAction - The event code.
	// [in] inParams - The event parameters.
	// [in] inRefCon - User data (a 'this' pointer, in our case).
	// [out] return - Undocumented (!!).  Return false for now.
	//
	static Boolean ActionFilterCallback(MovieController inController,
										short inAction, void *inParams,
										long inRefCon) throw ();
};

//END_NAMESPACE_FIVEL

#endif // TQTMovie_H
