// -*- Mode: C++; tab-width: 4; -*-

#ifndef TQTMovie_H
#define TQTMovie_H

#include <windows.h>

#include <string>
#include <stdexcept>

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
	static void PrepareWindowForMovies(HWND inWindow);
	static CGrafPtr GetPortFromHWND(HWND inWindow);

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
		MOVIE_PREPREROLLING,  // We're waiting for pre-prerolling to complete
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
	// The preferred playback rate of the movie.  We need to pass
	// this to a variety of QuickTime functions.
	// 
	Fixed mRate;

	//////////
	// Should we start this movie immediately once it's ready to
	// play?
	//
	bool mShouldStartWhenReady;

public:
	TQTMovie(CGrafPtr inPort, const std::string &inMoviePath);
	virtual ~TQTMovie() throw ();

	void Idle() throw ();
	bool HandleMovieEvent(HWND hWnd, UINT message, WPARAM wParam,
						  LPARAM lParam) throw ();
	void Redraw(HWND hWnd) throw ();

	bool IsBroken() throw () { return mState == MOVIE_BROKEN; }
	bool IsStarted() throw () { return mState == MOVIE_STARTED; }

	void StartWhenReady();
	void Start();
	bool IsPaused();
	void Pause();
	void Resume();

protected:
	virtual bool ActionFilter(short inAction, void* inParams);

private:
	void ProcessAsyncLoad();
	void AsyncLoadComplete();
	void PrePrerollComplete(OSErr inError);
	
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
	// We pass this function to QuickTime when we call PrePrerollMovie,
	// and QuickTime calls it when everything is ready.
	//
	// [in] inMovie - The movie being preprerolled.
	// [in] inError - noErr, or an error value if preprerolling failed.
	// [in] inRefCon - User data (a 'this' pointer, in our case).
	// 
	static void PrePrerollCompleteCallback(Movie inMovie,
										   OSErr inError,
										   void *inRefCon) throw ();

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
