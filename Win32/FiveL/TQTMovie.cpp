// -*- Mode: C++; tab-width: 4; -*-

#include <QTML.h>
#include "TQTMovie.h"

// Make sure some kind of assertions are available.
#include <crtdbg.h>
#define ASSERT(x) _ASSERTE(x)

//USING_NAMESPACE_FIVEL

bool TQTMovie::sIsQuickTimeInitialized = false;
CGrafPtr TQTMovie::sDummyGWorld = NULL;


//=========================================================================
// TMacError Methods
//=========================================================================

TMacError::TMacError(const char *inFile, int inLine, OSErr inErrorCode)
    : std::runtime_error("MacOS error"),
      mFile(inFile), mLine(inLine), mErrorCode(inErrorCode)
{
}

void TMacError::Check(const char *inFile, int inLine, OSErr inErrorCode)
{
    if (inErrorCode != noErr)
		throw TMacError(inFile, inLine, inErrorCode);
}


//=========================================================================
// TQTMovie Static Methods
//=========================================================================

void TQTMovie::InitializeMovies()
{
	ASSERT(sIsQuickTimeInitialized == false);

	// Start up the portions of the Macintosh Toolbox included with
	// QuickTime for Windows.
	::InitializeQTML(0);

	// As Chuck discovered, QuickTime likes to draw movies to 
	// inappropriate places on the screen, and to engage in other bits
	// of graphic weirdness during the startup process.
	Rect rect = {0, 0, 1, 1};
	OSErr err = ::NewGWorld(&sDummyGWorld, 8, &rect, NULL, NULL, 0);
	
	// Initialize QuickTime.
	::EnterMovies();
	sIsQuickTimeInitialized = true;
}

void TQTMovie::ShutDownMovies()
{
	ASSERT(sIsQuickTimeInitialized == true);
	if (sIsQuickTimeInitialized)
	{
		::ExitMovies();
		::DisposeGWorld(sDummyGWorld);
		::TerminateQTML();
		sIsQuickTimeInitialized = false;
	}
}

void TQTMovie::PrepareWindowForMovies(HWND inWindow)
{
	ASSERT(sIsQuickTimeInitialized);

	// Tell QuickTime to bind a CGrafPort to the window.
	// This also configures the window to receive idle events,
	// so we'll be able to do our movie processing.
	// TODO - Can an error happen here?
	::CreatePortAssociation(inWindow, NULL, 0);
}

CGrafPtr TQTMovie::GetPortFromHWND(HWND inWindow)
{
	return reinterpret_cast<CGrafPtr>(::GetNativeWindowPort(inWindow));
}


//=========================================================================
// TQTMovie Asynchronous Startup Methods
//=========================================================================
// These methods, together, form the asyncronous startup path.  I've
// grouped these together so you have some chance of seeing what's up.

TQTMovie::TQTMovie(CGrafPtr inPort, const std::string &inMoviePath)
    : mPort(inPort), mState(MOVIE_UNINITIALIZED),
	  mMovie(NULL), mMovieController(NULL), mShouldStartWhenReady(false)
{
	ASSERT(sIsQuickTimeInitialized);

	// If we allocate these resources, we must free them before exiting.
    Handle url_handle = NULL;
	bool have_refnum = false;
	short refnum;

	// We pass these flags to all the various NewMovieFrom... functions.
	// newMovieAsyncOK tells QuickTime to immediately return an
	// empty movie, and to load our data in the background.
	short load_flags = newMovieActive | newMovieAsyncOK;
	
    try
    {
		// Determine whether we're processing a URL or a local file
		// name, and load the movie accordingly.
		if (inMoviePath.find("http:") == 0 ||
			inMoviePath.find("rtsp:") == 0 ||
			inMoviePath.find("ftp:") == 0)
		{
			// Copy inMoviePath into a handle.
			url_handle = ::NewHandle(inMoviePath.length() + 1);
			CHECK_MAC_ERROR(::MemError());
			strcpy(*url_handle, inMoviePath.c_str());
			
			// Create our movie from the URL.
			CHECK_MAC_ERROR(::NewMovieFromDataRef(&mMovie, load_flags,
												  NULL, url_handle,
												  URLDataHandlerSubType));
		}
		else
		{
			// Convert our path to an FSSpec.  We need to pass a non-const
			// string because the underlying C API isn't const-correct.
			FSSpec spec;
			char *path = const_cast<char*>(inMoviePath.c_str());
			CHECK_MAC_ERROR(::NativePathNameToFSSpec(path, &spec, 0));
			
			// Open the file.
			CHECK_MAC_ERROR(::OpenMovieFile(&spec, &refnum, fsRdPerm));
			have_refnum = true;
			
			// Create a movie from the file.
			short res_id = 0;
			CHECK_MAC_ERROR(::NewMovieFromFile(&mMovie, refnum, &res_id, NULL,
											   load_flags, NULL));
		}

		// Hide our movie safely out of the way until we preroll it. 
		// This prevents certain codecs from drawing random cruft on
		// screen.  I learned this trick from Chuck's code.
		CGrafPtr safe_port = reinterpret_cast<CGrafPtr>(sDummyGWorld);
		::SetMovieGWorld(mMovie, safe_port, NULL);
		CHECK_MAC_ERROR(::GetMoviesError());

		// Wait for our "newMovieAsync" open to complete.  (Actually, it
		// may have *already* completed, but we want to have a single,
		// ultra-simple code path for all of our asynchronous work, so
		// we'll assume the worst-case scenario.)
		UpdateMovieState(MOVIE_INCOMPLETE);

		// NEXT: Our Idle method will call ProcessAsyncLoad repeatedly.
	}
    catch (...)
    {
		// We failed, so clean up everything.
		UpdateMovieState(MOVIE_BROKEN);
		ReleaseResources();
		if (url_handle)
			::DisposeHandle(url_handle);
		if (have_refnum)
			::CloseMovieFile(refnum);
		throw;
    }

    // We succeeded, so clean up our temporary data only.
    if (url_handle)
		::DisposeHandle(url_handle);
	if (have_refnum)
		::CloseMovieFile(refnum);
}

void TQTMovie::Idle() throw ()
{
	// Keep event-loop processing simple for our callers.
	if (IsBroken())
		return;

	try
	{
		// Give QuickTime some time to run.
		if (mMovieController)
			CHECK_MAC_ERROR(::MCIdle(mMovieController));
		else
			::MoviesTask(mMovie, 0);

		// See if we need to finish an asynchronous load.
		if (mState == MOVIE_INCOMPLETE)
			ProcessAsyncLoad();
	}
	catch (...)
	{
		UpdateMovieState(MOVIE_BROKEN);
	}
}

void TQTMovie::ProcessAsyncLoad()
{
	ASSERT(mState == MOVIE_INCOMPLETE);

	// Inside Macintosh says this function is expensive, and that
	// we shouldn't call it more than every 1/4 second or so.  For
	// now, we'll call it constantly, and see if that causes problems.
	long load_state = ::GetMovieLoadState(mMovie);

	// Check for loading errors.  We don't really know if GetMoviesError
	// returns the correct error value, so we'll just take a guess.
	if (load_state == kMovieLoadStateError)
		throw TMacError(__FILE__, __LINE__, ::GetMoviesError());
	
	// We don't advance to the next stage until the movie is playable.
	// Note there are other states beyond kMovieLoadStatePlayable
	// (kMovieLoadStatePlaythroughOK, kMovieLoadStateComplete), and those
	// are also good for our purposes.
	if (load_state >= kMovieLoadStatePlaythroughOK)
		AsyncLoadComplete();
}

void TQTMovie::AsyncLoadComplete()
{
	ASSERT(mState == MOVIE_INCOMPLETE);

	// "hintsScrubMode" tells QuickTime to jump to the nearest keyframe
	// when moving the controller bar.  It's a nice feature, but not
	// necessary, and it may cause some crashes with the QT6 preview
	// and/or VP3 network streaming.  I'm too lazy to test it for now.
	//::SetMoviePlayHints(mMovie, hintsScrubMode, hintsScrubMode);
	
	// Mark ourselves as ready to go.
	UpdateMovieState(MOVIE_READY);
	if (mShouldStartWhenReady == true)
		Start(mOptions, mPosition);
}

void TQTMovie::StartWhenReady(PlaybackOptions inOptions, Point inPosition)
{
	if (mState == MOVIE_READY)
		Start(inOptions, inPosition);
	else
	{
		// We can't just busy-loop on Idle until mState == MOVIE_READY,
		// because it deadlocks the application.  I'm guessing that
		// not all of our callbacks get run unless we go back to the
		// Windows event loop.
		mShouldStartWhenReady = true;
		mOptions = inOptions;
		mPosition = inPosition;
	}
}

void TQTMovie::Start(PlaybackOptions inOptions, Point inPosition)
{
	ASSERT(mState == MOVIE_READY);

	// Store our options so other routines can find them.
	mOptions = inOptions;
	mPosition = inPosition;

	// The movie should be done flashing (and drawing in awkward places),
	// so we should attach it to the correct port (if it has any graphics
	// we care about).
	if (!(mOptions & kAudioOnly))
	{
		::SetMovieGWorld(mMovie, mPort, NULL);
		CHECK_MAC_ERROR(::GetMoviesError());
	}
		
	// Figure out where to put our movie on the screen.  (We hope enough
	// of our movie is loaded by now to calculate the box accurately!)
	// TODO - Should this take the controller itself into account?  And if
	// so, how?
	Rect bounds;
	::GetMovieBox(mMovie, &bounds);
	ASSERT(bounds.left == 0 && bounds.top == 0);
	if (mOptions & kCenterMovie)
		::MacOffsetRect(&bounds,
						mPosition.h - (bounds.right / 2),
						mPosition.v - (bounds.bottom / 2));
	else
		::MacOffsetRect(&bounds, mPosition.h, mPosition.v);

	// Pick an appropriate set of controller options.
	long controller_flags = mcTopLeftMovie;
	if (!(mOptions & kEnableMovieController))
		controller_flags |= mcNotVisible;

	// Attach a movie controller to our movie.
	mMovieController = ::NewMovieController(mMovie, &bounds, controller_flags);
	CHECK_MAC_ERROR(::GetMoviesError());
	
	// Install our action filter.
	long refcon = reinterpret_cast<long>(this);
	CHECK_MAC_ERROR(::MCSetActionFilterWithRefCon(mMovieController,
												  &ActionFilterCallback,
												  refcon));

	// If interaction is enabled, pass keyboard events to the movie.
	if (mOptions & kEnableInteraction)
		CHECK_MAC_ERROR(::MCDoAction(mMovieController,
									 mcActionSetKeysEnabled,
									 reinterpret_cast<void*>(true)));

	// Handle our other controller options.
	if (mOptions & kLoopMovie)
		CHECK_MAC_ERROR(::MCDoAction(mMovieController,
									 mcActionSetLooping,
									 reinterpret_cast<void*>(true)));
	if (mOptions & kPlayEveryFrame)
		CHECK_MAC_ERROR(::MCDoAction(mMovieController,
									 mcActionSetPlayEveryFrame,
									 reinterpret_cast<void*>(true)));

	// Figure out our preferred playback rate.  We used to store this
	// in a member variable, but it's *much* safer to ask for it just
	// before you use it.
	Fixed rate = ::GetMoviePreferredRate(mMovie);
	
	// Start the movie.
	UpdateMovieState(MOVIE_STARTED);
	CHECK_MAC_ERROR(::MCDoAction(mMovieController,
								 mcActionPrerollAndPlay,
								 reinterpret_cast<void*>(rate)));
}


//=========================================================================
// Regular TQTMovie Methods
//=========================================================================

TQTMovie::~TQTMovie() throw ()
{
	ReleaseResources();
}

bool TQTMovie::IsDone() throw ()
{
	if (mState == MOVIE_BROKEN)
		return true;
	else if (mState < MOVIE_STARTED)
		return false;
	else if (mOptions & kLoopMovie)
		return false;
	else
	{
		// Get the current time in the movie.  As Yijin discovered, we
		// can't use ::IsMovieDone on streaming movies--it doesn't know how
		// to recognize the existance of anything beyond what's already
		// been loaded.
		TimeValue current_time = ::GetMovieTime(mMovie, NULL);
		// XXX - I don't seriously expect this error to occur, and I don't
		// want to have to catch it.
		//CHECK_MAC_ERROR(::GetMoviesError());

		// Get the movie's duration.
		// Unknown/indefinite duration is supposedly represented as
		// a duration of 0x7FFFFFF (yes, there really are *six* Fs in that
		// number).  This can occur for (1) partially loaded movies and
		// (2) live streaming broadcasts without a duration.
		TimeValue duration = ::GetMovieDuration(mMovie);
		// XXX - I don't seriously expect this error to occur, and I don't
		// want to have to catch it.
		//CHECK_MAC_ERROR(::GetMoviesError());

		return duration <= current_time;
	}
}

bool TQTMovie::IsPaused()
{
	ASSERT(mState == MOVIE_STARTED);
	long flags;
	CHECK_MAC_ERROR(::MCGetControllerInfo(mMovieController, &flags));
	return flags & mcInfoIsPlaying ? false : true;
}

void TQTMovie::Pause()
{
	ASSERT(mState == MOVIE_STARTED);
	if (!IsPaused())
	{
		::StopMovie(mMovie);
		CHECK_MAC_ERROR(::GetMoviesError());
		CHECK_MAC_ERROR(::MCMovieChanged(mMovieController, mMovie));
	}
}

void TQTMovie::Unpause()
{
	ASSERT(mState == MOVIE_STARTED);
	if (IsPaused())
	{
		::StartMovie(mMovie);
		CHECK_MAC_ERROR(::GetMoviesError());
		CHECK_MAC_ERROR(::MCMovieChanged(mMovieController, mMovie));
	}
}

bool TQTMovie::HandleMovieEvent(HWND hWnd, UINT message,
								WPARAM wParam, LPARAM lParam)
	throw ()
{
	if (IsBroken())
		return false;

	ASSERT(::GetNativeWindowPort(hWnd) != NULL);
	if (!::GetNativeWindowPort(hWnd))
		return false;
	
	// Build a Windows MSG object.  This code is from Apple's
	// sample SimplePlayerSDI application.
	MSG	msg;
	LONG thePoints = ::GetMessagePos();
	msg.hwnd = hWnd;
	msg.message = message;
	msg.wParam = wParam;
	msg.lParam = lParam;
	msg.time = ::GetMessageTime();
	msg.pt.x = LOWORD(thePoints);
	msg.pt.y = HIWORD(thePoints);
	
	// Convert the Windows event to a Mac event.
	EventRecord	mac_event;
	::WinEventToMacEvent(&msg, &mac_event);

	// Pass the event to our movie.
	bool res = false;
	if (mMovieController)
		res = ::MCIsPlayerEvent(mMovieController, &mac_event) ? true : false;
	else
		Idle();
	return res;
}

void TQTMovie::Redraw(HWND hWnd)
	throw ()
{
	if (IsBroken() || !mMovieController)
		return;

	WindowPtr mac_window =
		reinterpret_cast<WindowPtr>(TQTMovie::GetPortFromHWND(hWnd));
	::MCDraw(mMovieController, mac_window);
}

void TQTMovie::UpdateMovieState(MovieState inNewState)
{
	// Sanity-check our new state, and run any transition actions.
	switch (inNewState)
	{
		case MOVIE_INCOMPLETE:
			ASSERT(mState == MOVIE_UNINITIALIZED);
			break;

		case MOVIE_READY:
			ASSERT(mState == MOVIE_INCOMPLETE);
			break;

		case MOVIE_STARTED:
			ASSERT(mState == MOVIE_READY);
			break;

		case MOVIE_BROKEN:
			// We can enter the broken state from any state.
			// We shouldn't call ReleaseResources here,
			// because we might be nested deep inside some
			// QuickTime callback which would blow up if we
			// started deleting things.
			break;

		default:
			ASSERT(false);
	}

	// Actually update our state.
	mState = inNewState;
}

void TQTMovie::ReleaseResources() throw ()
{
	if (mMovieController)
		::DisposeMovieController(mMovieController);
	if (mMovie)
		::DisposeMovie(mMovie);
}

bool TQTMovie::ActionFilter(short inAction, void* inParams)
{
	switch (inAction)
	{
		case mcActionMovieEdited:
		case mcActionControllerSizeChanged:
			// We'll eventually want to do stuff here.
			break;

		case mcActionMovieClick:
			if (!(mOptions & kEnableInteraction))
			{
				// Devour mouse clicks using the trick recommended by
				// Inside Macintosh.
				EventRecord* evt = reinterpret_cast<EventRecord*>(inParams);
				evt->what = nullEvent;
			}
			break;
	}

	// This return value is officially undocumented, but 'false' is
	// used in all the sample code.
	return false;
}


//=========================================================================
//  TQTMovie Static Callbacks
//=========================================================================
//  All these routines are called by the Toolbox, so they're not allowed
//  to throw any exceptions.  In general, they catch the offending
//  exception, and mark the movie as broken.

Boolean TQTMovie::ActionFilterCallback(MovieController inController,
									   short inAction, void *inParams,
									   long inRefCon)
	throw ()
{
	TQTMovie *movie = reinterpret_cast<TQTMovie*>(inRefCon);
	bool result = false;
	if (!movie->IsBroken())
	{
		try
		{
			// Try to run our object's action filter.
			result = movie->ActionFilter(inAction, inParams);
		}
		catch (...)
		{
			// If something throws an exception, scrap our movie.
			movie->UpdateMovieState(MOVIE_BROKEN);
			result = false;
		}
	}
	return result;
}

