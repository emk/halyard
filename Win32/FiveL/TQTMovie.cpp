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
	  mMovie(NULL), mMovieController(NULL)
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

		// Hide our movie safely out of the way until we attach our
		// controller to it.  I learned this trick from Chuck's code.
		::SetMovieGWorld(mMovie,
						 reinterpret_cast<CGrafPtr>(sDummyGWorld),
						 NULL);
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
	// (kMovieLoadStatePlaythoughOK, kMovieLoadStateComplete), and those
	// are also good for our purposes.
	// TODO - WHYMANUAL
	if (load_state >= kMovieLoadStatePlayable)
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
	
	// Get some useful information from our movie.
	Fixed mRate = ::GetMoviePreferredRate(mMovie);
	
	// Begin the pre-prerolling process.  For network-streamed movies,
	// this opens up server connections and fun stuff like that.
	// TODO - WHYMANUAL
	// NEXT: When done, QuickTime will call the PrePrerollCompleteCallback,
	// which will (in turn) call our PrePrerollComplete method.  This
	// may happen immediately (or so I think), or later, when we're
	// in our Idle method.
	UpdateMovieState(MOVIE_PREPREROLLING);
	CHECK_MAC_ERROR(::PrePrerollMovie(mMovie, 0, mRate,
									  PrePrerollCompleteCallback, this));
}

void TQTMovie::PrePrerollComplete(OSErr inError)
{
	ASSERT(mState == MOVIE_PREPREROLLING);

	// Check whether the pre-prerolling failed.  (If we throw an
	// exception from here, PrePrerollCompleteCallback will catch it
	// and mark the movie as broken.)
	CHECK_MAC_ERROR(inError);

	// The movie should be done flashing (and drawing in awkward places),
	// so attach it to the correct port.
	::SetMovieGWorld(mMovie, mPort, NULL);

	// Preroll our movie.  This causes QuickTime to open all our
	// components and get our data buffers ready for playback.
	// TODO - WHYMANUAL
	CHECK_MAC_ERROR(::PrerollMovie(mMovie, 0, mRate));
	
	// Attach a movie controller to our movie.
	Rect bounds = {120, 160, 640, 480};
	long controller_flags = mcTopLeftMovie | mcNotVisible;
	mMovieController = ::NewMovieController(mMovie, &bounds,
											controller_flags);
	CHECK_MAC_ERROR(::GetMoviesError());
	
	// Mark the controller's rectangle as valid to avoid a
	// double repainting.
	//Rect movie_bounds;
	//::GetMovieBox(mMovie, &movie_bounds);
	//::ValidRect(&movie_bounds);
	
	// Install our action filter.
	long refcon = reinterpret_cast<long>(this);
	CHECK_MAC_ERROR(::MCSetActionFilterWithRefCon(mMovieController,
												  &ActionFilterCallback,
												  refcon));

	UpdateMovieState(MOVIE_READY);
	
	// Start the movie.  I tried doing this with mcActionPrerollAndPlay,
	// mcActionPlay and mcActionAutoPlay (whatever that is), and the
	// movie wouldn't play reliably.  Now I just call Resume, which
	// does some semi-naughty things, but *everything works*.
	UpdateMovieState(MOVIE_STARTED);
	Resume();
}


//=========================================================================
// Regular TQTMovie Methods
//=========================================================================

TQTMovie::~TQTMovie() throw ()
{
	ReleaseResources();
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

void TQTMovie::Resume()
{
	ASSERT(mState == MOVIE_STARTED);
	if (IsPaused())
	{
		::StartMovie(mMovie);
		CHECK_MAC_ERROR(::GetMoviesError());
		CHECK_MAC_ERROR(::MCMovieChanged(mMovieController, mMovie));
	}
}

void TQTMovie::UpdateMovieState(MovieState inNewState)
{
	// Sanity-check our new state, and run any transition actions.
	switch (inNewState)
	{
		case MOVIE_INCOMPLETE:
			ASSERT(mState == MOVIE_UNINITIALIZED);
			break;

		case MOVIE_PREPREROLLING:
			ASSERT(mState == MOVIE_INCOMPLETE);
			break;

		case MOVIE_READY:
			ASSERT(mState == MOVIE_PREPREROLLING);
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
			// Devour mouse clicks using the trick recommended by
			// Inside Macintosh.
			EventRecord* evt = reinterpret_cast<EventRecord*>(inParams);
			evt->what = nullEvent;
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

void TQTMovie::PrePrerollCompleteCallback(Movie inMovie,
										  OSErr inError,
										  void *inRefCon)
	throw ()
{
	TQTMovie *movie = reinterpret_cast<TQTMovie*>(inRefCon);
	if (!movie->IsBroken())
	{
		try
		{
			// Run the appropriate method on our object.
			movie->PrePrerollComplete(inError);
		}
		catch (...)
		{
			// If something throws an exception, scrap our movie.
			movie->UpdateMovieState(MOVIE_BROKEN);
		}
	}
}

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

