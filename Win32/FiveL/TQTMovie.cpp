// -*- Mode: C++; tab-width: 4; -*-

#include <QTML.h>
#include "TQTMovie.h"

#define ASSERT(x) 0

//USING_NAMESPACE_FIVEL

bool TQTMovie::sIsQuickTimeInitialized = false;


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
	::InitializeQTML(0);
	::EnterMovies();
	sIsQuickTimeInitialized = true;
}

void TQTMovie::ShutDownMovies()
{
	ASSERT(sIsQuickTimeInitialized == true);
	if (sIsQuickTimeInitialized)
	{
		::ExitMovies();
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
// TQTMovie Methods
//=========================================================================

TQTMovie::TQTMovie(const std::string &inMoviePath)
    : mMovie(NULL), mMovieController(NULL)
{
	// If we allocate these resources, we must free them before exiting.
    Handle url_handle = NULL;
	bool have_refnum = false;
	short refnum;

	// We pass these flags to all the various NewMovieFrom... functions.
	// newMovieAsyncOK tells QuickTime to immediately return an
	// empty movie, and to load our data in the background.
	short load_flags = newMovieActive /*| newMovieAsyncOK */;
	
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

		// Set some movie options.
		//::SetMoviePlayHints(mMovie, hintsScrubMode, hintsScrubMode);

		// Get some useful information from our movie.
		Fixed mRate = ::GetMoviePreferredRate(mMovie);

		// Get our movie ready to play.  Theoretically, the Movie
		// Controller should handle this for us, but if we don't
		// make these calls manually, it seems that the controller
		// ignores mcActionPlay and mcActionPrerollAndPlay.
		CHECK_MAC_ERROR(::PrePrerollMovie(mMovie, 0, mRate, NULL, NULL));
		CHECK_MAC_ERROR(::PrerollMovie(mMovie, 0, mRate));

		// Attach a movie controller to our movie.
		// TODO - Set the port.
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
													  &ActionFilterProc,
													  refcon));

		// Start the movie.
		CHECK_MAC_ERROR(::MCDoAction(mMovieController,
									 mcActionPlay,
									 reinterpret_cast<void*>(mRate)));
	}
    catch (...)
    {
		// We failed, so clean up everything.
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

TQTMovie::~TQTMovie()
{
	ReleaseResources();
}

bool TQTMovie::HandleMovieEvent(HWND hWnd, UINT message, WPARAM wParam,
								LPARAM lParam)
{
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
	
	// Convert the Windows event to a Mac event, and pass it to
	// our movie controller object.
	EventRecord	mac_event;
	::WinEventToMacEvent(&msg, &mac_event);
	return ::MCIsPlayerEvent(mMovieController, &mac_event) ? true : false;
}

void TQTMovie::Redraw(HWND hWnd)
{
	WindowPtr mac_window =
		reinterpret_cast<WindowPtr>(TQTMovie::GetPortFromHWND(hWnd));
	::MCDraw(mMovieController, mac_window);
}

bool TQTMovie::IsPaused()
{
	long flags;
	CHECK_MAC_ERROR(::MCGetControllerInfo(mMovieController, &flags));
	return flags & mcInfoIsPlaying ? false : true;
}

void TQTMovie::Pause()
{
	ASSERT(!IsPaused());
	::StopMovie(mMovie);
	CHECK_MAC_ERROR(::GetMoviesError());
	CHECK_MAC_ERROR(::MCMovieChanged(mMovieController, mMovie));
}

void TQTMovie::Resume()
{
	ASSERT(IsPaused());
	::StartMovie(mMovie);
	CHECK_MAC_ERROR(::GetMoviesError());
	CHECK_MAC_ERROR(::MCMovieChanged(mMovieController, mMovie));
}

void TQTMovie::ReleaseResources()
{
	if (mMovieController)
		::DisposeMovieController(mMovieController);
	if (mMovie)
		::DisposeMovie(mMovie);
}

Boolean TQTMovie::ActionFilterProc(MovieController inController,
								   short inAction, void *inParams,
								   long inRefCon)
{
	TQTMovie *movie = reinterpret_cast<TQTMovie*>(inRefCon);
	return movie->ActionFilter(inAction, inParams) ? true : false;
}

bool TQTMovie::ActionFilter(short inAction, void* inParams)
{
	switch (inAction)
	{
		case mcActionMovieEdited:
		case mcActionControllerSizeChanged:
			// We'll eventually want to do stuff here.
			return false;

		case mcActionMovieClick:
			// Devour mouse clicks using the trick recommended by
			// Inside Macintosh.
			EventRecord* evt = reinterpret_cast<EventRecord*>(inParams);
			evt->what = nullEvent;
			return false;
	}
	return false;
}
