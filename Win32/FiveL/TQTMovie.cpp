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
		
		// Attach a movie controller to our movie.
		// TODO - Set the port.
		Rect bounds = {0, 0, 640, 480};
		mMovieController = ::NewMovieController(mMovie, &bounds,
												mcTopLeftMovie);
		CHECK_MAC_ERROR(::GetMoviesError());
    }
    catch (...)
    {
		// We failed, so clean up everything.
		if (mMovie)
			::DisposeMovie(mMovie);
		if (mMovieController)
			::DisposeMovieController(mMovieController);
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
	ASSERT(mMovieController);
	ASSERT(mMovie);

	::DisposeMovieController(mMovieController);
	::DisposeMovie(mMovie);
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
