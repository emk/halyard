// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
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

#define _CRT_SECURE_NO_DEPRECATE (1)

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

TMacError::TMacError(const char *inFile, int inLine,
                     ComponentResult inErrorCode)
    : std::runtime_error("MacOS error"),
      mFile(inFile), mLine(inLine), mErrorCode(inErrorCode)
{
}

void TMacError::Check(const char *inFile, int inLine,
                      ComponentResult inErrorCode)
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
	//
	// Old comment from Win32/FiveL/FiveL.cpp: Use the
	// kInitializeQTMLUseGDIFlag as we have seen flashes on startup of
	// certain movies on certain platforms (Dell laptops and Win98).
	//
	// kInitializeQTMLUseGDIFlag breaks VP3.2 with QT6.  But if we don't
    // use it, QuickTime interacts horribly with the Boehm GC, causing
    // the performance of both to plummet.  So let's hope our VP3 codec
    // is up to date.
    //
    // UPDATE: The Boehm problem has theoretically been fixed by forcing
    // Boehm to only scan explicitly-marked roots for Scheme_Object *
    // values instead of scanning all global data for all loaded DLLs.
    // So we're going back to allowing hardware acceleration of QuickTime
    // for now, pending future complaints.
    //
	//CHECK_MAC_ERROR(::InitializeQTML(kInitializeQTMLUseGDIFlag));
	CHECK_MAC_ERROR(::InitializeQTML(0));

	// As Chuck discovered, QuickTime likes to draw movies to 
	// inappropriate places on the screen, and to engage in other bits
	// of graphic weirdness during the startup process.
	Rect rect = {0, 0, 1, 1};
	CHECK_MAC_ERROR(::NewGWorld(&sDummyGWorld, 8, &rect, NULL, NULL, 0));
	
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
		if (sDummyGWorld)
			::DisposeGWorld(sDummyGWorld);
		::TerminateQTML();
		sIsQuickTimeInitialized = false;
	}
}

void TQTMovie::RegisterWindowForMovies(HWND inWindow)
{
	ASSERT(sIsQuickTimeInitialized);

	// Tell QuickTime to bind a CGrafPort to the window.
	// This also configures the window to receive idle events,
	// so we'll be able to do our movie processing.
	// TODO - Can an error happen here?
	::CreatePortAssociation(inWindow, NULL, 0);
}

void TQTMovie::UnregisterWindowForMovies(HWND inWindow)
{
	ASSERT(sIsQuickTimeInitialized);

	// Dispose of the Macintosh drawing context associated with inWindow.
	CGrafPtr port = GetPortFromHWND(inWindow);
	if (port)
		::DestroyPortAssociation(port);
}

CGrafPtr TQTMovie::GetPortFromHWND(HWND inWindow)
{
	return reinterpret_cast<CGrafPtr>(::GetNativeWindowPort(inWindow));
}

bool TQTMovie::IsRemoteMoviePath(const std::string &inMoviePath) {
    return (inMoviePath.find("http:") == 0 ||
            inMoviePath.find("rtsp:") == 0 ||
            inMoviePath.find("ftp:") == 0);
}


//=========================================================================
// TQTMovie Asynchronous Startup Methods
//=========================================================================
// These methods, together, form the asyncronous startup path.  I've
// grouped these together so you have some chance of seeing what's up.

TQTMovie::TQTMovie(CGrafPtr inPort, const std::string &inMoviePath)
    : mPort(inPort), mState(MOVIE_UNINITIALIZED),
      mCanGetMovieProperties(false),
	  mMovie(NULL), mMovieController(NULL),
      mVolume(1.0f), mShouldStartWhenReady(false),
      mShouldPauseWhenStarted(false),
      mTimeoutStarted(false), mTimeoutDisabled(false),
      mTimeoutBase(0), mLastSeenTimeValue(0)
{
	ASSERT(sIsQuickTimeInitialized);

	// If we allocate these resources, we must free them before exiting.
    Handle url_handle = NULL;
	bool have_refnum = false;
	short refnum;

    // Record the time we started loading data.  We'll use this in various
    // calculations which need to estimate the load speed.
    mMovieOpenTime = ::time(NULL);
    //gDebugLog.Log("Starting %s at %d.", inMoviePath.c_str(), mMovieOpenTime);

	// We pass these flags to all the various NewMovieFrom... functions.
	// newMovieAsyncOK tells QuickTime to immediately return an
	// empty movie, and to load our data in the background.
    //
    // XXX - newMovieAsyncOK is necessary for asynchronous background
    // loading of movies.  Unfortunately, it makes certain audio and
    // video clips take vastly longer to reach kMovieLoadStatePlayable
    // for no obvious reason.  For now, I'm disabling it, and breaking
    // preloading.
	short load_flags = newMovieActive /*| newMovieAsyncOK*/;
	
    try
    {
		// Determine whether we're processing a URL or a local file
		// name, and load the movie accordingly.
		if (IsRemoteMoviePath(inMoviePath))
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
	catch (std::exception &)
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
            // Give MoviesTask up to 1000 milliseconds to process movies
            // (normally, we'd pass in 0, which means to give every movie
            // exactly one timeslice).  This code is from the
            // network-streaming-robustified version of TQTMovie, and we
            // don't know how it affects animation responsiveness and other
            // script-based tasks.  But if I recall correctly, this did
            // improve streaming performance in QT 6 for some clips.
			::MoviesTask(mMovie, 1000);

		// See if we need to finish an asynchronous load.
		if (mState == MOVIE_INCOMPLETE)
			ProcessAsyncLoad();

        // Maybe update our timeout.
        UpdateTimeout();
	}
	catch (...)
	{
		UpdateMovieState(MOVIE_BROKEN);
	}
}

void TQTMovie::ProcessAsyncLoad()
{
	ASSERT(mState == MOVIE_INCOMPLETE);

    // XXX - Give QuickTime plenty of idles to get this movie off the
    // ground.  This seems to be necessary if we want to avoid inexplicable
    // startup delays on some movies.  (This is merged from the
    // stream-robustified version of TQTMovie.  I haven't checked to see
    // whether it is still needed.)
    for (int i = 0; i < 100; i++)
        ::MoviesTask(mMovie, 0);

	// Inside Macintosh says this function is expensive, and that
	// we shouldn't call it more than every 1/4 second or so.  For
	// now, we'll call it constantly, and see if that causes problems.
	long load_state = GetMovieLoadState();

	// Check for loading errors.  We don't really know if GetMoviesError
	// returns the correct error value, so we'll just take a guess.
	if (load_state == kMovieLoadStateError)
		throw TMacError(__FILE__, __LINE__, ::GetMoviesError());
	
	// We don't advance to the next stage until the movie is playable.
    if (SafeToStart(load_state))
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

        // This is a good time to activate the timeout.
        UpdateTimeout(true);
	}
}

void TQTMovie::Start(PlaybackOptions inOptions, Point inPosition)
{
	ASSERT(mState == MOVIE_READY);

	// Store our options so other routines can find them.
	mOptions = inOptions;
	mPosition = inPosition;

    // Activate our timeout, if we haven't already.
    if (!mTimeoutStarted)
        UpdateTimeout(true);

	// The movie should be done flashing (and drawing in awkward places),
	// so we should attach it to the correct port (if it has any graphics
	// we care about).
	if (!(mOptions & kAudioOnly))
	{
        // See IsReadyToHandleOwnDrawing(), below, which needs to start
        // returning true as soon as we set up our new GWorld.
		::SetMovieGWorld(mMovie, mPort, NULL);
		CHECK_MAC_ERROR(::GetMoviesError());
	}

    // If we've been asked to use the text track as captions, do that
    // before computing the movie bounds (since disabling the text track
    // may make the movie smaller).
    if (mOptions & kUseTextTrackAsCaptions)
        DivertTextTrackToCaptions();
		
	// Figure out where to put our movie on the screen.  (We hope enough
	// of our movie is loaded by now to calculate the box accurately!)
	// TODO - Should this take the controller itself into account?  And if
	// so, how?
	Rect bounds;
	::GetMovieBox(mMovie, &bounds);
    // According to MacTech, the bounds rectangle may be offset from 0,0
    // for unspecified reasons.  I've only ever seen this happen for
    // QuickTime movies with text tracks.  MacTech suggests we just correct
    // for the offset and go about our business: <http://www.mactech.com/
    // articles/mactech/Vol.15/15.12/Dec99GettingStarted/index.html>.
    ::MacOffsetRect(&bounds, -bounds.left, -bounds.top);
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
		DoAction(mcActionSetKeysEnabled, reinterpret_cast<void*>(true));

	// Handle our other controller options.
	if (mOptions & kLoopMovie)
		DoAction(mcActionSetLooping, reinterpret_cast<void*>(true));
	if (mOptions & kPlayEveryFrame)
		DoAction(mcActionSetPlayEveryFrame, reinterpret_cast<void*>(true));

    // Now that we have a controller, we can finally set the volume.
    SetMovieVolume(mVolume);

	// Figure out our preferred playback rate.  We used to store this
	// in a member variable, but it's *much* safer to ask for it just
	// before you use it.
	Fixed rate = ::GetMoviePreferredRate(mMovie);
	
	// Start the movie.
	UpdateMovieState(MOVIE_STARTED);
    if (!mShouldPauseWhenStarted)
        DoAction(mcActionPrerollAndPlay, reinterpret_cast<void*>(rate));
}

bool TQTMovie::IsReadyToHandleOwnDrawing() const {
    // Until we have actual movie data, it's better to let the Stage handle
    // any drawing calls, because it knows what background lies beneath our
    // movie.
    //
    // We're ready to handle our own drawing as soon as we make our final
    // call to SetMovieGWorld.  This currently happens in Start(...),
    // above, so we take advantage of Start()'s transition from MOVIE_READY
    // to MOVIE_STARTED to detect the correct time to return true.
    return IsStarted();
}

void TQTMovie::DivertTextTrackToCaptions() {
    // Fetch the first text track associated with our movie.
    // http://developer.apple.com/technotes/tn/tn1087.html
    Track track = ::GetMovieIndTrackType(mMovie, 1, TextMediaType,
                                         movieTrackMediaType);
	CHECK_MAC_ERROR(::GetMoviesError());
    if (!track)
        return;

    // Get the media handler for our text track.
    MediaHandler handler = ::GetMediaHandler(GetTrackMedia(track));
    CHECK_MAC_ERROR(::GetMoviesError());
    ASSERT(handler);

    // Hide the text track by setting its dimensions to 0.  We could just
    // disable the track, but then we wouldn't get any events from it.
    //
    // TODO - Is this really the best solution?  I haven't been able to
    // find any other example code which does this.
    ::SetTrackDimensions(track, 0, 0);

    // Install our caption callback.
    long refcon = reinterpret_cast<long>(this);
    ::TextMediaSetTextProc(handler, &CaptionCallback, refcon);
    CHECK_MAC_ERROR(::GetMoviesError());
}

long TQTMovie::GetMovieLoadState() {
	// Inside Macintosh says this function is expensive, and that
	// we shouldn't call it more than every 1/4 second or so.  For
	// now, we'll call it constantly, and see if that causes problems.
	long load_state = ::GetMovieLoadState(mMovie);
    if (load_state >= kMovieLoadStatePlayable)
        mCanGetMovieProperties = true;
    return load_state;
}

bool TQTMovie::SafeToStart(long inLoadState) {
    // Currently known values for inLoadState, in order:
    //
    //   kMovieLoadStateError         - the movie is broken
    //   kMovieLoadStateLoading       - the movie can't be started
    //   kMovieLoadStatePlayable      - the movie could be started
    //   kMovieLoadStatePlaythroughOK - the movie is expected to play
    //                                  through to the end without stalling
    //   kMovieLoadStateComplete      - the entire movie is loaded
    //
    // Apple Computer, Inc., reserves the right to insert more load states
    // in between the existing ones, so always use '>=' or '<=' to test.
    if (inLoadState >= kMovieLoadStatePlaythroughOK) {
        // QuickTime thinks the movie will play through to the end.
        return true;
    }

    return false;
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

		// Get the movie's duration.  Unknown/indefinite duration is
		// represented as INDEFINITE_DURATION, a very large number.
		TimeValue duration = ::GetMovieDuration(mMovie);
		// XXX - I don't seriously expect this error to occur, and I don't
		// want to have to catch it.
		//CHECK_MAC_ERROR(::GetMoviesError());

		return duration <= current_time;
	}
}

bool TQTMovie::IsPaused()
{
    if (mState == MOVIE_BROKEN)
        return true;
    else if (mState < MOVIE_STARTED)
        return mShouldPauseWhenStarted;
    else
    {
        ASSERT(mState == MOVIE_STARTED);
        long flags;
        CHECK_MAC_ERROR(::MCGetControllerInfo(mMovieController, &flags));
        return flags & mcInfoIsPlaying ? false : true;
    }
}

void TQTMovie::Pause()
{
    if (mState == MOVIE_BROKEN)
        return;

    // XXX - Massive kludge, see comment for variable.
    mTimeoutDisabled = true;
    if (mState < MOVIE_STARTED)
        mShouldPauseWhenStarted = true;
    else
    {
        ASSERT(mState == MOVIE_STARTED);        
        if (!IsPaused())
            DoAction(mcActionPlay, reinterpret_cast<void*>(0));
    }
}

void TQTMovie::Unpause()
{
    if (mState == MOVIE_BROKEN)
        return;

    if (mState < MOVIE_STARTED)
        mShouldPauseWhenStarted = false;
    else
    {
        ASSERT(mState == MOVIE_STARTED);        
        if (IsPaused() && !IsDone())
        {
            Fixed rate = ::GetMoviePreferredRate(mMovie);
            DoAction(mcActionPlay, reinterpret_cast<void*>(rate));
        }
    }
}

TimeValue TQTMovie::GetMovieTime()
{
	ASSERT(CanGetMovieProperties());
	TimeValue current_time = ::GetMovieTime(mMovie, NULL);
	CHECK_MAC_ERROR(::GetMoviesError());
	return current_time;
}

void TQTMovie::SetMovieVolume(float inVolume)
{
    // Store the volume.  We'll need this later if we don't have an
    // mMovieController yet.
    mVolume = inVolume;
	if (mMovieController) {
        // Movie volume is allegedly a 16-bit fixed point number with the
        // lower 8 bits determining the fraction, the upper 8 bits
        // determining the integer part, and a value between -1.0 and 1.0,
        // where negative volumes are muted.  However, investigation reveals
        // that newly-created movies have a typical volume of 255, *not*
        // 256, so that's what we multiply by.
		//
		// Also, the documentation claims that we're supposed to pass
		// in a pointer to 'volume', but that doesn't work.  Instead, we
		// need to pass in 'volume' itself.
        SInt16 volume = static_cast<SInt16>(inVolume * 255);
		DoAction(mcActionSetVolume, reinterpret_cast<void*>(volume));
    }
}

TimeScale TQTMovie::GetTimeScale()
{
	ASSERT(CanGetMovieProperties());
	TimeScale scale = ::GetMovieTimeScale(mMovie);
	CHECK_MAC_ERROR(::GetMoviesError());
	return scale;
}

TimeValue TQTMovie::GetDuration()
{
	ASSERT(CanGetMovieProperties());
	TimeValue duration = ::GetMovieDuration(mMovie);
	CHECK_MAC_ERROR(::GetMoviesError());
	return duration;	
}

TimeValue TQTMovie::GetMaxLoadedTimeInMovie()
{
	ASSERT(CanGetMovieProperties());
    TimeValue max_loaded;
    CHECK_MAC_ERROR(::GetMaxLoadedTimeInMovie(mMovie, &max_loaded));
    return max_loaded;
}

void TQTMovie::ThrowIfBroken()
{
	if (IsBroken())
		// XXX - Find a better error to throw.
		throw TMacError(__FILE__, __LINE__, noErr);
}

unsigned int TQTMovie::GetTimeoutEllapsed() {
    // Under certain circumstances, we always return a timeout of 0.
    if (!mTimeoutStarted) {
        return 0; // The timeout hasn't been started yet.
    } else if (mTimeoutDisabled) {
        return 0; // We've disabled the timeout.  A total kludge.
    } else if (mOptions & (kEnableMovieController|kEnableInteraction)) {
        return 0; // This movie is potentially under user control.
    } else if (IsDone()) {
        return 0; // The movie has finished playing.
    } else if (IsBroken()) {
        return 0; // The movie is broken, so no timeout.
    }

    return ::time(NULL) - mTimeoutBase;
}

void TQTMovie::UpdateTimeout(bool inStart) {
    // Make sure the timeout is started.
    if (!mTimeoutStarted) {
        if (inStart) {
            mTimeoutStarted = true;
            mTimeoutBase = ::time(NULL);
        }
        return;
    }

    if (IsStarted()) {
        // If the movie has made progress, reset the timeout.
        TimeValue currentTimeValue = GetMovieTime();
        if (currentTimeValue != mLastSeenTimeValue) {
            mLastSeenTimeValue = currentTimeValue;
            mTimeoutBase = ::time(NULL);
        }
    }
}

bool TQTMovie::GetNextCaption(std::string &outCaption)
{
    if (mCaptionsToDisplay.empty())
        return false;
    outCaption = mCaptionsToDisplay.front();
	mCaptionsToDisplay.pop_front();
    return true;
}

void TQTMovie::FillOutMSG(HWND inHWND, UINT inMessage, WPARAM inWParam,
						  LPARAM inLParam, MSG *outMessage)
{
	// Build a Windows MSG object.  This code is from Apple's
	// sample SimplePlayerSDI application.
	outMessage->hwnd = inHWND;
	outMessage->message = inMessage;
	outMessage->wParam = inWParam;
	outMessage->lParam = inLParam;
	outMessage->time = ::GetMessageTime();

	// Fetch the point associated with the most recent message, and use it.
	// TODO - Is this safe in an environment such as wxWindows?
	LONG thePoints = ::GetMessagePos();
	outMessage->pt.x = LOWORD(thePoints);
	outMessage->pt.y = HIWORD(thePoints);	
}

void TQTMovie::FillOutEvent(HWND inHWND, UINT inMessage, WPARAM inWParam,
							LPARAM inLParam, EventRecord *outEvent)
{
	MSG msg;
	FillOutMSG(inHWND, inMessage, inWParam, inLParam, &msg);
	::WinEventToMacEvent(&msg, outEvent);
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
	
	// Convert everything into a Macintosh event record.
	EventRecord	mac_event;
	FillOutEvent(hWnd, message, wParam, lParam, &mac_event);

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

void TQTMovie::Activate(HWND hWnd, bool inIsActivating)
	throw ()
{
	if (IsBroken() || !mMovieController)
		return;

	WindowPtr mac_window =
		reinterpret_cast<WindowPtr>(TQTMovie::GetPortFromHWND(hWnd));
	::MCActivate(mMovieController, mac_window, inIsActivating);
}

void TQTMovie::Click(HWND hWnd, Point inWhere, long inWhen, long inModifiers)
	throw ()
{
	if (IsBroken() || !mMovieController)
		return;

	WindowPtr mac_window =
		reinterpret_cast<WindowPtr>(TQTMovie::GetPortFromHWND(hWnd));
	::MCClick(mMovieController, mac_window, inWhere, inWhen, inModifiers);
}


void TQTMovie::Key(HWND hWnd, SInt8 inKey, long inModifiers)
	throw ()
{
	if (IsBroken() || !mMovieController)
		return;

	WindowPtr mac_window =
		reinterpret_cast<WindowPtr>(TQTMovie::GetPortFromHWND(hWnd));
	::MCKey(mMovieController, inKey, inModifiers);	
}

void TQTMovie::DoAction(mcAction inAction, void *inParam)
{
	ASSERT(mMovieController);
	CHECK_MAC_ERROR(::MCDoAction(mMovieController, inAction, inParam));
}

void TQTMovie::ReleaseResources() throw ()
{
	if (mMovieController)
		::DisposeMovieController(mMovieController);
	if (mMovie)
		::DisposeMovie(mMovie);
}

void TQTMovie::UpdateMovieState(MovieState inNewState)
{
	// Sanity-check our new state.
	ASSERT(inNewState == MOVIE_BROKEN || (inNewState - 1 == mState));

	// Actually update our state.
	mState = inNewState;
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

void TQTMovie::Caption(const std::string &inText)
{
    // Queue the caption.
    mCaptionsToDisplay.push_back(inText);
}


//=========================================================================
//  TQTMovie Static Callbacks
//=========================================================================
//  All these routines are called by the Toolbox, so they're not allowed
//  to throw any exceptions.  In general, they catch the offending
//  exception, and mark the movie as broken.

#define BEGIN_QT_CALLBACK() \
    TQTMovie *movie = reinterpret_cast<TQTMovie*>(inRefCon); \
    if (!movie->IsBroken()) { \
        try {

#define END_QT_CALLBACK(RESULT_IF_FAILED) \
        } catch (...) { \
			/* If something throws an exception, scrap our movie. */ \
			movie->UpdateMovieState(MOVIE_BROKEN); \
            return (RESULT_IF_FAILED); \
        } \
    }

Boolean TQTMovie::ActionFilterCallback(MovieController inController,
									   short inAction, void *inParams,
									   long inRefCon)
	throw ()
{
	bool result;
    BEGIN_QT_CALLBACK();

    // Try to run our object's action filter.
    result = movie->ActionFilter(inAction, inParams);

    END_QT_CALLBACK(false);
    return result;
}

OSErr TQTMovie::CaptionCallback(Handle inText, Movie inMovie,
                                short *inDisplayFlag, long inRefCon) throw ()
{
    BEGIN_QT_CALLBACK();

    // Extract our actual caption.  This is a little tricky, and Apple
    // doesn't document the details, but there's a good explanation in
    // MacTech about how to get this data:
    // http://www.mactech.com/articles/develop/issue_20/20quicktime.html
    // However, we also need to swap the byte order, as shown here:
    // http://developer.apple.com/samplecode/qttext.win/listing11.html
	size_t text_length = EndianU16_BtoN(*reinterpret_cast<short*>(*inText));
    char *text_data = reinterpret_cast<char*>(*inText + sizeof(short));
    std::string text(text_data, text_data + text_length);

    // The caption strings usually seem to end in a carriage return, which
    // we have no use for.  If it's there, we remove it.
    if (text.size() > 0 && text[text.size() - 1] == '\r')
        text.resize(text.size() - 1);

    // Try to run our object's caption routine.
    movie->Caption(text);

    END_QT_CALLBACK(noErr);
    return noErr;
}
