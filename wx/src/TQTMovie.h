// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

#ifndef TQTMovie_H
#define TQTMovie_H

#include "AppConfig.h"

#if CONFIG_HAVE_QUICKTIME

// QuickTime requires a different set of headers on every platform.
#if defined __WXMSW__
#   include <windows.h>
    // Don't include Apple's debugging header--it conflicts nastily with the
    // rest of our environment, and we don't need it.
#   define __DEBUGGING__
#   include <QTML.h>
#   include <Movies.h>
#elif defined __WXMAC_CARBON__
#   include <Carbon/Carbon.h>
#   include <QuickTime/QuickTime.h>
#endif

// Our portable system headers.
#include <string>
#include <deque>
#include <stdexcept>
#include <time.h>

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


//////////
/// This exception class represents a MacOS error.  Note that QuickTime
/// for Windows uses MacOS-style errors.
///
/// TODO - Port this code to be a subclass of TException.
///
class TMacError : public std::runtime_error
{
    std::string mFile;
    int mLine;
    ComponentResult mErrorCode;

public:
    /// Create a Macintosh exception from a ComponentResult, which is
    /// apparently the slightly larger version of the OSErr type.
    TMacError(const char *inFile, int inLine, ComponentResult inErrorCode);

    virtual ~TMacError() throw () {}
    
    /// If inErrCode does not equal noErr, raise an error.
    static void Check(const char *inFile, int inLine,
                      ComponentResult inErrorCode);
};

//////////
/// Check the result of a MacOS function for an error.
///
/// \param expr  A value of type OSErr.
///
#define CHECK_MAC_ERROR(expr) \
    /*Halyard::*/ TMacError::Check(__FILE__, __LINE__, (expr))

//////////
/// A stack-based resource management class for the current Macintosh
/// QuickDraw port.  Look up "resource aquisition is initialization"
/// for a summary of why we do this.
///
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
/// This class is intended to be a portable QuickTime movie wrapper
/// containing all our low-level interfaces to QuickTime.  We're
/// implementing this because we're tired of non-portable,
/// duplicate QuickTime code that breaks every time sombody modifies it,
/// and every time that Apple updates QuickTime.  Cross your fingers.
///
/// Please design this class for maximum paranoia.  Use *lots* of
/// assertions, and code defensively.
///
class TQTMovie
{
private:
    static bool sIsQuickTimeInitialized;
    static GWorldPtr sDummyGWorld;

public:
    static void InitializeMovies();
    static void ShutDownMovies();

#if defined __WXMSW__
    static void RegisterWindowForMovies(HWND inWindow);
    static void UnregisterWindowForMovies(HWND inWindow);
    static CGrafPtr GetPortFromHWND(HWND inWindow);
#endif // defined __WXMSW__

    /// Does the specified path belong to a remote, streaming movie?
    static bool IsRemoteMoviePath(const std::string &inMoviePath);

    typedef unsigned long PlaybackOptions;
    enum /* PlaybackOptions */ {
        kDefaultOptions = 0,
        kCenterMovie = 1,
        kAudioOnly = 2,
        kEnableMovieController = 4,
        kEnableInteraction = 8,
        kPlayEveryFrame = 16,
        kLoopMovie = 32,
        kUseTextTrackAsCaptions = 64
    };

private:
    /// We use non-blocking versions of all our QuickTime calls in an
    /// effort to allow as much movie setup to run in the background
    /// as possible.
    ///
    /// This means our object is basically a state machine--it
    /// advances from one state to another as the movie opens.
    enum MovieState {
        MOVIE_BROKEN,         // Something went wrong; this object is dead

        // This sequence is guaranteed to be in this order.
        MOVIE_UNINITIALIZED,  // We're in the constructor
        MOVIE_INCOMPLETE,     // We're waiting for newMovieAsyncOK
        MOVIE_READY,          // Our movie is fully loaded, with a controller
        MOVIE_STARTED         // Our movie has been started
    };

    enum {
        /// Unknown/indefinite duration is supposedly represented as
        /// a duration of 0x7FFFFFF (yes, there really are *six* Fs in that
        /// number).  This can occur for (1) partially loaded movies and
        /// (2) live streaming broadcasts without a duration.
        INDEFINITE_DURATION = 0x7FFFFFF
    };

    /// The state of our object (and the attached Movie).  Set this using
    /// UpdateMovieState, which may take various actions for certain state
    /// transitions.
    MovieState mState;

    /// Is enough of the movie loaded to let us ask about movie properties?
    bool mCanGetMovieProperties;

    /// The port into which we're supposed to draw our movie.
    CGrafPtr mPort;

    /// The movie we're playing.
    Movie mMovie;

    /// We always wrap our movie in a MovieController, even if
    /// we chose not to show the controller bar.  The
    /// MovieController does a ton of work for us.  Furthermore,
    /// it *appears* that MovieController-based applications are
    /// more reliable (especially in the face of QuickTime updates)
    /// than applications which roll all their own QuickTime code.
    /// This is, however, merely a hypothesis.
    MovieController mMovieController;

    /// The volume of our movie, represented as a number from 0.0 to 1.0.
    float mVolume;

    /// Should we start this movie immediately once it's ready to
    /// play?
    bool mShouldStartWhenReady;

    /// Should we start this movie in paused mode?
    bool mShouldPauseWhenStarted;

    /// Our playback options.
    PlaybackOptions mOptions;

    /// Where we should draw our movie.
    Point mPosition;

    /// The time when we started opening the movie.
    time_t mMovieOpenTime;

    /// Have we started running the timeout?
    bool mTimeoutStarted;

    /// Have we disabled our timeout?
    ///
    /// XXX - This is a badly flawed kludge to detect when the movie may
    /// have been paused.  We'll need to do better.
    bool mTimeoutDisabled;

    /// The last time something interesting happened, for various values
    /// of "interesting".  This is used as the base number for calculating
    /// timeouts.
    time_t mTimeoutBase;

    /// The TimeValue the movie was at when we lasted updated TimeoutBase.
    TimeValue mLastSeenTimeValue;

    /// Captions which have been reported by QuickTime, but which have
    /// yet to be displayed.
    std::deque<std::string> mCaptionsToDisplay;

public:
    /// Create a new movie object, and begin the preloading process.
    ///
    /// \param inPort      The Macintosh CGrafPort which will eventually
    ///                    be used to display the movie.  Windows systems
    ///                    can use PrepareWindowForMovies and
    ///                    GetPortFromHWND to get a value for this parameter.
    /// \param inMoviePath  The URL or local filename of the movie.
    TQTMovie(CGrafPtr inPort, const std::string &inMoviePath);

    /// Stop movie playback, and release all resources.
    virtual ~TQTMovie() throw ();

    /// Allow QuickTime some time to process this movie.  QuickTime
    /// need these periodic idle calls to get work done.
    void Idle() throw ();

    /// Tell QuickTime to start the movie as soon as it's playable.  This
    /// could be immediately, or in several seconds.
    ///
    /// \param inOptions  PlaybackOptions value to control playback.
    /// \param inPosition  The location at which we should draw the movie.
    ///                    This is either the upper-left corner, or the
    ///                    center (if kCenterMovie is specified).
    void StartWhenReady(PlaybackOptions inOptions, Point inPosition);

    /// Tell QuickTime to start the movie immediately.  Don't call this
    /// unless IsReady returns true!  (Arguments are the same as the
    /// arguments to StartWhenReady.)
    void Start(PlaybackOptions inOptions, Point inPosition);

    /// Until we actually load some movie data, we don't know what kind of
    /// background we should paint underneath our movie--white? black? a
    /// graphic?--so it's better to let other, more knowledgable subsystems
    /// handle repainting for us.  But once we have movie data, we want to
    /// handle all our own drawing.
    bool IsReadyToHandleOwnDrawing() const;

    /// Did a problem occur either loading or playing this movie?  If
    /// this function returns true, the object is essentially scrap.
    bool IsBroken() const throw () { return mState == MOVIE_BROKEN; }

    /// Is the movie ready to play?
    bool IsReady() const throw () { return mState == MOVIE_READY; }

    /// Has the movie been started?
    bool IsStarted() const throw () { return mState == MOVIE_STARTED; }

    /// Is the movie done playing?
    bool IsDone() throw ();

    /// Has the movie been paused?  Don't call this function
    /// unless IsStated returns true.
    bool IsPaused();

    /// Pause the movie, if it isn't paused.  Don't call this function
    /// unless IsStated returns true.  It's safe to call this on a
    /// paused movie.
    void Pause();

    /// Unpause the movie, if it's paused.  Don't call this function
    /// unless IsStated returns true.  It's safe to call this on an
    /// unpaused movie.
    void Unpause();

    /// Is the movie looping?
    bool IsLooping() { return mOptions & kLoopMovie ? true : false; }

    /// Are we allowed to ask for movie properties yet?
    bool CanGetMovieProperties() { return mCanGetMovieProperties; }
    TimeValue GetMovieTime();
    void SetMovieVolume(float inVolume);
    TimeScale GetTimeScale();
    TimeValue GetDuration();
    TimeValue GetMaxLoadedTimeInMovie();
    void ThrowIfBroken();

    /// How much time has ellapsed towards a timeout?  (We don't actually
    /// worry about how long the timeout is; that's our caller's job.)
    unsigned int GetTimeoutEllapsed();

    /// Get the next available caption, or return false if no captions are
    /// available.  Captions should be displayed as soon as they are
    /// available.
    bool GetNextCaption(std::string &outCaption);

#if defined __WXMSW__

    /// Fill out a Win32 MSG object based on the parameters to this
    /// function and the per-thread message state.
    void FillOutMSG(HWND inHWND, UINT inMessage, WPARAM inWParam,
                    LPARAM inLParam, MSG *outMessage);

    /// Fill out a QuickTime event object based on the parameters to this
    /// function and the per-thread message state.
    void FillOutEvent(HWND inHWND, UINT inMessage, WPARAM inWParam,
                      LPARAM inLParam, EventRecord *outEvent);

#endif // defined __WXMSW__

    /// Notify the movie that the window has been redrawn.  If you
    /// don't call this, the Sorenson 2 codec will often fail to
    /// display video.  It's safe to call this function on broken
    /// or unready movies (so you don't need to overcomplicate your
    /// event loop).
    void Redraw() throw ();

    /// Notify the movie of window activation and deactivation.
    void Activate(bool inIsActivating) throw ();

    /// Notify the movie of a mouse click.
    void Click(Point inWhere, long inWhen, long inModifiers) throw ();

    /// Notify the movie of a key press.
    void Key(SInt8 inKey, long inModifiers) throw ();

protected:
    virtual bool ActionFilter(short inAction, void* inParams);
    virtual void Caption(const std::string &inText);

private:
    void ProcessAsyncLoad();
    void AsyncLoadComplete();
    void DivertTextTrackToCaptions();
    long GetMovieLoadState();
    bool SafeToStart(long inLoadState);
    
    /// Get a WindowRef to pass to the various movie controller APIs.
    /// Casting from a CGrafPtr to a WindowRef is apparently permissible,
    /// even though the two types of objects are entirely distinct under
    /// MacOS X, because the relevant functions can actually take arguments
    /// of either type and map them appropriately.
    ///
    /// TODO - I'm not at all certain if this is actually correct.  More
    /// investigation will be required.
    WindowRef GetMacWindow() { return reinterpret_cast<WindowRef>(mPort); }

    /// Call MCDoAction with the specified command and parameter.
    ///
    /// \param inAction  The action to perform.  There's about a zillion
    ///                 of these, and they're not all centrally
    ///                 documented.
    /// \param inParam  The parameter value to use.
    void DoAction(mcAction inAction, void *inParam);

    /// Release all resources held by this object.  (This call may
    /// safely be made on half-constructed objects.)
    void ReleaseResources() throw ();

    /// Advance to the next state in our state machine.
    void UpdateMovieState(MovieState inNewState);

    /// Update the timeout base with the current time.
    void UpdateTimeout(bool inStart = false);

    /// We use this function to process movie controller events.  This allows
    /// us to (1) receive events from the controller and (2) intercept
    /// the controller's own events.
    ///
    /// \param inController  The controller for this event.
    /// \param inAction  The event code.
    /// \param inParams  The event parameters.
    /// \param inRefCon  User data (a 'this' pointer, in our case).
    /// \return  Undocumented (!!).  Return false for now.
    static Boolean ActionFilterCallback(MovieController inController,
                                        short inAction, void *inParams,
                                        long inRefCon) throw ();

    /// We use this function to receive notifications of text track display
    /// events, so that we can implement custom caption processing.
    static OSErr CaptionCallback(Handle inText, Movie inMovie,
                                 short *inDisplayFlag, long inRefCon) throw ();
};

//////////
/// Register our QuickTime-related primitives with the engine.
///
extern void RegisterQuickTimePrimitives();

#endif // CONFIG_HAVE_QUICKTIME
#endif // TQTMovie_H
