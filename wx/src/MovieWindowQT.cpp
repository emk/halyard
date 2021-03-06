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

#include "AppHeaders.h"
#include "AppConfig.h"

#if CONFIG_HAVE_QUICKTIME

#include "TQTMovie.h"
#include "AppGlobals.h"
#include "HalyardApp.h"
#include "MovieWindow.h"
#include "MovieWindowQT.h"

using namespace Halyard;

BEGIN_EVENT_TABLE(MovieWindowQT, wxWindow) // XXX - What should the parent be?
    EVT_ERASE_BACKGROUND(MovieWindowQT::OnEraseBackground)
    EVT_PAINT(MovieWindowQT::OnPaint)
    EVT_IDLE(MovieWindowQT::OnIdle)
    EVT_MOTION(MovieWindowQT::OnMouseMove)
    EVT_ACTIVATE(MovieWindowQT::OnActivate)
    EVT_LEFT_DOWN(MovieWindowQT::OnLeftDown)
    //EVT_KEY_DOWN(MovieWindowQT::OnKeyDown) - Incomplete!
END_EVENT_TABLE()

MovieWindowQT::MovieWindowQT(wxWindow *inParent, wxWindowID inID,
                             const wxPoint &inPos,
                             const wxSize &inSize,
                             long inWindowStyle,
                             MovieWindowStyle inMovieWindowStyle,
                             const wxString &inName)
    : MovieWindow(inParent, inID, inPos, inSize, inWindowStyle,
                  inMovieWindowStyle, inName),
      mMovie(NULL), mCouldNotConstructMovie(false), mIsRemote(false),
      mTimeout(DEFAULT_TIMEOUT)
{
#ifdef __WXMSW__
    // Prepare this window for use with QuickTime.
    mHWND = GetHWND();
    TQTMovie::RegisterWindowForMovies((HWND) mHWND);
#endif
}

MovieWindowQT::~MovieWindowQT() {
    CleanUpMovie();

#ifdef __WXMSW__
    // Detach this window from QuickTime.
    TQTMovie::UnregisterWindowForMovies((HWND) mHWND);
#endif
}

#ifdef __WXMSW__

void *MovieWindowQT::GetMacPort() {
    return TQTMovie::GetPortFromHWND((HWND) mHWND);
}

wxPoint MovieWindowQT::GetMoviePosRelativeToPort() {
    // On Windows, we get a separate CGrafPtr for each wxWidget, and hence
    // a separate co-ordinate system.
    return wxPoint(0, 0);
}

#else // !__WXMSW__

void *MovieWindowQT::GetMacPort() {
    // This is based on how wxWidgets/src/mac/carbon/dc.cpp recovers
    // CGrafPtr values for a wxWindow.
    WindowRef window = reinterpret_cast<WindowRef>(MacGetTopLevelWindowRef());
    return reinterpret_cast<CGrafPtr>(GetWindowPort(window));
}

wxPoint MovieWindowQT::GetMoviePosRelativeToPort() {
    // On the Mac, we get a single coordinate system for each wxFrame,
    // so we need to map our co-ordinates appropriately.  Again, we figured
    // out how to do this by looking at wxWidgets/src/mac/carbon/dc.cpp.
    int x = 0, y = 0;
    MacWindowToRootWindow(&x, &y);
    return wxPoint(x, y);
}

#endif // !__WXMSW__

void MovieWindowQT::CleanUpMovie() {
    if (mMovie) {
        delete mMovie;
        mMovie = NULL;
    }

    // Reset status flags when movie is cleaned up.
    mCouldNotConstructMovie = false;
    mIsRemote = false;
}

void MovieWindowQT::SetMovie(const wxString &inName) {
    ASSERT(inName != wxT(""));

    // Detach any old movie, determine if the new one will be remote, and
    // try to attach it.  The TQTMovie constructor may throw an exception.
    CleanUpMovie();
    std::string name(inName.mb_str());
    mIsRemote = TQTMovie::IsRemoteMoviePath(name.c_str());
    try {
        mMovie = new TQTMovie(reinterpret_cast<CGrafPtr>(GetMacPort()),
                              name.c_str());
    } catch (std::exception &) {
        mCouldNotConstructMovie = true;
        throw;
    }

    // Figure out what playback options we want.
    MovieWindowStyle style = GetMovieWindowStyle();
    TQTMovie::PlaybackOptions opt = TQTMovie::kDefaultOptions;
    if (style & MOVIE_CONTROLLER)
        opt |= TQTMovie::kEnableMovieController;
    if (style & MOVIE_AUDIO_ONLY)
        opt |= TQTMovie::kAudioOnly;
    if (style & MOVIE_LOOP)
        opt |= TQTMovie::kLoopMovie;
    if (style & MOVIE_INTERACTION)
        opt |= TQTMovie::kEnableInteraction;
    if (style & MOVIE_REPORT_CAPTIONS)
        opt |= TQTMovie::kUseTextTrackAsCaptions;

    // Tell the movie to play whenever it feels ready.  (Once upon a time,
    // this comment suggested, "We'll change this to better integrate with
    // pre-rolling," but I'm not sure how relevant that notion is today.)
    wxPoint movie_pos(GetMoviePosRelativeToPort());
    Point p;
    p.h = movie_pos.x;
    p.v = movie_pos.y;
    mMovie->StartWhenReady(opt, p);
}

MovieFrame MovieWindowQT::GetFrame() {
    if (!mMovie || !mMovie->IsStarted())
        return 0;
    else
    {
        // TODO - I have no idea if this calculation is correct.  See
        // the QuickTime documentation.  But it appears to produce
        // correct results...
        TimeScale scale = mMovie->GetTimeScale();
        TimeValue time = mMovie->GetMovieTime();
        return time * FRAMES_PER_SECOND / scale;
    }
}

bool MovieWindowQT::IsReadyToHandleOwnDrawing() const {
    return (mMovie && mMovie->IsReadyToHandleOwnDrawing());
}

bool MovieWindowQT::IsRemoteMovie() {
    return mIsRemote;
}

bool MovieWindowQT::IsBroken() {
    return mCouldNotConstructMovie || (mMovie && mMovie->IsBroken());
}

bool MovieWindowQT::HasTimedOut() {
    // Check the timeout.
    return (mMovie && mTimeout && mMovie->GetTimeoutEllapsed() > mTimeout);
}

bool MovieWindowQT::GetNextCaption(std::string &outCaption) {
    return mMovie && mMovie->GetNextCaption(outCaption);
}

bool MovieWindowQT::IsLooping() {
    return mMovie && mMovie->IsLooping();
}

bool MovieWindowQT::IsDone() {
    if (!mMovie)
        return false;
    else
        return mMovie->IsDone();
}

void MovieWindowQT::Pause() {
    if (mMovie)
        mMovie->Pause();
}

void MovieWindowQT::Resume() {
    if (mMovie)
        mMovie->Unpause();
}

void MovieWindowQT::SetVolume(const std::string &inChannel, double inVolume) {
    float volume = inVolume;
    if (volume < 0.0)
        volume = 0.0;
    if (mMovie)
        mMovie->SetMovieVolume(volume);
}

void MovieWindowQT::SetTimeout(unsigned int timeout) {
    mTimeout = timeout;
}

void MovieWindowQT::OnEraseBackground(wxEraseEvent &inEvent) {
    // Ignore this event to prevent flicker.
    wxLogTrace(TRACE_STAGE_DRAWING, wxT("Ignoring request to erase movie."));
}

void MovieWindowQT::OnPaint(wxPaintEvent &inEvent) {
    wxLogTrace(TRACE_STAGE_DRAWING, wxT("Asked to repaint movie window."));
    if (mMovie) {
        wxLogTrace(TRACE_STAGE_DRAWING, wxT("Passing repaint event to movie."));
        mMovie->Redraw();
    }

    // Let wxWindows handle this paint event properly.  It won't paint
    // anything, but it will do a bunch of other magic with a PaintDC
    // and other stuff, none of which I wish to figure out right now.
    inEvent.Skip();
}

void MovieWindowQT::OnIdle(wxIdleEvent &inEvent) {
    if (mMovie)
        mMovie->Idle();
}

void MovieWindowQT::OnMouseMove(wxMouseEvent &inEvent) {
    // Ignore this event so it doesn't propogate to the stage, which
    // would try to update the cursor, which QuickTime would prefer
    // to manage on its own.
    // XXX - Nice theory--the events don't reach the stage--but
    // the flicker continues anyway.
}

void MovieWindowQT::OnActivate(wxActivateEvent &inEvent) {
    if (mMovie) {
        if (inEvent.GetActive())
            wxLogTrace(TRACE_STAGE_DRAWING, wxT("Activate movie window."));
        else
            wxLogTrace(TRACE_STAGE_DRAWING, wxT("Deactivate movie window."));
        mMovie->Activate(inEvent.GetActive());
    }
}

#ifdef __WXMSW__

void MovieWindowQT::OnLeftDown(wxMouseEvent &inEvent) {
    // TODO - Figure out why double-clicking is broken.
    if (mMovie) {
        // Get usable event.when and event.modifiers values.  Note that
        // these aren't strictly accurate, merely usable.
        EventRecord event;
        mMovie->FillOutEvent((HWND) mHWND, WM_LBUTTONDOWN, (WPARAM) MK_LBUTTON,
                             (LPARAM) 0 /* We'll do the mouse. */, &event);

        // Get the click location and convert it to a Mac point.
        wxPoint p = inEvent.GetPosition();
        Point mac_point;
        mac_point.h = p.x;
        mac_point.v = p.y;

        // Pass the event to our movie.
        // TODO - Fix 'when' and 'modifiers' parameters.
        mMovie->Click(mac_point, event.when, event.modifiers);
    }
}

void MovieWindowQT::OnKeyDown(wxKeyEvent &inEvent) {
    // TODO - Only pass appropriate keys to QuickTime, and Skip() the
    // rest of the events.  Figure out why SPACE isn't working.  And
    // re-enable this handler in our event table.
    if (mMovie) {
        EventRecord event;
        mMovie->FillOutEvent((HWND) mHWND, WM_KEYDOWN,
                             (WPARAM) inEvent.GetRawKeyCode(),
                             (LPARAM) inEvent.GetRawKeyFlags(), &event);
        mMovie->Key(event.message & charCodeMask, event.modifiers);
    }
}

#else // !defined __WXMSW__

//static void GetEventRecord(EventRecord &outEvent) {
//    // http://lists.wxwidgets.org/pipermail/wx-dev/2003-September/038087.html
//    EventRef event =
//        reinterpret_cast<EventRef>(wxTheApp->MacGetCurrentEvent());
//    bool ok = ::ConvertEventRefToEventRecord(event, &outEvent);
//    if (!ok)
//        THROW("Cannot convert EventRef to EventRecord");
//}

void MovieWindowQT::OnLeftDown(wxMouseEvent &inEvent) {
    // Not yet implemented on the Mac.
}

void MovieWindowQT::OnKeyDown(wxKeyEvent &inEvent) {
    // Not yet implemented on the Mac.
}

#endif // !defined __WXMSW__

#endif // CONFIG_HAVE_QUICKTIME
