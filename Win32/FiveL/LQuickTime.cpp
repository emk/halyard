// -*- Mode: C++; tab-width: 4; -*-
//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999, Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// LQuickTime.cpp : A generic QuickTime movie.  This class used to make
//                  QuickTime calls directly, but it was gutted and modified
//                  to be a fairly thin wrapper over TQTMovie, a much
//                  simpler and more modern playback system.  These days,
//                  LQuickTime is mostly a compatibility layer.
//

//=========================================================================
// Missing-in-Action Bugfixes
//=========================================================================
// While replacing Chuck's old QuickTime code with calls to TQTMovie, I
// removed a number of old bugfixes which were no longer directly
// applicable to TQTMovie.  Presumably, some portion of these fixes are
// obsolete, and some portion are still necessary.  If you're having
// QuickTime problems, read through this list carefully, and consider
// adding one or more of these fixes to TQTMovie.
//
// BEFORE PRELOADING:
//   - The old code set the bounds of a newly opened movie to -1,-1,-1,-1,
//     theoretically to "prevent flashing".  This workaround should be
//     completely obsoleted by TQTMovie's offscreen GWorld.
//
// DURING PRELOADING:
//   - If we called Idle 100,000 times during a preload--without the
//     preload completing--we assumed the movie was broken.  Chuck
//     thought this was a QuickTime bug.
//
// STARTING PLAYBACK:
//   - minBuffer: Removed code which called GetMaxLoadedTimeInMovie to make
//     sure a user-specified amount of data was streamed before starting.
//     This used the variables _QuickTimeAudioPreload and
//     _QuickTimeVideoPreload to determine how much to buffer, in frames
//     (where there are typically 30 frames per second).
//   - Some commented-out code set minBuffer=2000 for video, 500 for audio.
//   - If GetMaxLoadedTimeInMovie didn't exceed the minBuffer value
//     in 10,000 calls to MovieTask, then the code assumed the
//     movie wasn't going to arrive.
//   - minCycles: Removed code which called MoviesTask a specified number of
//     times (in a loop) before starting.  This used the variables
//     _QuickTimeVideoCycles and _QuickTimeAudioCycles for streamed video.
//     This code theoretically prevented "burping" at startup.
//   - Some commented-out code set minCycles=50 for video, 20 for audio.
//   - For local video, we always cycled 50 times.
//
// DURING PLAYBACK:
//   - Pause movie shortly before stalling to stream more data.
//     I assume the controller does this for us.
//     

#include "stdafx.h"

#include "ToolUtils.h"

#include "LQuickTime.h"
#include "Globals.h"
#include "Devices.h"

//
//	LQuickTime - the construct-from-stream constructor
//
LQuickTime::LQuickTime(void)
{
	mMovie = NULL;
	mPath = "";
	mPreloaded = false;
	mAudioOnly = false;
	mHaveOrigin = false;
	mLooping = false;
	mStreamed = false;
	mSync = false;
	mWaiting = false;
	mWaitTime = 0;
}

//
//	~LQuickTime - 
//
LQuickTime::~LQuickTime()
{
	if (mMovie)
		delete mMovie;
}

//
//	Idle
//
void LQuickTime::Idle(void)
{
	if (mMovie)
	{
		try
		{
			mMovie->Idle();
			
			if (mMovie->IsBroken())
			{
				gMissingMediaLog.Log("Broken movie: %s", mPath.GetString());
				gDebugLog.Log("Broken movie: %s", mPath.GetString());
				Kill();
			}
			else if (mMovie->IsDone())
			{
				gDebugLog.Log("Movie done: %s", mPath.GetString());
				Kill();
			}
			else if (mMovie->IsStarted() && mWaiting)
			{
				TimeValue time = mMovie->GetMovieTime();
				if (mWaiting && time > mWaitTime)
				{
					gDebugLog.Log("Hit waitpoint: %d > %d",
								  (int32) time, (int32) mWaitTime);
					mWaiting = false;
					mWaitTime = 0;
				}
			}
		}
		catch (...)
		{			
			gDebugLog.Log("Unexpected exception for movie: %s",
						  mPath.GetString());
			Kill();
		}
	}
}

//
//	SetVolume
//
void LQuickTime::SetVolume(int32 inVolume)
{
	if (mMovie)
		mMovie->SetMovieVolume((short) inVolume);
}

//
//	SetWaitPoint - Set a wait point at this frame. We have to be playing a 
//				movie or audio track for this to make any sense. 
//
void LQuickTime::SetWaitPoint(int32 inFrame)
{
	if (mMovie)
	{
		mWaiting = true;
		
		// Assume all movies are 30 frames / second.  We do the math
		// as floating point here because (1) the old code did and (2)
		// it might theoretically prevent an overflow for insanely
		// long movies, and I'm trying to be ultra-cautious while
		// gutting this code.
		TimeScale scale = mMovie->GetTimeScale();
		mWaitTime = (TimeValue) ((((double) inFrame) * scale) / 30.0);

		// 0 is the end, negative means back from the end.  We test
		// against the original inFrame, not mWaitTime, which might
		// have changed from positive to zero above.
		if (inFrame <= 0)
		{
			TimeValue end = mMovie->GetDuration();
			mWaitTime += end;
		}
	}
}

//
//	DrawSelf - draw the pane's frame and contents
//
void LQuickTime::DrawSelf(void)
{
	if (mMovie)
		mMovie->Redraw(hwndApp);
}

//
//	GetBounds
//
TRect LQuickTime::GetBounds(void)
{
	TRect	movieRect;
	movieRect.Set(mBounds);
	return movieRect;
}

//
//	SetOrigin - Set the origin for the NEXT movie to play.
//
void LQuickTime::SetOrigin(TPoint &inPoint)
{
	if ((inPoint.X() >= 0) and (inPoint.Y() >= 0))
		mHaveOrigin = true;
	else
		mHaveOrigin = false;

	mOrigin = inPoint.GetPoint();
}

//
//	Kill - We are really done with the movie, toss everything and
//				tell the PlayerView to bring the CardView up again.
//
void LQuickTime::Kill(void)
{
	if (mSync)
	{
		mSync = false;
		TInterpreter::GetInstance()->WakeUp();
	}

	if (not Playing())
		return;
			
	mHaveOrigin = false;			// no more origin for the movie
	
	Pause();						// stop the movie, if these is one playing	
	Cleanup();						// clean everything up	
}

//
//	Cleanup
//
void LQuickTime::Cleanup(void)
{
	mPath = "";
	mPreloaded = false;
	mAudioOnly = false;
	mLooping = false;
	mStreamed = false;
	mWaiting = false;
	mWaitTime = 0;

//	if (mSync)
//		TInterpreter::GetInstance()->WakeUp();

	mSync = false;
		
	if (mMovie != NULL)
	{
		delete mMovie;
		mMovie = NULL;
	}
}

//
//	Load
//
bool LQuickTime::Load(TString &inMoviePath, bool inAudioOnly, bool inFinish)
{
	try
	{
		mMovie = new TQTMovie(gGrafPtr, inMoviePath.GetString());
		mPath = inMoviePath;
		mAudioOnly = inAudioOnly;

		if (inFinish)
			mMovie->BlockUntilReadyOrBroken();

		mMovie->ThrowIfBroken();
	}
	catch (...)
	{
		return false;
	}

	return true;
}

LQTError LQuickTime::DoPreload(TString &inMoviePath, bool inAudioOnly,
							   bool inFinish /* = false */)
{
	LQTError	retValue = LQT_NoError;

	mPath = inMoviePath;
	mPreloaded = true;

	//mPreloadStateTime = ::timeGetTime();

	if (Load(inMoviePath, inAudioOnly, inFinish))
	{
		if (inFinish)
		{
			ASSERT(mMovie && (mMovie->IsReady() || mMovie->IsBroken()));
			if (mMovie->IsBroken())
			{
				// error - couldn't finish the load
				gDebugLog.Log("QuickTime: error loading movie <%s>",
							  inMoviePath.GetString());
				gLog.Log("ERROR: could not load movie <%s>",
						 inMoviePath.GetString());

				if (mSync)
				{
					mSync = false;
					TInterpreter::GetInstance()->WakeUp();
				}

				return LQT_PreloadError;
			}
		}
	}
	else
		retValue = LQT_NoFile;
	
	return (retValue);
}	

LQTError LQuickTime::PreloadVideo(TString &inMoviePath, bool inSync)
{
	return (Preload(inMoviePath, false, inSync));
}

LQTError LQuickTime::PreloadAudio(TString &inMoviePath, bool inSync)
{
	return (Preload(inMoviePath, true, inSync));
}

LQTError LQuickTime::PlayVideo(TString &inMoviePath, TRect &inRect, int32 inVolume)
{
	return (Play(inMoviePath, inRect, inVolume, false));
}
		
LQTError LQuickTime::PlayAudio(TString &inMoviePath, int32 inVolume, bool inLoop)
{
	TRect	dummyRect;
 
	if (inLoop)
		return (PlayLoop(inMoviePath, inVolume));
	else
		return (Play(inMoviePath, dummyRect, inVolume, true));
}

//
//	Preload
//
LQTError LQuickTime::Preload(TString &inMoviePath, bool inAudioOnly,
							 bool inSync)
{
	if (Playing())
	{
		// can't do this
		return (LQT_Playing);
	}

	Cleanup();

	if (inSync)
	{
		mSync = true;

		TInterpreter::GetInstance()->Pause();
	}

	gDebugLog.Log("QuickTime: Preload <%s>, %s",
		inMoviePath.GetString(), (inSync ? "sync" : "async"));
	
	return (DoPreload(inMoviePath, inAudioOnly, false));
}

//
//	Play
//
LQTError LQuickTime::Play(TString &inMoviePath, TRect &inRect, 
						  int32 inVolume, bool inAudioOnly)
{
	bool		playIt = false;
	LQTError	retValue = LQT_NoFile;

	gDebugLog.Log("QuickTime: Play <%s>", inMoviePath.GetString());

	if (mPreloaded)
	{
		// see if the same movie is preloaded, if not start over
		if (not inMoviePath.Equal(mPath, false))
		{
			Cleanup();
			mPreloaded = false;		// need to preload this one
		}
		else
		{
			ASSERT(mMovie);
			mMovie->BlockUntilReadyOrBroken();
		
			if (mMovie->IsBroken())
			{
				gDebugLog.Log("QuickTime: error loading movie <%s>",
							  mPath.GetString());
				gLog.Log("ERROR: Could not load movie <%s>",
						 mPath.GetString());
				Kill();
				return LQT_PreloadError;
			}
		}
	}
	else
		Cleanup();

	if (not mPreloaded)
	{
		gDebugLog.Log("QuickTime: not preloaded, preload now");
		retValue = DoPreload(inMoviePath, inAudioOnly, true);
	}
	else
		retValue = LQT_NoError;
	
	if (retValue == LQT_NoError)
	{
		ASSERT(mMovie);

		// Set up our default playback options.
		TQTMovie::PlaybackOptions options = TQTMovie::kDefaultOptions;
		Point loc;

		if (mHaveOrigin)
		{
			// Put our movie at the specified location.
			loc = mOrigin;
		}
		else
		{
			// Center it.
			options |= TQTMovie::kCenterMovie;
			loc.h = VSCREEN_WIDTH / 2;
			loc.v = VSCREEN_HEIGHT / 2;
		}

		if (inAudioOnly)
			options |= TQTMovie::kAudioOnly;

		// Start playback.
		mLooping = false;
		SetVolume(inVolume);
		mMovie->BlockUntilReadyOrBroken();
		mMovie->StartWhenReady(options, loc);
	}

	return (retValue);
}

//
//	PlayLoop - Play a movie by looping it.
//  To the best of my knowledge, this code has never finished.
//
LQTError LQuickTime::PlayLoop(TString &inMoviePath, int32 inVolume)
{
	bool		doThePreload = false;		// for now as we only do audio
	bool		playIt = false;				// assume the worst
	LQTError	retValue = LQT_NoFile;
	
	Cleanup();								// don't worry about mHaveOrigin as this must be audio

	if (not mPreloaded)
		retValue = DoPreload(inMoviePath, true, true);
	else
		retValue = LQT_NoError;
	
	if (retValue == LQT_NoError)
	{
		ASSERT(mMovie);

		TQTMovie::PlaybackOptions options =
			TQTMovie::kLoopMovie | TQTMovie::kAudioOnly;
		Point loc;
		loc.h = 0;
		loc.v = 0;
		
		mLooping = true;
		SetVolume(inVolume);
		mMovie->StartWhenReady(options, loc);
	}

	return (retValue);
}

//
//	Pause - Stop playing the movie. 
//
void LQuickTime::Pause(void)
{
	if (mMovie != NULL && mMovie->IsStarted())
		mMovie->Pause();
}

//
//	Resume - Play the movie from wherever we left it.
//
void LQuickTime::Resume(void)
{
	if (mMovie != NULL && mMovie->IsStarted())
		mMovie->Unpause();
}

//
//	HandleEvent - Handle the event.
//
bool LQuickTime::HandleEvent(HWND inWind, UINT inMessage, 
							  WPARAM inWParam, LPARAM inLParam)
{
	if (mMovie)
		return mMovie->HandleMovieEvent(inWind, inMessage, inWParam, inLParam);
	else
		return false;
}

/*
 $Log$
 Revision 1.5.2.1  2003/10/06 20:16:29  emk
 3.4.5 - Ripped out old QuickTime layer and replaced with TQTMovie wrapper.
 (Various parts of the new layer include forward ports from
 FiveL_3_2_0_5_TQTMovie and back ports from Tamale.)  This engine is
 completely untested and almost certainly has bugs and incomplete error
 handling.

 Revision 1.1.2.1.2.1  2002/07/03 09:27:58  emk
 3.2.0.5+TQTMovie - Experimental support for displaying movies through TQTMovie.
 This is incomplete, but we'll want to port it to 3.5 or so.

 Revision 1.1.2.1  2002/03/13 15:06:56  emk
 Merged changed from 3.1.1 -> 3.2.1 into the 3.2.0.1 codebase,
 because we want these in the stable engine.  Highlights:

   1) FiveL.prefs file support.
   2) Removal of -D command line flag.

 Revision 1.2  2002/02/19 12:35:12  tvw
 Bugs #494 and #495 are addressed in this update.

 (1) 5L.prefs configuration file introduced
 (2) 5L_d.exe will no longer be part of CVS codebase, 5L.prefs allows for
     running in different modes.
 (3) Dozens of compile-time switches were removed in favor of
     having a single executable and parameters in the 5L.prefs file.
 (4) CryptStream was updated to support encrypting/decrypting any file.
 (5) Clear file streaming is no longer supported by CryptStream

 For more details, refer to ReleaseNotes.txt

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.8  2000/08/08 19:03:40  chuck
 no message

 Revision 1.7  2000/05/11 12:54:54  chuck
 v 2.01 b2

 Revision 1.6  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.5  2000/03/01 15:46:55  chuck
 no message

 Revision 1.4  1999/12/16 17:17:36  chuck
 no message

 Revision 1.3  1999/11/02 17:16:37  chuck
 2.00 Build 8

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
