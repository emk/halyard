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
// LQuickTime.cpp : 
//

#include "stdafx.h"

#include "ToolUtils.h"

#include "FixMath.h"
#include "LQuickTime.h"
#include "Globals.h"
#include "Devices.h"

#define MOVIE_TASK_TIMES_LOCAL 50
#define MOVIE_TASK_TIMES_VIDEO_STREAM	50
#define MOVIE_TASK_TIMES_AUDIO_STREAM	20
#define MINIMUM_AUDIO_LOAD_TIME 500
#define MINIMUM_VIDEO_LOAD_TIME	2000

static const char *PREPREROLLEVENTNAME = "PrePreRollEvent";
//HANDLE	PrePreRollEvent;
pascal void	PrePrerollCompleteProc (Movie theMovie, OSErr thePrerollErr, void *theRefcon);

//
//	LQuickTime - the construct-from-stream constructor
//
LQuickTime::LQuickTime(void)
{
	mInMovie = false;
	mPrerolled = false;
	mPrerollCount = 0;
	mPaused = true;
	mAudioOnly = false;
	mHaveOrigin = false;
	mHavePal = false;
	mLooping = false;
	mStreamed = false;
	mPlaying = false;
	mLoadStep = LQT_NOT_LOADED;
	mPreloadEvent = NULL;

	mSync = false;
	mNapTime = 0;

	//mPrerollStateTime = 0;

	mWaiting = false;
	mWaitTime = 0;
	mWaitOffset = 0;
	mCurrentVolume = 0x0000;
	mPrerollError = LQT_NoError;
	
	mMovie = NULL;

	mRate = fixed1;
	mEndTime = 0;
	mScale = 0;	
}

//
//	~LQuickTime - 
//
LQuickTime::~LQuickTime()
{
}

//
//	Idle
//
void LQuickTime::Idle(void)
{
	TimeValue	movieTime = 0;
	
	if (mInMovie)
	{
		if (mLoadStep == LQT_LOAD_ERROR)
		{
			// log the error
			gDebugLog.Log("QuickTime: error loading movie <%s>", mPath.GetString());
			gLog.Log("ERROR: Could not load movie <%s>", mPath.GetString());

			Kill();
			return;
		}

		if (mLoadStep < LQT_LOADED)
		{
			DoLoadStep(false);

			if (mPrerolled)
				mPrerollCount++;

			return;
		}

		::MoviesTask(mMovie, 0);

		if (mPrerolled)
			mPrerollCount++;
	
		if (not mPaused)
		{
			if (not mLooping)
			{
				if (AtEnd(movieTime))
					Kill();				// clean everything up
				else
				{
					// if we aren't at the end, see if we have passed the wait time
					if (mWaiting and (mWaitTime > 0))
					{
						if (movieTime >= mWaitTime)
						{
							mWaiting = false;
							mWaitTime = 0;
						}
					}
					
					::MoviesTask(mMovie, 0);

					// if we are streaming, see if the buffer is empty
					if (mStreamed)
					{
						TimeValue	bufferTime = 0;
						OSErr		err;

						err = ::GetMaxLoadedTimeInMovie(mMovie, &bufferTime);
						if (err == noErr)
						{
							if (bufferTime <= (movieTime + 5))
							{
								// wait for a while
								Pause();

								for (int i = 1; i < 100; i++)
									::MoviesTask(mMovie, 0);

								Resume();
								gDebugLog.Log("QuickTime: Pause to buffer more of movie");
							}
						}
					}
				}
			}
		}
	}
}

//
//	SetVolume
//
void LQuickTime::SetVolume(int32 inVolume)
{
	if (mInMovie)
	{
		mCurrentVolume = inVolume;
		::SetMovieVolume(mMovie, (int16)inVolume);
	}
}


//
//	SetWaitPoint - Set a wait point at this frame. We have to be playing a 
//				movie or audio track for this to make any sense. 
//
void LQuickTime::SetWaitPoint(int32 inFrame)
{
	if (mInMovie)
	{
		mWaiting = true;
		
		if (inFrame <= 0)	// 0 is the end, negative means back from the end
			mWaitTime = mEndTime + (int32) (((float) inFrame * mScale) / (float) 30);
		else
			mWaitTime = (inFrame - mWaitOffset) * mScale / 30;
	}	
}


//
//	DrawSelf - draw the pane's frame and contents
//
void LQuickTime::DrawSelf(void)
{
	if ((mInMovie) and (mPlaying) and (not mAudioOnly))
		::UpdateMovie(mMovie);
}

//
//	GetBounds
//
TRect LQuickTime::GetBounds(void)
{
	TRect	movieRect;

	movieRect.Set(mBounds);
	return (movieRect);
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
		gCardManager.WakeUp();
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
	// before anything else, see if we need to stop the movie
	if (not mPaused)
		Pause();

	mPrerolled = false;
	mPrerollCount = 0;
	mInMovie = false;
	
	mAudioOnly = false;
	mHavePal = false;
	mLooping = false;
	mStreamed = false;
	mPlaying = false;
	mLoadStep = LQT_NOT_LOADED;
	if (mPreloadEvent != NULL)
	{
		CloseHandle(mPreloadEvent);
		mPreloadEvent = NULL;
	}

	mWaiting = false;
	mWaitTime = 0;
	mWaitOffset = 0;

//	if (mSync)
//		gCardManager.WakeUp();

	mSync = false;
	mNapTime = 0;

	//mPrerollStateTime = 0;

	mCurrentVolume = 0;
	mPrerollError = LQT_NoError;

	mPath = "";

	mEndTime = 0;
	mScale = 0;
		
	if (mMovie != NULL)
	{
		::DisposeMovie(mMovie);
		mMovie = NULL;
	}
}

//
//	Load
//
bool LQuickTime::Load(TString &inMoviePath, bool inAudioOnly, bool inFinish)
{
	OSErr		err = noErr;
	int16		actualResID = DoTheRightThing;
	int16		theRefNum;
	uint8		wasChanged;
	bool		retValue = true;

	mLoadStep = LQT_LOAD_STARTED;

	if ((inMoviePath.Contains("http:", false))
		or (inMoviePath.Contains("rtsp:", false)))
	{
		Handle		myHandle = NULL;
		//TimeValue	startTime;
		Size		mySize = 0;
				
		// get the size of the URL, plus the terminating null byte
		mySize = (Size) strlen(inMoviePath.GetString()) + 1;
		
		// allocate a new handle
		myHandle = NewHandleClear(mySize);

		// copy the URL into the handle
		BlockMove(inMoviePath.GetString(), *myHandle, mySize);

		// playing a URL
		short	movieFlags;

		// only call async if we aren't waiting for an answer
		if (not inFinish)
			movieFlags = (newMovieActive) | (newMovieAsyncOK);
		else
			movieFlags = newMovieActive;

		err = ::NewMovieFromDataRef(&mMovie, movieFlags, NULL, 
			myHandle, URLDataHandlerSubType);

		// toss the handle
		DisposeHandle(myHandle);

		if (err != noErr)
		{
			retValue = false;
			goto done;
		}

		mLoadStep = LQT_BEFORE_PRELOAD;		// wait till we can call prepreload
		mStreamed = true;	
	}
	else
	{
		// playing a file
		err = NativePathNameToFSSpec((char *) inMoviePath.GetString(), &mSpec, 0);
		if (err != noErr)
		{
			retValue = false;
			goto done;
		}
		
		err = ::OpenMovieFile(&mSpec, &theRefNum, fsRdPerm);

		if (err != noErr)
		{
			retValue = false;
			goto done;
		}
		
		err = ::NewMovieFromFile(&mMovie, theRefNum, &actualResID, NULL, 0, &wasChanged);

		if (err != noErr)
			retValue = false;
					
		err = ::CloseMovieFile(theRefNum);

		if (not retValue)
			goto done;

		mLoadStep = LQT_LOADED;		// all done
	}

	if (retValue)
	{
		// see if we have enough of the movie to make some movie calls
		if (mLoadStep > LQT_BEFORE_PRELOAD)
		{
			mRate = ::GetMoviePreferredRate(mMovie);

			if (not inAudioOnly)
			{
				// save the bounding rect for the movie and set
				// it off the screen to prevent flashing
				::GetMovieBox(mMovie, &mBounds);

				Rect	fakeBounds = {-1, -1, -1, -1};
				::SetMovieBox(mMovie, &fakeBounds);
			}
		}

		// set the movie gworld to keep it from drawing to the screen
		if (not inAudioOnly)
		{
			//
			// Modified by Chuck on Oct 23, 2000.
			//
			// Set gDummyGWorldPtr to avoid the BLACK WINDOW bug
			//  in 5L engine.  This seems to be the only place
			//  we should modified to avoid the "black window".
			//
			//::SetMovieGWorld(mMovie, gGrafPtr, NULL);
			::SetMovieGWorld(mMovie, gDummyGWorldPtr, NULL);
		}

		mInMovie = true;			// we have a movie
		mAudioOnly = inAudioOnly;
		mPaused = true;				// and it isn't playing yet
	}

done:
//	if ((not retValue) and (err != noErr))
//	{
//		gMissingMediaLog.Log("%s, error %d", inMoviePath.GetString(), err);
//	}

	return (retValue);
}

LQTError LQuickTime::DoPreroll(TString &inMoviePath, bool inAudioOnly, bool inFinish /* = false */)
{
	LQTError	retValue = LQT_NoError;

	mPath = inMoviePath;
	mPrerolled = true;

	//mPrerollStateTime = ::timeGetTime();

	if (Load(inMoviePath, inAudioOnly, inFinish))
	{
		if (inFinish)
		{
			// do everything now, we must need to run it
			if (mLoadStep != LQT_LOADED)
			{
				DoLoadStep(true);

				if (mLoadStep != LQT_LOADED)
				{
					// error - couldn't finish the load
					gDebugLog.Log("QuickTime: error loading movie <%s>", inMoviePath.GetString());
					gLog.Log("ERROR: could not load movie <%s>", inMoviePath.GetString());

					if (mSync)
					{
						mSync = false;
						gCardManager.WakeUp();
					}

					return (LQT_PreloadError);
				}
			}
		}
	}
	else
		retValue = LQT_NoFile;
	
	return (retValue);
}	

LQTError LQuickTime::PrerollVideo(TString &inMoviePath, int32 inTenths, bool inSync)
{
	return (Preroll(inMoviePath, false, inTenths, inSync));
}

LQTError LQuickTime::PrerollAudio(TString &inMoviePath, int32 inTenths, bool inSync)
{
	return (Preroll(inMoviePath, true, inTenths, inSync));
}

LQTError LQuickTime::PlayVideo(TString &inMoviePath, TRect &inRect, int32 inOffset, int32 inVolume)
{
	return (Play(inMoviePath, inRect, inOffset, inVolume, false));
}
		
LQTError LQuickTime::PlayAudio(TString &inMoviePath, int32 inVolume, int32 inOffset, bool inLoop)
{
	TRect	dummyRect;
 
	if (inLoop)
		return (PlayLoop(inMoviePath, inVolume));
	else
		return (Play(inMoviePath, dummyRect, inOffset, inVolume, true));
}

//
//	Preroll
//
LQTError LQuickTime::Preroll(TString &inMoviePath, bool inAudioOnly, int32 inTenths, bool inSync)
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

		gCardManager.Pause();
	}

	if (inTenths > 0)
	{
		// keep track of the start time
		int32 startTime = ::timeGetTime();

		mNapTime = (inTenths * 100) + startTime;
	}

	gDebugLog.Log("QuickTime: Preroll <%s>, nap <%d>, %s",
		inMoviePath.GetString(), inTenths, (inSync ? "sync" : "async"));
	
	return (DoPreroll(inMoviePath, inAudioOnly, false));
}

//
//	Play
//
LQTError LQuickTime::Play(TString &inMoviePath, TRect &inRect, int32 inWaitOffset, 
						  int32 inVolume, bool inAudioOnly)
{
	TimeValue	theStart, theDuration;
	bool		playIt = false;
	LQTError	retValue = LQT_NoFile;

	gDebugLog.Log("QuickTime: Play <%s>", inMoviePath.GetString());

	if (mPrerolled)
	{
		// see if the same movie is prerolled, if not start over
		if (not inMoviePath.Equal(mPath, false))
		{
			Cleanup();
			mPrerolled = false;		// need to preroll this one
		}

		// see if we have some more preloading to do
		if (mLoadStep < LQT_LOADED)
		{
			DoLoadStep(true);		// finish the rest of the preloading here

			if (mLoadStep != LQT_LOADED)	// had an error
			{
				gDebugLog.Log("QuickTime: error loading movie <%s>", mPath.GetString());
				gLog.Log("ERROR: Could not load movie <%s>", mPath.GetString());

				Kill();
				return (LQT_PreloadError);
			}
		}
	}
	else
		Cleanup();

	if (not mPrerolled)
	{
		gDebugLog.Log("QuickTime: not prerolled, preroll now");
		retValue = DoPreroll(inMoviePath, inAudioOnly, true);
	}
	else
		retValue = LQT_NoError;
	
	if (retValue == LQT_NoError)
	{					
		mWaitOffset = inWaitOffset;			// what video disk frame does the start of this movie correspond to?						
		mLooping = false;

		if (not mAudioOnly)
		{
			//::GetMovieBox(mMovie, &mBounds);
			::MacOffsetRect(&mBounds, -mBounds.left, -mBounds.top);
	
			if (not mHaveOrigin)
			{
				// center the movie in the view
				short		dh, dv;

				dh = VSCREEN_WIDTH - mBounds.right;
				dv = VSCREEN_HEIGHT - mBounds.bottom;
				::MacOffsetRect(&mBounds, dh/2, dv/2);
			}
			else
			{
				// use the stored origin
				::MacOffsetRect(&mBounds, mOrigin.h, mOrigin.v);
			}

			::SetMovieBox(mMovie, &mBounds);
		}
		
		if (mStreamed)
		{
			TimeValue	bufferTime = 0;
			OSErr		err;
		//	int32		numTimes;

			err = ::GetMaxLoadedTimeInMovie(mMovie, &bufferTime);

			int32	minBufferTime;

			if (mAudioOnly)
			{
				minBufferTime = gVariableManager.GetLong("_QuickTimeAudioPreload");
//				if (minBufferTime == 0)
//					minBufferTime = MINIMUM_AUDIO_LOAD_TIME;
//				else if (minBufferTime == -1)
//					minBufferTime = 0;
			}
			else
			{
				minBufferTime = gVariableManager.GetLong("_QuickTimeVideoPreload");
//				if (minBufferTime == 0)
//					minBufferTime = MINIMUM_VIDEO_LOAD_TIME;
//				else if (minBufferTime == -1)
//					minBufferTime = 0;
			}

			int32 minCycles;

			if (mAudioOnly)
			{
				minCycles = gVariableManager.GetLong("_QuickTimeAudioCycles");
//				if (minCycles == 0)
//					minCycles = MOVIE_TASK_TIMES_AUDIO_STREAM;
//				else if (minCycles == -1)
//					minCycles = 0;
			}
			else
			{
				minCycles = gVariableManager.GetLong("_QuickTimeVideoCycles");
//				if (minCycles == 0)
//					minCycles = MOVIE_TASK_TIMES_VIDEO_STREAM;
//				else if (minCycles == -1)
//					minCycles = 0;
			}

/*
			int32	startLoopTime = ::timeGetTime();
			int32	tmpTimer = (startLoopTime - mPrerollStateTime) / 100;
			mPrerollStateTime = 0;

			gDebugLog.Log("QuickTime: going to play, current loaded time <%d>, target load time <%d>, tenths preload time <%d>, cycles <%d>",
				bufferTime, minBufferTime, tmpTimer, minCycles);
*/

			if (bufferTime < minBufferTime)
			{
				int32 i = 0;
				while (bufferTime < minBufferTime)
				{
					// buffer some more
					::MoviesTask(mMovie, 0);
					i++;

					if ((i % 1000) == 0)
						::Sleep(1);

					err = ::GetMaxLoadedTimeInMovie(mMovie, &bufferTime);
					if (err != noErr)
					{
						bufferTime = minBufferTime;	// bug out of loop
						gDebugLog.Log("QuickTime: GetMaxLoadedTimeInMovie returned <%d>",
							err);
					}

					// check for endless looping
					if (i >= 10000)
					{
						gDebugLog.Log("QuickTime: endless loop waiting for movie to buffer");
						bufferTime = minBufferTime;
					}
				}
/*
				tmpTimer = (::timeGetTime() - startLoopTime) / 100;
				gDebugLog.Log("QuickTime: looped <%d> times, took <%d> tenths", i, tmpTimer);
*/
			}


			// now cycle that number of times
			for (int32 i = 0; i < minCycles; i++)
			{
				::MoviesTask(mMovie, 0);

				if ((i % 100) == 0)
					::Sleep(1);
			}
		}
		else
		{
			// cycle a bit to prevent burps on startup
			for (int32 i = 0; i < MOVIE_TASK_TIMES_LOCAL; i++)
			{
				::MoviesTask(mMovie, 0);

				if ((i % 100) == 0)
					::Sleep(10);
			}
		}

		SetVolume(inVolume);

		// get the end time for the movie
		mEndTime = ::GetMovieDuration(mMovie);
			
		// get the time scale for the movie
		mScale = ::GetMovieTimeScale(mMovie);
	
		// set the start and duration of the clip
		theStart = 0;
		theDuration = mEndTime - theStart;
		
		::GoToBeginningOfMovie(mMovie);	

		// set the movie's gworld
		::SetMovieGWorld (mMovie, gGrafPtr, nil);

		// preroll after GWorld is set
		::PrerollMovie(mMovie, 0, mRate);

		mPlaying = true;
		Resume();							// start playing
	}

	return (retValue);
}

//
//	PlayLoop - Play a movie by looping it.
//
LQTError LQuickTime::PlayLoop(TString &inMoviePath, int32 inVolume)
{
	TimeBase	theBase;					// time base for our movie
	bool		doThePreroll = false;		// for now as we only do audio
	bool		playIt = false;				// assume the worst
	LQTError	retValue = LQT_NoFile;
	
	Cleanup();								// don't worry about mHaveOrigin as this must be audio

	if (not mPrerolled)
		retValue = DoPreroll(inMoviePath, true, true);
	else
		retValue = LQT_NoError;
	
	if (retValue == LQT_NoError)
	{		
		theBase = ::GetMovieTimeBase(mMovie);	// get our time base
		::SetTimeBaseFlags(theBase, loopTimeBase);
		::SetMovieMasterTimeBase(mMovie, theBase, nil);
		
		mLooping = true;
				
		SetVolume(inVolume);

		Resume();						// start the movie playing
	}

	return (retValue);
}

//
//	Pause - Stop playing the movie. 
//
void LQuickTime::Pause(void)
{
	if (mMovie != NULL)
	{
		::StopMovie(mMovie);
			
		mPaused = true;
	}
}

//
//	Resume - Play the movie from wherever we left it.
//
void LQuickTime::Resume(void)
{
	if (mInMovie and mPaused)
	{
		::StartMovie(mMovie);
		mPaused = false;
		
		::MoviesTask(mMovie, 0);			// prime the pump
	}
}

//
//	AtEnd - Return true if the movie is at the end. Also return the time the movie
//			is currently at.
//
bool LQuickTime::AtEnd(TimeValue &inMovieTime)
{
	TimeValue	theTime = 0;
	bool		theRetValue = true;
	
	if (mMovie != NULL)
	{
		theTime = ::GetMovieTime(mMovie, nil);
		
		if (not ::IsMovieDone(mMovie))
			theRetValue = false;
	}
	
	inMovieTime = theTime;
	
	return (theRetValue);
}

pascal void PrePrerollCompleteProc (Movie theMovie, OSErr thePrerollErr, void *theRefcon)
{
	LQuickTime	*thisPtr = (LQuickTime *) theRefcon;

	if (thisPtr != NULL)
		thisPtr->SetPreloadEvent();
}

void LQuickTime::SetPreloadEvent(void)
{
	::SetEvent(mPreloadEvent);
}
	

//	
//	DoLoadStep - Perform a load step. If inFinish is true, stay here until
//		all the load steps are complete. 
//
void LQuickTime::DoLoadStep(bool inFinish /* = false */)
{
	if (mLoadStep == LQT_BEFORE_PRELOAD)
	{
		// waiting to see when we can start the preload
		::MoviesTask(mMovie, 0);

		// make sure we aren't just cycling here (I think this is a QuickTime bug)
		if (mPrerollCount > 100000)
		{
			gDebugLog.Log("QuickTime: preloading infinite loop?");

			// has to be an error
			if (mSync)
			{
				mSync = false;
				gCardManager.WakeUp();
			}

			mPrerollError = LQT_NoFile;
			mLoadStep = LQT_LOAD_ERROR;
			return;
		}

		bool	done = false;
		
		long	movieLoadState;
		while (not done)
		{
			::MoviesTask(mMovie, 0);

			if (not inFinish)
				done = true;		// stop after one loop

			movieLoadState = ::GetMovieLoadState(mMovie);

			if (movieLoadState == kMovieLoadStateError)
			{
				// can't load the movie
				mPrerollError = LQT_NoFile;
				mLoadStep = LQT_LOAD_ERROR;

				if (mSync)
				{
					mSync = false;
					gCardManager.WakeUp();
				}

				return;
			}

			if ((movieLoadState == kMovieLoadStatePlayable)
				or (movieLoadState == kMovieLoadStateComplete))
			{
				TimeValue	startTime;
				OSErr		err;
				
				// save the bounding rect for the movie and set
				// it off the screen to prevent flashing
				::GetMovieBox(mMovie, &mBounds);

				Rect	fakeBounds = {-1, -1, -1, -1};
				::SetMovieBox(mMovie, &fakeBounds);

				// see if we can preroll it
				mRate = ::GetMoviePreferredRate(mMovie);
				startTime = ::GetMovieTime(mMovie, NULL);

				// create the event we will wait on
				if (mPreloadEvent != NULL)
					CloseHandle(mPreloadEvent);

				mPreloadEvent = NULL;
				mPreloadEvent = ::CreateEvent(NULL, false, false, PREPREROLLEVENTNAME);
				if (mPreloadEvent == NULL)
					return;

				err = ::PrePrerollMovie(mMovie, startTime, mRate, PrePrerollCompleteProc, (void *) (this));

				// cbo_fix - check for ioInProgress
				if ((err != noErr) and (err != ioInProgress))
				{
					mLoadStep = LQT_LOAD_ERROR;

					if (mSync)
					{
						mSync = false;
						gCardManager.WakeUp();
					}

					return;
				}

				// change to the step that checks for PrePrerollMovie to finish
				mLoadStep = LQT_AFTER_PRELOAD;

				done = true;
			}
			// else - if we have to finish then we will loop and do it again
			//	if we don't have to finish then we want to leave the load state
			//	alone so we will do it again
		}
	}

	if (mLoadStep == LQT_AFTER_PRELOAD)
	{
		// waiting for the preload event
		DWORD	waitValue;
		DWORD	timeOut = 10;
		bool	waitForEvent = false;

		// If inFinish is true, wait here until the event is signaled.
		if (inFinish)
			waitForEvent = true;

		do
		{
			::MoviesTask(mMovie, 0);

			waitValue = ::WaitForSingleObject(mPreloadEvent, timeOut);

			if (waitValue == WAIT_OBJECT_0)
			{
				// The event has been signaled, we're done.
				::CloseHandle(mPreloadEvent);
				mPreloadEvent = NULL;

				mLoadStep = LQT_READY_TO_LOAD;
				waitForEvent = false;
			}

			if (waitForEvent)
			{
				long	movieLoadState;

				movieLoadState = ::GetMovieLoadState(mMovie);

				if (movieLoadState == kMovieLoadStateError)
				{
					mPrerollError = LQT_NoFile;
					mLoadStep = LQT_LOAD_ERROR;

					::CloseHandle(mPreloadEvent);
					mPreloadEvent = false;

					waitForEvent = false;
				}
			}
		} 
		while (waitForEvent);
	}

	if (mLoadStep == LQT_READY_TO_LOAD)
	{
		// now call the real preroll command

		// cbo_fix - try doing this after GWorld and Box are set correctly
		//::PrerollMovie(mMovie, 0, mRate);

		mLoadStep = LQT_MORE_LOAD_STREAM;
	}

	if (mLoadStep == LQT_MORE_LOAD_STREAM)
	{
		// this should really be a variable number of tries
		//	depending on the codec and other variables such as
		//	bandwidth
		::MoviesTask(mMovie, 0);

		// cbo_fix - there must be a better way to do this!!
//		if (mStreamed and inFinish)
//		{
//			for (int32 i = 0; i < MOVIE_TASK_TIMES; i++)
//			{
//				::MoviesTask(mMovie, 0);
//				mPrerollCount++;
//			}
//		}

		// done with preloading
		DWORD	endTime = ::timeGetTime();

		// see if we have to end a nap or anything like that
		if (mSync)
		{
			// wake up the card manager
			mSync = false;
			gDebugLog.Log("QuickTime: DoLoadStep, done loading, resume card execution");
			gCardManager.WakeUp();
		}

		// see if we need to set a nap now
		if (mNapTime > 0)
		{
			int32	timeNow = ::timeGetTime();
			
			if (mNapTime > timeNow)
			{
				// still haven't reached the nap time
				// have to wait some more
				int32 tenths = (mNapTime - timeNow) / 100;

				if (tenths >= 1)
				{
					gDebugLog.Log("QuickTime: DoLoadStep, done loading, napping for <%d> tenths",
						tenths);
					gCursorManager.CheckCursor();
					gView->Draw();
					gCardManager.Nap(tenths);
				}
			}
		}

		mLoadStep = LQT_LOADED;
	}
}

//
//	HandleEvent - Handle the event.
//
bool LQuickTime::HandleEvent(HWND /*inWind*/, UINT /*inMessage*/, 
							  WPARAM /*inWParam*/, LPARAM /*inLParam*/)
{
	return (false);
}

/*
 $Log$
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
