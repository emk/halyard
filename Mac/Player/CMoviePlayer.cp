//
// CMoviePlayer.cp
//

#include "debug.h"

#include <string.h>

#include "Mac5L.h"
#include "CMac5LApp.h"
#include "CConfig.h"
#include "CPalette.h"
#include "CPlayerView.h"
#include "CMoviePlayer.h"
#include "CVariable.h"
#include "CCard.h"
#include "gamma.h"

#ifdef DEBUG_5L
#include "CModule.h"
#endif


//
//	CMoviePlayer - the construct-from-stream constructor
//
CMoviePlayer::CMoviePlayer(void)
{
	mInMovie = false;
	mPrerolled = false;
	mPaused = true;
	mAudioOnly = false;
	mHaveOrigin = false;
	mHavePal = false;
	mLooping = false;
	
	mWaitTime = 0;
	mWaitOffset = 0;
	mLoopFadeTime = 0;
	
	mMovie = NULL;

	mRate = ::Long2Fix(0x00010000);
	mEndTime = 0;
	mScale = 0;
	
	::EnterMovies();
}

//
//	~CMoviePlayer - 
//
CMoviePlayer::~CMoviePlayer()
{
	::ExitMovies();
}

//
//	SpendTime
//
void CMoviePlayer::SpendTime(const EventRecord & /* inMacEvent */)
{
	TimeValue	movieTime = 0;
	
	if (mInMovie and (not mPaused))
	{
		::MoviesTask(mMovie, 0);
		
		// if we are looping don't check for the end
		if (not mLooping)
		{
			if (not AtEnd(movieTime))
			{
				// if we aren't at the end, see if the card is waiting
				if (mWakeCard and (mWaitTime > 0))
				{
					if (movieTime >= mWaitTime)
					{
						gCardManager.CurCardWakeUp();
						mWakeCard = false;
						mWaitTime = 0;
					}
				}
				
				::MoviesTask(mMovie, 0);
			}
			else
			{
				Kill();				// stop and clean everything up
			}
		}
	}
}

//
//	WakeCard - Wake up the card manager at this frame. We have to be playing a 
//				movie or audio track for this to make any sense. 
//
void CMoviePlayer::WakeCard(int32 inFrame)
{
	if (mInMovie)
	{
		mWakeCard = true;
		
		if (inFrame <= 0)	// 0 is the end, negative means back from the end
			mWaitTime = mEndTime + (int32) (((float) inFrame * mScale) / (float) 30);
		else
			mWaitTime = (inFrame - mWaitOffset) * mScale / 30;
	}	
#ifdef DEBUG_5L
	//else
	//	prinfo("wait called but no movie playing");
#endif
}


//
//	DrawSelf - draw the pane's frame and contents
//
void CMoviePlayer::DrawSelf(void)
{
	if ((mInMovie) and (not mAudioOnly))
		::UpdateMovie(mMovie);
}

//
//	SetOrigin - Set the origin for the NEXT movie to play.
//
void CMoviePlayer::SetOrigin(Point inPoint)
{
	if (inPoint.h == -1)
		mHaveOrigin = false;
	else
		mHaveOrigin = true;
		
	mOrigin = inPoint;
}

//
//	Kill - We are really done with the movie, toss everything and
//				tell the PlayerView to bring the CardView up again.
//
void CMoviePlayer::Kill(void)
{
	RGBColor	rgbBlack = {0, 0, 0};
	Rect		theFrame;
	bool		audioPlaying = mAudioOnly;
	bool		fullScreen = (not mHaveOrigin);

	if (not Playing())
		return;
		
	gPlayerView->DoResetPause();				// let player view know the movie is dead
			
	if (mWakeCard)
	{
		gCardManager.CurCardWakeUp();
		mWakeCard = false;
	}

	gVariableManager.SetString("_lpstat", "0");
	gVariableManager.SetString("_lpactive", "0");
	gVariableManager.SetString("_movieplaying", "0");
	
	Pause();			// stop the movie, if these is one playing

	if (not audioPlaying)
	{
		// we do the fade out and back in so that we can restore the palette
		//	without having the screen flash in some weird colors
		if (fullScreen)
			DoGFade(false, 0, false);				// fade the screen out immediately

		gPlayerView->CalcLocalFrameRect(theFrame);
		
		if (mHavePal)
			gTheApp->RestorePalette();		// go back to the graphics palette
		
		gPlayerView->AdjustMyCursor();		// see if we want the cursor back
		
		if (fullScreen)						// insurance against palette weirdness
		{
			::RGBForeColor(&rgbBlack);
			::PenMode(patCopy);
			::PaintRect(&theFrame);
			::ValidRect(&theFrame);			// so don't get update
		}
		else
			gPlayerView->Draw(nil);			// get the offscreen buffer back on the screen
		
		gPlayerView->ProcessTZones(true);	// turn touch zone processing back on 
	}

	mHaveOrigin = false;					// no more origin for the movie	
	Cleanup();								// clean everything up
		
	if ((not audioPlaying) and (fullScreen))
		DoGFade(true, 0, false);			// fade the screen back in immediately
}

//
//	Cleanup
//
void CMoviePlayer::Cleanup(void)
{
	// before anything else, see if we need to stop the movie
	if (not mPaused)
		Pause();

	mPrerolled = false;
	mInMovie = false;
	mWakeCard = false;
	mAudioOnly = false;
	mHavePal = false;
	mLooping = false;
	
	mWaitTime = 0;
	mWaitOffset = 0;
	mLoopFadeTime = 0;
	
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
//bool CMoviePlayer::Load(FSSpec *theSpec, bool inAudioOnly)
bool CMoviePlayer::Load(const char *inMovieName, bool inAudioOnly)
{
	Rect		theBounds;
	Rect		theFrame;
	TimeValue	theStart, theDuration;
	OSErr		err;
	int32		diff;
	int32		theStartTime;
	int16		dh, dv;
	int16		actualResID = DoTheRightThing;
	int16		theRefNum;
	uint8		wasChanged;
	bool		retValue = true;
	
	// look in the local movie directory first
	theConfig->FillMovieSpec(&mSpec, inMovieName, true);
	
	err = ::OpenMovieFile(&mSpec, &theRefNum, fsRdPerm);

	if (err != noErr)
	{
		// then check the one on the CD
		theConfig->FillMovieSpec(&mSpec, inMovieName, false);
		
		err = ::OpenMovieFile(&mSpec, &theRefNum, fsRdPerm);
		
		if (err != noErr)
		{
#ifdef DEBUG_5L
			prinfo("OpenMovieFile: returned <%d>", err);
#else
			prcaution("Could not open Movie <%s>", inMovieName);
#endif
			return (false);
		}
	}
	
	err = ::NewMovieFromFile(&mMovie, theRefNum, &actualResID, nil, 0, &wasChanged);

	if (err != noErr)
	{
#ifdef DEBUG_5L
		prinfo("NewMovieFromFile: returned <%d>", err);
#else
		prcaution("Could not create Movie <%s>", inMovieName);
#endif
		retValue = false;
	}
				
	err = ::CloseMovieFile(theRefNum);

	if (retValue)
	{
		if (not inAudioOnly)
		{	
			::GetMovieBox(mMovie, &theBounds);
			::OffsetRect(&theBounds, -theBounds.left, -theBounds.top);
	
			if (not mHaveOrigin)
			{
				// center the movie in the players view
				gPlayerView->CalcLocalFrameRect(theFrame);
				diff = ::DeltaPoint(botRight(theFrame), botRight(theBounds));
				dh = LoWord(diff);
				dv = HiWord(diff);
				::OffsetRect(&theBounds, dh/2, dv/2);
			}
			else
			{
				// use the stored origin
				::OffsetRect(&theBounds, mOrigin.h, mOrigin.v);
			}
			
			::SetMovieBox(mMovie, &theBounds);
		}
		
		// get the end time for the movie
		mEndTime = ::GetMovieDuration(mMovie);
			
		// get the time scale for the movie
		mScale = ::GetMovieTimeScale(mMovie);
	
		// set the start and duration of the clip
		theStart = mStartOffset;
		theDuration = mEndTime - theStart;
		
		// we don't support anything but starting at the start for now
		if (theStart == 0)
			::GoToBeginningOfMovie(mMovie);	
		else
		{
			theStartTime = mStartOffset * mScale / 30;
			::SetMovieTimeValue(mMovie, theStartTime);
		}		
			
		mInMovie = true;			// we have a movie
		mAudioOnly = inAudioOnly;
		mPaused = true;				// and it isn't playing yet
	}
	
	return (retValue);
}

bool CMoviePlayer::DoPreroll(const char *inMovieName, bool inAudioOnly, bool inPreroll)
{
	bool	playIt = false;

#ifdef DEBUG_5L
	if (gModMan->NoVolume())
		return (false);
#endif
	
	//theConfig->FillMovieSpec(&mSpec, inMovieName);
	playIt = true;
	
	if (playIt)
	{
		if (Load(inMovieName, inAudioOnly))
		//if (Load(&mSpec, inAudioOnly))
		{
			if (inPreroll)
			{
				::PrerollMovie(mMovie, 0, mRate);
				mPrerolled = true;
			}
		}
		else
			playIt = false;
	}
	
	return (playIt);
}		

//
//	Preroll
//
void CMoviePlayer::Preroll(const char *inMovieName, bool inAudioOnly)
{
	Cleanup();
	
	(void) DoPreroll(inMovieName, inAudioOnly, true);
}

//
//	Play
//
void CMoviePlayer::Play(const char *inMovieName, int32 inWaitOffset, 
	bool inAudioOnly, const char *inPalStr, int32 inStartOffset)
{
	CPalette	*thePalette = nil;
	bool		doThePreroll;
	bool		playIt = false;

#ifdef DEBUG_5L
	if (gModMan->NoVolume())		// if no volume, don't even look for the movie
		return;
#endif

	Cleanup();

	if (not inAudioOnly)
		doThePreroll = true;
	else
	{
		doThePreroll = false;
		mStartOffset = inStartOffset;		// where to start in movie (only audio can have this)
	}

	if (not inAudioOnly)
	{
		gPlayerView->ProcessEvents(true);	// make sure keybinding is on
		gPlayerView->ProcessTZones(false);	// no touchzone processing during video
		
		if (not mHaveOrigin)				// if we are centering, fade out
			DoGFade(false, 5, false);		// fade the screen out in half a second
			
		if (inPalStr != NULL)				// if we have a palette, find it and set it
		{
			thePalette = GetPalette(inPalStr);
			
			if (thePalette != nil)
				thePalette->SetPalette(false);			
			//UpdatePalette(inPalStr, false);
		}
	}
	else
		gPlayerView->ProcessEvents(true);
			
	if (not mPrerolled)
		playIt = DoPreroll(inMovieName, inAudioOnly, doThePreroll);
	else
		playIt = true;						// better be the same movie as the one we prerolled
	
	if (playIt)
	{		
		if (inPalStr != NULL)
			mHavePal = true;				// DoPreroll sets this to false
			
		mWaitOffset = inWaitOffset;			// what video disk frame does the start of this movie correspond to?
		
		if ((not inAudioOnly) and (not mHaveOrigin))	// check for audio or not full screen
		{
			gPlayerView->ColorCard(0);		// wipe out everything in offscreen -> why do we do this??
			gPlayerView->Draw(nil);			// put up the black background
		}
		else								// when audio only or not full screen, make sure the screen is up to date
			gPlayerView->Draw(nil);			// blast the gworld to the screen
		
		if ((not inAudioOnly) and (not mHaveOrigin))	// if we are centering, fade back in
			DoGFade(true, 0, false);				// fade the screen back in fast as all movies start with a fade
		
		mLooping = false;
		
		gPlayerView->DoResetPause();		// tell player view
				
		Resume();							// start playing
		
		gVariableManager.SetString("_movieplaying", "1");
	}
	else
	{
		if (not inAudioOnly)
		{
			DoGFade(true, 0, false);
		
			if (inPalStr != NULL)
				gTheApp->RestorePalette();
		}
	}
}

//
//	PlayLoop - Play a movie by looping it.
//
void CMoviePlayer::PlayLoop(const char *inMovieName, int32 inFadeTime)
{
	TimeBase	theBase;					// time base for our movie
	bool		doThePreroll = false;		// for now as we only do audio
	bool		playIt = false;				// assume the worst
	
#ifdef DEBUG_5L
	if (gModMan->NoVolume())				// if no volume, don't even look for the movie
		return;
#endif

	Cleanup();								// don't worry about mHaveOrigin as this must be audio

	gPlayerView->ProcessEvents(true);		// event processing on 
	
	playIt = DoPreroll(inMovieName, true, false);	// load the movie in
	
	if (playIt)
	{
		gPlayerView->Draw(nil);				// make sure the screen is up to date
		
		// do looping here - set variables too
		
		theBase = ::GetMovieTimeBase(mMovie);	// get our time base
		::SetTimeBaseFlags(theBase, loopTimeBase);
		::SetMovieMasterTimeBase(mMovie, theBase, nil);
		
		mLooping = true;
		mLoopFadeTime = inFadeTime;
		
		// here fade up if we are fading up
		if (inFadeTime > 0)
		{
			float		fadeStep;
			float		fadeVol = 0.0;
			int32		theTick;
			int32		theIterations = inFadeTime * 6;	// convert to ticks
			int32		i;
			int16		theVol = 0x0000;

#ifdef DEBUG_5L
	//		prinfo("fade in to the looping audio");
#endif
			
			gVariableManager.SetString("_lpstat", "1");
			gVariableManager.SetString("_lpactive", "1");
			
			theVol = 0x0000;						// volume off
			::SetMovieVolume(mMovie, theVol);
			
			fadeStep = (float) 0x0100/theIterations;		// full volume cut into chunks
				
			::StartMovie(mMovie);
			mPaused = false;
			
			for (i = 0; i <= theIterations; i++)
			{
				theTick = ::TickCount() + 1;

				if (fadeVol < 0x0100)
					fadeVol += fadeStep;
				
				::SetMovieVolume(mMovie, (int16) fadeVol);				
					
				::MoviesTask(mMovie, 0);
				
				if (i <= theIterations)
				{
					while (theTick > ::TickCount())
						::MoviesTask(mMovie, 0);
				}	
			}
			
			theVol = 0x0100;				// make sure we end up at full volume
			::SetMovieVolume(mMovie, theVol);
		}
		else
			Resume();						// start the movie playing
	}
}

//
//	Pause - Stop playing the movie. 
//
void CMoviePlayer::Pause(void)
{
	if (mMovie != NULL)
	{
		gVariableManager.SetString("_lpstat", "0");
		gVariableManager.SetString("_lpactive", "0");

#ifdef DEBUG_5L
	//	prinfo("REALLY pausing the movie");
#endif

		if (mLooping)
			FadeLoop();
		
		::StopMovie(mMovie);
			
		mPaused = true;
	}
}

//
//	Resume - Play the movie from wherever we left it.
//
void CMoviePlayer::Resume(void)
{
	if (mInMovie and mPaused)
	{
		gVariableManager.SetString("_lpstat", "1");
		gVariableManager.SetString("_lpactive", "1");

		::StartMovie(mMovie);
		mPaused = false;
		
		::MoviesTask(mMovie, 0);			// prime the pump
	}
}

//
//	AtEnd - Return true if the movie is at the end. Also return the time the movie
//			is currently at.
//
bool CMoviePlayer::AtEnd(TimeValue &inMovieTime)
{
	TimeValue	theTime = 0;
	Boolean		theRetValue = true;
	
	if (mMovie != NULL)
	{
		theTime = ::GetMovieTime(mMovie, nil);
		
		theRetValue = ::IsMovieDone(mMovie);
	}
#ifdef DEBUG_5L
	//else
	//	prerror("in AtEnd() - no movie");
#endif
	
	inMovieTime = theTime;
	
	return (theRetValue);
}

//
//	FadeLoop
//
void CMoviePlayer::FadeLoop(void)
{
	float		fadeStep;
	float		fadeVol = 0x0100;
	int32		theTick;
	int32		theIterations = mLoopFadeTime * 6;	// convert to ticks
	int32		i;
	int16		theVolume;
	
	if ((mMovie != NULL) and (mLooping))
	{
#ifdef DEBUG_5L
	//	prinfo("fade out of the looping audio");
#endif

		theVolume = 0x0100;							// full volume
		fadeVol = theVolume;
		fadeStep = (float) 0x0100/theIterations;	// full volume cut into equal chunks
		
		for (i = 0; i < theIterations; i++)
		{
			theTick = ::TickCount() + 1;

			if (fadeVol > 0x0000)
				fadeVol -= fadeStep;
				
			::SetMovieVolume(mMovie, (int16) fadeVol);
			
			::MoviesTask(mMovie, 0);
			
			if (i < theIterations)
			{
				while (theTick > ::TickCount())
					::MoviesTask(mMovie, 0);
			}
		}
			
		theVolume = 0x0000;							// make sure we end up at no volume
		::SetMovieVolume(mMovie, theVolume);
	}
}
		
	
	