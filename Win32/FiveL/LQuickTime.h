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
// LQuickTime.h : 
//

#if !defined (_LQuickTime_h_)
#define _LQuickTime_h_

#include "TCommon.h"
#include "TString.h"
#include "TRect.h"
#include "TPoint.h"

//////////
// Quicktime error enumeration for media files
//
typedef enum
{
	LQT_NoError = 0,
	LQT_NoFile,
	LQT_Playing,
	LQT_PreloadError
} LQTError;

//////////
// Quicktime load state enumeration for media files
//
typedef enum
{
	LQT_NOT_LOADED = 0,
	LQT_LOAD_STARTED,
	LQT_BEFORE_PRELOAD,
	LQT_AFTER_PRELOAD,
	LQT_READY_TO_LOAD,
	LQT_MORE_LOAD_STREAM,
	LQT_LOADED,
	LQT_LOAD_ERROR
} LQTLoadState;

/*-----------------------------------------------------------------

CLASS
    LQuickTime

	A Quicktime movie class that uses the Quicktime API to play
	media clips (audio or video).  Media clips may be local or streamed
	using HTTP or RTSP.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class LQuickTime : public TObject
{
	public:
      	//////////
		// Constructor.
		//
		LQuickTime();
		
		//////////
		// Destructor.
		//
		~LQuickTime();
		
		//////////
		// Idle time processing, should be called regularly.  Performs load steps if 
		// the movie is being loaded.  Clears wait points once they are passed.  Pauses to
		// buffer streaming movies if needed.
		//
		void			Idle(void);

		//////////
		// Preload a video clip
		//
		// [in] inMoviePath - full path to the video clip
		// [in] inTenths - make sure preload takes at least this much time, 
		//				   nap if needed (in 1/10 seconds)
		// [in] inSync - if true, pauses the gCardManager and do the entire preroll now
		// [out] return - error code
		//
		LQTError		PrerollVideo(TString &inMoviePath, int32 inTenths, bool inSync);
		
		//////////
		// Preload an audio clip
		//
		// [in] inMoviePath - full path to the audio clip
		// [in] inTenths - make sure preload takes at least this much time, 
		//				   nap if needed (in 1/10 seconds)
		// [in] inSync - if true, pauses the gCardManager and do the entire preroll now
		// [out] return - error code
		//
		LQTError		PrerollAudio(TString &inMoviePath, int32 inTenths, bool inSync);
		
		//////////
		// Finish prerolling if necessary and play the specified video clip.
		//
		// [in] inMoviePath - full path to the video clip
		// [in] inRect - box where the movie should be played
		// [in] inOffset - start playback at this offset in the movie
		// [in] inVolume - desired volume level
		// [out] return - error code
		//
		LQTError		PlayVideo(TString &inMoviePath, TRect &inRect, int32 inOffset,
							int32 inVolume);
		
		//////////
		// Finish prerolling if necessary and play the specified audio clip.
		//
		// [in] inMoviePath - full path to the audio clip
		// [in] inRect - box where the movie should be played
		// [in] inOffset - start audio playback at this offset
		// [in] inVolume - desired volume level
		// [out] return - error code
		//
		LQTError		PlayAudio(TString &inMoviePath, int32 inVolume, int32 inOffset,
							bool inLoop);

		//////////
		// Stop playing the movie.
		//
		void			Pause(void);
		
		//////////
		// Resume play of the movie from wherever we paused it.
		//
		void			Resume(void);
		
		//////////
		// We are really done with the movie, toss everything and wakeup 
		// the gCardManager.
		//
		void			Kill(void);

		//////////
		// Set the origin for the NEXT movie to play.
		//
		// [in] inPoint - a TPoint specifying the new origin
		//
		void			SetOrigin(TPoint &inPoint);
		
		//////////
		// Are we at the end of the movie?
		//
		// [in/out] inMovieTime - filled with the time the movie is currently at
		// [out] return - true if the movie has finished playback, false otherwise
		//
		bool			AtEnd(TimeValue &inMovieTime);
		
		//////////
		// Set the volume to the specified level.
		//
		// [in] inVolume - new volume level
		//
		void			SetVolume(int32 inVolume);

		//////////
		// Get the rectangular movie region.
		//
		// [out] return - a TRect for the movie region
		//
		TRect			GetBounds(void);
		
		//////////
		// Is there a movie (audio or video) playing?
		//
		// [out] return - true if a movie is playing, false otherwise
		//
		bool			Playing(void) { return (mInMovie); }
		
		//////////
		// Is the current movie prerolled?
		//
		// [out] return - true if the current movie is prerolled, false otherwise
		//
		bool			Prerolled(void) { return (mPrerolled); }
		
		//////////
		// Is there a video clip playing?
		//
		// [out] return - true if there is a video clip playing, false otherwise
		//
		bool			MoviePlaying(void) { return (mInMovie and (not mAudioOnly)); }
		
		//////////
		// Is there an audio clip playing?
		//
		// [out] return - true if there is an audio clip playing, false otherwise
		//
		bool			AudioPlaying(void) { return (mInMovie and mAudioOnly); }
		
		//////////
		// Is there a paused movie?
		//
		// [out] return - true if there is a movie in the paused state, false otherwise
		//
		bool			Paused(void) { return (mInMovie and mPaused); }
		
		//////////
		// Is there a full screen video clip playing?
		//
		// [out] return - true if there is a full screen video clip that is currently playing,
		//				false otherwise
		//
		bool			FullScreen(void) { return ((MoviePlaying()) and (not mHaveOrigin)); }
		
		//////////
		// Is there a looping audio clip playing?
		//
		// [out] return - true if there is an audio clip looping, false otherwise
		//
		bool			AudioLooping(void) { return ((AudioPlaying()) and (mLooping)); }
		
		//////////
		// Has the origin been set?
		//
		// [out] return - true if the origin has been set, false otherwise
		//
		bool			HaveOrigin(void) { return (mHaveOrigin); }
		
		//////////
		// Set a wait point at this frame. We have to be playing a movie or 
		// audio track for this to make any sense.
		//
		// [in] inFrame - the frame where the wait point should be placed
		//
		void			SetWaitPoint(int32 inFrame);
		
		//////////
		// Are we at a wait point?
		//
		// [out] return - true if we are at a wait point, false otherwise
		//
		bool			AtWaitPoint(void) { return ((Playing()) and (not mWaiting)); } // not mWaiting???

		//////////
		// Draw the pane's frame and contents.
		//
		void			DrawSelf(void);
		
		//////////
		// Set a Win32 OS signal for the preload event. 
		//
		void			SetPreloadEvent(void);

		//////////
		// NOT IN USE - returns false
		//
		bool			HandleEvent(HWND inWind, UINT inMessage, WPARAM inWParam, LPARAM inLParam);
		
	protected:
		//////////
		// Do we have a movie?
		//
		bool			mInMovie;
		
		//////////
		// Is the movie paused?
		//
		bool			mPaused;
		
		//////////
		// Has the movie been preloaded?
		//
		bool			mPrerolled;
		
		//////////
		// Is the movie being streamed?
		//
		bool			mStreamed;
		
		//////////
		// Preroll step count.
		//
		int32			mPrerollCount;
		
		//////////
		// Loading state.
		//
		LQTLoadState	mLoadStep;
		
		//////////
		// Preload Win32 event handle.
		//
		HANDLE			mPreloadEvent;
		
		//////////
		// Are we playing audio only?
		//
		bool			mAudioOnly;
		
		//////////
		// Has the origin been set?
		//
		bool			mHaveOrigin;
		
		//////////
		// Do we have a palette for the movie?
		//
		bool			mHavePal;
		
		//////////
		// Should we loop playback?
		//
		bool			mLooping;
		
		//////////
		// Is the movie playing?
		//
		bool			mPlaying;

		//////////
		// Synchronous preroll?
		//
		bool			mSync;
		
		//////////
		// Nap time for preroll.
		//
		int32			mNapTime;

		//////////
		// ???
		//
#ifdef DEBUG
		int32			mPrerollStateTime;
#endif

		//////////
		// Top-left point where movie should reside on screen.
		//
		Point			mOrigin;
		
		//////////
		// Rectangular region for movie box.
		//
		Rect			mBounds;
		
		//////////
		// Time to wait if a wait point is set.
		//
		TimeValue		mWaitTime;
		
		//////////
		// Wait offset.
		//
		int32			mWaitOffset;
		
		//////////
		// Is there a wait point set?
		//
		bool			mWaiting;

		//////////
		// Current volume.
		//
		int32			mCurrentVolume;
		
		//////////
		// Preroll error code.
		//
		LQTError		mPrerollError;

		//////////
		// Quicktime movie handle.
		//
		Movie			mMovie;
		
		//////////
		// Full path to the movie.
		//
		TString			mPath;
		
		//////////
		// Quicktime structure for full movie path. 
		//
		FSSpec			mSpec;
		
		//////////
		// Movie playback rate.
		//
		Fixed			mRate;
		
		//////////
		// End time for the movie
		//
		TimeValue		mEndTime;
		
		//////////
		// Time scale for the movie
		//
		TimeScale		mScale;

		//////////
		// Preload the movie.
		//
		// [in] inMoviePath - full path to the movie file
		// [in] inAudioOnly - audio only?
		// [in] inTenths - make sure preload takes at least this much time, 
		//				   nap if needed (in 1/10 seconds)
		// [in] inSync - if true, pauses the gCardManager and do the entire preroll now
		// [out] return - error code
		//
		LQTError		Preroll(TString &inMoviePath, bool inAudioOnly,
							int32 inTenths, bool inSync);
		
		//////////
		// Finish prerolling if necessary and play the movie.
		//
		// [in] inMoviePath - full path to the movie file
		// [in] inRect - box where the movie should be played
		// [in] inWaitOffset - start playback at this offset in the movie
		// [in] inVolume - desired volume level
		// [in] inAudioOnly - audio only?
		// [out] return - error code
		//
		LQTError		Play(TString &inMoviePath, TRect &inRect, int32 inWaitOffset, 
							int32 inVolume, bool inAudioOnly);
		
		//////////
		// Loop playback of the movie.
		//
		// [in] inMoviePath - full path to the movie file
		// [in] inVolume - desired volume level
		// [out] return - error code
		//
		LQTError		PlayLoop(TString &inMoviePath, int32 inVolume);

		//////////
		// Load the movie so it is ready for playback.
		//
		// [in] inMoviePath - full path to the movie file
		// [in] inAudioOnly - audio only?
		// [in] inFinish - finish all loading now?
		//
		bool			Load(TString &inMoviePath, bool inAudioOnly, bool inFinish);
		
		//////////
		// Perform a load step.
		//
		// [in_optional] inFinish - if true, stay here until all the load steps
		//							are complete (default false). 
		//
		void			DoLoadStep(bool inFinish = false);
		
		//////////
		// Preload the movie.
		//
		// [in] inMoviePath - full path to the movie file
		// [in] inAudioOnly - audio only?
		// [in_optional] inFinish - if true, do the entire preroll now (default false)
		// [out] return - error code
		//
		LQTError		DoPreroll(TString &inMoviePath, bool inAudioOnly, 
					              bool inFinish = false);
		
		//////////
		// Cleanup our mess.
		//
		void			Cleanup(void);

};

#endif // _LQuickTime_h_

/*
 $Log$
 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.5  2000/08/08 19:03:40  chuck
 no message

 Revision 1.4  2000/05/11 12:54:54  chuck
 v 2.01 b2

 Revision 1.3  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
