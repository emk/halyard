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
// LQuickTime.h : 
//

#if !defined (_LQuickTime_h_)
#define _LQuickTime_h_

#include "TCommon.h"
#include "TString.h"
#include "TRect.h"
#include "TPoint.h"
#include "TQTMovie.h"

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


/*-----------------------------------------------------------------

CLASS
    LQuickTime

	A Quicktime movie class that uses the Quicktime API to play
	media clips (audio or video).  Media clips may be local or streamed
	using HTTP or RTSP.

    This class once contained a full-fledged QuickTime preloading
	and playback system, but it has been gutted and turned into
    a thin wrapper over the more reliable and featureful TQTMovie
    class.

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
	// Idle time processing, should be called regularly.  Performs load
	// steps if the movie is being loaded.  Clears wait points once
	// they are passed.  Pauses to buffer streaming movies if needed.
	//
	void			Idle(void);
	
	//////////
	// Preload a video clip
	//
	// [in] inMoviePath - full path to the video clip
	// [in] inSync - if true, pauses the gCardManager and do the entire
	//               preload now
	// [out] return - error code
	//
	LQTError		PreloadVideo(TString &inMoviePath, bool inSync);
		
	//////////
	// Preload an audio clip
	//
	// [in] inMoviePath - full path to the audio clip
	// [in] inSync - if true, pauses the gCardManager and do the entire
    //               preload now
	// [out] return - error code
	//
	LQTError		PreloadAudio(TString &inMoviePath, bool inSync);
	
	//////////
	// Finish preloading if necessary and play the specified video clip.
	//
	// [in] inMoviePath - full path to the video clip
	// [in] inRect - box where the movie should be played
	// [in] inVolume - desired volume level
	// [out] return - error code
	//
	LQTError		PlayVideo(TString &inMoviePath, TRect &inRect,
							  int32 inVolume);
	
	//////////
	// Finish preloading if necessary and play the specified audio clip.
	//
	// [in] inMoviePath - full path to the audio clip
	// [in] inRect - box where the movie should be played
	// [in] inVolume - desired volume level
	// [out] return - error code
	//
	LQTError		PlayAudio(TString &inMoviePath, int32 inVolume,
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
	bool			Playing(void) { return mMovie ? true : false; }
	
	//////////
	// Is the current movie preloaded?
	//
	// [out] return - true if the current movie is preloaded, false otherwise
	//
	bool			Preloaded(void) { return (mPreloaded); }
	
	//////////
	// Is there a video clip playing?
	//
	// [out] return - true if there is a video clip playing, false otherwise
	//
	bool			MoviePlaying(void) { return (mMovie && !mAudioOnly); }
	
	//////////
	// Is there an audio clip playing?
	//
	// [out] return - true if there is an audio clip playing, false otherwise
	//
	bool			AudioPlaying(void) { return (mMovie && mAudioOnly); }
	
	//////////
	// Is there a paused movie?
	//
	// [out] return - true if there is a movie in the paused state,
	//                false otherwise
	//
	bool			Paused(void)
		{ return mMovie && mMovie->IsPaused(); }
	
	//////////
	// Is there a full screen video clip playing?
	//
	// [out] return - true if there is a full screen video clip that is
	//                currently playing, false otherwise
	//
	bool			FullScreen(void)
		{ return ((MoviePlaying()) and (not mHaveOrigin)); }
	
	//////////
	// Is there a looping audio clip playing?
	//
	// [out] return - true if there is an audio clip looping, false otherwise
	//
	bool			AudioLooping(void)
		{ return ((AudioPlaying()) and (mLooping)); }
	
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
	bool			AtWaitPoint(void)
		{ return ((Playing()) and (not mWaiting)); } // not mWaiting???
	
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
	bool			HandleEvent(HWND inWind, UINT inMessage,
								WPARAM inWParam, LPARAM inLParam);
	
private:
	//////////
	// Our associated movie.  May be NULL.
	//
	TQTMovie		*mMovie;

	//////////
	// The path to our movie.
	//
	TString			mPath;

	//////////
	// Has the movie been preloaded?
	//
	bool			mPreloaded;
	
	//////////
	// Is the movie being streamed?
	//
	bool			mStreamed;
	
	//////////
	// Are we playing audio only?
	//
	bool			mAudioOnly;
	
	//////////
	// Has the origin been set?
	//
	bool			mHaveOrigin;
	
	//////////
	// Should we loop playback?
	//
	bool			mLooping;
	
	//////////
	// Top-left point where movie should reside on screen.
	//
	Point			mOrigin;
	
	//////////
	// Rectangular region for movie box.
	//
	Rect			mBounds;
	
	//////////
	// When playing video, we fake synchronous calls by pausing the
	// interpreter and unpausing when we're ready.
	//
	bool			mSync;

	//////////
	// Time to wait if a wait point is set.
	//
	TimeValue		mWaitTime;
	
	//////////
	// Is there a wait point set?
	//
	bool			mWaiting;
	
	//////////
	// Current volume.
	//
	int32			mCurrentVolume;
	
	//////////
	// Preload the movie.
	//
	// [in] inMoviePath - full path to the movie file
	// [in] inAudioOnly - audio only?
	// [in] inSync - if true, pauses the gCardManager and do the entire
	//               preload now
	// [out] return - error code
	//
	LQTError		Preload(TString &inMoviePath, bool inAudioOnly,
							bool inSync);
	
	//////////
	// Finish preloading if necessary and play the movie.
	//
	// [in] inMoviePath - full path to the movie file
	// [in] inRect - box where the movie should be played
	// [in] inVolume - desired volume level
	// [in] inAudioOnly - audio only?
	// [out] return - error code
	//
	LQTError		Play(TString &inMoviePath, TRect &inRect,
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
	bool			Load(TString &inMoviePath, bool inAudioOnly,
						 bool inFinish);
	
	//////////
	// Preload the movie.
	//
	// [in] inMoviePath - full path to the movie file
	// [in] inAudioOnly - audio only?
	// [in_optional] inFinish - if true, do the entire preload now
	//                          (default false)
	// [out] return - error code
	//
	LQTError		DoPreload(TString &inMoviePath, bool inAudioOnly, 
							  bool inFinish = false);
	
	//////////
	// Cleanup our mess.
	//
	void			Cleanup(void);	
};

#endif // _LQuickTime_h_

/*
 $Log$
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

 Revision 1.5  2000/08/08 19:03:40  chuck
 no message

 Revision 1.4  2000/05/11 12:54:54  chuck
 v 2.01 b2

 Revision 1.3  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
