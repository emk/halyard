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
// Video.h : 
//

#if !defined (_Video_h_)
#define _Video_h_

#include "TCommon.h"
#include "TObject.h"
#include "TString.h"
#include "TRect.h"
#include "TPoint.h"

#include "Card.h"
#include "LQuickTime.h"
//#include "LQuickTime2.h"

/*-----------------------------------------------------------------

CLASS
    Video

	A Video clip.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class Video : public TObject
{
	public:
		//////////
		// Constructor.
		//
		// [in] inName - name of the video clip (e.g. clip1.mov)
		//
		Video(TString &inName);

		//////////
		// Destructor.
		//
		~Video();
		
		//////////
		// Idle time processing.  Should be called regularly while this clip is active. 
		//
		void			Idle(void);

		//////////
		// Play the video clip.
		//
		// [in_optional] inVolume - desired volume level (default 100)
		// [out] return - true if playback was successfully started, false otherwise
		//
		bool			Play(int32 inVolume = 100);
		
		//////////
		// Preload the video clip
		//
		// [in] inSync - if true, pauses the gCardManager and do the entire preload now
		// [out] return - true if the clip was successfully preloaded, false otherwise
		//
		bool			Preload(bool inSync);

		//////////
		// Set an alarm to go off at the given frame of the currently playing movie.
		//
		// [in] inWaitFrame - frame where the alarm should be placed
		//
		void			Wait(int32 inWaitFrame);
		
		//////////
		// Kill the current Quicktime movie.
		//
		void			Kill(void);
		
		//////////
		// Pause the movie
		//
		// [in] inTenths - resume after this amount of time (in 1/10 seconds)
		//
		void			Pause(int32 inTenths);
		
		//////////
		// Resume the movie after a Pause().
		//
		void			Resume(void);
		
		//////////
		// Draw the movie pane.  Should be called when a screen redraw is needed. 
		//
		void			Draw(void);

		//////////
		// Get the rectangular movie region.
		//
		// [out] return - a TRect for the movie region
		//
		TRect			GetBounds(void);

		//////////
		// Set the origin for the movie.  Should be called before playback.
		//
		// [in] inPoint - a TPoint specifying the new origin
		//
		void			SetOrigin(TPoint &inPoint) { m_QT.SetOrigin(inPoint); }

		//////////
		// Is there a video clip playing?
		//
		// [out] return - true if a video clip playing, false otherwise
		//
		bool			Playing(void) { return(m_QT.Playing()); }

		//////////
		// Is this movie preloaded?
		//
		// [out] return - true if the current movie is preloaded, false otherwise
		//
		bool			Preloaded(void)	{ return (m_QT.Preloaded()); }

		//////////
		// Is the movie playing fullscreen?
		//
		// [out] return - true if there is a full screen movie playing, false otherwise
		//
		bool			FullScreen(void) { return(m_QT.Playing() and m_QT.FullScreen()); }

		//////////
		// Is the movie paused?
		//
		// [out] return - true if the movie is paused, false otherwise
		//
		bool			Paused(void) { return (m_QT.Playing() and m_QT.Paused()); }

		//////////
		// Has the origin been set?
		//
		// [out] return - true if the origin has been set, false otherwise
		//
        bool			HaveOrigin(void) { return (m_QT.HaveOrigin()); }

		//////////
		// Get the name of the video clip
		//
		// [out] return - the name of the video clip
		//
		const char		*Name(void) { return (m_Name.GetString()); }

		//////////
		// NOT IN USE - returns false
		//
		bool			HandleEvent(HWND inWind, UINT inMessage, WPARAM inWParam, LPARAM inLParam);

	private:
		//////////
		// How many steps should we pause?
		// 
		int32			m_PauseSteps;
		
		//////////
		// Should we wake the card manager when the video is done?
		//
		bool			m_WakeCard;

		//////////
		// Name of the video clip.
		//
		TString			m_Name;
		
		//////////
		// Our quicktime movie.
		//
		LQuickTime		m_QT;

		//////////
		// Convert the 0 - 100 volume setting into 0 - 256.
		//
		// [in] inVolume - the volume (0 to 100)
		// [out] return - the volume in 0 to 256 range.
		//
		int32			VolumeXForm(int32 inVolume);		
};

/*-----------------------------------------------------------------

CLASS
    VideoManager

	Manages a set of Video objects.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class VideoManager : public TObject
{
	public:
		//////////
		// Constructor.
		//
		VideoManager();
		
		//////////
		// Destructor.
		//
		~VideoManager();

		//////////
		// Idle time processing.  Should be called regularly from a higher level 
		// event loop.
		//
		void			Idle();
		
		//////////
		// Play the specified video clip.
		//
		// [in] inName - name of the video clip
		// [in_optional] inVolume - desired volume level (default 100)
		//
		void			Play(TString &inName, int32 inVolume = 100);
		
		//////////
		// Preload a video clip
		//
		// [in] inName - name of the video clip
		// [in] inSync - if true, pauses the gCardManager and do the entire preload now
		//
		void			Preload(TString &inName, bool inSync);

		//////////
		// Set an alarm to go off at the given frame of the currently playing movie.
		//
		// [in] inWaitFrame - frame where the alarm should be placed
		//
		void			Wait(int32 inWaitFrame);
		
		//////////
		// Kill the current playing clip and any clips that are preloaded.
		//
		void			Kill(void);
		
		//////////
		// Pause the current video clip
		//
		// [in] inTenths - resume after this amount of time (in 1/10 seconds)
		//
		void			Pause(int32 inTenths);
		
		//////////
		// Resume the current video clip after a Pause().
		//
		void			Resume(void);
		
		//////////
		// Draw the movie pane for the current video clip.  Should be called
		// when a screen redraw is needed. 
		//
		void			Draw(void);

		//////////
		// Set the origin for the current vidoe clip.  Should be called before playback.
		//
		// [in] inPoint - a TPoint specifying the new origin
		//
		void			SetOrigin(TPoint &inPoint);

		//////////
		// Is there a video clip playing?
		//
		// [out] return - true if a video clip playing, false otherwise
		//
		bool			Playing(void);
		
		//////////
		// Is there a movie playing fullscreen?
		//
		// [out] return - true if there is a full screen movie playing, false otherwise
		//
		bool			FullScreen(void);
		
		//////////
		// Is there a movie paused?
		//
		// [out] return - true if the movie is paused, false otherwise
		//
		bool			Paused(void);
		
		//////////
		// Has the origin been set?
		//
		// [out] return - true if the origin has been set, false otherwise
		//
		bool			HaveOrigin(void);

		//////////
		// NOT IN USE - returns false
		//
		bool			HandleEvent(HWND inWind, UINT inMessage, WPARAM inWParam, LPARAM inLParam);

	protected:
		//////////
		// Our array of Video clips.
		//
		TArray			m_Clips;
		
		//////////
		// Currently playing clip.
		//
		Video			*m_PlayingClip;
		
		//////////
		// Top-left corner of playback box.
		//
		TPoint			m_Origin;
		
		//////////
		// Do we know the playback origin?
		//
		bool			m_HaveOrigin;

		//////////
		// Find a Video clip by name.
		//
		// [in] inName - name of the clip to look for
		// [out] return - the Video clip, or NULL if not found
		//
		Video			*Find(TString &inName);
		
		//////////
		// Find and remove a Video clip by name.
		//
		// [in] inName - name of the clip to be removed
		// [out] return - the Video clip that was removed, or NULL if not found
		//
		Video			*FindAndRemove(TString &inName);
		
		//////////
		// Remove all clips from our array.
		//
		void			RemoveAllClips(void);
};

#endif // _Video_h_

/*
 $Log$
 Revision 1.1.12.1  2002/07/03 09:27:58  emk
 3.2.0.5+TQTMovie - Experimental support for displaying movies through TQTMovie.
 This is incomplete, but we'll want to port it to 3.5 or so.

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.5  2000/08/08 19:03:41  chuck
 no message

 Revision 1.4  2000/05/11 12:54:54  chuck
 v 2.01 b2

 Revision 1.3  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
