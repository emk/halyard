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
// Audio.h : 
//

#if !defined (_Audio_h_)
#define _Audio_h_
                  
#include "TCommon.h"
#include "TObject.h"
#include "TArray.h"
#include "TString.h" 
#include "LQuickTime.h"
#include "Card.h"


/*-----------------------------------------------------------------

CLASS
    Audio

	This class encapsulates an audio clip.  Methods are provided
	to play, pause, resume and preload among others.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class Audio : public TObject
{
	    
	public:
		
		//////////
		// Constructor
		//
		// [in] inName - name of the audio clip
		// [in_optional] inOffset - offset to start playing of clip (default 0)
		//
		Audio(TString &inName, int32 inOffset = 0);
		
		//////////
		// Destructor
		//
		virtual		~Audio();

		//////////
		// Give idle time to the clip. If we are changing the volume then do it.
		//
		void		Idle(void); 

		//////////
		// Set a wait point at the specified frame.  Clip must be playing.
		//
		// [in] inWaitFrame - frame to place a wait point
		//
		void		Wait(int32 inWaitFrame);
		
		//////////
		// Have we reached the wait point?
		//
		// [out] return - true if the clip has reached the wait point, false otherwise
		//
		bool		AtWaitPoint(void) { return (m_QT.AtWaitPoint()); }

		//////////
		// Is the clip playing?
		//
		// [out] return - true if the clip is playing, false otherwise
		//
		bool		Playing(void) { return ((m_Active) and (m_QT.Playing())); }
		
		//////////
		// Has the clip been prerolled?
		//
		// [out] return - true if the clip has been prerolled (see Preroll), false otherwise
		//
		bool		Prerolled(void) { return ((m_Active) and (m_QT.Prerolled())); }
		
		//////////
		// Is the clip paused?
		//
		// [out] return - true if the clip is currently paused, false otherwise
		//
		bool		Paused(void) { return (m_QT.Paused()); }

		//////////
		// Is the clip set to loop?
		//
		// [out] return - true if the clip is set to loop, false otherwise
		//
		bool		Looping(void) 		// are we looping?
						{ return (m_Looping); }
		//////////
		// Play the specified audio clip.
		//
		// [in] inVolume - volume used to play the clip
		// [in] inFadeTime - fade time (0 if no fade desired)
		// [in] inOffset - offset at which to start playing of the clip
		// [in] inLoop - if true, the clip will loop
		//
		bool		Play(int32 inVolume, int32 inFadeTime, int32 inOffset, bool inLoop);
		
		//////////
		// Preload the audio clip so it is ready to play.
		//
		// [in] inTenths - make sure preload takes at least this much time (in 1/10 sec)
		// [in] inSync - if true, pause the gCardManager and do the entire preroll now
		// [out] return - true if successfully prerolled, false otherwise
		//
		bool		Preroll(int32 inTenths, bool inSync);
		
		//////////
		// Kill the audio clip. 
		//
		// [in] inFadeTime - if inFadeTime > 0, fade the volume to 0
		//					 and then kill the clip (time for fade in 1/10 seconds)
		//
		void		Kill(int32 inFadeTime);
		
		//////////
		// Set up a volume fade.
		//
		// [in] inFadeTime - amount of time for the fade (in 1/10 seconds)
		// [in] inTargetVol - target volume after fade
		//
		void		FadeVolume(int32 inFadeTime, int32 inTargetVol);
		
		// int32		FrameToTime(int32 inFrame);
		
		//////////
		// Pause the audio clip.
		//
		void		Pause(void);
		
		//////////
		// Resume the audio clip after a Pause().
		//
		void		Resume(void);
		
		//////////
		// Is the specified clip the same as this clip?
		//
		// [in] inName - name of the clip to compare to this clip
		// [out] return - true if the clips are the same, false otherwise
		//
		bool		SameClip(const char *inName);
		
		//////////
		// Get the name of this clip.
		//
		// [out] return - the name of the clip
		//
		const char	*Name(void)
						{ return ((const char *) m_Name); }

	private:
		//////////
		// Is the clip looping?
		//
		bool		m_Looping;
		
		//////////
		// Should we kill the clip when the volume gets to 0?
		//
		bool		m_Kill;	
		
		//////////
		// Is the clip active?
		//
		bool		m_Active;
		
		//////////
		// Time to use for fading.
		//
		int32		m_FadeTime;
		
		//////////
		// Number for conversion of clip time to frame
		//
		int32		m_Offset;
		
		//////////
		// Current volume.
		//
		int32		m_Volume; 
		
		//////////
		// Target volume.
		//
		int32		m_TargetVol;
		
		//////////
		// How much to change volume each time.
		//
		int32		m_Step;	
		
		//////////
		// Time of next volume change.
		//
		DWORD		m_VolTime;
        
		//////////
		// How much time between volume changes.
		//
		int32		m_TimeStep;
        
		//////////
		// Name of audio file (full path).
		//
		TString		m_Name;
		
		//////////
		// Our quicktime movie
		//
		LQuickTime	m_QT;

		//////////
		// Kill playback of this audio clip immediately.
		//
		void		KillNow(void);			
};

/*-----------------------------------------------------------------

CLASS
    AudioManager

	Manages a list of audio clips, many of which may be prerolled
	at a given time.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class AudioManager : public TObject
{		
	public:
		//////////
		// Constructor
		//
		AudioManager();
		
		//////////
		// Destructor
		//
		virtual		~AudioManager();

		//bool		HandleEvent(HWND inWind, UINT inMessage, WPARAM inWParam, LPARAM inLParam);		
		
		//////////
		// Give idle time to all audio clips in the list.
		//
		void		Idle();
		
		//////////
		// Create and play an audio clip.
		//
		// [in] inName - name of the audio clip 
		// [in] inOffset - offset to start playback 
		// [in] inVolume - volume used to play the clip
		// [in] inFadeTime - fade time in 1/10 seconds (0 if no fade desired)
		// [in] inLoop - if true, the clip will loop
		// [in] inKill - if true, all other clips with be killed
		//
		void		Play(TString &inName, int32 inOffset, int32 inVolume, 
						int32 inFadeTime, bool inLoop, bool inKill);
		//////////
		// Preload an audio clip so it is ready to play.
		//
		// [in] inName - name of the audio clip
		// [in] inTenths - make sure preload takes at least this much time, 
		//				   nap if needed (in 1/10 seconds)
		// [in] inSync - if true, pauses the gCardManager and do the entire preroll now
		//
		void		Preroll(TString &inName, int32 inTenths, bool inSync);
		
		//////////
		// Kill all playing clips.
		//
		// [in_optional] inFadeTime - fade time before kill in 1/10 seconds (default 0)
		// [in_optional] inLoops - IGNORED
		//
		void		Kill(int32 inFadeTime = 0, bool inLoops = false);
		
		//////////
		// Set the volume for all playing clips.
		//
		// [in] inVolLevel - desired volume level
		// [in] inFadeTime - amount of time (in 1/10 sec) to fade to this volume level
		//
		void		Volume(int32 inVolLevel, int32 inFadeTime);
		
		//////////
		// Send a wake up message when the given offset is reached in the current clip. 
		// If there is more than one clip playing, choose the first non-looping clip. 
		// The wait point is passed in as a frame number. Convert it to a movie time.
		//
		// [in] inWaitFrame - frame number for the wakeup message
		//
		void		Wait(int32 inWaitFrame);
		
		//////////
		// Pause all clips.
		//
		// [in] inTenths - amount of time to pause in 1/10 seconds
		//
		void		Pause(int32 inTenths);
		
		//////////
		// Resume all clips after a Pause().
		//
		void		Resume(void);
				
		//////////
		// Is there a clip playing?
		//
		// [out] return - true if there is a clip playing, false otherwise
		//
		bool		Playing(void)
						{ return (m_Playing); }
		//////////
		// Is there a looping clip playing?
		//
		// [out] return - true if there is a clip looping, false otherwise
		//
		bool		Looping(void)
						{ return (m_Playing and m_Looping); }		
		//////////
		// Is there a non-looping clip playing?
		//
		// [out] return - true if there is a clip playing but no looping clips, false otherwise
		//
		bool		PlayNoLoop(void)
						{ return (m_Playing and (not m_Looping)); }
        //////////
		// Is there a paused clip?
		//
		// [out] return - true if there is a clip playing that is currently paused, false otherwise
		//
		bool		Paused(void)
        				{ return (m_Playing and m_Paused); }

		//////////
		// DEPRECATED
		//
		// [out] return - returns false
		//
		bool		HandleEvent(HWND inWind, UINT inMessage, WPARAM inWParam, LPARAM inLParam);

	private:
		//////////
		// Audio clip that is looping.
		//
		Audio		*m_Loop;
		
		//////////
		// List of currently playing clips.
		//
		TArray		m_List; 
		
		//////////
		// Audio clip used for waiting.
		//
		Audio		*m_WaitClip;	

		//////////
		// Time we should wait for.
		//
		int32		m_WaitTime;
		
		//////////
		// Amount of time we should pause.
		//
		DWORD		m_PauseTime;
		
		//////////
		// Are we playing anything?
		//
		bool		m_Playing;
		
		//////////
		// Are we looping anything?
		//
		bool		m_Looping;
		
		//////////
		// Are we paused?
		//
		bool		m_Paused;

		//////////
		// Find an audio clip.
		//
		// [in] inName - name of the audio clip
		// [out] return - an Audio clip or NULL if not found
		//
		Audio		*Find(TString &inName);

		//////////
		// Convert the 0 - 100 volume setting into 0 - 256.
		//
		// [in] inVolume - the volume (0 to 100)
		// [out] return - the volume in 0 to 256 range.
		//
		int32		VolumeXForm(int32 inVolume);
};

#endif // _Audio_h_

/*
 $Log$
 Revision 1.1  2001/09/24 15:11:00  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.6  2000/08/08 19:03:40  chuck
 no message

 Revision 1.5  2000/05/11 12:54:53  chuck
 v 2.01 b2

 Revision 1.4  2000/04/07 17:05:15  chuck
 v 2.01 build 1

 Revision 1.3  1999/11/02 17:16:37  chuck
 2.00 Build 8

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
