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
// Audio.cpp : Source file for the audio manager and audio object classes.
//

#include "stdafx.h"

#include <stdlib.h>

#include "Debug.h"
#include "Audio.h"
#include "Config.h"
#include "TVariable.h"
#include "LUtil.h"
#include "Globals.h"

// methods for Audio class

//
//	Audio
//
Audio::Audio(TString &inName, int32 inOffset /* = 0 */)
{
	m_Active = false;
	m_Looping = false;
	m_Kill = false;
	m_FadeTime = 0;
	m_Offset = inOffset;
	m_Volume = 0;
	m_TargetVol = 0;
	m_Step = 0;
	m_VolTime = 0;
	m_TimeStep = 0;
	m_Name = inName;
}

//
//	~Audio
//
Audio::~Audio()
{
	KillNow();
} 

//
//	Idle - Give idle time to the clip. If we are changing the
//		volume then do it.
//
void Audio::Idle(void)
{
	DWORD		theTimeNow; 
	int32		theSkipper;

	m_QT.Idle();

	if (Playing())
	{
		// see if we need to adjust the volume
		if (m_Volume != m_TargetVol)
		{
			// see if this is a time to adjust the volume
			theTimeNow = ::timeGetTime();
			if (m_VolTime < theTimeNow)
			{
				if (theTimeNow > (m_VolTime + m_TimeStep))
				{
					// we missed at least one step
					// see how many steps we should skip
					theSkipper = ((theTimeNow - m_VolTime) / m_TimeStep) + 1;
					
					m_Volume += (m_Step * theSkipper);
				}
				else
					m_Volume += m_Step;
				
				if (m_Step < 0)
				{ 	// volume is going down, see if too low
					if (m_Volume < m_TargetVol)
						m_Volume = m_TargetVol;
				}
				else
				{	// volume is going up, see if too high
					if (m_Volume > m_TargetVol)
						m_Volume = m_TargetVol;
				}

				m_QT.SetVolume(m_Volume);
				
				//::MCDoAction(m_Movie, mcActionSetVolume, (LPVOID) MAKESFIXED(0, m_Volume));
				m_VolTime = theTimeNow + m_TimeStep;
			}
		}

		// see if we are done fading out a looping clip
		if ((m_QT.AudioLooping()) and m_Kill and (m_Volume == 0))
			KillNow();
	}
}	

void Audio::Wait(int32 inWaitFrame)
{
	if (Playing())
		m_QT.SetWaitPoint(inWaitFrame);
}

//
//	Play - Play the audio clip.
//
bool Audio::Play(int32 inVolume, int32 inFadeTime, int32 inOffset, bool inLoop)
{
	TString		moviePath;
	LQTError	retValue;

	m_Kill = false;
	
	m_Offset = inOffset;

	gVariableManager.SetString("_ERROR", "0");

	moviePath = gConfigManager.GetAudioPath(m_Name);
	
	retValue = m_QT.PlayAudio(moviePath, inVolume, inOffset, inLoop);
 
	if (retValue != LQT_NoError)
	{
		gMissingMediaLog.Log("%s", moviePath.GetString());
		gVariableManager.SetString("_ERROR", "-2");
		gVariableManager.SetString("_FileNotFound", moviePath.GetString());

		return (false);
	}

    if (inFadeTime > 0)
    {
    	m_FadeTime = inFadeTime;
    	FadeVolume(inFadeTime, inVolume); 
    }

	m_Active = true;
	return (true);
}

//
//	Preroll - 
//
bool Audio::Preroll(int32 inTenths, bool inSync)
{
	TString		moviePath;
	LQTError	retValue;

	m_Kill = false;

	gVariableManager.SetString("_ERROR", "0");
	moviePath = gConfigManager.GetAudioPath(m_Name);

	retValue = m_QT.PrerollAudio(moviePath, inTenths, inSync);

	if (retValue != LQT_NoError)
	{
		if (retValue == LQT_NoFile)
		{
			gMissingMediaLog.Log("%s", moviePath.GetString());
			gVariableManager.SetString("_ERROR", "-2");
			gVariableManager.SetString("_FileNotFound", moviePath.GetString());
		}

		return (false);
	}

	m_Active = true;
	return (true);
}

//
//	Kill - Kill the audio clip. If the fade time is a positive
//		nuber, use it as tenths of a second to take to fade the
//		volume to 0 and then kill the clip.
//
void Audio::Kill(int32 inFadeTime)
{
	if ((inFadeTime > 0) and (m_Volume != 0))
	{
		m_Kill = true;
		FadeVolume(inFadeTime, 0);
	}
	else
	    KillNow();
}

//
//	KillNow - Really kill the movie.
//
void Audio::KillNow(void)
{	
	m_QT.Kill();
	m_Kill = false;
	m_Active = false;
} 

//
//	FadeVolume - Set up the member fields for a volume fade.
//
void Audio::FadeVolume(int32 inFadeTime, int32 inTargetVol)
{
	int32	vol_diff;
	int32	steps;
	int32	msecs;

	gDebugLog.Log("AudioManager: FadeVolume time <%ld>, target vol <%ld>", inFadeTime, inTargetVol);
	gDebugLog.Log("   current volume is <%ld>", m_Volume);

	msecs = inFadeTime * 100L;			// milliseconds the fade should take   
		
	vol_diff = inTargetVol - m_Volume;	// how much to change the volume
	if (vol_diff == 0)
		return;							// nothing to do
			
	m_TimeStep = 50;					// change the volume every 50 msecs
	steps = msecs / m_TimeStep;			// how many steps to take	
	m_Step = vol_diff / steps;			// pos or neg amount to change volume each step
	if (m_Step == 0) 					// make sure it is something
	{
		if (vol_diff < 0)
			m_Step = -1;
		else
			m_Step = 1;
	}
	
	m_TargetVol = inTargetVol;			// final volume
	m_VolTime = ::timeGetTime();		// start the timer going now
	
	gDebugLog.Log("AudioManager: FadeVolume volstep <%ld>, targetvol <%ld>, steps <%ld>",
		m_Step, m_TargetVol, steps);
}

//
//	Pause - Pause the audio.
//
void Audio::Pause(void)
{
	m_QT.Pause();
}

//
//	Resume - Resume the audio.
//
void Audio::Resume(void)
{
	m_QT.Resume();
}

//
//	SameClip - Return true if the given name is the same as our name.
//
bool Audio::SameClip(const char *inName)
{
	if (m_Name.Equal(inName, false))
		return (false);
	return (true);
}

//
//	methods for AudioManager
//

//
//	AudioManager
//
AudioManager::AudioManager()
{
	m_Loop = NULL;
	m_WaitClip = NULL;	
}

//
//	~AudioManager - Kill and delete all the audio clips.
//
AudioManager::~AudioManager()
{
	Audio		*theClip;
	int32		i;

	for (i = 0; i < m_List.NumItems(); i++)
	{
		theClip = (Audio *) m_List.Item(i);
		
		if (theClip != NULL)
		{
			theClip->Kill(0);
			delete theClip;
		}
	}

	m_List.RemoveAll();
}	

//
//	Idle - Give idle time to all audio clips in the list.
//
void AudioManager::Idle(void)
{
	Audio	*theClip;
	int32	i;
		
	if (m_Paused)
	{ 
		if (m_PauseTime > 0)
		{
			if (::timeGetTime() >= m_PauseTime)
				Resume();
			else
				return;
		}
		else
			return;
	}
	
	// go from the top of the list down so that we can
	// delete things as we go
	for (i = m_List.NumItems() - 1; i >= 0; i--)
	{
		theClip = (Audio *) m_List.Item(i);
		
		if (theClip != NULL)
		{
			theClip->Idle();
			
			if (not theClip->Playing())
			{
				if (theClip == m_WaitClip)
				{
					gDebugLog.Log("AudioManager: Idle - normal end of wait clip, wake card");

					// this is the clip we are waiting on
					gCardManager.WakeUp();
					
					m_WaitClip = NULL;
					m_WaitTime = 0;
				} 

				m_List.RemoveIndex(i);	// remove it from the list of clips
				delete theClip;			// and delete it

				gVariableManager.SetString("_lpactive", "0");
			}
		}
	}
	
	// see if we are at the wait frame
	if (m_WaitClip != NULL)
	{
		if (m_WaitClip->AtWaitPoint())
		{
			gDebugLog.Log("AudioManager: Idle - at wait frame, wake card");
			gCardManager.WakeUp();
			
			m_WaitClip = NULL;
			m_WaitTime = 0;
		}
	}
	
	// see if we need to change our status
	if ((m_Playing) and (m_List.NumItems() == 0))
	{
		m_Playing = false;
		m_Looping = false;
		m_Loop = NULL;
		m_Paused = false;
		m_PauseTime = 0; 
	}	
}

//
//	Play - Create and play an audio clip.
//
void AudioManager::Play(TString &inName, int32 inOffset,
			int32 inVolume, int32 inFadeTime, bool inLoop, bool inKill)
{
	Audio		*theClip;
	int32		theVolume = VolumeXForm(inVolume);
	bool		newClip = false;

	// Debugging Info
	gDebugLog.Log("AudioManager: Play <%s>, offset <%ld>, volume <%ld>, fade time <%ld>",
		inName.GetString(), inOffset, inVolume, inFadeTime);
	if (inLoop)
		gDebugLog.Log("   make it loop");
	if (inKill)
		gDebugLog.Log("   kill other clips");
	
	if (inKill, false)
		Kill(inFadeTime);
		
	if (not gConfigManager.PlayMedia())
	{
		gDebugLog.Log("AudioManager: Play: <%s>, not playing media, nothing to do", 
			inName.GetString());
		return;
	}
    
    // see if we are trying to loop the clip that is already looping
	if (inLoop)
	{
		if ((m_Looping) and (m_Loop != NULL))
		{
			// see if it is the same clip
			if (m_Loop->SameClip(inName.GetString()))
			{
				gDebugLog.Log("AudioManager: looping the same clip that is looping, nothing to do");
				return;			// nothing to do
			}
		}
	}

	// look for the clip first, we may have prerolled it
	theClip = Find(inName);

	if (theClip == NULL)
	{
		newClip = true;
		theClip = new Audio(inName, inOffset);
	}
	
	if (theClip == NULL)
	{
		gLog.Log("Memory error in AudioManager: could not allocate memory for movie");
		gDebugLog.Log("AudioManager: could not allocate clip for <%s>",
			inName.GetString());
		return;
	}
	
	if (inLoop)
	{
		// if there is another loop going, kill it
		if ((m_Looping) and (m_Loop != NULL))
			m_Loop->Kill(inFadeTime); 
			
		m_Loop = theClip;
		m_Looping = true;
	}

    if (theClip->Play(theVolume, inFadeTime, inOffset, inLoop))
	{
		m_Playing = true;
		gVariableManager.SetString("_lpactive", "1"); 

		// add the clips to our list of clips
		if (newClip)
			m_List.Add(theClip);
	}
	else
	{
		gLog.Log("AudioManager: could not play audio clip <%s>", 
			inName.GetString());		

		gDebugLog.Log("AudioManager: could not play audio clip <%s>", 
			inName.GetString());

		// kill the clip
		if (m_Loop == theClip)
		{
			m_Loop = NULL;
			m_Looping = false;
		}

		delete theClip;
	}
}

//
//	Kill - Kill all playing clips.
//
void AudioManager::Kill(int32 inFadeTime, bool inLoops)
{
	Audio		*theClip;
	int32		i;

	gDebugLog.Log("AudioManager: Kill all playing clips, fade time <%ld>", inFadeTime);
	if (inLoops)
		gDebugLog.Log("   kill loops too!");

	// cbo_fix - just kill everything, look at loops later
	for (i = 0; i < m_List.NumItems(); i++)
	{
		theClip = (Audio *) m_List.Item(i);
		if (theClip != NULL)
		{
			gDebugLog.Log("AudioManager: Kill clip <%s>", theClip->Name());
			theClip->Kill(0);
			delete theClip;
		}
	}
	m_List.RemoveAll();
	m_Loop = NULL;
	m_Playing = false;
	m_Looping = false;

/*
	// go from top of list down so that we can delete things as we go
//	for (i = m_List.NumItems() - 1; i >= 0; i--)
	for (i = 0; i < m_List.NumItems(); i++)
	{
		theClip = (Audio *) m_List.Item(i);
		
		if (theClip != NULL)
		{
			// see if we should kill this clip (looping clips are the 
			// only ones we might not kill)
			// cbo_fix - no looping audio for now
			//if ((inLoops) or (not m_Looping) or (m_Loop != theClip))
			{
				gDebugLog.Log("AudioManager: Kill clip <%s>", theClip->Name());
				theClip->Kill(0);

				// cbo_fix - if we don't remove this here the next clip
				// messes up
			//	m_List.RemoveIndex(i);
			//	delete theClip;
			}
		}
	}
*/
	
	if (m_WaitClip != NULL)
	{
		gDebugLog.Log("AudioManager: Kill - wake card");
		gCardManager.WakeUp();
		
		m_WaitClip = NULL;
		m_WaitTime = 0;
	}
	
	// make sure to set m_Paused to false so that the clips
	// get idle time
	m_Paused = false;
	
	gVariableManager.SetString("_lpactive", "0");
}

//
//	Volume - Set the volume for all playing clips.
//
void AudioManager::Volume(int32 inVolLevel, int32 inFadeTime)
{
	Audio		*theClip;
	int32		theVolume = VolumeXForm(inVolLevel);
	int32		i;

	gDebugLog.Log("AudioManager: set volume of clips to <%ld>, fade time <%ld>", inVolLevel, inFadeTime);
	
	for (i = 0; i < m_List.NumItems(); i++)
	{
		theClip = (Audio *) m_List.Item(i);
		
		if (theClip != NULL)
			theClip->FadeVolume(inFadeTime, theVolume);
	}
}

//
//	Wait - Send a wake up message when the given offset
//		is reached in the current clip. If there is more
//		than one clip playing, choose the first non-looping
//		clip. The wait point is passed in as a frame number.
//		Convert it to a movie time.
//
void AudioManager::Wait(int32 inWaitFrame)
{
	Audio		*theClip;
	int32		i;
	bool		found;
	
	// nothing to do if there aren't any clips
	if (not m_Playing)
	{
		gDebugLog.Log("AudioManager: wait - nothing to wait for");
		return;
	}
		
	// if we aren't already waiting on a clip, figure out
	// which one we should wait on - take the first non-looping
	// clip	
	if (m_WaitClip == NULL)
	{ 
		i = 0;
		found = false;
		while (not found)
		{
		    theClip = (Audio *) m_List.Item(i);
			if (theClip != NULL)
			{
				//STS if (not theClip->Looping())
				if (theClip->Playing() && not theClip->Paused() && not theClip->Looping())
				{
		    		m_WaitClip = theClip;
		    		found = true;
				}
			}
		    
		    i++;
		}
	}
	
	// see if we have a clip to wait for
	if (m_WaitClip != NULL)
	{
		gDebugLog.Log("AudioManager: Wait for frame <%ld>", inWaitFrame);

		m_WaitClip->Wait(inWaitFrame);
		gCardManager.Pause();
	}
}

//
//	Preroll - 
//
void AudioManager::Preroll(TString &inName, int32 inTenths, bool inSync)
{
	Audio		*theClip = NULL;
	bool		newClip = false;

	gDebugLog.Log("AudioManager: Preroll <%s>", inName.GetString());

	if (not gConfigManager.PlayMedia())
	{
		gDebugLog.Log("AudioManager: Preroll: <%s>, not playing media, nothing to do", 
			inName.GetString());
		return;
	}

	theClip = Find(inName);
	if (theClip != NULL)
	{
		if (theClip->Playing())
		{
			gDebugLog.Log("AudioManager: Preroll <%s>, but it is already playing",
				inName.GetString());

			// error, can't preroll a playing clip
			gLog.Log("Trying to preroll <%s> but it is already playing", 
				inName.GetString());

			return;
		}
		else if (theClip->Prerolled())
		{
			// nothing to do, already prerolled
			gDebugLog.Log("AudioManager: Preroll <%s>, already prerolled",
				inName.GetString());
			return;
		}
	}
	else
	{
		newClip = true;
		theClip = new Audio(inName);

		if (theClip == NULL)
		{
			gLog.Log("Memory error in AudioManager: could not allocate memory for movie");
			gDebugLog.Log("AudioManager: could not allocate clip for <%s>",
				inName.GetString());
			return;
		}
	}

	if (theClip->Preroll(inTenths, inSync))
	{
		if (newClip)
			m_List.Add(theClip);
	}
	else
	{
		gLog.Log("Could not preroll audio clip <%s>", 
			inName.GetString());

		gDebugLog.Log("AudioManager: could not preroll audio clip <%s>",
			inName.GetString());

		delete theClip;
	}
}

//
//	Pause - Pause all clips.
//
void AudioManager::Pause(int32 inTenths)
{
	Audio	*theClip;
	int32	i;

	gDebugLog.Log("AudioManager: Pause all clips");
	
	if ((m_Playing) and (not m_Paused))
	{
		for (i = 0; i < m_List.NumItems(); i++)
		{
			theClip = (Audio *) m_List.Item(i);
			
			if (theClip != NULL)
				theClip->Pause();		
		}
		
		m_Paused = true;
		if (inTenths > 0)
			m_PauseTime = ::timeGetTime() + (inTenths * 100L);
		else
			m_PauseTime = 0;
			
		gVariableManager.SetString("_lpactive", "0");
	}
}

//
//	Resume - Resume all clips.
//
void AudioManager::Resume(void)
{
	Audio	*theClip;
	int32	i;

	gDebugLog.Log("AudioManager; Resume all clips");
	
	if (m_Playing and m_Paused)
	{
		for (i = 0; i < m_List.NumItems(); i++)
		{
			theClip = (Audio *) m_List.Item(i);
			
			if (theClip != NULL)
				theClip->Resume();
		}
		
		m_Paused = false;
		m_PauseTime = 0;
	
		gVariableManager.SetString("_lpactive", "1");
	}
}

//
//	Find - Find the clip with the given name.
//
Audio *AudioManager::Find(TString &inName)
{
	Audio	*theClip = NULL;

	for (int32 i = 0; i < m_List.NumItems(); i++)
	{
		theClip = (Audio *) m_List.Item(i);

		if (theClip != NULL)
		{
			if (inName.Equal(theClip->Name()))
			{
				// found it
				return (theClip);
			}
		}
	}

	return (NULL);
}

//
//	VolumeXForm - Turn the 0 - 100 volume setting into 0 - 256.
//
int32 AudioManager::VolumeXForm(int32 inVolume)
{
	int32	retValue = 0;
	
	if (inVolume > 0)
		retValue = (int32) (((float) inVolume / (float) 100) * (float) 255.0);
		
	return (retValue);
}

bool AudioManager::HandleEvent(HWND /*inWind*/, UINT /*inMessage*/, 
							   WPARAM /*inWParam*/, LPARAM /*inLParam*/)
{
	return (false);
}

/*
 $Log$
 Revision 1.3  2002/05/15 11:05:33  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.2.2.1  2002/04/30 07:57:30  emk
 3.3.2.5 - Port Win32 code to use the 20Kloc of Common code that now
 exists.  The (defstyle ...) command should work, but (textaa ...) isn't
 available yet.

 Next up: Implement the (textaa ...) command and the low-level
 GraphicsTools::Image::DrawBitMap.

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
