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
// Video.cpp : Source for video manager. This only supports
//		playing one movie at a time for now.
//
//

#include "stdafx.h"

#include "Video.h"
#include "TLogger.h"

#include "FiveL.h"

#include "Config.h"
#include "View.h"
#include "TVariable.h"
#include "LUtil.h" 
#include "Globals.h"

//
//	Video - Initialize the video clip.
//
Video::Video(TString &inName)
{
	m_Name = inName;
	m_PauseSteps = 0;
	m_WakeCard = false;
}

//
//	~Video - Clean up this clip.
//
Video::~Video()  
{	
	if (Playing())
		m_QT.Kill();
}

//
//	Idle - Return true if the given event was for the movie controller,
//		false otherwise.
//
void Video::Idle(void)
{
	m_QT.Idle();

	if (Playing())
	{
		// check if we should resume from a timed pause
		if (Paused())
		{
			if (m_PauseSteps > 0)
			{
				m_PauseSteps--;

				if (m_PauseSteps == 0)
				{
					m_PauseSteps = 0;
					Resume();
				}
			}
		}
		// check if we should wake up the card manager
		else if (m_WakeCard)
		{
			if (m_QT.AtWaitPoint())
			{
				m_WakeCard = false;
				gCardManager.WakeUp();
			}
		}
	}
	else if (m_WakeCard)
	{
		m_WakeCard = false;
		gCardManager.WakeUp();
	}
}

//
//	Wait - Set an alarm to go off at the given frame of the currently
//		playing movie.
//
void Video::Wait(int32 inWaitFrame)
{
	if (not Playing())
	{
		gDebugLog.Log("Wait: nothing to wait for");
		return;
	}
	
	m_WakeCard = true;	
	m_QT.SetWaitPoint(inWaitFrame);

	gCardManager.Pause();
} 				
	
//
//	Play - Play the given QuickTime file.
//
bool Video::Play(int32 inOffset, int32 inVolume /* = 100 */)
{
	TString		moviePath;
	TRect		movieRect;
	LQTError	retValue;
	int32		theVolume = VolumeXForm(inVolume);
    
  
	gVariableManager.SetString("_ERROR", "0");

	moviePath = gConfigManager.GetVideoPath(m_Name);

	// use a default rect (will be ignored if an origin was set)
	movieRect.Set(0, 0, H_SCREEN, V_SCREEN);
	
	// hide the cursor
	gCursorManager.ChangeCursor(NO_CURSOR);

	if (not HaveOrigin())
		gView->BlackScreen();
	else
		gView->Draw();			// make sure the screen is up to date
	 
	// keep this stuff
   	    
	retValue = m_QT.PlayVideo(moviePath, movieRect, inOffset, theVolume);

	if (retValue == LQT_NoError)
	{
		gVariableManager.SetString("_lpactive", "1");
		gVariableManager.SetString("_movieplaying", "1");	
	}
	else
	{
		gMissingMediaLog.Log("%s", moviePath.GetString());
		gVariableManager.SetString("_ERROR", "-2");
		gVariableManager.SetString("_FileNotFound", moviePath.GetString());

		gLog.Log("Video: could not play movie <%s>", moviePath.GetString());
		gDebugLog.Log("Video: could not play movie <%s>", moviePath.GetString());

		gCursorManager.CheckCursor();

		// fade in if necessary
		if (gView->Faded())
		{
			if (gView->BitDepth() <= 8)
				gView->Fade(true, 2, false);
			else
				gView->BlackScreen();
		}

		return (false);
	}

	return (true);
}

//
//	Preroll - Preroll the movie. Only allow this if we are not currently playing a movie.
//
bool Video::Preroll(int32 inTenths, bool inSync)
{
	TString		moviePath;
	LQTError	retValue;

	if (Playing())
	{
		// print out an error
		gDebugLog.Log("Video: Preroll <%s>, but already playing <%s>",
			m_Name.GetString(), m_Name.GetString());
		gLog.Log("Trying to preroll <%s> but already playing <%s>",
			m_Name.GetString(), m_Name.GetString());

		return (false);
	}

	gVariableManager.SetString("_ERROR", "0");

	// see if we have a video CD
	if (not gConfigManager.PlayMedia())
	{
		gDebugLog.Log("VideoManger:Preroll: <%s>, not playing media, nothing to do", 
			m_Name.GetString());
		
		return (false);
	}

	moviePath = gConfigManager.GetVideoPath(m_Name);

	retValue = m_QT.PrerollVideo(moviePath, inTenths, inSync);

	if (retValue != LQT_NoError)
	{
		if (retValue == LQT_NoFile)
		{
			gMissingMediaLog.Log("%s", moviePath.GetString());
			gVariableManager.SetString("_ERROR", "-2");
			gVariableManager.SetString("_FileNotFound", moviePath.GetString());
		}

		gLog.Log("Video: could not preroll movie <%s>", moviePath.GetString());
		gDebugLog.Log("Video: could not preroll movie <%s>", moviePath.GetString());
		return (false);
	}

	return (true);
}

//
//	Kill - Kill the current movie.
//
void Video::Kill(void)
{
	bool	doFade = false;

	if (not Playing())
		return;

	doFade = m_QT.FullScreen();

	m_QT.Kill();
 
  	if (m_WakeCard)
  	{
  		gCardManager.WakeUp();
  		m_WakeCard = false;
  	}
 	
	gCursorManager.CheckCursor();	// see if we should show the cursor
    
    if (doFade)
    {
    	if (gView->BitDepth() <= 8)
    		gView->Fade(true, 2, false);
    	else
    		gView->BlackScreen();
    }
	else 
		gView->Draw(true);		// blast the screen back up to wipe out the movie

	m_PauseSteps = 0;
	m_Name = "";

    gVariableManager.SetString("_lpactive", "0");
    gVariableManager.SetString("_movieplaying", "0");
}    

//
//	Pause - Pause the movie. If inTenths is > 0, set a time for
//		resuming.
//
void Video::Pause(int32 inTenths)
{
	if (Playing())
	{
		m_QT.Pause();

		// figure out how many idle steps we should pause
		if (inTenths > 0)
			m_PauseSteps = ((inTenths * 10) * 1000) / FIVEL_TIMER_MSECS;
		else
			m_PauseSteps = 0;
	    
	    gVariableManager.SetString("_lpactive", "0");
	}
}

//
//	Resume - Resume the movie.
//
void Video::Resume(void)
{
	if (Playing() and Paused())
	{
		m_QT.Resume();
		
		gVariableManager.SetString("_lpactive", "1");
	}
} 

//
//	GetBounds -
//
TRect Video::GetBounds(void)
{
	return (m_QT.GetBounds());
}

//
//	Draw
//
void Video::Draw(void)
{
	if (Playing())
		m_QT.DrawSelf();
}

//
//	HandleEvent - 
//
bool Video::HandleEvent(HWND inWind, UINT inMessage,
						WPARAM inWParam, LPARAM inLParam)
{
	return (m_QT.HandleEvent(inWind, inMessage, inWParam, inLParam));
}

//
//	VolumeXForm - Turn the 0 - 100 volume setting into 0 - 256.
//
int32 Video::VolumeXForm(int32 inVolume)
{
	int32	retValue = 0;
	
	if (inVolume > 0)
		retValue = (int32) (((float) inVolume / (float) 100) * (float) 255.0);
		
	return (retValue);
}

//
//	VideoManager methods
//

VideoManager::VideoManager()
{
	m_PlayingClip = NULL;
	m_HaveOrigin = false;
}

VideoManager::~VideoManager()
{
	Kill();
}

//
//	Idle -
//
void VideoManager::Idle(void)
{
	Video	*theClip;

	// if we have a playing clip, give it all the time
	if (m_PlayingClip != NULL)
	{
		m_PlayingClip->Idle();
		if (not m_PlayingClip->Playing())
		{
			// done, we can toss it
			delete m_PlayingClip;
			m_PlayingClip = NULL;
		}

		return;
	}

	// no playing clip, see if we have any clips prerolling
	//	that need time
	for (int32 i = m_Clips.NumItems() - 1; i >= 0; i--)
	{
		theClip = (Video *) m_Clips.Item(i);
		if (theClip != NULL)
		{
			theClip->Idle();
		}
	}
}

//
//	Play
//
void VideoManager::Play(TString &inName, int32 inOffset, int32 inVolume /* = 100 */)
{
	Video	*theClip;

	if (Playing())
		Kill();		// only one clip playing at a time

	gDebugLog.Log("VideoManager: Play <%s>, offset <%ld>, volume <%ld>",
		inName.GetString(), inOffset, inVolume);

	// see if we have a video CD
	if (not gConfigManager.PlayMedia())
	{
		gDebugLog.Log("VideoManger:Play: <%s>, not playing media, nothing to do", 
			inName.GetString());

		if (gView->Faded())
		{
			// fade back in
			if (gView->BitDepth() <= 8)
				gView->Fade(true, 2, false);
			else
				gView->BlackScreen();
		}
		
		return;
	}

	// look for the clip first
	theClip = FindAndRemove(inName);

	if (theClip == NULL)
	{
		theClip = new Video(inName);
	}

	if (theClip == NULL)
	{
		gLog.Log("Memory error in VideoManager: could not allocate memory for video clip");
		gDebugLog.Log("VideoManager: could not allocate clip for <%s>",
			inName.GetString());
		return;
	}

	if (m_HaveOrigin)
	{
		theClip->SetOrigin(m_Origin);
		m_HaveOrigin = false;

		gDebugLog.Log("VideoManager: setting origin to X <%d>, Y <%d>",
			m_Origin.X(), m_Origin.Y());
	}

	if (theClip->Play(inOffset, inVolume))
	{
		m_PlayingClip = theClip;

		// invalidate the rect the movie is playing in
		// cbo_fix - don't do this to see if it was causing 
		//	flash on startup
//		{
//			RECT	dirtyRect;
//			TRect	movieBounds;
//
//			movieBounds = theClip->GetBounds();
//			dirtyRect = movieBounds.GetRECT();
//
//			::InvalidateRect(hwndApp, &dirtyRect, false);
//		}

		// remove all the other clips so they don't take up memory
		RemoveAllClips();
	}
	else
		delete theClip;
}

void VideoManager::Preroll(TString &inName, int32 inTenths, bool inSync)
{
	Video	*theClip = NULL;
	bool	newClip = false;

	gDebugLog.Log("VideoManager: Preroll <%s>", inName.GetString());

	// see if we have a video CD
	if (not gConfigManager.PlayMedia())
	{
		gDebugLog.Log("VideoManger:Preroll: <%s>, not playing media, nothing to do", 
			inName.GetString());
		return;
	}

	theClip = Find(inName);
	if (theClip != NULL)
	{
		// must already be prerolling
		if (theClip->Prerolled())
		{
			// nothing to do 
			gDebugLog.Log("VideoManager: Preroll <%s>, already prerolled",
				inName.GetString());
			return;
		}
	}
	else
	{
		newClip = true;
		theClip = new Video(inName);

		if (theClip == NULL)
		{
			gLog.Log("Memory error in VideoManager: could not allocate memory for movie");
			gDebugLog.Log("VideoManager: could not allocate clip for <%s>",
				inName.GetString());
			return;
		}
	}
	
	if (theClip->Preroll(inTenths, inSync))
	{
		if (newClip)
			m_Clips.Add(theClip);
	}
	else
	{
		gLog.Log("Could not preroll video clip <%s>",
			inName.GetString());

		gDebugLog.Log("VideoManager: could not preroll clip <%s>",
			inName.GetString());
		delete theClip;
	}
}

void VideoManager::SetOrigin(TPoint &inPoint)
{
	if ((inPoint.X() >= 0) and (inPoint.Y() >= 0))
	{
		if (m_PlayingClip != NULL)
			m_PlayingClip->SetOrigin(inPoint);
		else 
		{
			// save it for the next clip
			m_Origin = inPoint;
			m_HaveOrigin = true;
		}
	}
}

void VideoManager::Wait(int32 inWaitFrame)
{
	if (m_PlayingClip != NULL)
		m_PlayingClip->Wait(inWaitFrame);
}

void VideoManager::Kill(void)
{
	if (m_PlayingClip != NULL)
	{
		m_PlayingClip->Kill();

		delete m_PlayingClip;
		m_PlayingClip = NULL;
	}

	// now toss everything!
	Video	*theClip = NULL;

	for (int32 i = 0; i < m_Clips.NumItems(); i++)
	{
		theClip = (Video *) m_Clips.Item(i);
		if (theClip != NULL)
		{
			gDebugLog.Log("VideoManager: Kill clip <%s>",
				theClip->Name());
			theClip->Kill();
			delete theClip;
		}
	}

	m_Clips.RemoveAll();
	m_HaveOrigin = false;
}

void VideoManager::Pause(int32 inTenths)
{
	if (m_PlayingClip != NULL)
		m_PlayingClip->Pause(inTenths);
}

void VideoManager::Resume(void)
{
	if (m_PlayingClip != NULL)
		m_PlayingClip->Resume();
}

void VideoManager::Draw(void)
{
	if (m_PlayingClip != NULL)
		m_PlayingClip->Draw();
}

//
//	Playing 
//
bool VideoManager::Playing(void)
{
	if (m_PlayingClip != NULL)
		return (true);
	return (false);
}

bool VideoManager::FullScreen(void)
{
	if (m_PlayingClip != NULL)
		return (m_PlayingClip->FullScreen());
	return (false);
}

bool VideoManager::Paused(void)
{
	if (m_PlayingClip != NULL)
		return (m_PlayingClip->Paused());
	return (false);
}

bool VideoManager::HaveOrigin(void)
{
	if (m_PlayingClip != NULL)
		return (m_PlayingClip->HaveOrigin());
	else if (m_HaveOrigin)
		return (true);
	return (false);
}


//
//	Find - Find the clip with the given name.
//
Video *VideoManager::Find(TString &inName)
{
	Video	*theClip = NULL;

	for (int32 i = 0; i < m_Clips.NumItems(); i++)
	{
		theClip = (Video *) m_Clips.Item(i);

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
//	FindAndRemove - Find the clip with the given name
//		and remove it from the list.
//
Video *VideoManager::FindAndRemove(TString &inName)
{
	Video	*theClip = NULL;

	for (int32 i = 0; i < m_Clips.NumItems(); i++)
	{
		theClip = (Video *) m_Clips.Item(i);

		if (theClip != NULL)
		{
			if (inName.Equal(theClip->Name()))
			{
				// found it, remove it from the list
				m_Clips.RemoveIndex(i);

				return (theClip);
			}
		}
	}

	return (NULL);
}

void VideoManager::RemoveAllClips(void)
{
	Video	*theClip = NULL;

	for (int32 i = 0; i < m_Clips.NumItems(); i++)
	{
		theClip = (Video *) m_Clips.Item(i);
		if (theClip != NULL)
		{
			theClip->Kill();
			delete theClip;
		}
	}
	m_Clips.RemoveAll();
}

bool VideoManager::HandleEvent(HWND inWind, UINT inMessage, 
							   WPARAM inWParam, LPARAM inLParam)
{

	if (m_PlayingClip != NULL)
		return (m_PlayingClip->HandleEvent(inWind, inMessage, inWParam, inLParam));
	return (false);
}


/*
 $Log$
 Revision 1.2.2.1  2002/04/30 07:57:31  emk
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

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.7  2000/08/08 19:03:41  chuck
 no message

 Revision 1.6  2000/05/11 12:54:54  chuck
 v 2.01 b2

 Revision 1.5  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.4  2000/03/01 15:46:55  chuck
 no message

 Revision 1.3  1999/11/04 14:18:50  chuck
 2.00 Build 10

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
