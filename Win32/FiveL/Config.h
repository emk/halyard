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

#if !defined (_Config_h_)
#define _Config_h_

#include "TCommon.h"
#include "TString.h"

#define DLS_USER_PROFILE "user.profile"

/*-----------------------------------------------------------------

CLASS
    Config

	Simple configuration class.

AUTHOR
    Chuck Officer<br>
	Yijin He

-----------------------------------------------------------------*/
class Config : public TObject
{
	public:
		
		//////////
		// Empty Constructor.
		//
		Config();

		//////////
		// Empty Destructor.
		//
		~Config();
		
		//////////
		// Name of the script associated with this Config entry.
		//
		TString		m_ScriptName;
		
		//////////
		// The directory where media files are located.
		//
		TString		m_MediaDir;
};

/*-----------------------------------------------------------------

CLASS
    ConfigManager

	Manages a set of Config objects and contains all global
	configuration settings.

AUTHOR
    Chuck Officer <br>
	Sean Sharp <br>

-----------------------------------------------------------------*/
class ConfigManager : public TObject
{
	public:
		//////////
		// Constructor.
		//
		ConfigManager();
					
		//////////
		// Empty Destructor.
		//
		~ConfigManager();

		//////////
		// Process the command line args and read the config file.
		//
		// [in] inCmdLine - Command line arguments
		// [out] return - false if any errors occured, true otherwise
		//
		bool		Init(LPSTR inCmdLine);

		//////////
		// Switch scripts (all scripts should be specified in the config file).
		// This is only valid for the default, legacy interpreter; newer
		// interpreter modules ignore it.
		//
		// [in] inScriptNum - script number to switch to
		// [out] return - true on success, false otherwise
		//
		bool		SwitchScripts(int32 inScriptNum);
		
		//////////
		// Is the play media switch turned on?
		//
		// [out] return - true if set to play media, false otherwise
		//
		bool		PlayMedia(void)
				{ return (m_PlayMedia); }
		
		//////////
		// Get current script name.  This is only valid for the
		// default, legacy interpreter; newer interpreter modules
		// ignore it.
		//
		// [out] return - name of the current script
		//
		const char	*CurScript(void)
				{ return ((const char *) m_CurScript); } 
		
		//////////
		// Get current media path.
		//
		// [out] return - path where media files are located
		//
		const char	*CurMediaPath(void)
				{ return ((const char *) m_CurMediaDir); }
	
		//////////
		// Get media drive.
		//
		// [out] return - drive where media files may be found
		//
		const char	*MediaDrive(void)
				{ return ((const char *) m_MediaDrive); }
		
		//////////
		// Get DLS user name.
		//
		// [out] return - DLS username of the current user
		//
		const char	*DLSUser(void)
				{ return ((const char *) m_DLSUser); }

		//////////
		// Get full path for the specified video clip.
		//
		//	1. Have a full URL already (starts with http: or rtsp:), do nothing.
		//	2. Have a full path, do nothing.
		//	3. Have a partial path, put the installation directory on the front.
		//	4. Put the contents of _BaseVideoLocation on the front of the clip name.		
		//
		// [in] inName - name of the video clip (in path or URL form)
		// [out] return - full path for the specified video file
		//
		TString		GetVideoPath(TString &inName);
		
		//////////
		// Get full path for the specified audio clip.
		//
		//	1. Have a full URL already (starts with http: or rtsp:), do nothing.
		//	2. Have a full path, do nothing.
		//	3. Have a partial path, put the installation directory on the front.
		//	4. Put the contents of _BaseAudioLocation on the front of the clip name.		
		//
		// [in] inName - name of the audio clip (in path or URL form)
		// [out] return - full path for the specified audio file
		//
		TString		GetAudioPath(TString &inName);
		
		//////////
		// Get full path for the specified audio clip.
		//
		//	1. Have a full URL already (starts with http: or rtsp:), do nothing.
		//	2. Have a full path, do nothing.
		//	3. Have a partial path, put the installation directory on the front.
		//	4. Put the contents of _BaseAudioLocation on the front of the clip name.		
		//
		// [in] inName - name of the graphic (in path or URL form)
		// [out] return - full path for the specified graphic file
		//
		TString		GetGraphicsPath(TString &inName);
		
		//TString		GetPalettesPath(TString &inName);
		
		//TString		GetDataPath(TString &inName);
		
	private:
		//////////
		// Should media clips be played?
		//
		bool		m_PlayMedia;
		
		//////////
		// Number of 5L modules.
		//
		int32		m_NumModules;

		//	TArray		m_CDDrives;
		
		//////////
		// DLS Username pulled from user.profile file.
		//
		TString		m_DLSUser;

        /*** The following are static for a title ***/
        
		//////////
		// Filename for the config file.
		//
		TString		m_ConfigFile;
		
		//////////
		// Installation directory.
		//
		TString		m_InstallDir;
		
		//////////
		// Graphics directory.
		//
		TString		m_GraphicsDir;
		
		//////////
		// Local media directory.
		//
		TString		m_LocalMediaDir;
		
		//////////
		// Local media drive.
		//
		TString		m_MediaDrive;
		
		/*** the following two will change with every module ***/
		
		//////////
		// Current script name.
		//
		TString		m_CurScript;
		
		//////////
		// Current media directory.
		//
		TString		m_CurMediaDir;
		
		//////////
		// Config array.
		//
		Config		*m_Configs;
};

#endif // _Config_h_

/*
 $Log$
 Revision 1.4.8.2  2002/06/05 08:50:52  emk
 A small detour - Moved responsibility for script, palette and data directories
 from Config.{h,cpp} to FileSystem.{h,cpp}.

 Revision 1.4.8.1  2002/06/05 07:05:30  emk
 Began isolating the 5L-language-specific code in Win5L:

   * Created a TInterpreter class, which will eventually become the
     interface to all language-related features.
   * Moved ssharp's developer preference support out of Config.{h,cpp}
     (which are tighly tied to the language) and into TDeveloperPrefs.{h,cpp},
     where they will be isolated and easy to port to other platforms.

 Revision 1.4  2002/03/05 10:25:41  tvw
 Added new option to 5L.prefs to optionally allow multiple
 instances of 5L to run.

 Revision 1.3  2002/02/19 12:35:12  tvw
 Bugs #494 and #495 are addressed in this update.

 (1) 5L.prefs configuration file introduced
 (2) 5L_d.exe will no longer be part of CVS codebase, 5L.prefs allows for
     running in different modes.
 (3) Dozens of compile-time switches were removed in favor of
     having a single executable and parameters in the 5L.prefs file.
 (4) CryptStream was updated to support encrypting/decrypting any file.
 (5) Clear file streaming is no longer supported by CryptStream

 For more details, refer to ReleaseNotes.txt

 Revision 1.2  2002/01/23 20:39:20  tvw
 A group of changes to support a new stable build.

 (1) Only a single instance of the FiveL executable may run.

 (2) New command-line option "-D" used to lookup the installation directory in the system registry.
     Note: Underscores will be parsed as spaces(" ").
     Ex: FiveL -D HIV_Prevention_Counseling

 (3) Slow down the flash on buttpcx so it can be seen on
     fast machines.  A 200 mS pause was added.

 (4) Several bugfixes to prevent possible crashes when error
     conditions occur.

 Revision 1.1  2001/09/24 15:11:00  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.5  2000/08/08 19:03:40  chuck
 no message

 Revision 1.4  2000/04/07 17:05:15  chuck
 v 2.01 build 1

 Revision 1.3  2000/02/02 15:15:32  chuck
 no message

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
