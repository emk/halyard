// -*- Mode: C++; tab-width: 4; -*-
/////////////////////////////////////////////////////////////////////////////////////////
//
//		(c) Copyright 1999, Trustees of Dartmouth College. All rights reserved
//			Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//			$Date$
//			$Revision$
//
//
/////////////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////////////////
//
// CConfig.cp - Class for getting the configuration 
//					of the machine we are running on.
//

#include "THeader.h"
#include "MacCarbonGlue.h"

#include <string.h>

#include <OSUtils.h>

#ifdef DEBUG
#include <iostream.h>
#endif

#include "TLogger.h"
#include "TString.h"

#include "CMac5LApp.h"
#include "CConfig.h"
#include "CModule.h"
#include "TVariable.h"

USING_NAMESPACE_FIVEL

CConfig		*FiveL::theConfig;				// global so everybody can query for stuff

//
//	CConfig:
//
CConfig::CConfig()
{
	DefineConfiguration();
	DefineDevices();
}

//
//	~CConfig
//
CConfig::~CConfig()
{
}

//
//	DumpConfiguration - Only for debugging.
//
void CConfig::DumpConfiguration(void)
{
}

//
//	CheckConfig - Check the configuration.
//
bool CConfig::CheckConfig(void)
{
	if (theConfiguration.systemVersion < 0x0700)
	{
		gLog.Caution("This program will only run under System 7 or later.");
		return (false);
	}
	
	if (not theConfiguration.hasQuickTime)
	{
		gLog.Caution("QuickTime is not installed. Please reinstall the program.");
		return (false);
	}		
	
	return (CheckDevices());
}

//
//	CheckDevices 
//
bool CConfig::CheckDevices(void)
{
//	int32	memReq;
	
	if (bitDepth < 8)
	{
		gLog.Caution("You need to have at least 256 colors to run this program.");
		return (false);
	}
	
	// check memory requirements for this depth
//	if (bitDepth == 8)
//		memReq = 3500000;
//	else if (bitDepth == 16)
//		memReq = 4000000;
//	else
//		memReq = 6000000;
		
//	if (memReq > memAlloc)
//	{
//		if (bitDepth < 32)
//			(void) ::CautionAlert(2050, nil);
//		else
//			(void) ::CautionAlert(2060, nil);
//			
//		return (false);
//	}
	
	return (true);
} 


//
//	DefineDevices
//
void CConfig::DefineDevices(void)
{
//	ProcessSerialNumber	serNum;
//	ProcessInfoRec		infoRec;	
	PixMapHandle		pixMap;
	//GDHandle			device;
	//int32				area;
//	OSErr				err;
	
	
	mainDevice = ::GetMainDevice();
	//numDevices = CountAvailableDevices();
	mainRect = (**mainDevice).gdRect;
	pixMap = (**mainDevice).gdPMap;
	bitDepth = (**pixMap).pixelSize;
	
//	serNum.highLongOfPSN = 0;
//	serNum.lowLongOfPSN = kCurrentProcess;
//	err = ::GetProcessInformation(&serNum, &infoRec);
//	if (err == noErr)
//		memAlloc = infoRec.processSize;
//	else
//		memAlloc = 3500000;	// assume it is the smallest
	
//		for (device = GetDeviceList(); device != NULL; device = GetNextDevice(device))
//		{
//			if ((TestDeviceAttribute(device, screenDevice)
//			and (TestDeviceAttribute(device, screenActive)))
//			{
//				
//					
//				area = ((long)(*device)->gdRect.bottom - (*device)->gdRect.top)
//					 * ((long)(*device)->gdRect.right - (*device)->gdRect.left);
//				
//						 
//			}

//		}
}


//
//	GetScreenRect
//
Rect CConfig::GetScreenRect(void)
{
	return (mainRect);
}

//----------------------------------------------------------------------------------------
// DefineConfiguration: 
//----------------------------------------------------------------------------------------

void CConfig::DefineConfiguration(void)
{
	OSErr err;
	long response;

	err = Gestalt(gestaltVersion, &response);
	theConfiguration.environsVersion = (short)response;

	err = Gestalt(gestaltMachineType, &response);
	theConfiguration.machineType = (short)response;

	theConfiguration.hasROM128K = theConfiguration.machineType >= gestaltMac512KE;
	
#if TARGET_API_MAC_CARBON
	theConfiguration.hasHFS = TRUE;
#else
	if (theConfiguration.hasROM128K)
		theConfiguration.hasHFS = TRUE;
	else
		theConfiguration.hasHFS = LMGetFSFCBLen() > 0;
#endif

	err = Gestalt(gestaltSystemVersion, &response);
	theConfiguration.systemVersion = (short)response;

	theConfiguration.hasQuickTime = (Gestalt(gestaltQuickTime, &response) == noErr);
	if (!theConfiguration.hasQuickTime)
		gVariableManager.SetLong("_QuickTimeVersion", -1);
	else
	{
		// QuickTime Version is stored in the upper 16 bits of the long response in hexadecimal form. 
		// First we shift to those bits. This may not match the Windows convention for reporting the
		// version number. This version reports 5.0.2 as 50002. 
		// TODO: Check this against the Windows engine for consistency. 
		long quickTimeV = ((unsigned long) response >> 16); 					
		long major = ((quickTimeV & 0xFF00) >> 8); 
		short minor = ((quickTimeV & 0x00F0) >> 4);
		short revision = (quickTimeV & 0x000F);	
		long fullVersion = (major * 10000) + (minor * 100) + revision;
		gVariableManager.SetLong("_QuickTimeVersionMajor", major);
		gVariableManager.SetLong("_QuickTimeVersionMinor", minor);
		gVariableManager.SetLong("_QuickTimeVersionRevision", revision);
		gVariableManager.SetLong("_QuickTimeVersion", fullVersion);
	}
	
} // DefineConfiguration 


//
//	FillDataSpec - Fill in the volume ref and parent ID fields of the given
//			FSSpec with information about the Data directory.
//
bool CConfig::FillDataSpec(FSSpec *theSpec, const char *inName)
{
	TString		fileName;
	bool		retValue = false;
	
	if ((theSpec != NULL) and (inName != NULL))
	{
		fileName = gModMan->GetDataPath();
		fileName += inName;
		
		retValue = FillSpec(theSpec, fileName);
	}
	
	return (retValue);
}

bool CConfig::FillDebugSpec(FSSpec *theSpec, const char *inName)
{
	TString		fileName;
	bool		retValue = false;
	
	if ((theSpec != NULL) and (inName != NULL))
	{	
		fileName = gModMan->GetMasterPath();
		fileName += inName;

		retValue = FillSpec(theSpec, fileName);
	}
	
	return (retValue);
}

//
//	FillGraphicsSpec - Fill in the volume ref and parent ID fields of the given
//			FSSpec with information about the Graphics directory.
//
bool CConfig::FillGraphicsSpec(FSSpec *theSpec, const char *inName)
{
	TString		tmpName = inName;
	TString		fileName;
	bool		retValue = false;
	
	if (theSpec != NULL)
	{
		fileName = gModMan->GetGraphicsPath(tmpName);
				
		retValue = FillSpec(theSpec, fileName);
	}
	
	return (retValue);
}

//
//	FillCLUTSpec - Fill in the volume ref and parent ID fields of the given
//			FSSpec with information about the CLUTs directory.
//
bool CConfig::FillCLUTSpec(FSSpec *theSpec, const char *inName)
{
	TString		tmpName = inName;
	TString		fileName;
	bool		retValue = false;
	
	if (theSpec != NULL)
	{
		fileName = gModMan->GetCLUTPath(tmpName);

		retValue = FillSpec(theSpec, fileName);
	}
	
	return (retValue);
}

bool CConfig::FillSpec(FSSpec *theSpec, TString &inName)
{
	Str255		thePath;
	OSErr		err;
	bool		retValue = false;
	
	strcpy((char *) thePath, inName.GetString());
	c2pstr((char *) thePath);
	
	if ((err = ::FSMakeFSSpec(0, 0, thePath, theSpec)) == noErr)
		retValue = true;
		
	return (retValue);
}



	
//----------------------------------------------------------------------------------------
// MAGestaltAttribute: 
//----------------------------------------------------------------------------------------

Boolean CConfig::MAGestaltAttribute(OSType itsAttr, short itsBit)
{
	long response;

	return (Gestalt(itsAttr, &response) == noErr) && (((response >> itsBit) & 1) != 0);
	
} // MAGestaltAttribute 

#if !TARGET_API_MAC_CARBON
//----------------------------------------------------------------------------------------
// NumToolboxTraps: InitGraf is always implemented (trap 0xA86E). If the trap table is
// big enough, trap 0xAA6E will always point to either Unimplemented or some other trap,
// but will never be the same as InitGraf. Thus, you can check the size of the trap table
// by asking if the address of trap 0xA86E is the same as 0xAA6E.
//----------------------------------------------------------------------------------------
short CConfig::NumToolboxTraps(void)
{
	if (NGetTrapAddress(_InitGraf, ToolTrap) == NGetTrapAddress(0xAA6E, ToolTrap))
		return 0x200;
	else
		return 0x400;
} // NumToolboxTraps 

//----------------------------------------------------------------------------------------
// TrapExists: 
//----------------------------------------------------------------------------------------
Boolean CConfig::TrapExists(short theTrap)
{
	TrapType theTrapType = GetTrapType(theTrap);
	short localTrap = theTrap;					// since theTrap is a const
	if (theTrapType == ToolTrap)
	{
		localTrap = (localTrap & 0x07FF);
		if (localTrap >= NumToolboxTraps())
			localTrap = _Unimplemented;
	}

	return NGetTrapAddress(_Unimplemented, ToolTrap) != NGetTrapAddress(localTrap, theTrapType);
} // TrapExists 

//----------------------------------------------------------------------------------------
// GetTrapType: 
//----------------------------------------------------------------------------------------

TrapType CConfig::GetTrapType(short theTrap)
{
	// OS traps start with A0, Tool with A8 or AA. 
	if ((theTrap & 0x0800) == 0)				// per D.A.
		return OSTrap;
	else
		return ToolTrap;
} // GetTrapType 

#endif // !TARGET_MAC_API_CARBON

/*
$Log$
Revision 1.9  2002/06/20 16:32:57  emk
Merged the 'FiveL_3_3_4_refactor_lang_1' branch back into the trunk.  This
branch contained the following enhancements:

  * Most of the communication between the interpreter and the
    engine now goes through the interfaces defined in
    TInterpreter.h and TPrimitive.h.  Among other things, this
    refactoring makes will make it easier to (1) change the interpreter
    from 5L to Scheme and (2) add portable primitives that work
    the same on both platforms.
  * A new system for handling callbacks.

I also slipped in the following, unrelated enhancements:

  * MacOS X fixes.  Classic Mac5L once again runs under OS X, and
    there is a new, not-yet-ready-for-prime-time Carbonized build.
  * Bug fixes from the "Fix for 3.4" list.

Revision 1.8.6.2  2002/06/18 21:57:02  emk
3.3.4.8 - Added (BODY ...) command on Mac, fixed arguments of BUTTPCX, TOUCH,
and KEYBIND to match Win32 engine, and refactored Mac engine to more-or-less
respect the TInterpreter interface.

Things to test: REDOSCRIPT, redo-REDOSCRIPT (feed REDOSCRIPT a bogus script,
try to fix it, then run REDOSCRIPT again), TOUCH, BUTTPCX, ORIGIN.

Some low-level details:

  - Added a KillCurrentCard method to the TInterpreter interface.  This
    works a lot like Pause, but it cannot be resumed.
  - Added a rough-cut TMac5LInterpreter class (with some methods stubbed
    out, because they are not needed on the Mac--we should look at
    this API in detail and formalize it sometime after 3.4).
  - Modified CTouchZone to take TCallback objects.
  - Modified CPlayerView's keybinding support to take TCallback objects
    (and to use a std::map instead of a PowerPlant list class).
  - Began to separate special forms (IF, BODY, EXIT, RETURN) from other
    commands.
  - Moved ReadSpecialVariable_* methods out of CCard and into CMac5LApp.
  - Made sure CMac5LApp::mReDoReDo got initialized to false.
  - Merged OpenScript and OpenScriptAgain into one function.

Revision 1.8.6.1  2002/06/15 01:06:55  emk
3.3.4.7 - Carbonization of Mac build, support for running non-Carbonized build
in MacOS X's OS 9 emulator, and basic support for 5L.prefs on the Mac.  The
Carbon build isn't yet ready for prime time--see BugHunt for details--but it
is good enough to use for engine development.

* Language changes

  - CHECKDISC is gone; use CHECKVOL instead.
  - EJECT is disabled in the Carbon build, because Carbon has no way to
    identify CD drives reliably.  EJECT still works in the regular build.
  - Gamma fades are ignored in the Carbon build.
  - KEYBINDs must now be accessed with the Command key only--not Option.

* Things to test

Please be hugely brutal to 5L; this is a big update.

  - 8-bit systems, palettes, ORIGIN, EJECT on the non-Carbon build.

* Internal changes

  - TException class (and all subclasses) now take a __FILE__ and __LINE__
    parameter.  This is ugly, but it allows me to debug 5L exceptions even
    without a working debugger (under the OS 9 emulator, for example).
  - FileSystem::Path::(DoesExist|IsRegularFile|IsDirectory) now rely on
    native MacOS File Manager calls instead of the broken MSL stat()
    function (which fails in the OS 9 emulator).
  - The ImlUnit test harness flushes its output more often.
  - Many data structure accessors (and such functions as c2pstr) have been
    replaced by their Carbon equivalents.
  - We now use PowerPlant accessors to get at the QuickDraw globals.
  - We now use PowerPlant calls in place of ValidRect and InvalRect.
  - Some very nasty code which set the palettes of our offscreen GWorlds
    has been removed (offscreen GWorlds have CLUTs, not palettes!).
    The various drawing commands now use gPaletteManager to map indexes
    to RGBColor values, and RGBForeColor to set the color--no more calls
    to ::PmForeColor on offscreen GWorlds, thank you!
  - The CMenuUtil code (which used low-memory system globals to hide
    and show the menu bar) has been removed entirely and replaced by
    calls to HideMenuBar and ShowMenuBar (which are present in 8.5 and
    Carbon).  This is much simpler, nicer, more portable and safer.
  - A bunch of code which had been disabled with #ifdefs has been
    removed entirely.  This mostly related to palettes and an obsolete
    version of the fade code which used GWorlds.
  - Code which used ROM-based KCHR resources to map option keys back to
    their unmodified key caps has been removed.  This means KEYBINDs
    can only be accessed using the Command key.
  - We assume Carbon systems always support the HFS file system (duh).
  - We use PowerPlant glue to access either StandardFile or Navigation
    Services, under OS 8/9 and Carbon, respectively.
  - Some old subroutines in CModuleManager appeared to have been
    snarfed from More Files, an old Mac utility library.  These have
    been moved into MoreFiles.{h,cpp}.

* Known Carbon Problems

Fades, ejecting CD-ROMs and playing QuickTime movies are all broken in
the Carbon build.  Douglas has found a problem with ORIGIN.  It looks
like we should continue to ship the OS 9 build for use with MacOS X,
at least for next few months.

Revision 1.8  2002/05/15 11:05:27  emk
3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
is available, and several subsystems have been refactored.  For more
detailed descriptions, see the CVS branch.

The merged Mac code hasn't been built yet; I'll take care of that next.

Revision 1.7.4.2  2002/04/23 11:36:07  emk
More merge-related fixes.

1) Removed all code which treats scripts as FSSpecs, and replaced it with code that treats scripts as filenames, minus the path and extension.  This mirrors how TFileIndex wants to work.  A side effect: Dragging scripts to the engine will no longer work (if it ever did).

2) Use TVersion to get the version number.

3) KLogger -> TLogger changes.

Revision 1.7.4.1  2002/04/22 13:20:08  emk
Major Mac cleanups:

- We use subprojects to recursively build 5L and its support libraries.  This means no more opening up four different projects, etc., just to do a build.

- Search & replaced class names for merged classes.  This doesn't quite work or build yet, but I'm working on a branch, so that's OK.

- Other Mac build fixes.

Tomorrow: Get things running again, make a test binary, and continue merging.

Revision 1.7  2002/03/11 17:50:10  hamon
 Added logging of _QuickTimeVersion.
_QuickTimeVersionMajor returns major number, _QuickTimeVersionMinor returns minor number,_QuickTimeVersionRevision returns revision number,
  and _QuickTimeVersion returns version number. Version 5.0.2 will be returned as 50002 and 12.14.10 would be returned as 121410 (if this version ever exists). 
This numbering system DOES NOT match the Windows. Consistency between the two (likely involving a change in the Windows, since it seems likely to be broken) would be good.
Changes by Elizabeth, okayed by Eric.

Revision 1.6  2002/03/04 15:41:34  hamon
Changed calls to KString, KRect etc to TString, TRect, etc to reflect new names of merged common code.

Added namespace support for compiler. New files that use TString or TArray need to specify the FiveL namespace to differentiate between Code Warrior's template string and array classes and FiveL's common string and array classes.

Changes by Elizabeth and Eric, okayed by Eric.

Revision 1.5  2002/02/04 20:04:22  hamon
Updated Macintosh engine to compile under CodeWarrior 7.0 (Professional) and tested it lightly.  This will give us a base for future development and testing.

* Changed IntN types to SIntN types.
* Added some new libraries.
* Renamed LTimerTask to CTimerTask to avoid conflict with built-in Metrowerks class.
* Included C <string.h> header as needed.  This also affects one file in the Common directory, which we'll need to merge into Stable/ later on.
* We no longer init TString with 'nil', because of function overloading errors.  We use "", the empty string, instead.  We *think* this is a safe change, but the TString code is pretty iffy.
* Replaced a call to 'max', which can no longer be found in the system headers, with an explicit if statement.

Changes by Elizabeth Hamon with help from Eric Kidd.  Code reviewed by Eric.

Revision 1.4  2000/05/11 12:56:09  chuck
v 2.01 b1

Revision 1.3  2000/02/01 16:50:49  chuck
Fix cursors on overlapping touch zones.

Revision 1.2  1999/10/07 12:51:14  chuck
Initial revision

*/
