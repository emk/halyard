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

#include "KHeader.h"

#include <string.h>

#include <OSUtils.h>

#ifdef DEBUG
#include <iostream.h>
#endif

#include "KLogger.h"
#include "KString.h"

#include "CMac5LApp.h"
#include "CConfig.h"
#include "CModule.h"
#include "CVariable.h"

CConfig		*theConfig;				// global so everybody can query for stuff

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
	if (theConfiguration.hasROM128K)
		theConfiguration.hasHFS = TRUE;
	else
		theConfiguration.hasHFS = LMGetFSFCBLen() > 0;

	err = Gestalt(gestaltSystemVersion, &response);
	theConfiguration.systemVersion = (short)response;

	theConfiguration.hasQuickTime = (Gestalt(gestaltQuickTime, &response) == noErr);
	theConfiguration.quicktimeVersion = (short) response;
	
} // DefineConfiguration 


//
//	FillDataSpec - Fill in the volume ref and parent ID fields of the given
//			FSSpec with information about the Data directory.
//
bool CConfig::FillDataSpec(FSSpec *theSpec, const char *inName)
{
	KString		fileName;
	bool		retValue = false;
	
	if ((theSpec != NULL) and (inName != NULL))
	{
		fileName = gModMan->GetDataPath();
		fileName += inName;
		
		retValue = FillSpec(theSpec, fileName);
	}
	
	return (retValue);
}

bool CConfig::FillScriptSpec(FSSpec *theSpec, const char *inName)
{
	KString		fileName;
	bool		retValue = false;
	
	if ((theSpec != NULL) and (inName != NULL))
	{
		fileName = gModMan->GetScriptPath();
		fileName += inName;
				
		retValue = FillSpec(theSpec, fileName);
	}
	
	return (retValue);
}

bool CConfig::FillDebugSpec(FSSpec *theSpec, const char *inName)
{
	KString		fileName;
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
	KString		tmpName = inName;
	KString		fileName;
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
	KString		tmpName = inName;
	KString		fileName;
	bool		retValue = false;
	
	if (theSpec != NULL)
	{
		fileName = gModMan->GetCLUTPath(tmpName);

		retValue = FillSpec(theSpec, fileName);
	}
	
	return (retValue);
}

bool CConfig::FillSpec(FSSpec *theSpec, KString &inName)
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

/*
$Log$
Revision 1.5  2002/02/04 20:04:22  hamon
Updated Macintosh engine to compile under CodeWarrior 7.0 (Professional) and tested it lightly.  This will give us a base for future development and testing.

* Changed IntN types to SIntN types.
* Added some new libraries.
* Renamed LTimerTask to CTimerTask to avoid conflict with built-in Metrowerks class.
* Included C <string.h> header as needed.  This also affects one file in the Common directory, which we'll need to merge into Stable/ later on.
* We no longer init KString with 'nil', because of function overloading errors.  We use "", the empty string, instead.  We *think* this is a safe change, but the KString code is pretty iffy.
* Replaced a call to 'max', which can no longer be found in the system headers, with an explicit if statement.

Changes by Elizabeth Hamon with help from Eric Kidd.  Code reviewed by Eric.

Revision 1.4  2000/05/11 12:56:09  chuck
v 2.01 b1

Revision 1.3  2000/02/01 16:50:49  chuck
Fix cursors on overlapping touch zones.

Revision 1.2  1999/10/07 12:51:14  chuck
Initial revision

*/
