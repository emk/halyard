//
// CConfig.cp - Class for getting the configuration 
//					of the machine we are running on.
//

#include "debug.h"

#include <OSUtils.h>

#ifdef DEBUG_5L
#include <iostream.h>
#endif

#include "Mac5L.h"
#include "CConfig.h"
#include "CString.h"
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
		prcaution("This program will only run under System 7 or later.");
		return (false);
	}
	
	if (not theConfiguration.hasQuickTime)
	{
		prcaution("QuickTime is not installed. Please reinstall the program.");
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
		prcaution("You need to have at least 256 colors to run this program.");
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
	
} // DefineConfiguration 


//
//	FillDataSpec - Fill in the volume ref and parent ID fields of the given
//			FSSpec with information about the Data directory.
//
bool CConfig::FillDataSpec(FSSpec *theSpec, char *inName)
{
	CString		fileName;
	bool		retValue = false;
	
	if ((theSpec != NULL) and (inName != NULL))
	{
		gModMan->GetMasterPath(fileName);
				
		fileName += "Data:";
		fileName += inName;
		
		retValue = FillSpec(theSpec, fileName);
	}
	
	return (retValue);
}

bool CConfig::FillScriptSpec(FSSpec *theSpec, const char *inName)
{
	CString		fileName;
	bool		retValue = false;
	
	if ((theSpec != NULL) and (inName != NULL))
	{
		gModMan->GetMasterPath(fileName);
		
		fileName += "Scripts:";
		fileName += inName;
		
		retValue = FillSpec(theSpec, fileName);
	}
	
	return (retValue);
}

bool CConfig::FillDebugSpec(FSSpec *theSpec, const char *inName)
{
	CString		fileName;
	bool		retValue = false;
	
	if ((theSpec != NULL) and (inName != NULL))
	{	
		gModMan->GetMasterPath(fileName);
		// don't use Debug directory as it doesn't always exist	
		//fileName += "Debug:";
		fileName += inName;

		retValue = FillSpec(theSpec, fileName);
	}
	
	return (retValue);
}

//
//	FillMovieSpec - Fill in the volume ref and parent ID fields of the given
//			FSSpec with information about the Movie directory.
//
bool CConfig::FillMovieSpec(FSSpec *theSpec, const char *inName, bool inLocal)
{
	CString		fileName;
	bool		retValue = false;
	
	if (theSpec != NULL)
	{
		if (inLocal)
		{
			gModMan->GetMasterPath(fileName);
			fileName += "Media:";
		}
		else
			gModMan->GetMoviePath(fileName);
		
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
	CString		fileName;
	bool		retValue = false;
	
	if (theSpec != NULL)
	{
		gModMan->GetMasterPath(fileName);
		
		fileName += "Graphics:";
		fileName += inName;
		// no longer have to add .pic as GetPicture() ensures that all
		// picture names end with it
		//fileName += ".pic";
		
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
	CString		fileName;
	bool		retValue = false;
	
	if (theSpec != NULL)
	{
		gModMan->GetMasterPath(fileName);
		
		fileName += "CLUTs:";
		fileName += inName;
		// no longer have to add .CLUT as GetPalette() ensures that all
		// palette names end with it
		//fileName += ".CLUT";

		retValue = FillSpec(theSpec, fileName);
	}
	
	return (retValue);
}

bool CConfig::FillSpec(FSSpec *theSpec, CString &inName)
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


