//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999,2000 Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// LDiskUtils.cpp : 
//

#include "stdafx.h"
#include <mmsystem.h>

#include "TCommon.h"
#include "TString.h"

#include "Globals.h"
//#include "TList.h"

#include "LDiskUtils.h"

//TList<TString>	cdDriveList;
//
//void FillCDDriveList(void)
//{
//	TString		theDisk;
//	char		buffer[256];
//	int			driveStrLen;
//
//	cdDriveList.RemoveAll();
//
//	driveStrLen = ::GetLogicalDriveStrings(256, buffer);
//	if (driveStrLen != 0)
//	{
//		for (int i = 0; i < driveStrLen; i++)
//		{
//			if (buffer[i] == '\0')
//			{
//				// have a drive
//				if (::GetDriveType(theDisk.GetString()) == DRIVE_CDROM)
//					cdDriveList.AddTail(theDisk);
//				else
//					theDisk.Empty();
//			}
//			else
//				theDisk += buffer[i];
//		}
//	}
//}
//
//int32 CDDriveCount(void)
//{
//	return (cdDriveList.Count());
//}

//void ResetCDDriveList()
//{
//	cdDriveList

//
//	FindMountPoint - 
//
bool FindMountPoint(const TString &inVolumeName, TString &outMountPoint)
{
	TString		theDisk;
	char		buffer[256];
	int			driveStrLen;
	CDStatus	status;

	outMountPoint.Empty();

	driveStrLen = ::GetLogicalDriveStrings(256, buffer);
	if (driveStrLen != 0)
	{
		for (int i = 0; i < driveStrLen; i++)
		{
			if (buffer[i] == '\0')
			{
				// have a drive
				//if (::GetDriveType(theDisk.GetString()) == DRIVE_CDROM)
				{
					// see if this is our volume
					status = VolIsMounted(theDisk.GetString(), inVolumeName);
					if (status == CDMounted)
					{
						// found it, return the drive mapping
						outMountPoint = theDisk;
						return (true);
					}
				}
				//else
					theDisk.Empty();
			}
			else
				theDisk += buffer[i];
		}
	}

	return (false);
}

//
//	VolIsMounted - 
//	
CDStatus VolIsMounted(const char *inRootPath, const TString &inVolumeName)
{
	CDStatus	retValue;
	char	theBuf[255];
	DWORD	theBufLen = 255;
	DWORD	theMaxLen;
	DWORD	theSysFlags;
	char	theNameBuf[32];
	DWORD	theNameBufLen = 32;

	if (::GetVolumeInformation(inRootPath, theBuf, theBufLen, 
			NULL, &theMaxLen, &theSysFlags, theNameBuf, theNameBufLen))
	{
		// now compare to see if this is it
		if (inVolumeName.Compare(theBuf, false))
			retValue = CDMountedWrongVolume;
		else
			retValue = CDMounted;
	}
	else
		retValue = CDNotMounted;

	return(retValue);
}


void EjectCD(void)
{
	MCI_OPEN_PARMS 	mciOpenParms;
    DWORD			retValue;

	// Open a compact disc device by specifying the device name
	mciOpenParms.lpstrDeviceType = "cdaudio";
	if (retValue = mciSendCommand(NULL, MCI_OPEN, MCI_OPEN_TYPE, (DWORD)(LPVOID) &mciOpenParms))
	{
		gDebugLog.Log("Couldn't open CD device, <%ld>", retValue);
	//	if (mciGetErrorString(retValue, errorBuff, 256))
	//		gDebugLog.Log("Error string <%s>", errorBuff);
		return;
	}

	// eject
	if (retValue = mciSendCommand(mciOpenParms.wDeviceID, MCI_SET, MCI_SET_DOOR_OPEN, NULL))
	{
		gDebugLog.Log("Command to open CD door failed, <ld>", retValue);
//		if (mciGetErrorString(retValue, errorBuff, 256))
//			gDebugLog.Log("Error string <%s>", errorBuff);
		return;
	}

	mciSendCommand(mciOpenParms.wDeviceID, MCI_CLOSE, 0, NULL);
}

bool CDInDrive(void)
{
	MCI_OPEN_PARMS 		mciOpenParms;
	MCI_STATUS_PARMS	mciStatusParms;
	DWORD				mciStatus;
	bool				retValue = true;
	
	// check with MCI if there is a disc in the drive
	mciOpenParms.lpstrDeviceType = "cdaudio";
	mciStatus = ::mciSendCommand(NULL, MCI_OPEN, 
		MCI_OPEN_TYPE | MCI_OPEN_SHAREABLE, (DWORD)(LPVOID) &mciOpenParms);

	if (mciStatus == 0)
	{
		// success
		// try to send status command
		mciStatusParms.dwItem = MCI_STATUS_MEDIA_PRESENT;
		
		if (mciStatus = ::mciSendCommand(mciOpenParms.wDeviceID, MCI_STATUS, MCI_STATUS_ITEM, (DWORD)(LPVOID) &mciStatusParms))
		{
			gDebugLog.Log("CDInDrive: couldn't get status of CD device, <%ld>", retValue);
				//	if (mciGetErrorString(retValue, errorBuff, 256))
				//		gDebugLog.Log("Error string <%s>", errorBuff);
			// assume no disc in drive
			retValue = false;

//            do_jump = true; 
//            jump_card = no_disc;
		}
		else
		{
			if (not mciStatusParms.dwReturn)
			{
				// no CD in the drive, nothing to check
				gDebugLog.Log("CDInDrive: no CD in the drive, jumping");
				retValue = false;

//				do_jump = true; 
//				jump_card = no_disc;
			}
			else
				retValue = true;	// have a CD in the drive
		}
		
		::mciSendCommand(mciOpenParms.wDeviceID, MCI_CLOSE, 0, NULL);
	}
	else if (mciStatus == MCIERR_MUST_USE_SHAREABLE)
	{		
		// this is OK, somebody else has it open, assume it is closed
		retValue = true;

		//do_jump = false;
	}
	else
	{
		// error
		gDebugLog.Log("CDInDrive: couldn't open CD device, <%ld>", retValue);
		retValue = false;

	//	do_jump = true;
	//	jump_card = no_disc;
	}

	return (retValue);
}

/*
 $Log$
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

 Revision 1.1  2000/08/09 14:37:06  chuck
 2.01 Build 5

*/
