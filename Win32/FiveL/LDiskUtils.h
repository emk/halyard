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

#if !defined (_LDiskUtils_h_)
#define _LDiskUtils_h_

//////////
// CD Drive Status Enumeration
//
typedef enum
{
	CDMounted,
	CDMountedWrongVolume,
	CDNotMounted
} CDStatus;


//////////
// Find the mount point for a volume given the name of the volume.
//
// [in] inVolumeName - the volume name
// [in/out] outMountPoint - when returning true, 
//				the mount point (drive mapping) where the volume was found
// [out] return - true if volume was found, false otherwise
//
bool		FindMountPoint(const TString &inVolumeName, TString &outMountPoint);

//////////
// Check if a volume is mounted.
//
// [in] inRootPath - path to the volume to be checked
// [in] inVolumeName - name of the volume needed
// [out] return - CDMounted if CD volume is mounted and the volume name matches<br>
//				  CDMountedWrongVolume if the CD volume is mounted, but the 
//						volume name does not match<br>	
//				  CDNotMounted if the CD no CD colume is mounted
//
CDStatus	VolIsMounted(const char *inRootPath, const TString &inVolumeName);

//bool		CDIsMounted(TString &inVolumeName, TString &outPath);

//////////
// Eject the CD-ROM drive.
//
void		EjectCD(void);

//////////
// Is the a CD in the CD-ROM drive?
// 
// [out] return - true if there is a CD in the drive, false otherwise
//
bool		CDInDrive(void);

#endif // _LDiskUtils_h_

/*
 $Log$
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
