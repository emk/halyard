//
//	MacUtils.cpp
//

#include "MacUtils.h"

#include <string.h>

USING_NAMESPACE_FIVEL

bool PathToFSSpec(TString &inPath, FSSpec *inSpec)
{
	Str255		thePath;
	OSErr		err;
	bool		retValue = true;	// cbo - why is it not returning noErr?
	
	strcpy((char *) thePath, inPath.GetString());
	c2pstr((char *) thePath);
	
	if ((err = ::FSMakeFSSpec(0, 0, thePath, inSpec)) == noErr)
		retValue = true;
		
	return (retValue);
}

TString NameFromFSSpec(FSSpec *inSpec)
{
	TString		retStr;
	int32		nameLen;
	
	if (inSpec == NULL)
		return (retStr);
		
	nameLen = inSpec->name[0];
	for (int i = 1; i < (nameLen + 1); i++)
		retStr += (const char) (inSpec->name[i]);
		
	return (retStr);
}

TString PathFromFSSpec(FSSpec *inSpec)
{
	TString		retPath;
	CInfoPBRec	pbRec;
	Str255		dirName;
	OSErr		err;
	bool		firstTime = true;
	
	if (inSpec == NULL)
		return (retPath);

	// start off with a relative path to the file	
	retPath = ":";
	p2cstr(&inSpec->name[0]);
	retPath += ((const char *) (&inSpec->name[0]));
	c2pstr((char *) &inSpec->name[0]);	
	
	// set up the parameter block
	pbRec.dirInfo.ioNamePtr = dirName;
	pbRec.dirInfo.ioVRefNum = inSpec->vRefNum;
	pbRec.dirInfo.ioDrParID = inSpec->parID;
	pbRec.dirInfo.ioDrDirID = pbRec.dirInfo.ioDrParID;
	pbRec.dirInfo.ioFDirIndex = -1;
	
	// get the directory names
	while (pbRec.dirInfo.ioDrDirID != fsRtDirID) 
	{
		pbRec.dirInfo.ioDrDirID = pbRec.dirInfo.ioDrParID;
		err = PBGetCatInfo(&pbRec, false);
		if (err == noErr)
		{
			TString		tmpStr;
			TString		tmpStr2;
			
			p2cstr(dirName);
			tmpStr = (const char *) &dirName[0];
			
			// already have a ':' in front of the filename
			if (not firstTime)
				tmpStr += ":";
			else
				firstTime = false;
			
			tmpStr2 = retPath;
			retPath = tmpStr;
			retPath += tmpStr2;
		}	
	}
	
	return (retPath);
}
