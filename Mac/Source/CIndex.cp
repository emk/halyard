/*  CIndex.c

    Routines for reading script indices from disk and loading
    headers, macros and cards into memory. Some files may be
    encrypted; these are decrypted once they are read into
    memory.
*/

#include "debug.h"

#include <string.h>
#include <stdlib.h>

#include "CIndex.h"
#include "CCard.h"
#include "CHeader.h"
#include "CMacroManager.h"

#include "util.h"


/***********************

    GLOBAL VARIABLES

***********************/

extern int      XDrop, YDrop, DropColor, DropShadow;
extern int      Alignment, TextColor, HighLightColor;

extern char*	installDir;

CTextFileStream	*theScriptFile = NULL;

/**********************

    CLASS VARIABLES

**********************/

bool			CIndex::encrypted = FALSE;

/********************

    INDEX METHODS

********************/

//  Construct the Index. The Bnode ancestor handles setting the
//  name.
//
CIndex::CIndex(const char *name, long p1, long p2) : CBNode(name)
{                                                                     
    fStart = p1;
    fEnd = p2;
}

/***********************************************************************
 * Function: CIndex::SetScript
 *
 *  Parameter (null)
 * Return:
 *
 * Comments:
 *  Read the data that belongs to this index from the script.
 *  We're mucking with the insides of CString here. Be careful...
 ***********************************************************************/
void CIndex::SetScript()
{
	int32	err;
    int32	len;
    char	*p;

    if (!script.empty())
        return;

    len = (int)(fEnd - fStart) + 1;

    script.resize(len + 1);
    p = script.GetString();

	// read card from the script file:
	theScriptFile->SetMarker(fStart, streamFrom_Start);
	
    err = theScriptFile->GetBytes(p, len);
    
    if (err != noErr)
    {
        prerror("I/O error reading data for index %s", (char *) key);
        return;
    }

    //  Set the terminating 0 and tell the string to recalc its
    //  length since we did some direct manipulation.
    //
    p[len] = 0;
    script.update();

    if (encrypted)
        script.Decrypt(Cipher);
}

/***********************************************************************
 * Function: CIndex::FlushScript
 *
 *  Parameter (null)
 * Return:
 *
 * Comments:
 *  Simply delete the contents of the index
 ***********************************************************************/
void CIndex::FlushScript()
{
    if (!script.empty())
        script = "";
}

/***********************************************************************
 * Function: CIndex::GetScript
 *
 *  Parameter (null)
 * Return:
 *      Return the contents of "this"..
 * Comments:
 *
 ***********************************************************************/
char *CIndex::GetScript()
{
    return script.GetString();
}

/****************************

    INDEX MANAGER METHODS

****************************/

CIndexManager::CIndexManager() : CBTree()
{  
}

/*************************

    PUBLIC INITIALIZER

*************************/

//
//	KillIndex
//
void KillIndex(void)
{
	if (theScriptFile != NULL)
		delete theScriptFile;
}

/***********************************************************************
 * Function: InitIndex
 *
 *  Parameter fName
 *  Parameter fEncrypted        (true for Encryption)
 * Return:
 *
 * Comments:
 *  Initialize all managers from the single index file.
 ***********************************************************************/
bool InitIndex(FSSpec *scriptSpec, FSSpec *indexSpec, bool fEncrypted)
{
    CTextFileStream    	*theIndexFile;
    Int32        		numEntries, a, b, i;
    CString     		theName;

	// Check that the index file is more recent than the script file.
	if (not CheckFileDates(indexSpec, scriptSpec))
	{
		// make sure the script name is 0 terminated
		Int16	nameLen;
		
		nameLen = scriptSpec->name[0];
		scriptSpec->name[nameLen+1] = '\0';
		
		prerror("The index file for <%s> needs to be regenerated!!",
				&(scriptSpec->name[1]));
				
		return (FALSE);
	}
    
    CIndex::encrypted = fEncrypted;
		
	// cbo_fix - do something else when want encrypted file
    
    // open the files
    try
    {
	    theIndexFile = new CTextFileStream(*indexSpec);
	    theIndexFile->OpenDataFork(fsRdPerm);
	
	    theScriptFile = new CTextFileStream(*scriptSpec);
	    theScriptFile->OpenDataFork(fsRdPerm);
    }
    catch (const LException& inException) 
    {
    	// Couldn't open or find one of the files.
    	return (FALSE);
    }
	
	//    		
    //  Headers.
    //
    
    *theIndexFile >> numEntries;
    for (i = 0; i < numEntries; i++) 
    {
        *theIndexFile >> theName >> a >> b; 

        gHeaderManager.MakeNewIndex(theName, a, b);
    }
    if (i != numEntries)
    {
        prerror("Did not get the %d headers expected.", numEntries);
        return (FALSE);
    }

	//
    //  Macros.
    //
    *theIndexFile >> numEntries;
    for (i = 0; i < numEntries; i++) 
    {
        *theIndexFile >> theName >> a >> b;
        gMacroManager.MakeNewIndex(theName, a, b);
    }
    
    if (i != numEntries)
    {
        prerror("Did not get the %d macros expected.", numEntries);
        return (FALSE);
    }

	//
    //  Cards.
    //
    *theIndexFile >> numEntries;
    
    for (i = 0; i < numEntries; i++) 
    {
        *theIndexFile >> theName >> a >> b;
         	
        gCardManager.MakeNewIndex(theName, a, b);
    }
    if (i != numEntries)
	{
		prerror("Did not get the %d cards expected.", numEntries);
		return (FALSE);
	}
        
    theIndexFile->CloseDataFork();
	return (TRUE);
}

//
//	CheckFileDates - Make sure that the index file is more recent than the
//			script file.
//
bool CheckFileDates(FSSpec *inIndexSpec, FSSpec *inScriptSpec)
{
	CInfoPBRec	theIndexPB;
	CInfoPBRec	theScriptPB;
	OSErr		theErr;
	bool		retValue = true;
	
	theIndexPB.hFileInfo.ioFDirIndex = 0;
	theIndexPB.hFileInfo.ioNamePtr = (unsigned char *) inIndexSpec->name;
	theIndexPB.hFileInfo.ioVRefNum = inIndexSpec->vRefNum;
	theIndexPB.hFileInfo.ioDirID = inIndexSpec->parID;
	
	theErr = PBGetCatInfoSync(&theIndexPB);
	
	if (theErr != noErr)
		retValue = false;
	else
	{
		theScriptPB.hFileInfo.ioFDirIndex = 0;
		theScriptPB.hFileInfo.ioNamePtr = (unsigned char *) inScriptSpec->name;
		theScriptPB.hFileInfo.ioVRefNum = inScriptSpec->vRefNum;
		theScriptPB.hFileInfo.ioDirID = inScriptSpec->parID;
		
		theErr = PBGetCatInfoSync(&theScriptPB);
		
		if (theErr != noErr)
			retValue = false;
		else if (theScriptPB.hFileInfo.ioFlMdDat >= theIndexPB.hFileInfo.ioFlMdDat)
			retValue = false;
	}
		
	return (retValue);
	
}


