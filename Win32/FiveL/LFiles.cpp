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
// LFiles.cpp : 
//

#include "stdafx.h"

#include "TString.h"
#include "LFiles.h"
#include "LUtil.h"
#include "Globals.h"

#define     BUFFER_SIZE     512

//  Storage space for ReadUntil.

static char     buffer[BUFFER_SIZE];

/********************

    LFILE METHODS

********************/

LFile::LFile()
{
	itsName = "";
	itsFile = NULL;
}

//  Open the given file for reading and writing.
//
LFile::LFile(const char *filename, FileKind fKind)
{
    int     fMode;

    itsKind = fKind;
    itsName = filename;
    switch (itsKind) 
    {
        case fWriteAppend:
            fMode = ios::in | ios::out | ios::ate;
            break;

        case fWriteNew:
            fMode = ios::out;
            break;

        case fReadOnly:
            fMode = ios::in;
            break;
    }

    itsFile = new fstream(filename, fMode);
    if (itsFile->fail())
        gLog.Log("Error opening file %s.", filename);
}

//  Cleanup. Destroy the file object.
//
LFile::~LFile()
{
    delete itsFile;
}

/***********************************************************************
 * Function: LFile::Match
 *
 *  Parameter aName
 * Return:
 *   Return true if aName is the file's name.
 * Comments:
 *      Beautiful!  this shit only used once ^$%&#!
 ***********************************************************************/
bool LFile::Match(const char *aName)
{
    return itsName.Equal(aName, false);
}

/***********************************************************************
 * Function: LFile::Read
 *
 *  Parameter str
 * Return:
 *
 * Comments:
 *      Read the next word as determined by whitespace.
 ***********************************************************************/
void LFile::Read(TString &str)
{
    if (itsKind != fReadOnly)
	{
        gLog.Log("Error: File %s is write-only.", itsName.GetString());
		return;
	}

    if (itsFile->eof())
    {
        str = ""; 
#ifdef _DEBUG_FILES
		gDebugLog.Log("read: at EOF, got bupkus");
#endif
	}
    else 
    {
        *itsFile >> str;
        if (itsFile->fail())
		{
            gLog.Log("Read error on file %s.", itsName.GetString());
			return;
		}

#ifdef _DEBUG_FILES
		gDebugLog.Log("read: got <%s>", str.GetString());
#endif
    }
}

/***********************************************************************
 * Function: LFile::ReadUntil
 *
 *  Parameter str (where to put it)
 *  Parameter delim  (If delim == 0, read until EOF.)
 * Return:
 *
 * Comments:
 *   Read until the given delimiter. Return everything up to
 *  the delimiter, and throw the delimiter away.
 ***********************************************************************/
void LFile::ReadUntil(TString &str, unsigned char delim)
{
    if (itsKind != fReadOnly)
	{
        gLog.Log("Error: File %s is write-only.", itsName.GetString());
		return;
	}

    ReadUntilCore(str, delim);
}

/***********************************************************************
 * Function: LFile::ReadUntilCore
 *
 *  Parameter str
 *  Parameter delim
 * Return:
 *
 * Comments:
 *  Main read until code. Functionally identical to ReadUntil
 ***********************************************************************/
void LFile::ReadUntilCore(TString &str, unsigned char delim)
{
    int     done = false;

    str = "";
    done = itsFile->eof();

#ifdef _DEBUG_FILES
	if (done)
		gDebugLog.Log("readuntil: at EOF");
#endif
	//
    //  Read in BUFFER_SIZE sized chunks at a time.
    //
    while (!done) 
    {
        //TAB & SPACE eating function.
        itsFile->eatwhite();
        itsFile->getline(buffer, BUFFER_SIZE, delim);
        
        if (itsFile->bad())
		{
            gLog.Log("Error: Read error on file %s.", itsName.GetString());
			return;
		}

        str += buffer;

        if (itsFile->gcount() < BUFFER_SIZE - 1) 
        	done = true;
        if (itsFile->eof()) 
        	done = true;
    }
#ifdef _DEBUG_FILES
	gDebugLog.Log("readuntil: returned <%s>", str.GetString());
#endif
}

/***********************************************************************
 * Function: LFile::Write
 *
 *  Parameter data  (what to write)
 * Return:
 *
 * Comments:
 *  Write some data to the file. Convert \t and \n to tab and
 *  newline characters respectively.
 ***********************************************************************/
void LFile::Write(TString &data)
{
    const char   *p;
    char		ch;

    if (itsKind == fReadOnly)
	{
        gLog.Log("Error: File %s is read-only.", itsName.GetString());
		return;
	}

    p = data.GetString();
    while (ch = *p++) 
    {
        switch (ch) 
        {
            case '\\':			// Escaped control char
                ch = *p++;
                if (ch == 0) return;
                if (ch == 't') 
                {
                    *itsFile << '\t';
                    break;
                }
                if (ch == 'n') 
                {
                    *itsFile << endl;
                    break;
                }

            default:
                *itsFile << ch;
                break;
        }

        if (itsFile->fail())
		{
            gLog.Log("Write error on file %s.", itsName.GetString());
			return;
		}
    }
}

/***********************************************************************
 * Function: LFile::Lookup
 *
 *  Parameter searchString
 *  Parameter numFields
 * Return:
 *
 * Comments:
 *      Read until we find the given search string at the start of a
 *  line or we hit the end of file.
 ***********************************************************************/
void LFile::Lookup(TString &searchString, int numFields)
{
    TString     theField, comparison;
    int         count;
    int         done = false;

    itsFile->seekg(0);  //  Reset file pointer.

    while (!done) 
    {
        comparison = "";

        //  Read the first n fields of the record for comparison.
        //
        for (count = 0; count < numFields; count++) 
        {
            ReadUntilCore(theField, '\t');
            if (count > 0)
                comparison += '\t';
            comparison += theField;
        }

        //  Now compare. Case insensitive.
        //
        if (searchString.Equal(comparison, false))  
        {
#ifdef _DEBUG_FILES
			gDebugLog.Log("lookup: found it in <%s>", comparison.GetString());
#endif
            done = true;
        }
#ifdef _DEBUG_FILES
		else
			gDebugLog.Log("lookup: didn't find in <%s>", comparison.GetString());
#endif

        if (itsFile->eof())
            done = true;

        //  If not a match, read until the next line.
        //
        if (!done)
        {
            ReadUntilCore(theField, '\n');
#ifdef _DEBUG_FILES
			gDebugLog.Log("lookup: skip over <%s> to end of line", theField.GetString());
#endif
		}
    }
}

void LFile::Lookup(const char *searchString, int numFields)
{
	TString str = searchString;
	Lookup(str, numFields);
}

/***********************************************************************
 * Function: LFile::Rewrite
 *
 *  Parameter searchString (replace entry starting w/. this)
 *  Parameter numFields     (match NumFields records)
 * Return:
 *
 * Comments:
 *  We want to rewrite a record. To do this, we move the record to
 *  the end of the file and let the scriptor append data with write
 *  commands.
 ***********************************************************************/
void LFile::Rewrite(TString &searchString, int numFields)
{
    TString     theLine;
    TString     tempName("temp5L.xxx");
    int         done = false;
    fstream     *tempFile;

    if (itsKind != fWriteAppend)
	{
        gLog.Log("Error: Rewrite expects WRITEAPPEND files.");
		return;
	}

    //  Add a tab to the end of the search string so that
    //   "Smith TAB 1" doesn't find "Smith TAB 11". Since we only
    //  check that the searchString begins the record line
    //  this could happen.
    //
    searchString += '\t';

    tempFile = new fstream(tempName, ios::out);

    itsFile->seekg(0);  //  Reset file pointer.

    while (!done) 
    {
        //  Copy all lines that don't match to the temp file.
        //
        ReadUntilCore(theLine, '\n');
        if (theLine.IsEmpty()) 
        {
            done = true;
        } 
        else if (theLine.StartsWith(searchString, false) == false)
        {
            *tempFile << theLine << endl;
#ifdef _DEBUG_FILES
			gDebugLog.Log("rewrite: writing <%s>", theLine.GetString());
#endif
		}
#ifdef _DEBUG_FILES
		else
			gDebugLog.Log("rewrite: saving <%s>", theLine.GetString());
#endif

        if (itsFile->eof())
            done = true;
    }

    //  Add the desired record information.
    *tempFile << searchString; 
    
#ifdef _DEBUG_FILES
	gDebugLog.Log("rewrite: now write <%s>", searchString.GetString());
#endif

    //  Close both files.
    delete tempFile;
    delete itsFile;

	// Delete the original file.
	::DeleteFile(itsName.GetString());

	// Copy the temp file to the original file.
	::CopyFile(tempName.GetString(), itsName.GetString(), false);

	// Delete the temp file.
	::DeleteFile(tempName.GetString());

	// Open up the new file.
    itsFile = new fstream(itsName, ios::out | ios::in | ios::ate);
} 

//
//	AtEOF - Return true if we are at EOF marker, false otherwise.
//
bool LFile::AtEOF(void)
{
	if (itsFile->eof())
		return (true);
	else
		return (false);
}

/************************

    LFILELIST METHODS

************************/

LFileList::LFileList()  
{
    CurrentFile = NULL;
}

LFileList::~LFileList()  
{
    delete CurrentFile;
}

/***********************************************************************
 * Function: LFileList::FindFile
 *
 *  Parameter filename
 *  Parameter failClosed
 * Return:
 *  File
 * Comments:
 *  Find the file in the array by name. If failClosed == true,
 *  fail if the file isn't found.
 ***********************************************************************/
LFile *LFileList::FindFile(const char *filename, int failClosed)
{
    if ((CurrentFile != NULL) and (CurrentFile->Match(filename)))
            return (CurrentFile);

    if (failClosed)
        gLog.Log("Error: File %s not found.", filename);

    return NULL;
}

/***********************************************************************
 * Function: LFileList::Open
 *
 *  Parameter filename
 *  Parameter fKind     (READ, WRITE or APPEND)
 * Return:
 *
 * Comments:
 *    Open a file. Make sure it's not already open.
 ***********************************************************************/
void LFileList::Open(const char *filename, FileKind fKind)
{
    LFile   *theFile;

    theFile = FindFile(filename, false);
    if (theFile != NULL)
	{
        gLog.Log("Error. File %s is already open.", filename);
		return;
	}	

    CurrentFile = new LFile(filename, fKind);
}

/***********************************************************************
 * Function: LFileList::Close
 *
 *  Parameter filename
 * Return:
 *
 * Comments:
 *   Close an open file and remove it from the array.
 ***********************************************************************/
void LFileList::Close(const char *filename)
{
	LFile	*theFile;
	
    theFile = FindFile(filename, true);
    
    if (theFile != NULL)
    {
		delete CurrentFile;
    	CurrentFile = NULL; 
    }
}

/***********************************************************************
 * Function: LFileList::Read
 *
 *  Parameter filename
 *  Parameter str
 * Return:
 *
 * Comments:
 *   Read certain data from a file.
 ***********************************************************************/
void LFileList::Read(const char *filename, TString &str)
{
    LFile   *theFile;

    theFile = FindFile(filename, true);
    theFile->Read(str);
}
 
/***********************************************************************
 * Function: LFileList::ReadUntil
 *
 *  Parameter filename
 *  Parameter str      (put data here)
 *  Parameter delim    (use this delimiter to read one item)
 * Return:
 *
 * Comments:
 *  Read certain data from a file.
 ***********************************************************************/
void LFileList::ReadUntil(const char *filename, TString &str, unsigned char delim)
{
    LFile   *theFile;

    theFile = FindFile(filename, true);
    theFile->ReadUntil(str, delim);
}
    
/***********************************************************************
 * Function: LFileList::Write
 *
 *  Parameter filename
 *  Parameter data   (to write)
 * Return:
 *
 * Comments:
 *      Write given data to the file.
 ***********************************************************************/
void LFileList::Write(const char *filename, TString &data)
{
    LFile   *theFile;

    theFile = FindFile(filename, true);
    theFile->Write(data);
}

/***********************************************************************
 * Function: LFileList::Lookup
 *
 *  Parameter filename
 *  Parameter searchString    (search what)
 *  Parameter numFields       (up to how many records)
 * Return:
 *
 * Comments:
 *   Try to find a particular record in the file.
 ***********************************************************************/
void LFileList::Lookup(const char *filename, TString &searchString, int numFields)
{
    LFile   *theFile;

    theFile = FindFile(filename, true);
    theFile->Lookup(searchString, numFields);
}

/***********************************************************************
 * Function: LFileList::Rewrite
 *
 *  Parameter filename
 *  Parameter searchString
 *  Parameter numFields
 * Return:
 *
 * Comments:
 *  Rewrite (move to end of file) a particular record in the file.
 ***********************************************************************/
void LFileList::Rewrite(const char *filename, TString &searchString, int numFields)
{
    LFile   *theFile;

    theFile = FindFile(filename, true);
    theFile->Rewrite(searchString, numFields);
}


bool LFileList::CurFileOpen(void)
{
	if (CurrentFile != NULL)
		return (true);
	else
		return (false);
}


bool LFileList::CurFileAtEOF(void)
{
	if (CurrentFile != NULL)
		return (CurrentFile->AtEOF());
	else
		return (true);
}

/*
 $Log$
 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.4  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.3  1999/11/02 17:16:37  chuck
 2.00 Build 8

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
