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
// Index.cpp : Routines for reading script indices from disk and loading
//    headers, macros and cards into memory. Some files may be
//    encrypted; these are decrypted once they are read into
//    memory.
//

#include "stdafx.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>  
#include <sys\types.h> 
#include <sys\stat.h>
#include <time.h>

#include "Index.h"
#include "Globals.h"

// define IGNORE_IDX_FILE to parse script files on the fly
#define IGNORE_IDX_FILE
#ifdef IGNORE_IDX_FILE
#include "Parser.h"
#endif


Index::Index(IndexFile *inFile, const char *inName, int32 inStart, int32 inEnd) : TBNode(inName)
{
	m_File = inFile;
	if (m_File != NULL)
		m_File->AddReference();

    m_Start = inStart;
    m_End = inEnd;
}

Index::~Index()
{
	if (m_File != NULL)
		m_File->RemoveReference();
}

//
//	SetScript - Read in the text for this index from the file.
//
bool Index::SetScript()
{
    int32	length;
	int		count;
    char    *str;

	// see if we have already read the script in
    if (not m_Script.IsEmpty())
        return (true);

    length = (m_End - m_Start) + 1;

    m_Script.Resize(length + 1);
    str = m_Script.GetBuffer();

	// read card from the script file:
    m_File->Seek(m_Start);
    count = m_File->Read(str, length);

    if (count < length)
	{
        gLog.Log("I/O error reading data for index %s.", Key());
		return (false);
	}

    //  Set the terminating 0 and tell the string to recalc its
    //  length since we did some direct manipulation.
    //
    str[length] = '\0';
    m_Script.Update();

	return (true);
}

//
//	FlushScript - Toss the contents of the script.
//
void Index::FlushScript()
{
    m_Script.Empty();
}

//
//	IndexManager methods
//

//
//	IndexManager - Constructor
//
IndexManager::IndexManager() : TBTree()
{
}

IndexManager::~IndexManager()
{
	if (m_Root != NULL) 
		RemoveAll();
}

//
//	IndexFileManager methods
//

IndexFileManager::IndexFileManager() : TBTree()
{
}

IndexFileManager::~IndexFileManager()
{
}

//
//	NewIndex - Create a new index for the file specified by inName
//
bool IndexFileManager::NewIndex(const char *inName)
{
	IndexFile	*newIndex = NULL;
	bool		retValue = false;

	// make sure we don't already have this node
	newIndex = (IndexFile *) Find(inName);
	if (newIndex != NULL)
	{
		gLog.Error("Trying to open script <%s> but it is already open.",
			inName);
		return (false);
	}

	newIndex = new IndexFile(inName);
	if (newIndex == NULL)
	{
		gLog.Error("Could not allocate memory for script <%s>",
			inName);
		return (false);
	}

	retValue = newIndex->Init();
	if (not retValue)
		delete newIndex;
	else
	{
		Add(newIndex);
		
		// Close only if redoscript is enabled (not sure why we keep it open otherwise??)
		if (gConfigManager.GetUserPref(REDOSCRIPT) == REDOSCRIPT_ON)
			newIndex->Close();
	}

	return (retValue);
}

//
//	IndexFile methods
//
IndexFile::IndexFile(const char *inName) : TBNode(inName)
{
	m_ReferenceCount = 0;
	m_AtEnd = false;
}

IndexFile::~IndexFile()
{
	Close();
}

// Increment reference count for this index file
void IndexFile::AddReference()
{
	m_ReferenceCount++;
}

// Decrement reference count for this index file
void IndexFile::RemoveReference()
{
	m_ReferenceCount--;

	if (m_ReferenceCount <= 0)
	{
		// remove ourselves from the index file tree
		gIndexFileManager.Remove(Key());
	}
}

// Open the file stream associated with this index file
// inPath is the full path including filename
bool IndexFile::Open(const char *inPath)
{
	int		len = strlen(inPath);
	char	*path = new char[len+1];
	char	*filename = new char[len+1];
	char	*ptr;

	ptr = strrchr(inPath, '\\');
	
	if (ptr != NULL)
	{
		strncpy(path, inPath, ptr - inPath);
		path[ptr - inPath] = '\0';
		strcpy(filename, ptr + 1);
	}
	else	// assume no path, just filename
		strcpy(filename, inPath);
		
	// determine whether the script is encrypted
	cryptStream = new CryptStream(path, filename, PAYLOAD_SCRIPT, HCK, sizeof(HCK));
	isEncrypted = cryptStream->in_verify();
	
	delete [] path;
	delete [] filename;
	
	if(!isEncrypted)
	{
		// CryptStream not needed
		delete cryptStream;
		cryptStream = NULL;

		m_File.open(inPath, ios::in | ios::binary);
		
	}

	if (!IsOpen())
		return (false);
	
	return (true);
}

// Returns true if the associated file stream is open
bool IndexFile::IsOpen()
{
	if (!isEncrypted)
	{
		if (m_File.is_open())
			return true;
	}
	else
	{
		if (cryptStream->in_isOpen())
			return true;
	}

	return (false);
}

// Seek to the given position in the associated file stream
void IndexFile::Seek(int32 inPos)
{
	if (!isEncrypted)
		m_File.seekg(inPos);
	else
		cryptStream->in_seek(inPos);
}

// Returns the current position in the associated file stream
int32 IndexFile::GetPos()
{
	if (!isEncrypted)
		return m_File.tellg();
	else
		return cryptStream->in_tellg();
}

// Returns true if the EOF has been reached in the associated file 
// stream, returns false otherwise
bool IndexFile::AtEnd()
{
	return (m_AtEnd);
}

// Read from the associated file stream
int32 IndexFile::Read(char *inBuffer, int32 inLength)
{
	int32	bytesRead = -1;

	ASSERT(inBuffer != NULL);
	ASSERT(inLength > 0);

	if (!isEncrypted)
	{
		m_File.read(inBuffer, inLength);
		bytesRead = m_File.gcount();

		if (m_File.eof())
		{
			m_AtEnd = true;
			m_File.clear();
		}
	}
	else
	{
		cryptStream->read((uchar *)inBuffer, inLength);
		bytesRead = cryptStream->gcount();

		if (cryptStream->in_eof())
		{
			m_AtEnd = true;
			cryptStream->in_reset();
		}
	}

	return (bytesRead);
}

// Close the associated file stream
void IndexFile::Close()
{
	if (isEncrypted)
	{
		if (cryptStream != NULL)
		{
			delete cryptStream;
			cryptStream = NULL;
		}
	}
	else
		m_File.close();
}

//
//	Init - Initialize the index file/script file pair. Open the 
//		index file, read all the info, initialize headers, cards
//		and macros.
//
bool IndexFile::Init()
{
	ifstream	   theIndexFile;
    TString     	indexName;
	TString			scriptName; //root name without extensions
    TString     	theName;
  
	indexName = gConfigManager.ScriptsPath();
	scriptName = gConfigManager.ScriptsPath();
	
	indexName += Key();
	scriptName += Key();
   
    indexName += ".IDX";
    scriptName += ".SCR";

#ifndef IGNORE_IDX_FILE
    struct _stat	theIndexBuf;
    struct _stat 	theScriptBuf;
	long			numEntries, a, b, i;
	int				theResult;

	// see if the .IDX file is more recent than the .SCR file
	theResult = _stat(scriptName.GetString(), &theScriptBuf);
	if (theResult != 0)
	{
		gLog.Error("Couldn't get information about script file <%s>.", scriptName.GetString());
		return (false);
	}
	theResult = _stat(indexName.GetString(), &theIndexBuf);
	if (theResult != 0)
	{
		gLog.Error("Couldn't get information about index file <%s>.", indexName.GetString());
		return (false);
	}
	
	if (theScriptBuf.st_mtime > theIndexBuf.st_mtime)
	{
		gLog.Error("You need to run Update# on <%s>", scriptName.GetString());
		return (false);
	}
#endif

    // Open the script file.
	if (not Open(scriptName))
	{
        gLog.Error("Couldn't open script file <%s>.", scriptName.GetString());
		return (false);
	}

#ifdef IGNORE_IDX_FILE
	// Instantiate a parser to check out the script file and initialize all the 
	//	index information.
	{
		Parser		theParser;
		DWORD		startTime;
		DWORD		endTime;

		startTime = ::timeGetTime();

		if (not theParser.Parse(this))
		{
			gLog.Error("There are errors in the script file <%s>", scriptName.GetString());
			return (false);
		}

		endTime = ::timeGetTime();

		gDebugLog.Log("It took <%ld> milli-seconds to parse the script file",
			endTime - startTime);
	}
#else

	// Open the index file.
	theIndexFile.open(indexName, ios::in | ios::binary);
    if (not theIndexFile.is_open())
	{
       gLog.Error("Error: Couldn't open index file <%s>.", indexName.GetString());
	   return (false);
	}	   

    //  Headers.
    //
    theIndexFile >> numEntries;
    for (i = 0; i < numEntries && !theIndexFile.eof(); i++) 
    {
        theIndexFile >> theName >> a >> b; 
        gHeaderManager.MakeNewIndex(this, theName.GetString(), a, b);
    } 
    
    if (i != numEntries)
	{
        gLog.Error("Error: Did not get the %d headers expected. Exiting...", numEntries);
		return (false);
	}

    //  Macros.
    //
    theIndexFile >> numEntries;
    for (i = 0; i < numEntries && !theIndexFile.eof(); i++) 
    {
        theIndexFile >> theName >> a >> b;
        gMacroManager.MakeNewIndex(this, theName.GetString(), a, b);
    }
    if (i != numEntries)
	{
        gLog.Error("Error: Did not get the %d macros expected. Exiting...", numEntries);
		return (false);
	}

    //  Cards.
    //
    theIndexFile >> numEntries;
    for (i = 0; i < numEntries && !theIndexFile.eof(); i++) 
    {
        theIndexFile >> theName >> a >> b;
        gCardManager.MakeNewIndex(this, theName.GetString(), a, b);
    }
    if (i != numEntries)
	{
        gLog.Error("Error: Did not get the %d cards expected. Exiting...", numEntries); 
		return (false);
	}
#endif
	
   return (true);

}


/*
 $Log$
 Revision 1.3  2002/03/26 17:03:49  tvw
 Crypt library rewrite, support for encrypted 5L scripts, command-line tool
 for encrypting/decrypting 5L scripts, 5LDB, potentially other 5L files.

 (1) Complete overhaul of the Crypt library.  It now supports streaming
 reads and writes.  Many function names were changed.  The encryption
 header was modified to include signature, payload, and timestamp.
 NOTE: Previous versions of 5LDB will be incompatible because of this
 change.

 (2) Added CryptTool, a command-line utility to encrypt/decrypt scripts,
 5LDB, etc.  Run with no options for help.

 (3) Modified IndexFile to automatically detect encrypted scripts and
 use a CryptStream for I/O if detected.

 (4) Added TestSuite project to house FiveL unit testing.  Added some unit
 tests for CryptStream.

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

 Revision 1.4  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.3  2000/03/01 15:46:55  chuck
 no message

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
