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

//////////////////////////////////////////////////////////////////////////////
//
// TIndex.cpp : Routines for reading script indices from disk and loading
//    headers, macros and cards into memory. Some files may be
//    encrypted; these are decrypted once they are read into
//    memory.
//

#include "TCommon.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#if !FIVEL_PLATFORM_MACINTOSH
#	include <sys/types.h>
#endif
#include <sys/stat.h>
#include <time.h>

#include "TIndex.h"
#include "TLogger.h"

// define IGNORE_IDX_FILE to parse script files on the fly
#define IGNORE_IDX_FILE
#ifdef IGNORE_IDX_FILE
#include "TParser.h"
#endif

USING_NAMESPACE_FIVEL

TIndexFileManager FIVEL_NS gIndexFileManager;

TIndex::TIndex(TIndexFile *inFile, const char *inName,
			   int32 inStart, int32 inEnd)
	: TBNode(inName)
{
	m_File = inFile;
	if (m_File != NULL)
		m_File->AddReference();

    m_Start = inStart;
    m_End = inEnd;
}

TIndex::~TIndex()
{
	if (m_File != NULL)
		m_File->RemoveReference();
}

//
//	SetScript - Read in the text for this index from the file.
//
bool TIndex::SetScript()
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
        gLog.Error("I/O error reading data for index %s.", Key());
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
void TIndex::FlushScript()
{
    m_Script.Empty();
}

//
//	GetScript - 
//
const char *TIndex::GetScript()
{
    return (m_Script.GetString());
}

//
//	TIndexManager methods
//

//
//	TIndexManager - Constructor
//
TIndexManager::TIndexManager() : TBTree()
{
}

TIndexManager::~TIndexManager()
{
	if (m_Root != NULL) 
		RemoveAll();
}

//
//	TIndexFileManager methods
//

TIndexFileManager::TIndexFileManager() : TBTree()
{
}

TIndexFileManager::~TIndexFileManager()
{
}

//
//	NewIndex - Create a new index for the file specified by inName
//
bool TIndexFileManager::NewIndex(const char *inName)
{
	TIndexFile	*newIndex = NULL;
	bool		retValue = false;

	// make sure we don't already have this node
	newIndex = (TIndexFile *) Find(inName);
	if (newIndex != NULL)
	{
		gLog.Error("Trying to open script <%s> but it is already open.",
			inName);
		return (false);
	}

	newIndex = new TIndexFile(inName);
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
		
		// We used to call newIndex->Close() here when redoscript
		// was enabled, but it isn't necessary on the Mac.
	}

	return (retValue);
}

//
//	TIndexFile methods
//
TIndexFile::TIndexFile(const char *inName) : TBNode(inName)
{
	m_ReferenceCount = 0;
	m_AtEnd = false;
}

TIndexFile::~TIndexFile()
{
	Close();
}

// Increment reference count for this index file
void TIndexFile::AddReference()
{
	m_ReferenceCount++;
}

// Decrement reference count for this index file
void TIndexFile::RemoveReference()
{
	ASSERT(m_ReferenceCount > 0);

	m_ReferenceCount--;
	if (m_ReferenceCount <= 0)
	{
		// remove ourselves from the index file tree
		gIndexFileManager.Remove(Key());
	}
}

// Open the file stream associated with this index file
bool TIndexFile::Open(const FileSystem::Path &inDirectory,
					  const char *inFile)
{
	// determine whether the script is encrypted
	cryptStream = new CryptStream(inDirectory, inFile, PAYLOAD_SCRIPT,
								  HCK, HCK_SIZE);
	isEncrypted = cryptStream->in_verify();
	
	if(!isEncrypted)
	{
		// CryptStream not needed
		delete cryptStream;
		cryptStream = NULL;

		std::string file =
			inDirectory.AddComponent(inFile).ToNativePathString();
		m_File.open(file.c_str(), ios::in | ios::binary);
		
	}

	if (!IsOpen())
		return (false);
	
	return (true);
}

// Returns true if the associated file stream is open
bool TIndexFile::IsOpen()
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
void TIndexFile::Seek(int32 inPos)
{
	if (!isEncrypted)
		m_File.seekg(inPos);
	else
		cryptStream->in_seek(inPos);
}

// Returns the current position in the associated file stream
int32 TIndexFile::GetPos()
{
	if (!isEncrypted)
		return m_File.tellg();
	else
		return cryptStream->in_tellg();
}

// Returns true if the EOF has been reached in the associated file 
// stream, returns false otherwise
bool TIndexFile::AtEnd()
{
	return (m_AtEnd);
}

// Read from the associated file stream
int32 TIndexFile::Read(char *inBuffer, int32 inLength)
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
void TIndexFile::Close()
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
bool TIndexFile::Init()
{
	ifstream		theIndexFile;
    TString     	indexName;
	TString			scriptName; //root name without extensions
    TString     	theName;
  
	FileSystem::Path basename =
		FileSystem::GetScriptsDirectory().AddComponent(Key());
	indexName = basename.ReplaceExtension("idx").ToNativePathString();
	scriptName = basename.ReplaceExtension("scr").ToNativePathString();

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
	if (not Open(FileSystem::GetScriptsDirectory(),
				 (TString(Key()) + TString(".scr")).GetString()))
	{
        gLog.Error("Couldn't open script file <%s>.", scriptName.GetString());
		return (false);
	}

#ifdef IGNORE_IDX_FILE
	// Instantiate a parser to check out the script file and initialize all the 
	//	index information.
	{
		TParser		theParser;

		//DWORD		startTime;
		//DWORD		endTime;
		//startTime = ::timeGetTime();

		if (not theParser.Parse(this))
		{
			gLog.Error("There are errors in the script file <%s>", scriptName.GetString());
			return (false);
		}

		//endTime = ::timeGetTime();
		//gDebugLog.Log("It took <%ld> milli-seconds to parse the script file",
		//	endTime - startTime);
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
 Revision 1.4  2002/05/15 11:05:17  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.3.2.5  2002/04/30 07:57:24  emk
 3.3.2.5 - Port Win32 code to use the 20Kloc of Common code that now
 exists.  The (defstyle ...) command should work, but (textaa ...) isn't
 available yet.

 Next up: Implement the (textaa ...) command and the low-level
 GraphicsTools::Image::DrawBitMap.

 Revision 1.3.2.4  2002/04/29 06:19:11  emk
 Some over-the-weekend performance tuning.

 - Added fonttools/fontspeed.cpp, which mimics 5L drawing patterns, but with
 an empty DrawPixMap routine.

 - Added a pre-rendered glyph cache to the Typography module.

 - Added new features to GraphicsTools to support the glyph cache.

 - visual-test.png has apparently changed, but I can't see any difference.
 It's probably slight changes in anti-aliased pixel intensity.

 - Miscellaneous other cleanups and tweaks.

 Revision 1.3.2.3  2002/04/24 04:32:32  emk
 After much thought, I've finally decided that TIndexFileManager shouldn't close individual files, even if redoscript is turned on.  If this breaks the Windows engine, I'll port the Macintosh redoscript code to Windows.

 Revision 1.3.2.2  2002/04/22 08:17:57  emk
 Updated Common code to build on Macintosh and pass all unit tests.

 Revision 1.3.2.1  2002/04/22 05:22:33  emk
 A weekend's worth of merging, in preparation for the Typography switchover.

 MOVED
 -----

 * Win32/Crypt/md5.c -> Common/libs/crypto/md5.c
 * Win32/Crypt/md5.h -> Common/libs/crypto/md5.h
 * Win32/Crypt/md5main.c -> Common/libs/crypto/md5main.c
 * Win32/Crypt/_blowfish.c -> Common/libs/crypto/blowfish.c
 * Win32/Crypt/blowfish.h -> Common/libs/crypto/blowfish.h

 Third-party cryptography files moved to the new Common/libs/crypto
 directory.  In general, third-party code should go under Common/libs, so we
 can find it all in one place for updates and license checks.
 Common/freetype2 will probably move there soon for the sake of consistency.

 MERGED
 ------

 * Win32/Crypt/CryptStream.cpp -> Common/CryptStream.cpp
 * Win32/Crypt/CryptStream.h -> Common/CryptStream.h
 * Win32/TestSuite/TestCryptStream.cpp -> Common/CryptStreamTests.cpp

 Modified to use the portable Path abstraction.  Included our standard key
 once in this file, instead of having it in many different headers
 throughout the program. Coerced uchar* to char* in several places required
 by the fstream API (and some other coercions).

 * Win32/FiveL/Parser.cpp -> Common/TParser.cpp
 * Win32/FiveL/Parser.h -> Common/TParser.h

 Merged in Elizabeth's improved escape-handling code.  Factored out all code
 which specifically referred to "card", "header" or "macrodef" forms, and
 added a generic API for registering abitrary top-level forms.

 * Win32/FiveL/Index.cpp -> Common/TIndex.cpp
 * Win32/FiveL/Index.h -> Common/TIndex.h
 * NEW: Common/TIndexTests.cpp
 * NEW: Common/Scripts/test.scr

 Merged TIndex::GetScript from the Macintosh.  Temporarily stopped closing
 the TIndexFile in the presence of REDOSCRIPT.  Merged some Macintosh code
 for building indices from FSSpecs; this probably doesn't work.  Changed the
 Open and Init methods to use the portable Path library (the APIs might be
 slightly suboptimal).

 * Win32/FiveL/LUtil.cpp -> Common/TDateUtil.cpp
 * Win32/FiveL/LUtil.h -> Common/TDateUtil.h

 Extracted date-related code from LUtil.*.  Changed wsprintf calls to
 sprintf.

 * Win32/FiveL/Variable.cpp -> Common/TVariable.cpp
 * Win32/FiveL/Variable.h -> Common/TVariable.h

 Disabled certain special variables that caused awkward dependencies, and
 replaced them with an interface for registering arbitrary special
 variables.

 MODIFIED
 --------

 * Common/FileSystem.cpp
 * Common/FileSystem.h

 Added a RenameFile function, and a GetScriptsDirectory function.  Also
 added a ReplaceWithTemporaryFile function, which overwrites an existing
 file with a temporary file (someday, we can implement this as an atomic
 operation on most operating systems).

 * Common/GraphicsTools.h

 Added a no-arguments constuctor for Point.

 * Common/TString.cpp
 * Common/TString.h

 Lots of "signed/unsigned comparison" and other warning fixes.

 * Common/TStyleSheet.cpp
 * Common/TStyleSheet.h

 Added full-fledged INCR_X, INCR_Y support!

 * Common/Typography.cpp
 * Common/Typography.h

 Made sure that kerning+advance can never move the drawing cursor backwards.
 Fixed warnings.

 * Common/fonttools/pngtest.cpp

 Added a test of transparent text (just for fun).

 KNOWN ISSUES
 ------------

 * Logging code needs to have Mac-specific features merged back in.

 * TIndexFile doesn't close the underlying file properly in the presence of
 REDOSCRIPT.  What's going on here?

 * TParser--and maybe TStream--need to have cross-platform end-of-line
 handling.

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

 (3) Modified TIndexFile to automatically detect encrypted scripts and
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
