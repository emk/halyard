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
// TIndex.h : Class to get scripts, headers, and
//    macros from text files.
//

#if !defined (TIndex_H)
#define TIndex_H

#include <fstream.h>

#include "TCommon.h"
#include "TBTree.h"
#include "TStream.h"
#include "CryptStream.h"

BEGIN_NAMESPACE_FIVEL

class TIndexFile;

/*-----------------------------------------------------------------

CLASS
    TIndex

	A class for indexing script files.   

AUTHOR
    Chuck Officer<br>
	Sean Sharp

-----------------------------------------------------------------*/
class TIndex : public TBNode 
{
    public:

		//////////
		// Constructor
		//
		// [in] inIndex - TIndexFile associated with this index
		// [in_optional] inName - name of this index (default NULL)
		// [in_optional] inStart - starting offset of the index (default 0)
		// [in_optional] inEnd - ending offset of the index (default 0)
		//
		TIndex(TIndexFile *inIndex, const char *inName = NULL, 
				int32 inStart = 0, int32 inEnd = 0);

		//////////
		// Destructor
		//
		virtual			~TIndex();
		
		//////////
		// Get the name of the TIndex
		//
		// [out] return - the name of this TIndex
		//
        const char *Name(void) { return (Key()); }
		
		//////////
		// Read in the text for this index from the file.
		//
		// [out] return - false if there is an file I/O error, true otherwise
		//
        bool	SetScript(void);
        
		//////////
		// Toss the contents of the script.
		//
		void	FlushScript(void);

		//////////
		// Get the contents of the script.  Be sure to call SetScript
	    // first!
		//
		const char *GetScript();

	protected:
		//////////
		// The TIndexFile for this index.
		//
        TIndexFile		*m_File;

		//////////
		// Starting point (in the index file) for this index.
		//
		int32			m_Start;
		
		//////////
		// Ending point (in the index file) for this index.
		//
		int32			m_End;
		
		//////////
		// Script file stream that we are indexing.
		//
		TStream			m_Script;
};

/*-----------------------------------------------------------------

CLASS
    TIndexManager

	Manages a set of TIndex objects.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class TIndexManager : public TBTree 
{
    public:
        
		//////////
		// Constructor.
		//
		TIndexManager();
        
		//////////
		// Destructor.
		//
		virtual			~TIndexManager();
		
		//////////
		// Abstract virtual method to be overridden by the descendant.
		//
		// [in] inFile - index file associated with this index
		// [in] inName - name of this index
		// [in] inStart - starting offset of the index
		// [in] inEnd - ending offset of the index
		//
		virtual void	MakeNewIndex(TIndexFile *inFile, const char *inName,
									 int32 inStart, int32 inEnd) = 0;
};

/*-----------------------------------------------------------------

CLASS
    TIndexFile

	Creates a file pair (associated (script) file / index file)
	to index the associated file.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class TIndexFile : public TBNode
{
	public:
		
		//////////
		// Constructor.
		//
		// [in] inName - name of the file we are indexing
		//
		TIndexFile(const char *inName);
		
		//////////
		// Destructor
		//
		virtual			~TIndexFile();
		
		//////////
		// Initialize the index file/script file pair. Open the index file,
		// read all the info, initialize headers, cards	and macros.
		//
		// Note: If IGNORE_IDX_FILE is defined, a Parser is used to initialize
		//		 all the index information.
		//
		// [out] return - true if success, false if there were any errors
		//
		bool			Init();
	
		//////////
		// Open the file stream associated with this index file.
		//
		// [in] inPath - full path of the file to be indexed
		// (e.g. c:\foo\test.scr)
		//
		bool			Open(const FileSystem::Path &inDirectory,
							 const char *inFile);
		
		//////////
		// Is the assocaiated file stream open?
		//
		// [out] return - true if the file stream is open, false otherwise
		//
		bool			IsOpen();
		
		//////////
		// Seek to the given position in the associated file stream
		//
		// [in] inPos - position to seek to in the file
		//
		void			Seek(int32 inPos);
		
		//////////
		// Get the current position in the associated file stream.
		//
		// [out] return - the current position
		//
		int32			GetPos();
		
		//////////
		// Read from the associated file stream.
		//
		// [in/out] inBuffer - buffer for the data that is read
		// [in] inLength - desired number of bytes to read
		// [out] return - actual number of bytes that were read into inBuffer
		//
		int32			Read(char *inBuffer, int32 inLength);
		
		//////////
		// Have we reached the end of the associated file stream?
		//
		// [out] return - true if the EOF has been reached in the associated file 
		//				  stream, false otherwise
		//
		bool			AtEnd();
		
		//////////
		// Close the associated file stream.
		//
		void			Close();
		
		//////////
		// Increment reference count for this index file.
		//
		void			AddReference();
		
		//////////
		// Decrement reference count for this index file.
		//
		void			RemoveReference();

	protected:
		//////////
		// Is this index file encrypted?
		//
		bool		isEncrypted;

		//////////
		// File input stream.
		//
		ifstream		m_File;
		
		//////////
		// Encrypted input stream.
		//
		CryptStream		*cryptStream;

		//////////
		// Have we reached the end of the file?
		//
		bool			m_AtEnd;
		
		//////////
		// Number of references made to this index file.
		//
		int32			m_ReferenceCount;
};

/*-----------------------------------------------------------------

CLASS
    TIndexFileManager

	Manages a set of TIndexFile objects.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class TIndexFileManager : public TBTree
{
	public:
		
		//////////
		// Constructor.
		//
		TIndexFileManager();
		
		//////////
		// Destructor.
		//
		virtual		~TIndexFileManager();

		//////////
		// Create a new index for the file specified.
		//
		// [in] inName - file to be indexed
		//
		bool		NewIndex(const char *inName);
};		

//
//  Initializer for all indices.
//
//bool InitIndex(const char *fName);

extern TIndexFileManager gIndexFileManager;

END_NAMESPACE_FIVEL

#endif // TIndex_H

/*
 $Log$
 Revision 1.3  2002/05/15 11:05:17  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.2.2.1  2002/04/22 05:22:33  emk
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

 Revision 1.2  2002/03/26 17:03:49  tvw
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

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.3  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
