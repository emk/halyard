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
// Index.h : Class to get scripts, headers, and
//    macros from text files.
//

#if !defined (_Index_h_)
#define _Index_h_

#include <fstream.h>

#include "TCommon.h"
#include "TBTree.h"
#include "LStream.h"
#include "CryptStream.h"

class IndexFile;

/*-----------------------------------------------------------------

CLASS
    Index

	A class for indexing script files.   

AUTHOR
    Chuck Officer<br>
	Sean Sharp

-----------------------------------------------------------------*/
class Index : public TBNode 
{
    public:

		//////////
		// Constructor
		//
		// [in] inIndex - IndexFile associated with this index
		// [in_optional] inName - name of this index (default NULL)
		// [in_optional] inStart - starting offset of the index (default 0)
		// [in_optional] inEnd - ending offset of the index (default 0)
		//
		Index(IndexFile *inIndex, const char *inName = NULL, 
				int32 inStart = 0, int32 inEnd = 0);

		//////////
		// Destructor
		//
		virtual			~Index();
		
		//////////
		// Get the name of the Index
		//
		// [out] return - the name of this Index
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
		//
		void	FlushScript(void);

	protected:
		//////////
		// The IndexFile for this index.
		//
        IndexFile		*m_File;

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
		LStream			m_Script;
};

/*-----------------------------------------------------------------

CLASS
    IndexManager

	Manages a set of Index objects.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class IndexManager : public TBTree 
{
    public:
        
		//////////
		// Constructor.
		//
		IndexManager();
        
		//////////
		// Destructor.
		//
		virtual			~IndexManager();
		
		//////////
		// Abstract virtual method to be overridden by the descendant.
		//
		// [in] inFile - index file associated with this index
		// [in] inName - name of this index
		// [in] inStart - starting offset of the index
		// [in] inEnd - ending offset of the index
		//
        virtual void	MakeNewIndex(IndexFile *inFile, const char *inName, int32 inStart, int32 inEnd) {}
};

/*-----------------------------------------------------------------

CLASS
    IndexFile

	Creates a file pair (associated (script) file / index file)
	to index the associated file.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class IndexFile : public TBNode
{
	public:
		
		//////////
		// Constructor.
		//
		// [in] inName - name of the file we are indexing
		//
		IndexFile(const char *inName);
		
		//////////
		// Destructor
		//
		virtual			~IndexFile();
		
		//////////
		// Initialize the index file/script file pair. Open the index file,
		// read all the info, initialize headers, cards	and macros.
		//
		// Note: If IGNORE_IDX_FILE is defined,a Parser is used to initialize
		//		 all the index information.
		//
		// [out] return - true if success, false if there were any errors
		//
		bool			Init();
	
		//////////
		// Open the file stream associated with this index file.
		//
		// [in] inPath - full path of the file to be indexed (e.g. c:\foo\test.scr)
		//
		bool			Open(const char *inPath);
		
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
    IndexFileManager

	Manages a set of IndexFile objects.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class IndexFileManager : public TBTree
{
	public:
		
		//////////
		// Constructor.
		//
		IndexFileManager();
		
		//////////
		// Destructor.
		//
		virtual		~IndexFileManager();

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

#endif // _Index_h_

/*
 $Log$
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

 (3) Modified IndexFile to automatically detect encrypted scripts and
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
