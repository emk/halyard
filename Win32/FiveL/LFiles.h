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
// LFiles.h : 
//

#if !defined (_LFiles_h_)
#define _LFiles_h_

#include <fstream.h>

#include "TCommon.h"
#include "TObject.h"

//////////
// Enumeration for types of 5L files.
//
enum FileKind 
{
	fWriteAppend,
	fWriteNew,
	fReadOnly
};

/*-----------------------------------------------------------------

CLASS
    LFile

	A 5L data file with methods to make reading and writing more 
	straightforward.  Data files are treated as simple text
	databases.

AUTHOR
	Chuck Officer

-----------------------------------------------------------------*/
class LFile : public TObject 
{
	public:
		
		//////////
		// Constructor.
		//
		LFile();
		
		//////////
		// Constructor.
		//
		// [in] filename - name of the file
		// [in_optional] fKind - FileKind (default fReadOnly)
		//
		LFile(const char *filename, FileKind fKind = fReadOnly);
		
		//////////
		// Destructor.
		//
		virtual ~LFile();

		//////////
		// Does the given filename match this file?
		//
		// [in] aName - a filename to compare against
		// [out] return - true if the filenames match, false otherwise
		//
		bool		Match(const char *aName);

		//////////
		// Read the next word as determined by whitespace.
		//
		// [in/out] str - string to fill with the next word
		//
		void		Read(TString &str);
		
		//////////
		// Do some error checking and then call ReadUntilCore().
		//
		// [in/out] str - string to fill with the data that was read
		// [in] delim - delimiter
		//
		void		ReadUntil(TString &str, unsigned char delim);
		
		//////////
		// Read until the given delimiter. Return everything up to
		// the delimiter, and throw the delimiter away.
		//
		// [in/out] str - string to fill with the data that was read
		// [in] delim - delimiter
		//
		void		ReadUntilCore(TString &str, unsigned char delim);
		
		//////////
		// Write some data to the file (convert \t and \n).
		//
		// [in] data - data to be written to the file
		//
		void		Write(TString &data);
		
		//////////
		// Read until we find the given search string at the start of a
		// line or we hit the end of file.
		//
		// [in] searchString - string to search for
		// [in] numFields - number of fields in the searchString
		//
		void		Lookup(TString &searchString, int numFields);
		
		//////////
		// Read until we find the given search string at the start of a
		// line or we hit the end of file.
		//
		// [in] searchString - string to search for
		// [in] numFields - number of fields in the searchString
		//
		void		Lookup(const char *searchString, int numFields);
		
		//////////
		// Rewrite a record. To do this, we move the record to the end
		// of the file and let the 5L scriptor append data with write commands.
		//
		// [in] searchString - replace record starting with this string
		// [in] numFields - number of fields in the searchString
		//
		void		Rewrite(TString &searchString, int numFields);
		
		//////////
		// Have we reached the end of the file?
		//
		// [out] return - return true if we have reached the end of file, false otherwise
		//
		bool		AtEOF(void);

private:
		//////////
		// Name of the file.
		TString			itsName;
		
		//////////
		// File stream for reading and writing.
		fstream			*itsFile;
		
		//////////
		// FileKind of this file.
		FileKind		itsKind;

};

/*-----------------------------------------------------------------

CLASS
    LFileList

	A list of LFile objects.

AUTHOR
	Chuck Officer

-----------------------------------------------------------------*/
class LFileList : public TObject 
{
	public:
 		//////////
		// Constructor.
		//
		LFileList();
		
		//////////
		// Destructor.
		//
		virtual ~LFileList();

		//////////
		// Open a file
		//
		// [in] filename - name of the file to open
		// [in] fKind - (READ, WRITE or APPEND)
		//
		void	Open(const char *filename, FileKind fKind);

		//////////
		// Close a file
		//
		// [in] filename - name of the file to close
		//
		void	Close(const char *filename);
		
		//////////
		// Read the next word as determined by whitespace, place it in buf
		//
		// [in] filename - name of the file to read from
		// [in/out] str - string buffer to hold data that is read
		//
		void	Read(const char *filename, TString &str);
		
		//////////
		// Read until the given delimiter. Return everything up to
		// the delimiter, and throw the delimiter away. 
		// (If delim == 0, read until EOF)
		//
		// [in]  filename - name of the file to read from 
		// [in/out] str - string buffer to hold data that is read
		// [in]  delim - single char delimiter
		//
		void	ReadUntil(const char *filename, TString &str,
							unsigned char delim);
		//////////
		// Write some data to the file. Convert \t and \n to tab and
		// newline characters respectively.
		//
		// [in] filename - name of the file to write
		// [in] data - data to be written 
		//
		void	Write(const char *filename, TString &data);

		//////////
		// Positions the filepointer immediately after the searchString, 
		// if the searchString is not found, position at EOF
		//
		// [in] filename - name of the file used for lookup
		// [in]	searchString - string to search for
		// [in]	numFields - number of fields in searchString 
		//
		void	Lookup(const char *filename, TString &searchString,
						int numFields);
		//////////
		// We want to rewrite a record. To do this, we move the record to
		// the end of the file and let the scriptor append data with write 
		// commands
		//
		// [in] filename - file name to use for rewrite
		// [in]	searchString - record to be rewritten
		// [in] numFields - number of fields in the searchString 
		//
		void	Rewrite(const char *filename, TString &searchString,
						int numFields);
		
		//////////
		// Is the current file open?
		//
		// [out] return - true if it is open, false otherwise 
		//
		bool	CurFileOpen(void);
		
		//////////
		// Have we hit the EOF for the current file?
		//
		// [out] return - true if we have reached the EOF, false otherwise
		//
		bool	CurFileAtEOF(void);

	private:
		//////////
		// Current file opened.
		LFile	*CurrentFile;

	protected:
		//////////
		// Check if CurrentFile is open and matches the given filename.
		//
		// [in] filename - name of the file
		// [in] failClosed - if true, logs an error message if the file is closed or 
		//					 doesn't match. 	
		// [out] return - LFile handle for current file or NULL if no file is open or the
		//				  filename does not match
		//
		LFile	*FindFile(const char *filename, int failClosed);
};

#endif // _LFiles_h_

/*
 $Log$
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
