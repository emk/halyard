// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
#if !defined (_LFileBundle_h_)
#define _LFileBundle_h_

#include <string>

//#include "FiveL.h"		// need HCK
#include "TString.h"
#include "TBTree.h"
#include "Config.h"
#include "LFiles.h"
#include "lang/old5l/CryptStream.h"

#define STRING_MIN_RESIZE 128
#define READ_BUF_SIZE 512
#define IN_FILENAME "5L.db"			// Filename of the Bundle file

/*-----------------------------------------------------------------

CLASS
    LFileBundle
	
	Uses the same public interface as LFileList, but stores all 5L files in a
	single database file on the filesystem.  This allows 5L to integrate
	with DLS and provides a more secure solution.  The database file (5LDB)
	is encrypted in release builds.

MACRO DEFINITIONS
	#define STRING_MIN_RESIZE<br>
	#define READ_BUF_SIZE<br>
	#define IN_FILENAME<br>
	#define TMP_FILENAME<br>
	#define INIT_DATA_DIR<br>

AUTHOR
    Sean Sharp

-----------------------------------------------------------------*/
class LFileBundle : public TObject
{	
	public:
		//////////
		// Constructor
		//
		// Init() should be called before using an LFileBundle.
		//
		LFileBundle();
		
		//////////
		// Destructor
		//
		virtual ~LFileBundle();
		
		//////////
		// This function was needed to avoid having to make too many changes 
		// to the 5L codebase to support LFileBundle.  In particular, 
		// gConfigManager variables have not been set when gFileManager is 
		// initialized by a call to the constructor above.
		//
		// Init() should always be called before using an LFileBundle.
		//
		// [out] return - false if there is an error initializing the bundle, true otherwise
		//
		bool Init();
		

		/************ Methods we need to support from LFileList ***********/

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
		// [in/out] buf - buffer to hold data that is read
		//
		void	Read(const char *filename, TString &buf);
		
		//////////
		// Read until the given delimiter. Return everything up to
		// the delimiter, and throw the delimiter away. 
		// (If delim == 0, read until EOF)
		//
		// [in]  filename - name of the file to read from 
		// [in/out] buf - buffer to hold data that is read
		// [in]  delim - single char delimiter
		//
		void	ReadUntil(const char *filename, TString &buf, unsigned char delim);
		
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
		void	Lookup(const char *filename, TString &searchString, int numFields);
		
		//////////
		// We want to rewrite a record. To do this, we move the record to
		// the end of the file and let the scriptor append data with write 
		// commands
		//
		// [in] filename - file name to use for rewrite
		// [in]	searchString - record to be rewritten
		// [in] numFields - number of fields in the searchString 
		//
		void	Rewrite(const char *filename, TString &searchString, int numFields);
		
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

		//////////
		// Import a file into the Bundle, if the file already exists
		// it is overwritten by the imported file
		//
		// [in] filename - an absolute filename	(e.g. c:\foo\test.dat) 
		//
		void	Import(const char *filename);

		//////////
		// Mark the list of files as "global" files.  Global files are those that
		// can be accessed by multiple users and therfore reside in a special 
		// global files block.
		//
		// [in] fileList - a comma-spearated list of files that should be marked as global files
		//
		void	AddGlobalFiles(TString &fileList);

	private:
		//////////
		// Has the bundle been initialized?
		//
		bool		mIsInitialized;

		//////////
		// Are we using encryption for the bundle?
		//
		bool		isEncrypted;

		//////////
		// Absolute filename (incl path info) for the bundle.
		//
		std::string	bundleFilename;
		
		//////////
		// Used for the file stream when acting in clear mode.
		//
		ifstream	*clearFileStream;

		//////////
		// Encyption/Decryption stream used when acting in encrypted mode.
		//
		CryptStream *cryptStream;
		
		//////////
		// Cache for all 5L files.
		//
		TString cache;

		//////////
		// How should the cache be written back to disk?
		// (see DB_WRITES_XXX contants in Config.h)
		//
		int cacheWriteFreq;
		
		//////////
		// Is the cache dirty (have any files been modified)?
		//
		bool dirtyCache;
		
		//////////
		// Has the current file been modified?
		//
		bool dirtyFile;
		
		//////////
		// Three state hanging end-of-line flag.  Used to ensure there is an
		// EOL before each tag and prevent extraneous EOLs.
		//
		int hangingEOL; 

		//////////
		// Current read index (index into cache).
		//
		int readIndex;
		
		//////////
		// Location of <UserDB> tag (index into cache).
		//
		int userDBTagIndex;
		
		//////////
		// Location of <User name="_GLOBAL_"> tag (index into cache).
		//
		int globalUserTagIndex;
		
		//////////
		// Location of <User> tag for current user (index into cache).
		//
		int userTagIndex;
		
		//////////
		// Location of <File> tag for current file (index into cache).
		//
		int fileTagIndex;

		//////////
		// Binary tree containing names of all global files.
		//
		TBTree		globalFiles;

		/***** Attributes associated with the currently opened FiveL file *****/
		
		//////////
		// Position of readIndex WRT the current file 
		// (-, =, +) <==> (before, inside, past) current file boundaries
		//
		char		filePos;
		
		//////////
		// Name of current user.
		//
		TString		currentUser;
		
		//////////
		// Name of the current file.
		//
		TString		currentFilename;
		
		//////////
		// FileKind of the current file.
		//
		FileKind	currentFileKind;
		
		//////////
		// Is the current file a global file?
		//
		bool		currentFileIsGlobal;
		
		/******** Private Methods ********/

		//////////
		// Chop the directories of the filename and make it lower-case.<br>
		// e.g. "c:\foo\bar\Test.dat" becomes "test.dat"
		//
		// [in] filename - filename with path
		// [in/out] sFilename - filename with path removed and case lowered
		//
		void	ShortenFilename(const char *filename, char *sFilename);
		
		//////////
		// Read from the bundle until delim is found.  Toss the delimiter.
		//
		// [in/out] buf - buffer to place contents read
		// [in] delim - delimiter
		//
		void	BundleReadUntil(TString &buf, unsigned char delim);
		
		//////////
		// Read from the bundle until delim is found.  Toss the delimiter.
		// Ignore contents read.
		//
		// [in] delim - delimiter
		//
		void	BundleReadUntil(unsigned char delim);
		
		//////////
		// Search for a string at the beginning of a line. Positions the
		// readIndex according to position:<br> 
		//	(0 = beginning of line following searchString, 
		//   1 = immediately after searchString)
		//
		// [in] searchString - string to search for
		// [in_optional] position - how to position readIndex (default 0)
		// [out] return - index into Bundle where searchString was found,
		//				  or -1 if not found.
		//
		int		BundleSearch(const char *searchString, int position = 0);
		
		//////////
		// Creates the initial tags for a new bundle.
		//
		void	BundleNew();
		
		//////////
		// Create a file (or erase a file if it already exists) for the current
		// user.  The initial file contents may be supplied.
		// Positions the readIndex at the beginning of the file data.
		//
		// [in] filename - name of the new file
		// [in_optional] contents - initial file contents (default "")
		// [out] return - the file tag index of the newly created file
		//
		int		BundleCreateFile(const char *filename, const TString &contents = "");
		
		//////////
		// Make tags for a new user.<br>
		// readIndex is set to the beginning of the User tag
		//
		// [in] username - name of the user
		// [out] return - the user tag index of the new user
		//
		int		BundleCreateUser(const char *username);
		
		//////////
		// Have we reached the end of the bundle?
		//
		// [out] return - true (!=0) if the end of the bundle has been reached, 
		//				  false otherwise
		//
		int		BundleEOF();
		
		//////////
		// Write the cache back to disk using the encryption stream.
		//
		void	WriteCache();
		
		//////////
		// Reposition the READ file pointer to the beginning of the current file
		//
		// [out] return - file tag index for the current file or -1 if 
		//				  there was an error
		//
		int		ResetFileReadIndex();
		
		//////////
		// Initialize tag indices
		//
		void	InitIndices();
		
		//////////
		// Initializes globalUserTagIndex which becomes invalid if a non-global
		// file is modified.
		//
		void	InitGlobalUserTag();
		
		//////////
		// Returns a date/time stamp in the format DD/MM/YY HH:MM:SS (24-hour time)
		//
		// [out] return - date and time stamp
		//
		TString GetTimestamp();
		
		//////////
		// Compute the MD5 hash for the given string.
		//
		// [in] str - the string
		// [out] return - the MD5 hash for the string
		//
		TString ComputeMD5(const TString &str);	
		
		//////////
		// Deprectated.  Use UpdateFileHeader instead.
		//
		//void	UpdateMD5(const char *filename, int fTagIndex);

		//////////
		// Update the header info for the file with tag located at fTagIndex. <br>
		// Modifies cache with the updated MD5, timestamp, etc for this file.
		//
		// [in] filename - name of the file
		// [in] fTagIndex - index into cache where this file is located
		//
		void	UpdateFileHeader(const char *filename, int fTagIndex);
		
		//////////
		// Import all files located in INIT_DATA_DIR into the bundle. <br>
		// Assumes all imported files are global.
		//
		void	ImportFiles();

		/******* Routines to handle file operations on bundle *******
		 ******* (which may be clear or encrypted)			  *******/
		
		//////////
		// Convert the bundle.
		//
		// [in] dir - conversion direction (0 = clear -> encrypted, 1 = encrypted -> clear) 
		//
		void ConvertBundle(int dir);

		//////////
		// Open the bundle and read its contents into cache.
		//
		// [out] return - true if successful, false otherwise
		//
		bool OpenAndReadBundle();

		//////////
		// Close the bundle.
		//
		void CloseBundle();

		//////////
		// Rewrite the bundle to disk using contents stored in cache.
		//
		void RewriteBundle();

};


#endif // _LFileBundle_h_
