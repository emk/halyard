//
//	LFileBundle: Acts as a file database for 5L files. 
//
//	Author ssharp
//
// (see header file for additional documentation)
//

#include "stdafx.h"

#include "TDeveloperPrefs.h"
#include "LFileBundle.h"
#include "Globals.h"
#include <time.h>
#include <fstream.h>
#include <string.h>
#include <windows.h>

#include "md5.h"
#define MD5_SIZE (16)

LFileBundle::LFileBundle()
{
	
}

LFileBundle::~LFileBundle()
{
	CloseBundle();
	
	delete bundleFilename;
	delete clearFileStream;

	//delete cs;	// Causes memory errors ??
}

// This function was needed to avoid having to make too many changes 
// to the 5L codebase to support LFileBundle.  In particular, 
// gConfigManager variables have not been set when gFileManager is 
// initialized by a call to the constructor above.
//
bool LFileBundle::Init()
{
	/*
	// Test CryptStream cryptFile()
 	DWORD startTime, endTime;
	CryptStream *test;
	startTime = ::timeGetTime();
 	test = new CryptStream(gConfigManager.InstallPath(), "hiv.test", (unsigned char *)HCK, sizeof(HCK));
 	test->cryptFile(0);
 	test->close();
	endTime = ::timeGetTime();
	gDebugLog.Log("CryptFile test time = %ld millis", endTime - startTime);
	*/

	isEncrypted = (gDeveloperPrefs.GetPref(DB_TYPE) == DB_TYPE_ENCRYPTED);
	
	// Construct bundleFilename
	const char *dataDir = gConfigManager.DataPath();
	bundleFilename = new char[strlen(dataDir) + strlen(IN_FILENAME) + 1];
	strcpy(bundleFilename, dataDir);
	strcat(bundleFilename, IN_FILENAME);
	
	filePos = '-';
	currentFileIsGlobal = true;	// Assume true for imports
	currentUser = gConfigManager.DLSUser();

	cache.SetMinResize(STRING_MIN_RESIZE);
	cacheWriteFreq = gDeveloperPrefs.GetPref(DB_WRITES);

    // Open and Read the Bundle into cache
	if (!OpenAndReadBundle())
	{
		gLog.Error("Fatal Error trying to read %s", IN_FILENAME);
		return false;
	}
	
	if (cache.IsEmpty()) 
	{
        gLog.Log("Creating File %s.", IN_FILENAME);
		BundleNew();
		InitIndices();		// Set Read and User and File Tag Indices
		ImportFiles();
	}
	else 
	{
		dirtyCache = false;
		InitIndices();		// Set Read and User and File Tag Indices
	}	

	return true;
}

// Read the next word as determined by whitespace, place it in buf
//
void LFileBundle::Read(const char *filename, TString &buf)
{
	bool eof = false;
	buf = "";

	if (filePos = '+')				// Filemarker past desired file or file not found
		return;
	else if (filePos == '-')
		ResetFileReadIndex();
	
	filePos = '=';
	BundleReadUntil(buf, '\t');
	
	if (buf.Find("</File>")) {
		filePos = '+';
		buf = "";
		return;
	}
}

// Read until the given delimiter. Return everything up to
// the delimiter, and throw the delimiter away. 
// (If delim == 0, read until EOF)
//
void LFileBundle::ReadUntil(const char *filename, TString &buf, unsigned char delim)
{
	int eofIndex;
	buf = "";

	if (currentFileKind != fReadOnly)
	{
        gLog.Log("Error: File %s is write-only.", currentFilename.GetString());
		return;
	}

	if (filePos == '+')				// Filemarker past desired file or file not found
		return;
	else if (filePos == '-')
		ResetFileReadIndex();
	
	BundleReadUntil(buf, delim);
	
	// In case delim doesn't exist in the file, make sure the returned string
	// doesn't cross the file boundary
	if ((eofIndex = buf.Find("</File>")) >= 0)
	{
		filePos = '+';
		buf = buf.Mid(0, eofIndex-1); 
		return;
	}
}

// Write some data to the file. Convert \t and \n to tab and
// newline characters respectively.
//
void LFileBundle::Write(const char *filename, TString &data)
{
	const char  *p;
    char		ch;
	TString		buf;
	int			tmpIndex;
	int			writeIndex;

	if (currentFileKind == fReadOnly)
	{
        gLog.Log("Error: File %s is read-only.", currentFilename.GetString());
		return;
	}

	tmpIndex = readIndex;	// save readIndex

	// We don't want resizes too often when adding char by char
	buf.SetMinResize(STRING_MIN_RESIZE);
	
	// Locate the write location
	if (filePos != '=')
		ResetFileReadIndex();

	writeIndex = BundleSearch("</File>");
	if (hangingEOL > 0)
		--writeIndex;

	// Save data up until write location
	buf = cache.Mid(0, writeIndex);

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
                    buf += '\t';
                    break;
                }
                if (ch == 'n') 
                {
                    if (hangingEOL == 2)
						hangingEOL = 1;
					else
						buf += '\n'; 
                    break;
                }

            default:
                buf += ch;
                break;
        }
	}

	// If write didn't include and endline, write one so the file tag 
	// gets written on it's own line
	if (hangingEOL == 0 && buf(buf.Length()-1) != '\n') 
	{
		buf += '\n';
		hangingEOL = 2;
	}
	else if (hangingEOL == 1)
		hangingEOL = 0;

	// Update cache to buf + old stuff located after the write index
	buf += cache.Mid(writeIndex);
	cache = buf;

	dirtyCache = true;
	dirtyFile = true;

	// Write cache to disk if needed
	if (cacheWriteFreq == DB_WRITES_WRITE)
		WriteCache();
		
	readIndex = tmpIndex;	// restore readIndex
}

// Positions the filepointer immediately after the searchString, 
// if the searchString is not found, position at EOF
//
void LFileBundle::Lookup(const char *filename, TString &searchString, int numFields)
{
	int tmpIndex;
	int done = false;
	TString line;

	ResetFileReadIndex();
	
	while (!done) 
	{
		tmpIndex = readIndex;
		BundleReadUntil(line, '\n');
		if (line.StartsWith(searchString)) 
		{
			gDebugLog.Log("Lookup: found \"%s\"", searchString.GetString());
            done = true;
        }

		if (line.StartsWith("</File>"))	// EOF check
            done = true;
	}

	if (line.StartsWith("</File>"))
	{
		filePos = '+';
		gDebugLog.Log("Lookup: NOT found \"%s\"", searchString.GetString());
	}
	else
	{	
		// position the readIndex immediately after searchString
		readIndex = tmpIndex + strlen(searchString);

		// if there is a tab at the readIndex then eat it
		if (cache(readIndex) == '\t')
			++readIndex;
	}
}

// We want to rewrite a record. To do this, we move the record to
// the end of the file and let the scriptor append data with write 
// commands
//
void LFileBundle::Rewrite(const char *filename, TString &searchString, int numFields)
{
	TString		tmpCache;
	TString		buf;
	TString     theLine;
	int			tmpIndex;
    int         done = false;
	int			dLen = 0;

    if (currentFileKind != fWriteAppend)
	{
        gLog.Log("Error: Rewrite expects WRITEAPPEND files.");
		return;
	}

	// We don't want resizes too often when adding line by line
	buf.SetMinResize(STRING_MIN_RESIZE);

	//  Add a tab to the end of the search string so that
    //   "Smith TAB 1" doesn't find "Smith TAB 11". Since we only
    //  check that the searchString begins the record line
    //  this could happen.
    //
    searchString += '\t';

	ResetFileReadIndex();	// Reset file pointer
	tmpIndex = readIndex;
	
	while (!done) 
    {
        //  Copy all lines that don't match to the temp file.
        //
        BundleReadUntil(theLine, '\n');
        if (theLine.StartsWith("</File>"))
		{
			done = true;
		} 
        else if (theLine.StartsWith(searchString, false) == false)
        {
			buf += theLine;
			buf += '\n';
		}
		else 
		{
			dLen = theLine.Length() - searchString.Length();
		}
    }

	//  Add the desired record information.
    buf += searchString;

	// Update the cache
	tmpCache = cache.Mid(0, tmpIndex);
	tmpCache += buf;
	tmpCache += "\n</File>\n";			// readIndex is located past this tag
	tmpCache += cache.Mid(readIndex);	// copy the rest of the bundle into tmpCache

	cache = tmpCache;
	
	dirtyCache = true;
	dirtyFile = true;
	hangingEOL = 2;		// An extra \n was used before </File> tag

	// Write cache to disk if needed
	if (cacheWriteFreq == DB_WRITES_WRITE)
		WriteCache();

	// Position read index imed. after fields rewritten
	readIndex -= 10 + dLen;
}

// Open a file
//
void LFileBundle::Open(const char *filename, FileKind fKind)
{
	char *sFilename = new char[strlen(filename)];
	ShortenFilename(filename, sFilename);	// remove the directory info from filename

	// Check for a duplicate open or a missing close
	if (currentFilename.Equal(sFilename)) {
		gLog.Log("Error. File %s is already open.", sFilename);
		return;
	}
	else if (currentFilename.Equal("") == false)
		Close(currentFilename);

	// update current file attributes
	currentFilename = sFilename;
	currentFileKind = fKind;
	dirtyFile = false;
	hangingEOL = 0;

	// Is it a user or global file?
	currentFileIsGlobal = (globalFiles.Find(sFilename) != NULL);

	// A new file requires tags to be written to our file DB
	// Overwriting a file requires empty its contents on our file DB
	if (fKind == fWriteNew)
	{
		fileTagIndex = BundleCreateFile(sFilename);
	}

	// Position the read pointer
	if (ResetFileReadIndex() < 0)
	{
		gVariableManager.SetString("_ERROR", "-1");
		gDebugLog.Log("Setting _ERROR to -1");
		currentFilename = "";	
	}
}

// Close a file
// (note: filename is in long foramt)
void LFileBundle::Close(const char *filename)
{
	char *sFilename = new char[strlen(filename)];
	ShortenFilename(filename, sFilename);	// remove the directory info from filename
	
	if (currentFilename.Compare(sFilename) != 0)	// currentFilename != filename
	{
		delete [] sFilename;
		return;
	}

	// Update lastmod times and MD5 hashes
	if (dirtyFile)
		UpdateFileHeader(sFilename, fileTagIndex);

	// Write cache to disk if needed
	if (cacheWriteFreq == DB_WRITES_CLOSE)
		WriteCache();

	// Reset file vars
	currentFilename = "";
	filePos = '-';
	fileTagIndex = -1;
	readIndex = userTagIndex;

	//delete [] sFilename; // crashes program when Open calls Close ??
}

// Is the current file open?
//
bool LFileBundle::CurFileOpen(void)
{
	return (currentFilename.Length() > 0);
}

// Have we hit the EOF for the current file?
//
bool LFileBundle::CurFileAtEOF(void)
{
	return (filePos == '+');
}

// Import a file into the Bundle, if the file already exists
// it is overwritten by the imported file
//
void LFileBundle::Import(const char *filename)
{
	ifstream f;
	char inBuf[READ_BUF_SIZE+1];
	TString str;

	gLog.Log("Importing %s.", filename);
	
	f.open(filename, ios::in);
	if (f.fail()) 
	{
		gLog.Log("Failed to open %s.", filename);
		return;
	}
	
	while (!f.eof()) 
	{
		f.read(inBuf, READ_BUF_SIZE);
		inBuf[f.gcount()] = '\0';
		str += inBuf;
	}
	f.close();

	// Make sure an endline occurs at the end of the file data
	if (str(str.Length()-1) != '\n')
		str += '\n';

	char *sFilename = new char[strlen(filename)];
	ShortenFilename(filename, sFilename);	// remove the directory info from filename

	BundleCreateFile(sFilename, str);
	dirtyCache = true;
}

// Mark the list of files as "global" files.  Global files are those that
// can be accessed by multiple users and therfore reside in a special 
// global files block.
//
void LFileBundle::AddGlobalFiles(TString &fileList)
{
	int start = 0, end = 0;
	TString gFile;

	while (end >= 0) 
	{
		end = fileList.Find(',', start);
		if (end > 0) 
		{
			gFile = fileList.Mid(start, end - start);
			globalFiles.Add(new TBNode(gFile));
			start = end + 1;
		}
		else // last file in the list - no ending comma
		{
			gFile = fileList.Mid(start);
			globalFiles.Add(new TBNode(gFile));
		}
	}
}

//////////////////////////////////////////////////////////////////////////
///////////////////////////// Private ////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

// Chop the directories of the filename and make it lower-case
// E.g. "c:\foo\bar\Test.dat" becomes "test.dat"
void LFileBundle::ShortenFilename(const char *filename, char *sFilename)
{
	int i, len, slash = 0;

	len = strlen(filename); 
	for (i=len-1; i>=0; i--)
	{
		if (filename[i] == '\\') 
		{
			slash = i;
			break;
		}
	}	

	if (slash == 0) {
		strcpy(sFilename, filename);
		return;
	}

	for (i=0; i < len - slash; i++)
		sFilename[i] = filename[slash+i+1];
		
	_strlwr(sFilename);				// make the filename lower case
}

// Read from the bundle until delim is found (contents stored into buf)
// Note: Delimiter is tossed 
void LFileBundle::BundleReadUntil(TString &buf, unsigned char delim)
{
	int		index;

    buf = "";

	if (BundleEOF()) 
	{
		gDebugLog.Log("BundleReadUntil: at EOF");
		return;
	}

	index = cache.Find(delim, readIndex);
	if (index >= 0) {
		buf = cache.Mid(readIndex, index - readIndex);
		readIndex = index + 1;
	}
	else {
		buf = cache.Mid(readIndex);	// read until EOF	
		readIndex = cache.Length();
	}

	//gDebugLog.Log("BundleReadUntil: returned \"%s\"", buf.GetString());
} 

// Just to make code a little cleaner when not interested in string read 
// Tosses the data stored in the buffer
void LFileBundle::BundleReadUntil(unsigned char delim)
{
	TString foo;

	BundleReadUntil(foo, delim);
}

// Search for a string at the beginning of a line
// Positions the readIndex according to position 
//	(0=beginning of line following searchString, 1=immediately after searchString)
// Returns index into Bundle where searchString was found
// If not found, returns -1
int LFileBundle::BundleSearch(const char *searchString, int position)
{
	int tmpIndex;
	TString buf;
	
	while (true) {
		tmpIndex = readIndex;
		BundleReadUntil(buf, '\n');
		if (buf.StartsWith(searchString)) 
		{
			//gDebugLog.Log("BundleSearch: found \"%s\"", searchString);
            break;
        }

		if (BundleEOF())	// EOF check
            return -1;
	}

	if (position == 1)
	{	
		// position the readIndex immediately after searchString
		readIndex = tmpIndex + strlen(searchString);
	}
	return tmpIndex;
}

// Creates the initial tags for the bundle
void LFileBundle::BundleNew()
{
	cache = "<Program name=\"XXX\">\n";
	cache += "<UserDB lastmod=\"";
	cache += GetTimestamp();
	cache += "\">\n";

	cache += "<User name=\"_GLOBAL_\" lastmod=\"";
	cache += GetTimestamp();
	cache += "\">\n";
	cache += "</User>\n";
	
	cache += "</UserDB>\n";
	cache += "</Program>\n";
}

// Make tags for a new user 
// readIndex is set to the beginning of the User tag
// returns the user tag index of the new user
int LFileBundle::BundleCreateUser(const char *username)
{
	TString buf;

	readIndex = 0;
	BundleSearch("<UserDB");
	//BundleReadUntil(buf, '\n');  // Not needed since BundleSearch reads a line at a time
	
	buf = cache.Mid(0, readIndex);

	buf += "<User name=\"";
	buf += currentUser;
	buf += "\" lastmod=\"";
	buf += GetTimestamp();
	buf += "\">\n";
	buf += "</User>\n";
	
	cache = buf + cache.Mid(readIndex);
	return readIndex;
}

// Create a file or erase a file if it already exists for the current user
// Optionally the initial file contents may be supplied
// Positions the readIndex at the beginning of the file data
// Return the file tag index of the newly created file
int LFileBundle::BundleCreateFile(const char *filename, const TString &contents)
{
	TString buf;
	TString searchString;
	int tmpIndex;
	int fIndex;
	int fTagIndex;
	int found = 0;

	buf.SetMinResize(STRING_MIN_RESIZE);
	readIndex = currentFileIsGlobal ? globalUserTagIndex : userTagIndex;

	BundleReadUntil(buf, '\n');
	tmpIndex = readIndex;

	// Does this file already exist, if so overwrite it
	searchString = "<File name=\"";
	searchString += filename;
	while (!found) 
	{
		BundleReadUntil(buf, '\n');
		if (buf.StartsWith(searchString))
            found = 1;
		else if (buf.StartsWith("</User>"))
			found = -1;
	}
	
	// Erase existing file contents
	if (found > 0) 
	{
		tmpIndex = readIndex;
		fTagIndex = readIndex - (21 + strlen(filename) + MD5_SIZE*2) - 1;

		fIndex = BundleSearch("</File>");
		buf = cache.Mid(0, tmpIndex);
		buf += contents;					// initial contents
		cache = buf + cache.Mid(fIndex);
		UpdateFileHeader(filename, fTagIndex);

		readIndex = tmpIndex;
		return fTagIndex;
	}
	// New user file
	else {
		readIndex = tmpIndex;

		buf = cache.Mid(0, readIndex);
		buf += searchString;			// Contains first part of file tag

		//buf += "\" lastmod=\"";		// if added back in, need to fix offset
		//buf += GetTimestamp();		// when changing md5
		
		buf += "\" md5=\"";
		buf += ComputeMD5(contents);
		
		buf += "\">\n";
		buf += contents;				// initial contents of file
		buf += "</File>\n";
		
		cache = buf + cache.Mid(readIndex);
		
		return readIndex; 
	}
}

// Write the cache back to disk using an encrypted stream
void LFileBundle::WriteCache()
{
	if (!dirtyCache)
		return;

	if (dirtyFile)
		UpdateFileHeader(currentFilename.GetString(), fileTagIndex);

	RewriteBundle();

	dirtyCache = false;
}

// Reposition the READ file pointer to the beginning of the current file
// Returns file tag index or -1 if there is an error
int LFileBundle::ResetFileReadIndex()
{
	TString searchString;

	if (fileTagIndex > 0) 
	{
		readIndex = fileTagIndex;
		BundleReadUntil('\n');
	}
	// We need to search for the file tag index
	else 
	{
		readIndex = currentFileIsGlobal ? globalUserTagIndex : userTagIndex;
		searchString = "<File name=\"";
		searchString += currentFilename;
		fileTagIndex = BundleSearch(searchString);
		// Do not need to read until end of line, BundleSearch reads a line at a time
	}

	if (fileTagIndex > 0) 
	{
		filePos = '=';
		gDebugLog.Log("ResetFileIndex: %s", currentFilename.GetString());
	}
	else 
	{						// file not found
		filePos = '+';
		gDebugLog.Log("ResetFileIndex Failed: %s", currentFilename.GetString());
		return -1;
	}
	return fileTagIndex;
}

// Initialize tag indices
void LFileBundle::InitIndices()
{
	int tmpIndex;
	TString searchString;

	readIndex = 0;		// Tell BundleReadUntil to start at index 0
	
	// First locate the UserDBTag
	searchString = "<Program name=\"";
	searchString += "XXX";			// We need a way of pulling in a program name
	BundleSearch(searchString); 
	//BundleReadUntil('\n');		// Not needed since BundleSearch reads a line at a time
	userDBTagIndex = readIndex; 
	
	// We know the user, so find the user tag
	searchString = "<User name=\"";
	searchString += currentUser;
	tmpIndex = BundleSearch(searchString);

	userTagIndex = (tmpIndex > 0) ? tmpIndex : BundleCreateUser(currentUser);
	
	InitGlobalUserTag();

	fileTagIndex = -1;
}

// recalculates globalUserTagIndex
void LFileBundle::InitGlobalUserTag()
{
	TString searchString;
	int		saveReadIndex;

	saveReadIndex = readIndex;
	readIndex = 0;

	searchString = "<User name=\"";
	searchString += "_GLOBAL_";
	globalUserTagIndex = BundleSearch(searchString);
	ASSERT(globalUserTagIndex > 0);

	readIndex = saveReadIndex;
}

// Have we reached the end of the bundle?
int LFileBundle::BundleEOF()
{
	return (readIndex >= cache.Length());		
}


// Returns a date/time stamp in the format DD/MM/YY HH:MM:SS (24-hour time)
TString LFileBundle::GetTimestamp()
{
	char tmpbuf[32];
	TString str;

	_strdate( tmpbuf );
	str = tmpbuf;

	str += " ";

	_strtime( tmpbuf );
	str += tmpbuf;

	return str;
}

// Compute the MD5 hash for the given string
TString LFileBundle::ComputeMD5(const TString &str)
{
	md5_state_t		state;
	md5_byte_t		digest[MD5_SIZE];
	char			md5_string[MD5_SIZE*2+1];

	// Calculate the MD5 digest of our data.
	md5_init(&state);
	md5_append(&state, reinterpret_cast<const md5_byte_t*>(str.GetString()), str.Length());
	md5_finish(&state, digest);

	// Convert the digest to a string and return it.
	for (int i = 0; i < MD5_SIZE; i++)
		sprintf(md5_string + i * 2, "%02x", (int) digest[i]);
	return TString(md5_string);
}

// Update the MD5 hash for the the file with tag located at fTagIndex
/*
void LFileBundle::UpdateMD5(const char *filename, int fTagIndex)
{
	int index; 
	int len; 
	TString md5; 

	index = fTagIndex + 21 + strlen(filename) + MD5_SIZE*2;
	len = cache.Find("</File>", index) - index;
	md5 = ComputeMD5(cache.Mid(index, len));
	cache.Set(fTagIndex + 19 + strlen(filename), md5.Length(), md5);
}
*/

void LFileBundle::UpdateFileHeader(const char *filename, int fTagIndex)
{
	int		index; 
	int		len; 
	TString timestamp;
	TString md5;

	timestamp = GetTimestamp();
	
	// <UserDB> Tag
	cache.Set(userDBTagIndex + 17, timestamp.Length(), timestamp);

	// <User> Tag
	if (fTagIndex > globalUserTagIndex)
		cache.Set(globalUserTagIndex + 23 + 8, timestamp.Length(), timestamp); // _GLOBAL_ = 8
	else
		cache.Set(userTagIndex + 23 + currentUser.Length(), timestamp.Length(), timestamp);
	
	// <File> Tag
	//
	// update MD5
	index = fTagIndex + 21 + strlen(filename) + MD5_SIZE*2;
	len = cache.Find("</File>", index) - index;
	md5 = ComputeMD5(cache.Mid(index, len));
	cache.Set(fTagIndex + 19 + strlen(filename), md5.Length(), md5);

	// Recalc global user index if needed
	if (fTagIndex < globalUserTagIndex)
		InitGlobalUserTag();

	// Reset dirty file if we are dealing with the current file
	if (currentFilename.Equal(filename))
		dirtyFile = false;
}

// Import all files located in INIT_DATA_DIR into the bundle
// Assumes all imported files are global
void LFileBundle::ImportFiles()
{
	TString str;
	const char *dataPath;
	char *searchString;
	WIN32_FIND_DATA FindFileData;
	HANDLE hFind;

	dataPath = gConfigManager.DataPath();

	searchString = new char[strlen(dataPath) + 20];	// 20 should be enough
	strcpy(searchString, dataPath);
	strcat(searchString, INIT_DATA_DIR);
	strcat(searchString, "\\*");	// add wilcard
	
	// Assume all imported files are global
	currentFileIsGlobal = true;

	hFind = ::FindFirstFile(searchString, &FindFileData);

	if (hFind == INVALID_HANDLE_VALUE) {
		//printf ("Invalid File Handle. Get Last Error reports %d\n", GetLastError ());
		return;
	} 
	
	// Skip over the "." and ".." files
	//Import(FindFileData.cFileName);
	::FindNextFile(hFind, &FindFileData);

	while (::FindNextFile(hFind, &FindFileData))
	{
		char *cFile = FindFileData.cFileName;
		
		if (strcmp(cFile, IN_FILENAME) != 0 &&
			strstr(cFile, ".exe") == NULL &&	 // ignore .exe setup files
			FindFileData.dwFileAttributes != FILE_ATTRIBUTE_DIRECTORY) // ignore directories
		{
			str = dataPath;
			str += INIT_DATA_DIR;
			str += "\\";
			str += FindFileData.cFileName;
			Import(str.GetString());
			//::DeleteFile(str.GetString());
		}
	}
	FindClose(hFind);
}

// 0 = clear -> encrypted,  1 = encrypted -> clear 
void LFileBundle::ConvertBundle(int dir)
{
	if (dir == 0)
	{
		gLog.Log("ConvertBundle: Trying to convert %s, clear -> encrypted", IN_FILENAME);

		if (cryptStream != NULL)
			cryptStream->cryptFile(0);
	}
	else if (dir == 1)
	{
		gLog.Log("ConvertBundle: Trying to convert %s, encrypted -> clear", IN_FILENAME);

		if (cryptStream == NULL)
			cryptStream = new CryptStream(FileSystem::GetDataDirectory(), IN_FILENAME,
						                  PAYLOAD_5LDB, HCK, HCK_SIZE);

		cryptStream->cryptFile(1);
		delete cryptStream;
	}
}

// Open the Bundle and read the contents in cache
bool LFileBundle::OpenAndReadBundle()
{
	if (!isEncrypted)
	{
		uchar inBuf[READ_BUF_SIZE+1];
		int readCount;

		clearFileStream = new ifstream(bundleFilename, ios::in);
		cache = "";

		// Is the file empty?
		if (clearFileStream->peek() == EOF)
			return true;

		// Read in the data
		while (!clearFileStream->eof()) 
		{
			clearFileStream->read(inBuf, READ_BUF_SIZE);
			readCount = clearFileStream->gcount();
			inBuf[readCount] = '\0';
			cache += (char *)inBuf;
			if (clearFileStream->bad())
				return false;
		}

		// Verification
		if (cache.Find("<Program") < 0)
		{
			// Try conversion
			clearFileStream->close();
			ConvertBundle(1);

			// Re-open, read and re-verify
			clearFileStream->open(bundleFilename, ios::in);
			cache = "";
			while (!clearFileStream->eof()) 
			{
				clearFileStream->read(inBuf, READ_BUF_SIZE);
				readCount = clearFileStream->gcount();
				inBuf[readCount] = '\0';
				cache += (char *)inBuf;
				if (clearFileStream->bad())
					return false;
			}

			if (cache.Find("<Program") < 0)
				return false;
		}
	}
	else
	{
		cryptStream = new CryptStream(FileSystem::GetDataDirectory(), IN_FILENAME, 
									  PAYLOAD_5LDB, HCK, HCK_SIZE);
		cache = "";

		// Is the file empty?
		if (cryptStream->in_isEmpty())
			return true;

		// Read in the data
		cryptStream->fileToString(cache);
		cache.RTrim();						// trim whitespace at end
		if (cryptStream->in_bad())
			return false;

		// Verification
		if (cache.Find("<Program") < 0)
		{
			// Try conversion
			ConvertBundle(0);

			// Read in and re-verify
			cryptStream->fileToString(cache);
			cache.RTrim();
			if (cryptStream->in_bad())
				return false;

			if (cache.Find("<Program") < 0)
				return false;
		}
	}

	return true;
}

void LFileBundle::CloseBundle()
{
	if (!isEncrypted)
	{
		clearFileStream->close();
	}
	else
		cryptStream->close();

	// Write-back the cache
	WriteCache();	
}

// Rewrite the bundle to disk using contents stored in cache.
void LFileBundle::RewriteBundle()
{
	//TString tmpFilename = bundleFilename;
	//tmpFilename += ".tmp";

	if (!isEncrypted)
	{
		ofstream out;
		
		clearFileStream->close();

		out.open(bundleFilename, ios::out | ios::binary);	// overwrite the old file
		out << cache;

		//out.open(tmpFilename, ios::out);
		//out << cache;

		if (out.fail())
		{
			gLog.Log("Write error on file \"%s\".", IN_FILENAME);
			return;
		}
		out.close();
		
		/*
		// Delete Original
		::DeleteFile(bundleFilename);

		// Rename temp file
		::MoveFile(tmpFilename, bundleFilename);
		*/

		// Reopen
		clearFileStream->open(bundleFilename, ios::in);
	}
	else
	{
		cryptStream->rewriteFile(cache.GetString(), cache.Length());

		if (cryptStream->out_fail())
		{
			gLog.Log("Write error on file \"%s\".", IN_FILENAME);
			return;
		}
	}
}
