/*
    Files.cpp

*/

#include "debug.h"

#include "CString.h"
#include "CFiles.h"
#include "CConfig.h"
#include "CVariable.h"

#include "util.h"

/********************

    CFile METHODS

********************/

//  Open the given file for reading and writing. If writing and it doesn't
//			exist, create it first.
//
CFile::CFile(char *filename, FileKind fKind)
{
	FInfo	theFInfo;
	FSSpec	fSpec;
	OSErr	err;
    int8    fPerm;

    itsKind = fKind;
    itsName = filename;
    
    gVariableManager.SetLong("_error", 0);		// assume everything will be OK
    
    switch (itsKind) 
    {
		case fWriteAppend:
        case fWriteNew:
        	fPerm = fsRdWrPerm;
            break;
        case fReadOnly:
        	fPerm = fsRdPerm;
            break;
    }
    
    theConfig->FillDataSpec(&fSpec, filename);
	
	// cbo_fix - 
	// we have to catch the exceptions that could get thrown here
	try
	{
	    itsFile = new CTextFileStream(fSpec);
    
	    if (itsKind == fWriteNew)					// create it first
	    {
	    	err = FSpGetFInfo(&fSpec, &theFInfo);	// unless it already exists
	    	
	    	if (err == fnfErr)						// not there, create it
    			itsFile->CreateNewDataFile('R*ch', 'TEXT');	// cbo_fix - BBEdit as the creator for now
    	}
    	      
    	itsFile->OpenDataFork(fPerm);
    	
    	if (itsKind == fWriteNew)
    	{
    		itsFile->SetLength(0);					// clear out the file
    	}
		else if (itsKind == fWriteAppend)
		{			
    		itsFile->SetMarker(itsFile->GetLength(), streamFrom_Start);	// seek to the end of the file
   		}
    }
    
    catch (const LException& inException) 
    {
#ifdef DEBUG_5L
		prinfo("Couldn't open file <%s>, setting _error to -1", filename);
#endif

    	gVariableManager.SetLong("_error", -1);		// couldn't open the file
    	
    	// cbo_fix - do something appropriate here
    	// prerror("File <%s> not found. Quitting.", filename);
    
    }
}

//  Cleanup. Destroy the file object.
//
CFile::~CFile()
{
	itsFile->CloseDataFork();
    delete itsFile;
}

/***********************************************************************
 * Function: CFile::Match
 *
 *  Parameter aName
 * Return:
 *   Return true if aName is the file's name.
 * Comments:
 *      Beautiful!  this shit only used once ^$%&#!
 ***********************************************************************/
int CFile::Match(const char *aName)
{
    return itsName.Equal(aName, FALSE);
}

/***********************************************************************
 * Function: CFile::Read
 *
 * Parameter: str - CString to assign into.
 * Return:
 *
 * Comments:
 *      Read the next word as determined by whitespace.
 ***********************************************************************/
void CFile::Read(CString &str)
{
    if ((itsKind != fReadOnly) and (itsKind != fWriteAppend))
        prcaution("File %s is write-only.", (char *)itsName);
	else
	{
		readBuf[0] = '\0';
		itsFile->ReadString(readBuf);
	
		str = readBuf;
	}
}

/***********************************************************************
 * Function: CFile::ReadUntil
 *
 *  Parameter str (where to put it)
 *  Parameter delim  (If delim == 0, read until EOF.)
 * Return:
 *
 * Comments:
 *   Read until the given delimiter. Return everything up to
 *  the delimiter, and throw the delimiter away.
 ***********************************************************************/
void CFile::ReadUntil(CString &str, unsigned char delim)
{
    if ((itsKind != fReadOnly) and (itsKind != fWriteAppend))
        prcaution("File %s is write-only.", (char *)itsName);
    else
    {    
		readBuf[0] = '\0';
		itsFile->ReadTillChar(readBuf, delim);
	
		str = readBuf;
	}
}

/***********************************************************************
 * Function: CFile::Write
 *
 *  Parameter data  (what to write)
 * Return:
 *
 * Comments:
 *  Write some data to the file. Convert \t and \n to tab and
 *  newline characters respectively.
 ***********************************************************************/
void CFile::Write(CString &data)
{
	char    *ptr;
	int32	count = 0;
    char    ch;
    bool	done = FALSE;

    if (itsKind == fReadOnly)
    {
        prcaution("File %s is read-only.", (char *) itsName);
        return;
    }

    ptr = data.GetString();
    
    while (not done)
    {
    	if ((ch = *ptr++) != '\0')
    	{
    		if (ch == '\\')					// backslashed char?
    		{
    			if ((ch = *ptr++) != '\0')	
    			{
    				if (ch == 't')
    					ch = '\t';			// tab
    				else if (ch == 'n')
    					ch = NEWLINE_CHAR;
    					//ch = '\n';			// newline
    				else
    					ch = ' ';			// make it a space??
    			}
    			else
    				done = TRUE;
    		}
    		
    		if (not done)
    		{
    			writeBuf[count++] = ch;		// put char in write buffer
    			
    			if (count == (BUFFER_SIZE + 1))
    			{
    				itsFile->Write(writeBuf, count);	// write out a buffer
    				count = 0;
    			}
    		}
    	}
    	else
    		done = TRUE;
    }
    
	itsFile->Write(writeBuf, count);
}

/***********************************************************************
 * Function: CFile::Lookup
 *
 *  Parameter searchString
 *  Parameter numFields
 * Return:
 *
 * Comments:
 *      Read until we find the given search string at the start of a
 *  line or we hit the end of file.
 ***********************************************************************/
void CFile::Lookup(CString &searchString, int32 numFields)
{
    CString     theField, comparison;
    int32       count;
    bool        done = FALSE;

	itsFile->SetMarker(0, streamFrom_Start);	// seek to start of file

    while (not done) 
    {
        comparison = "";

        //  Read the first n fields of the record for comparison.
        //
        for (count = 0; count < numFields; count++) 
        {
        	Read(theField);				// will skip whitespace
           // ReadUntil(theField, '\t');
            if (count > 0)
                comparison += '\t';
            comparison += theField;
        }

        //  Now compare. Case insensitive.
        //
        if (searchString.Equal(comparison, FALSE))  
        {
            done = TRUE;
#ifdef DEBUG_5L
            prinfo("lookup: found <%s>, in <%s>", searchString.GetString(), comparison.GetString());
#endif
        }
#ifdef DEBUG_5L
		else
			prinfo("lookup: didn't find <%s> in <%s>", (char *) searchString, (char *) comparison);
#endif
        
        if (itsFile->AtEOF())
        	done = TRUE;

        //  If not a match, read until the next line.
        //
        if (not done)
           itsFile->SkipEOL();	// don't use ReadUntil here as it skips whitespace BEFORE
           						// reading which could skip a whole line
    }
}

/***********************************************************************
 * Function: CFile::Rewrite
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
void CFile::Rewrite(CString &searchString, int32 /* numFields */)
{
	CFile			*tempFile;
    CString     	theLine;
    CString     	tempName("temp5L");
    FSSpec			tempSpec, goodSpec;
    OSErr			err;
    bool       		done = FALSE;

    if (itsKind != fWriteAppend)
    {
        prcaution("Rewrite expects WRITEAPPEND files.");
        return;
    }

    //  Add a tab to the end of the search string so that
    //   "Smith TAB 1" doesn't find "Smith TAB 11". Since we only
    //  check that the searchString begins the record line
    //  this could happen.
    //
    searchString += '\t';

    tempFile = new CFile(tempName, fWriteNew);
    
    itsFile->SetMarker(0, streamFrom_Start);	// seek to start of file

    while (not done) 
    {
        //  Copy all lines that don't match to the temp file.
        //
        ReadUntil(theLine, NEWLINE_CHAR);
        
        if (theLine.empty()) 
            done = TRUE;
        else if (theLine.StartsWith(searchString, FALSE) == FALSE)
        {
        	// this line doesn't match the search criteria, write it out
        	theLine += NEWLINE_CHAR;		// put the newline back on the file
        	tempFile->Write(theLine);
		}
		
        if (itsFile->AtEOF())
            done = TRUE;
    }

    tempFile->Write(searchString);			// write out the search string

    //  Close both files.
    //
    tempFile->itsFile->CloseDataFork();
    tempFile->itsFile->GetSpecifier(tempSpec);
    itsFile->CloseDataFork();
    itsFile->GetSpecifier(goodSpec);
    
    //  Exchange the two files and delete the temp one.
    //
    err = ::FSpExchangeFiles(&tempSpec, &goodSpec);
    err = ::FSpDelete(&tempSpec);
    
    // now reopen the file and seek to the end so all writes will come after what we just wrote
    itsFile->OpenDataFork(fsRdWrPerm);
	itsFile->SetMarker(itsFile->GetLength(), streamFrom_Start);
    
    delete tempFile;
}

//
//	AtEOF - Return true if we are at the end of the file, otherwise return
//			true.
//
bool CFile::AtEOF(void)
{
	if (itsFile != nil)
		return (itsFile->AtEOF());
	else
		return (false);
}

/************************

    CFileLIST METHODS

************************/

CFileList::CFileList()  
{
    CurrentFile = nil;
}

CFileList::~CFileList()  
{
    delete CurrentFile;
    CurrentFile = nil;
}

/***********************************************************************
 * Function: CFileList::FindFile
 *
 *  Parameter filename
 *  Parameter failClosed
 * Return:
 *  	CFile
 * Comments:
 *  Find the file in the array by name. If failClosed == TRUE,
 *  fail if the file isn't found.
 ***********************************************************************/
CFile *CFileList::FindFile(char *filename, int failClosed)
{
	CheckPath(filename);
	
    if ((CurrentFile != nil) and (CurrentFile->Match(filename)))
            return (CurrentFile);

    if (failClosed)
        prcaution("File <%s> not found.", filename);

    return NULL;
}

/***********************************************************************
 * Function: CFileList::Open
 *
 *  Parameter filename
 *  Parameter fKind     (READ, WRITE or APPEND)
 * Return:
 *
 * Comments:
 *    Open a file. Make sure it's not already open.
 ***********************************************************************/
void CFileList::Open(char *filename, FileKind fKind)
{
    CFile   *theFile;
    int32	theError;

    theFile = FindFile(filename, FALSE);
    if (theFile != NULL)
    {
        prcaution("File %s is already open.", filename);
        return;
    }

    CurrentFile = new CFile(filename, fKind);
    
    theError = gVariableManager.GetLong("_error");
    if (theError < 0)
    {
    	delete CurrentFile;
    	CurrentFile = nil;
    }
}

/***********************************************************************
 * Function: CFileList::Close
 *
 *  Parameter filename
 * Return:
 *
 * Comments:
 *   Close an open file and remove it from the array.
 ***********************************************************************/
void CFileList::Close(char *filename)
{
	CFile	*theFile;
	
    theFile = FindFile(filename, TRUE);
    if (theFile != nil)
    {
		delete CurrentFile;
		CurrentFile = nil;
	}
#ifdef DEBUG_5L
	else
		prcaution("Trying to close file <%s>, it isn't open!", filename);
#endif
}

/***********************************************************************
 * Function: CFileList::Read
 *
 *  Parameter filename
 *  Parameter str
 * Return:
 *
 * Comments:
 *   Read certain data from a file.
 ***********************************************************************/
void CFileList::Read(char *filename, CString &str)
{
    CFile   *theFile;

    theFile = FindFile(filename, TRUE);
    if (theFile != NULL)
    	theFile->Read(str);
}
 
/***********************************************************************
 * Function: CFileList::ReadUntil
 *
 *  Parameter filename
 *  Parameter str      (put data here)
 *  Parameter delim    (use this delimiter to read one item)
 * Return:
 *
 * Comments:
 *  Read certain data from a file.
 ***********************************************************************/
void CFileList::ReadUntil(char *filename, CString &str, unsigned char delim)
{
    CFile   *theFile;

    theFile = FindFile(filename, TRUE);
    if (theFile != NULL)
    	theFile->ReadUntil(str, delim);
}
    
/***********************************************************************
 * Function: CFileList::Write
 *
 *  Parameter filename
 *  Parameter data   (to write)
 * Return:
 *
 * Comments:
 *      Write given data to the file.
 ***********************************************************************/
void CFileList::Write(char *filename, CString &data)
{
    CFile   *theFile;

    theFile = FindFile(filename, TRUE);
    if (theFile != NULL)
    	theFile->Write(data);
}

/***********************************************************************
 * Function: CFileList::Lookup
 *
 *  Parameter filename
 *  Parameter searchString    (search what)
 *  Parameter numFields       (up to how many records)
 * Return:
 *
 * Comments:
 *   Try to find a particular record in the file.
 ***********************************************************************/
void CFileList::Lookup(char *filename, CString &searchString, int numFields)
{
    CFile   *theFile;

    theFile = FindFile(filename, TRUE);
    if (theFile != NULL)
    	theFile->Lookup(searchString, numFields);
}

/***********************************************************************
 * Function: CFileList::Rewrite
 *
 *  Parameter filename
 *  Parameter searchString
 *  Parameter numFields
 * Return:
 *
 * Comments:
 *  Rewrite (move to end of file) a particular record in the file.
 ***********************************************************************/
void CFileList::Rewrite(char *filename, CString &searchString, int numFields)
{
    CFile   *theFile;

    theFile = FindFile(filename, TRUE);
    if (theFile != NULL)
    	theFile->Rewrite(searchString, numFields);
}

//
//	CurFileOpen - Return true if there is a current open file, false
//		otherwise.
//
bool CFileList::CurFileOpen(void)
{
	if (CurrentFile != nil)
		return (true);
	else
		return (false);
}

//
//	CurFileAtEOF - Return true if the current file is at the end of file,
//		otherwise return false.
//
bool CFileList::CurFileAtEOF(void)
{
	if (CurrentFile != nil)
		return (CurrentFile->AtEOF());
	else
		return (true);
}

//
//	CheckPath - Get any DOS-ness out of the path.
//
void CFileList::CheckPath(char *inPath)
{
	char	*slashPtr;
	
	// See if the name is a DOS path.
	slashPtr = inPath;
	while (slashPtr != NULL)
	{
		slashPtr = strstr(slashPtr, "\\");
		if (slashPtr != NULL)
			*slashPtr = ':';
	}
}
