// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "stdafx.h"

// Needed for RegisterWindowsPrimitives.
#include "TCommon.h"
#include "TPrimitives.h"
#include "TWin5LPrimitives.h"

// Needed to implement primitives.
#include "TWin5LInterpreter.h"
#include "FileSystem.h"

using FileSystem::GetDataFilePath;
using FileSystem::Path;


//=========================================================================
//  RegisterWindows5LPrimitives
//=========================================================================
//  Install our Windows- and 5L-specific primitive functions.  Most of this
//  code has been migrated out of TWinPrimitives.cpp.

void FIVEL_NS RegisterWindows5LPrimitives()
{
    REGISTER_5L_PRIMITIVE(Close);
    REGISTER_5L_PRIMITIVE(GlobalFiles);
    REGISTER_5L_PRIMITIVE(Lookup);
    REGISTER_5L_PRIMITIVE(Open);
    REGISTER_5L_PRIMITIVE(Read);
    REGISTER_5L_PRIMITIVE(Rewrite);
    REGISTER_5L_PRIMITIVE(Write);
}

/*----------------------------------------------------------------------
    (CLOSE FILENAME)

    Close the given text file.
------------------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Close)
{
	std::string	filename;

	inArgs >> filename;
    Path path = FileSystem::GetDataFilePath(filename);
    gFileManager.Close(path.ToNativePathString().c_str());
}

/*---------------------------------------------------------------
    (GLOBALFILES FILE1 [FILE2]...[FILEN])

    Append file(s) to the list of global files.  Global files are
	those that may be accessed my multiple FiveL users.
-----------------------------------------------------------------*/
#ifdef USE_BUNDLE
DEFINE_5L_PRIMITIVE(GlobalFiles)
{
	TString gFile;
	TString fileList;
	int numFields = 0;

	//  Make a comma separated list
    //
    while (inArgs.HasMoreArguments()) 
    {
        inArgs >> gFile;
        if (numFields > 0)
            fileList += ',';
        numFields++;
        fileList += gFile;
    }

	if (!fileList.IsEmpty())
		gFileManager.AddGlobalFiles(fileList);
}
#endif

/*------------------------------------------------------------------
    (LOOKUP FILENAME FIELD1 <FIELD2> ... <FIELDN>)

    FILENAME    The text file to lookup the record in.

    FIELD1..N   The fields which must match (case not important) the
                record.

    Assumes the text file is a tab delimited, return marked database
    and tries to find the record that matches the first N fields. If
    it succeeds, the file pointer is positioned after the first N
    fields and the scriptor can start doing (read..until tab) calls.
    If it fails, the pointer is at end of file.
--------------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Lookup)
{
    TString     searchString, param, filename;
    int         numFields = 0;

    inArgs >> filename;

    //  Append all the fields together into a search string that looks
    //  like "field1 TAB field2 TAB ... fieldN"
    //
    while (inArgs.HasMoreArguments()) 
    {
        inArgs >> param;
        if (numFields > 0)
            searchString += '\t';
        numFields++;
        searchString += param;
    }

	Path path = FileSystem::GetDataFilePath(filename.GetString());

    gFileManager.Lookup(path.ToNativePathString().c_str(), searchString, numFields);
}

/*----------------------------------------------------------------
    (OPEN FILENAME KIND)

    Open a text file. KIND specifies the kind of access one
    will have to the file. It may be APPEND (write only, appending
    to the end of the file), NEW (write only, overwriting
    anything in the file), or READONLY.
------------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Open)
{ 
    TString     filename, kind;
    FileKind    fKind; 
    bool		open_file = true;
    
    inArgs >> filename >> kind;
    kind.MakeLower();

    if (kind.Equal("append"))
    	fKind = fWriteAppend;
    else if (kind.Equal("new"))
    	fKind = fWriteNew;
    else if (kind.Equal("readonly"))
    	fKind = fReadOnly;
    else
	{
        gLog.Log("Error: Unknown open file kind: %s.", (const char *) kind);
		return;
	}

	Path path = GetDataFilePath(filename.GetString());
    gVariableManager.SetString("_ERROR", "0");
    
/*	Does not seem to work, also leaves an empty file when non-existent -SS
	
	//check for existence of file:
    if ((fKind == fReadOnly) || (fKind == fWriteAppend))
    {
		ifstream theFile;

		theFile.open(tempname, ios::binary);    
        if (!theFile.is_open())
    	{
        	gVariableManager.SetString("_ERROR", "-1");
			gDebugLog.Log("open: file doesn't exist, setting _ERROR to -1");
			open_file = false;
        }
        else
        	theFile.close();
    }	
*/    
    // if not new and we couldn't find it, don't try to open it
    if (open_file)
    	gFileManager.Open(path.ToNativePathString().c_str(), fKind);
}

/*-----------------------------------------------------------------
    (READ FILENAME VARIABLE <UNTIL DELIM>)

    Read data from a text file and put it into the variable.
    Normally this will read the next word as defined by whitespace.
    Use the UNTIL DELIM construct to read until some other
    delimiter.

    Valid delimiters are TAB, RETURN, EOF, or any single character.

    TODO - Replace the bogus "UNTIL" argument with a keyword
	argument, or remove it entirely.
-------------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Read)
{
	TString 		tempname;
    TString         filename, vname, until, delimstr;
    unsigned char   delim;
    TString         res;

    inArgs >> filename >> vname;
 
	Path path = GetDataFilePath(filename.GetString());

    if (inArgs.HasMoreArguments()) 
    {
        inArgs >> until >> delimstr;
        delimstr.MakeLower();
        if (delimstr.Equal("tab")) 
        	delim = '\t';
        else if (delimstr.Equal("return")) 
        	delim = '\n';
        else if (delimstr.Equal("eof"))
        	delim = 0;
        else 
        	delim = delimstr(0);

        gFileManager.ReadUntil(path.ToNativePathString().c_str(), res, delim);
        
    } 
	else
	{
		gFileManager.Read(path.ToNativePathString().c_str(), res);
	}

    gVariableManager.SetString(vname, res);
}

/*----------------------------------------------------------------------
    (REWRITE FILENAME FIELD1 <FIELD2> ... <FIELDN>)

    FILENAME    The text file which contains the record to be rewritten.

    FIELD1..N   The fields which define the record.

    Given a file that is open for appending, this command will look up
    the given record, as specified by the fields. It will move that
    record to the end of the file and position the file pointer at the
    end of the file (appending) so that the specific data may be
    written.
------------------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Rewrite)
{ 
    TString     searchString, param, filename;
    int         numFields = 0;

    inArgs >> filename;

    //  Append all the fields together into a search string that looks
    //  like "field1 TAB field2 TAB ... fieldN"
    //
    while (inArgs.HasMoreArguments()) 
    {
        inArgs >> param;
        if (numFields > 0)
            searchString += '\t';
        numFields++;
        searchString += param;
    }

	Path path = GetDataFilePath(filename.GetString());

    gFileManager.Rewrite(path.ToNativePathString().c_str(), searchString, numFields);
}

/*-----------------------------------------------------------
    (WRITE FILENAME DATA)

    Write the given data to the file.
-------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Write)
{
	TString 	tempname;
    TString     filename, data;

    inArgs >> filename >> data;

	Path path = GetDataFilePath(filename.GetString());

    gFileManager.Write(path.ToNativePathString().c_str(), data);
}
