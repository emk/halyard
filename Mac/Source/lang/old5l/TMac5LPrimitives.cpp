// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

// Needed for RegisterMacintoshPrimitives.
#include "TCommon.h"
#include "TPrimitives.h"
#include "TMac5LPrimitives.h"

// Needed to implement primitives.
#include "TMac5LInterpreter.h"
#include "FileSystem.h"
#include "CFiles.h"

using FileSystem::GetDataFilePath;
using FileSystem::Path;


//=========================================================================
//  RegisterMacintosh5LPrimitives
//=========================================================================
//  Install our Macintosh- and 5L-specific primitive functions.  Most of this
//  code has been migrated out of TMacPrimitives.cpp.

void FIVEL_NS RegisterMacintosh5LPrimitives()
{
    REGISTER_5L_PRIMITIVE(Close);
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
    TString     filename;

    inArgs >> filename;

    gFileManager.Close(filename);
}

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
    int16       numFields = 0;

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

    gFileManager.Lookup(filename, searchString, numFields);
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
    char		*slashPtr;
    FileKind    fKind = fReadOnly;

    inArgs >> filename >> kind;
    kind.MakeLower();

    if (kind == (char *)"append") 
    	fKind = fWriteAppend;
    else if (kind == (char *)"new") 
    	fKind = fWriteNew;
    else if (kind == (char *)"readonly") 
    	fKind = fReadOnly;
    else
        gLog.Caution("Unknown open file kind: %s", (const char *)kind);
    
    // Filenames can look DOS like.     
    slashPtr = strstr(filename.GetString(), "\\");
    if (slashPtr != NULL)
    	*slashPtr = ':';

    gFileManager.Open(filename, fKind);
}

/*-----------------------------------------------------------------
    (READ FILENAME VARIABLE <UNTIL DELIM>)

    Read data from a text file and put it into the variable.
    Normally this will read the next word as defined by whitespace.
    Use the UNTIL DELIM construct to read until some other
    delimiter.

    Valid delimiters are TAB, RETURN, EOF, or any single character.
-------------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Read)
{
    TString         filename, vname, untilstr, delimstr;
    unsigned char   delim;
    TString         res;

    inArgs >> filename >> vname;

    if (inArgs.HasMoreArguments()) 
    {
        inArgs >> untilstr >> delimstr;
        delimstr.MakeLower();
        
        if (delimstr == (char *)"tab") 
        	delim = '\t';
        else if (delimstr == (char *)"return") 
        	delim = NEWLINE_CHAR;
        else if (delimstr == (char *)"eof") 
        	delim = 0;
        else 
        	delim = delimstr(0);

        gFileManager.ReadUntil(filename, res, delim);
    } 
    else 
    	gFileManager.Read(filename, res);

    gVariableManager.SetString(vname.GetString(), res.GetString());
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
    TString		searchString, param, filename;
    int16		numFields = 0;

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

    gFileManager.Rewrite(filename, searchString, numFields);
}

/*-----------------------------------------------------------
    (WRITE FILENAME DATA)

    Write the given data to the file.
-------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Write)
{
    TString     filename, data;

    inArgs >> filename >> data;

    gFileManager.Write(filename, data);
}
