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
// TParser.cpp : Class for parsing a 5L script to produce index information.
//

#include <string.h>
#include <ctype.h>
#include <fstream.h>
#include <string.h>
#include <stdio.h>

#include "TCommon.h"
#include "TLogger.h"
#include "TParser.h"

USING_NAMESPACE_FIVEL

std::map<std::string,TIndexManager*> TParser::sManagerMap;

void TParser::RegisterIndexManager(const std::string &inTypeName,
								   TIndexManager *inManager)
{
	sManagerMap.insert(std::pair<std::string,TIndexManager*>(inTypeName,
															 inManager));
}

TParser::TParser()
{
	filePos = 0;
	curPos = 0;
	curBufCount = 0;
	bufAtEOF = false;
	atEOF = false;
	putCh = 255;
	curLine = 1;
	lastGoodType = ILLEGAL_TYPE;
	inComment = false;
	inEscape = false;
	escapeNext = false;
	mLastCharWasCR = false;
	haveErr = false;
}

TParser::~TParser()
{	

}

//
//	Parse - Parse the input file.
//
bool TParser::Parse(TIndexFile *inFile)
{
	TString		theName;
	TString		type;
	int32		startPos;
	int32		endPos;
	int32		state = 1;
	bool		retValue = false;
	
	scriptFile = inFile;

	if ((scriptFile == NULL) or (not scriptFile->IsOpen()))
		return (false);

	scriptFile->Seek(0);

	while ((not atEOF) and (not haveErr))
	{
	    switch (state)
	    {
	    	case 1:
	    		startPos = findStart();
	    		state = 2;
	    		break;
	    	case 2:
	    		type = findType();
	    		state = 3;
	    		break;
	    	case 3:
	    		findName(theName);
	    		state = 4;		
	    		break;
	    	case 4:
	    		endPos = findClose();
	    		state = 5;
	    		break;
	    	case 5:	
				// create the index for the correct manager
				std::map<std::string,TIndexManager*>::iterator found =
					sManagerMap.find(std::string(type));
				ASSERT(found != sManagerMap.end());
				found->second->MakeNewIndex(scriptFile, theName,
											startPos, endPos);

				// Update our error-reporting information.
				lastGoodThing = theName;
				lastGoodType = type;

	    		state = 1;
	    		break;
	    }
	}
	
	if (haveErr)
	{
	    gDebugLog.Error("Error on line <%ld>", curLine);
		if (lastGoodType != ILLEGAL_TYPE)
		{
			gDebugLog.Error("The last good %s was <%s>",
							lastGoodType.GetString(),
							lastGoodThing.GetString());
		}
		retValue = false;
	}
	else
	{
		retValue = true;
	}

	return (retValue);
}

//
//	findStart - Find the start of the next command.
//
int32 TParser::findStart(void)
{
	unsigned char	ch;
	bool	done = false;

	while ((not done) and (not atEOF))
	{
		ch = getRealChar();
		if ((ch == '(') and (not inEscape))
			done = true;
		else if ((not isspace(ch)) and (not inComment) and (not atEOF)) 
		{
			gDebugLog.Log("Error: expecting open paren, got <%c>", ch);
			done = true;
			haveErr = true;
		}  
	}

	return (filePos - 1);
}

//
//	findClose - Find the end of the current command.
//
int32 TParser::findClose(void)
{
	TString			theType;
	int16			parenLevel = 0;
	unsigned char	ch;
	bool			done = false;	

	while ((not done ) and (not atEOF))
	{
		ch = getRealChar();

		if ((ch == '(') and (not inEscape))
		{
			parenLevel++;

			if (getString(theType))
			{
				// Make sure this string does not begin with a valid
				// top-level-form name ("card", etc.).
				theType.MakeLower();
				if (sManagerMap.find(std::string(theType)) !=
					sManagerMap.end())
					haveErr = true;

				if (haveErr)
				{
					gDebugLog.Log("Error: looking for command name, got <%s>", 
						theType.GetString());
					gDebugLog.Log("This usually means there are too few parens.");
					done = true;
				}
			}
		}
		else if ((ch == ')') and (not inEscape))
		{
			if (parenLevel == 0)
				done = true;
			else
				parenLevel--;
		}
	}

	if (not done)
	{
		gDebugLog.Log("Error: unexpected end-of-file while looking for closing paren");
		haveErr = true;
	}

	return (filePos - 1);	// filePos is one beyond the char we just read
}

TString TParser::findType(void)
{
	TString theType = ILLEGAL_TYPE;

	// Read in our type string.
	if (getString(theType))
		theType.MakeLower();
	else
		theType = ILLEGAL_TYPE; // I don't trust getString to leave this alone.

	if (atEOF)
	{
		gDebugLog.Log("Error: unexpected end-of-file");
		haveErr = true;
	}
	else if (theType == ILLEGAL_TYPE)
	{
		gDebugLog.Log("Error: unexpected getString() failure");
		haveErr = true;
	}
	else
	{
		std::map<std::string,TIndexManager*>::iterator found =
			sManagerMap.find(std::string(theType));
		if (found == sManagerMap.end())
		{
			gDebugLog.Log("Error: expecting top-level form, got <%s>",
						  theType.GetString());
			haveErr = true;
		}
	}

	return (theType);
}

//
//	findName - Get the next identifier out of the input file.
//
bool TParser::findName(TString &inString)
{	
	if (not getString(inString))
	{
		gDebugLog.Log("Error: expecting string, got nothing");
		haveErr = true;
	}

	if (atEOF)
	{
		gDebugLog.Log("Error: unexpected end-of-file while looking for name");
		haveErr = true;
	}

	return (not haveErr);
}



//
//	getString - Get the next string out of the input file. Allocate
//				memory to just fit the string and return it. Skip
//				white space before the string.
//
bool TParser::getString(TString &inString)
{
	unsigned char	inBuf[128];	
	int16			strLen = 0;
	unsigned char	ch;
	bool			done = false;
	bool			retValue = false;
	
	inString = "";				// wipe out the string

	ch = getRealChar();			// skip spaces and comments to start string
	if (((ch != ')') and (ch != '(')) or (inEscape))
	{
		inBuf[strLen++] = ch;	

		while ((not done) and (not atEOF))
		{
			ch = getChar();		// no more skipping within a string

			if ((isspace(ch)) || ((ch == '#') && !inEscape))		// stop when come to a space
				done = true;						// don't worry about pushing char back
			else if (((ch == '(') or (ch == ')')) and (not inEscape))	// can't have parens in strings
			{
				putChar(ch);
				done = true;
			}
			else
				inBuf[strLen++] = ch;
		}		

		inBuf[strLen] = '\0';
		inString = (char *) inBuf;
		retValue = true;
	}
	else
		putChar(ch);						// put it back

	return (retValue);
}

//
//	getRealChar - Get a non-comment, non-whitespace character.
//
unsigned char TParser::getRealChar()
{
	bool	done = false;
	unsigned char	ch;

	while ((not done) and (not atEOF))
	{
		ch = getChar();
		
		if ((not isspace(ch)) and (not inComment))
			done = true;
	}
	
	return (ch);
}


//
//	getChar - This routine will return the next character from the
//			input file. We buffer the input making it possible to
//			push a character back into the data stream. 
//
unsigned char TParser::getChar(void)
{
	unsigned char	ch;

	if (escapeNext)			// we are supposed to escape this character
		inEscape = true;	// so set it as such
	else 
		inEscape = false;	// this character shouldn't be escaped
	escapeNext = false;		// unless character is SLASH, we won't escape next character

	if (putCh != 255)					// do we have a put character
	{
		ch = putCh;						// return it
		putCh = 255;					// wipe it out
	}
	else
	{		
		if (curPos >= curBufCount)		// at end of buffer
		{
			if (bufAtEOF)				// any more to read in the buffer?
				atEOF = true;			// no, mark at end of file
			else
				getBuffer();			// yes, get another buffer
		}

		if ((not atEOF) and (curPos < curBufCount))	// did we get anything?
		{
			ch = inBuffer[curPos];		// yes, give a character and increment position in buffer
			curPos++;					// increment position in input buffer
			filePos++;					// increment position in file
		}
		else
		{
			ch = 0;						// no, return garbage
			gDebugLog.Log("TParser::getChar returning garbage");
		}
	}

	// Attempt to detect the end-of-line in a portable fashion.
	bool is_newline;
	if (ch == '\r')
	{
		is_newline = true;
		mLastCharWasCR = true;
	}
	else if (ch == '\n' && !mLastCharWasCR)
	{
		is_newline = true;
		mLastCharWasCR = false;
	}
	else
	{
		is_newline = false;
		mLastCharWasCR = false;
	}
	
	// If we found a new line, advance.
	if (is_newline)
	{
		inComment = false;
		curLine++;
	}
	
	if (ch == '#' && !inEscape)
		inComment = true;
	else if (ch == '\\' && !inEscape)
		escapeNext = true;

	return (ch);
}

// 
//	putChar - Put a character back into the input file. We can only do 
//				this if we didn't just read in a new buffer of characters.
//	
void TParser::putChar(unsigned char ch)
{
	putCh = ch;
}

//
//	getBuffer - Get a buffer full of characters from the input file.
//	
void TParser::getBuffer(void)
{
	int32			count = PARSER_BUFFER_SIZE;
	
	scriptFile->Seek(filePos);
	count = scriptFile->Read((char *) inBuffer, count);

	curPos = 0;
	curBufCount = count;
	
	if (scriptFile->AtEnd())
		bufAtEOF = true;
}	

/*
 $Log$
 Revision 1.3  2002/05/15 11:05:17  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.2.2.3  2002/04/30 07:57:24  emk
 3.3.2.5 - Port Win32 code to use the 20Kloc of Common code that now
 exists.  The (defstyle ...) command should work, but (textaa ...) isn't
 available yet.

 Next up: Implement the (textaa ...) command and the low-level
 GraphicsTools::Image::DrawBitMap.

 Revision 1.2.2.2  2002/04/22 08:17:58  emk
 Updated Common code to build on Macintosh and pass all unit tests.

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

 Revision 1.4  2000/08/08 19:03:41  chuck
 no message

 Revision 1.3  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
