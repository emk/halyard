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
// Parser.cpp : Class for parsing a 5L script to produce index information.
//
//


#include "stdafx.h"

#include <string.h>
#include <ctype.h>
#include <fstream.h>
#include <string.h>
#include <stdio.h>

#include "Parser.h"
#include "Card.h"
#include "Header.h"
#include "Macro.h"

#include "LUtil.h"
#include "Globals.h"

Parser::Parser()
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
	haveErr = false;
}

Parser::~Parser()
{	

}

//
//	Parse - Parse the input file.
//
bool Parser::Parse(IndexFile *inFile)
{
	TString		theName;
	int32		iType;
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
	    		iType = findType();
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
				switch (iType)
				{
					case CARD_TYPE:
						gCardManager.MakeNewIndex(scriptFile, theName, startPos, endPos);
						break;
					case MACRO_TYPE:
						gMacroManager.MakeNewIndex(scriptFile, theName, startPos, endPos);
						break;
					case HEADER_TYPE:
						gHeaderManager.MakeNewIndex(scriptFile, theName, startPos, endPos);
						break;
				}

				lastGoodThing = theName;
				lastGoodType = iType;

	    		state = 1;
	    		break;
	    }
	}
	
	if (haveErr)
	{
	    gDebugLog.Error("Error on line <%ld>", curLine);
		if (lastGoodType != ILLEGAL_TYPE)
		{
			switch (lastGoodType)
			{
				case CARD_TYPE:
					gDebugLog.Error("The last good card was <%s>", lastGoodThing.GetString());
					break;
				case MACRO_TYPE:
					gDebugLog.Error("The last good macro was <%s>", lastGoodThing.GetString());
					break;
				case HEADER_TYPE:
					gDebugLog.Error("The last good header was <%s>", lastGoodThing.GetString());
					break;
			}
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
int32 Parser::findStart(void)
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
int32 Parser::findClose(void)
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
				theType.MakeLower();
				if (theType.Equal("card"))
					haveErr = true;
				else if (theType.Equal("header"))
					haveErr = true;
				else if (theType.Equal("macrodef"))
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

int32 Parser::findType(void)
{
	TString			theType;
	int32			iType = ILLEGAL_TYPE;

	if (getString(theType))
	{
		theType.MakeLower();
		if (theType.Equal("card"))
			iType = CARD_TYPE;
		else if (theType.Equal("header"))
			iType = HEADER_TYPE;
		else if (theType.Equal("macrodef"))
			iType = MACRO_TYPE;
	}

	if (iType == ILLEGAL_TYPE)
	{
		gDebugLog.Log("Error: expecting <header>, <macrodef> or <card>, got <%s>", theType.GetString());
		haveErr = true;
	}

	if (atEOF)
	{
		gDebugLog.Log("Error: unexpected end-of-file");
		haveErr = true;
	}

	return (iType);
}

//
//	findName - Get the next identifier out of the input file.
//
bool Parser::findName(TString &inString)
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
bool Parser::getString(TString &inString)
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

			if ((isspace(ch)) or (ch == '#'))		// stop when come to a space
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
unsigned char Parser::getRealChar()
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
unsigned char Parser::getChar(void)
{
	unsigned char	ch;

	inEscape = false;

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
			ch = -1;					// no, return garbage
	}

	if (ch == '\n')
	{
		if (inComment)
			inComment = false;

		curLine++;
	}
	else if (ch == '#')
		inComment = true;
	else if (ch == '\\')
		inEscape = true;

	return (ch);
}

// 
//	putChar - Put a character back into the input file. We can only do 
//				this if we didn't just read in a new buffer of characters.
//	
void Parser::putChar(unsigned char ch)
{
	putCh = ch;
}

//
//	getBuffer - Get a buffer full of characters from the input file.
//	
void Parser::getBuffer(void)
{
	int32			count = BUFFER_SIZE;
	
	scriptFile->Seek(filePos);
	count = scriptFile->Read((char *) inBuffer, count);

	curPos = 0;
	curBufCount = count;
	
	if (scriptFile->AtEnd())
		bufAtEOF = true;	
}	

/*
 $Log$
 Revision 1.1.2.1  2002/03/13 15:06:56  emk
 Merged changed from 3.1.1 -> 3.2.1 into the 3.2.0.1 codebase,
 because we want these in the stable engine.  Highlights:

   1) FiveL.prefs file support.
   2) Removal of -D command line flag.

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
