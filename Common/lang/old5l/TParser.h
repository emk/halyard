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

#if !defined (_Parser_h_)
#define _Parser_h_

#include "TCommon.h"
#include "TString.h"
#include "Index.h"
#include "LUtil.h"

#define BUFFER_SIZE		512
#define ILLEGAL_TYPE	0
#define CARD_TYPE		1
#define HEADER_TYPE		2
#define MACRO_TYPE		3

/*-----------------------------------------------------------------

CLASS
    Parser

	Parser for 5L scripting language.  Parses out Card, Macro, and 
	Header constructs from an IndexFile.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class Parser : public TObject
{

	public:
		//////////
		// Constructor.
		//
		Parser();
		
		//////////
		// Destructor.
		//
		~Parser();
	
		//////////
		// Parse the given IndexFile, consisting of Card, Macro, and Header
		// constructs.
		//
		// [in] inFile - index file to parse
		// [out] return - false if there was an error parsing the index file, otherwise true
		//
		bool			Parse(IndexFile *inFile);

	protected:
		//////////
		// Current position in file.
		//
		int32			filePos;
		
		//////////
		// Current position in input buffer.
		//
		int32			curPos;	
		
		//////////
		// Number of characters in input buffer.
		//
		int32			curBufCount;
		
		//////////
		// Input buffer.
		//
		unsigned char	inBuffer[BUFFER_SIZE];
		
		//////////
		// Has the last buffer been read in?
		//
		bool			bufAtEOF;
		
		//////////
		// Is curPos at the end?
		//
		bool			atEOF;
		
		//////////
		// Pushback character.
		//
		unsigned char	putCh;
		
		//////////
		// Has a parsing error been found?
		//
		bool			haveErr;
		
		//////////
		// Are we currently parsing inside a comment?
		//
		bool			inComment;
		
		//////////
		// Are we currently parsing inside an escape sequence?
		//
		bool			inEscape;
		
		//////////
		// Current line in the input file.
		//
		int32			curLine;

		//////////
		// Last type of identifier successfully parsed.<br>
		// (CARD_TYPE, MACRO_TYPE, HEADER_TYPE, ILLEGAL_TYPE)
		//
		int32			lastGoodType;
		
		//////////
		// Name of the lastGoodType. 
		//
		TString			lastGoodThing;
		
		//////////
		// IndexFile for the script that we are parsing.
		//
		IndexFile		*scriptFile;
				
		//////////
		// Find the start of the next command.
		//
		// [out] return - index for the start of the next command
		//
		int32			findStart(void);
		
		//////////
		// Find the end of the current command.
		//
		// [out] return - index for the end of the current command
		//
		int32			findClose(void);
		
		//////////
		// Figure out the type of this identifier.
		//
		// [out] return - the type of this object, one of:<br> 
		//				  (CARD_TYPE, MACRO_TYPE, HEADER_TYPE, ILLEGAL_TYPE)
		//
		int32			findType(void);
		
		//////////
		// Get the next identifier out of the input file.
		//
		// [in/out] inString - filled with name of identifier found
		// [out] return - true on success, false otherwise
		//
		bool			findName(TString &inString);
		
		//////////
		// Get the next string out of the input file. Allocate memory to just fit
		// the string and return it. Skip white space before the string.
		//
		// [in/out] inString - filled with name of next string found
		// [out] return - true on success, false otherwise
		//
		bool			getString(TString &inString);
		
		//////////
		// Get a non-comment, non-whitespace character.
		//
		// [out] return - the next non-comment, non-whitespace character
		//
		unsigned char	getRealChar(void);
		
		//////////
		// This routine will return the next character from the	input file. 
		// We buffer the input making it possible to push a character back 
		// into the data stream if needed.
		//
		// [out] return - the next character from the input stream 
		//
		unsigned char	getChar(void);
		
		//////////
		// Put a character back (pushback) into the input file. We can only do 
		// this if we didn't just read in a new buffer of characters.
		//
		// [in] ch - character to be pushed back onto the input stream
		//
		void			putChar(unsigned char ch);
		
		//////////
		// Get a buffer full of characters from the input file.
		//
		void			getBuffer(void);
		
};

#endif // _Parser_h_

/*
 $Log$
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
