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
// Parser.h : Front end for the IndexGen application. It takes a script
//            file and parses it to find the start of commands.
//
// This code is used to split the file into separate top-level forms, which
// are headers, cards, and macrodefs. It doesn't actually parse the code
// for use by the engine. Instead, the engine creates TStreams from each
// top-level form and uses TStream to do the actual parsing.
//
// This means that a lot of paren balancing code and escape processing is
// duplicated between TStream and TParser in some form or another, and if
// the two files disagree, the results might be weird.  Quite frankly, this
// is not the cleanest design.

#if !defined (_TParser_h_)
#define _TParser_h_

#include <string>
#include <map>
#include <boost/utility.hpp>

#include "TCommon.h"
#include "TString.h"
#include "TIndex.h"

BEGIN_NAMESPACE_FIVEL

#define PARSER_BUFFER_SIZE 512
#define ILLEGAL_TYPE ("")


/*-----------------------------------------------------------------

CLASS
    TParser

	Parser for 5L scripting language.  Parses out Card, Macro, and 
	Header constructs from an TIndexFile.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class TParser : public TObject
{
	private:
		//////////
	    // A mapping from top-level-form names ("card", "macrodef")
		// to the appropriate processor.
	    //
	    static std::map<std::string,TTopLevelFormProcessor*> sProcessorMap;

	public:
		//////////
		// Register a processor to handle a new kind of top-level-form
		// (e.g., "card", "macrodef", etc.).
	    //
	    // [in] inTypeName - The lower-case name ("card", etc.).
		// [in] inProcessor - The object which handles this type of tlf.
	    //
		static void RegisterTlfProcessor(const std::string &inTypeName,
										 TTopLevelFormProcessor *inProcessor);

		//////////
		// Constructor.
		//
		TParser();
		
		//////////
		// Destructor.
		//
		~TParser();
	
		//////////
		// Parse the given TIndexFile, consisting of Card, Macro, and Header
		// constructs.
		//
		// [in] inFile - index file to parse
		// [out] return - false if there was an error parsing the index file,
        //                otherwise true
		//
		bool			Parse(TIndexFile *inFile);

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
		unsigned char	inBuffer[PARSER_BUFFER_SIZE];
		
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
		// Should the next character be escaped?
		//
		bool			escapeNext;
		
		//////////
		// Was the last character a carriage return?
		//
		bool			mLastCharWasCR;
		
		//////////
		// Current line in the input file.
		//
		int32			curLine;

		//////////
		// Last type of identifier successfully parsed
		// ("card", "macrodef", etc.).
		//
		TString			lastGoodType;
		
		//////////
		// Name of the lastGoodType. 
		//
		TString			lastGoodThing;
		
		//////////
		// TIndexFile for the script that we are parsing.
		//
		TIndexFile		*scriptFile;
				
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
		TString			findType(void);
		
		//////////
		// Get the next identifier out of the input file.
		//
		// [in/out] inString - filled with name of identifier found
		// [out] return - true on success, false otherwise
		//
		bool			findName(TString &inString);
		
		//////////
		// Get the next string out of the input file. Allocate memory to
		// just fit the string and return it. Skip white space before the
		// string.
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

END_NAMESPACE_FIVEL

#endif // _TParser_h_

/*
 $Log$
 Revision 1.3  2002/08/17 01:41:55  emk
 3.5.1 - 16 Aug 2002 - emk

 Added support for defining stylesheets in Scheme.  This means that Scheme
 can draw text!  (The INPUT doesn't work yet, because this relies on the
 separate, not-yet-fixed header system.)  This involved lots of refactoring.

   * Created TTopLevelFormProcessor as an abstract superclass of
     TIndexManager, and modified TParser to use TTopLevelFormProcessor.
     This allows the legacy 5L language to contain non-TIndex tlfs.
   * Implemented a TPrimitiveTlfProcessor class, which allows
     top-level-forms to be implemented as calls to regular 5L primitives.
   * Yanked our ValueOrPercent support from TStream into the
     TArgumentList superclass, and implemented it for all TArgumentList
     subclasses.  This allows non-5L languages to specify the funky
     percentage arguments used by the DEFSTYLE command.
   * Removed all TIndex/TIndexManager support from TStyleSheet, and
     reimplemented it using an STL std::map.  This breaks the dependencies
     between stylesheets and the old 5L interpreter.
   * Implemented a DEFSTYLE primitive.

 Revision 1.2  2002/05/15 11:05:17  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.1.4.2  2002/04/22 08:17:58  emk
 Updated Common code to build on Macintosh and pass all unit tests.

 Revision 1.1.4.1  2002/04/22 05:22:33  emk
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
