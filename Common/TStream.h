// -*- Mode: C++; tab-width: 4; -*-
//////////////////////////////////////////////////////////////////////////////
//
// (c) Copyright 1999-2002, Trustees of Dartmouth College, All rights reserved.
// Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

#if !defined (TStream_H)
#define TStream_H

#include <string>

#include "TString.h"
#include "TRect.h"
#include "TPoint.h"
#include "GraphicsTools.h"

BEGIN_NAMESPACE_FIVEL

/*-----------------------------------------------------------------

CLASS
    TStream

	An input class based on TString that allows '>>'-style setting.
    This does most of the heavy work of parsing FiveL code,
    interpolating variable values, etc.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class TStream : public TString 
{
public:
	//////////
	// Constructor.
	//
	TStream();
	
	//////////
	// Constructor.  Construct given initial input buffer size.
	//
	// [in] newsize - initial input buffer size
	//
	TStream(const int32 newsize);
	
	//////////
	// Constructor.  Construct given initial input buffer.
	//
	// [in] s - initial input buffer
	//
	TStream(const char *s);
	
	//////////
	// Constructor.  Construct given initial input buffer.
	//
	// [in] other - initial input buffer
	//
	TStream(const TString &other);
	
	//////////
	// Constructor.  Wrap around another TStream.
	//
	// [in] other - another TStream to wrap around
	//
	TStream(const TStream &other);

	//////////
	// Get character at current position in the input stream.
	//
	// [out] return - char at current position
	//
	char 		curchar(); 
	
	//////////
	// Return the next character in the stream. If it is a comment
	// then ignore it and the rest of the line.
	//
	// [out] return - the next char in the input stream
	//
	char        nextchar();
	
	//////////
	// Get character at current position in the input stream.
	//
	// [out] return - char at previous position
	//
		char 		prevchar() { return pos ? m_String[pos - 1] : 0; }
	
	//////////
	// Have we reached the end of the stream?
	//
	// [out] return - true if we have reached the end of the stream, false
	// otherwise
	//
	int  	eof() { return (pos >= m_Length); }
	
	//////////
	// Is there more data for this field?  Used by parsing functions with
	// optional commands to see if there is more there.
	//
	// [out] return - Return true if we haven't hit the closing ) or 
	//				  the end of the string yet.
	//
	int     more();
        
	//////////
	// Reset the stream get pointer to 0. Check now to see if first
	// character is a comment and skip it.
	//
	void    reset();
	
	//////////
	// Increment the position mark to the next non-whitespace character.
	//
	void    skipwhite();
	
	//////////
	// Return the given characters, substituting variable contents where 
	// appropriate. Should never have to worry about white space.
	//
	// [in] startPos - starting postion
	// [in] numChars - number of characters
	// [out] return - TString with variable substitutions
	//
	TString copystr(uint32 startPos, uint32 numChars);
	
	//////////
	// Increment the position mark to the next whitespace character or
	// parenthesis; in other words, one character past the current word.
	//
	void    scanword();
	
	//////////
	// Scan until we hit an open parenthesis, and then set the position
	// mark to the first character after the parenthesis. Since we ignore
	// everything prior to the open parenthesis comments can be added to
	// the 5L source anywhere as long as they are not within
	// commands. (They may be within CARD and MACRODEF commands, however.)
	//
	void    scanopen();
	
	//////////
	// Scan until we find a closing parenthesis. If we find open parentheses
	// then we must find closing parentheses for them too, so keep a counter 
	// going. Set the position mark to the first character after the close.
	//
	void    scanclose();
	
	//////////
	// Read a token and discard it. We don't care about it, we just
	// want to get it out of the way.
	//
	void    discard();
	
	//////////
	// Basic extraction operator. Most others just use this and then
	// convert the type.
	//
	// [out] dest - destination string to store extraction results 
	//
	TStream&    operator>>(TString &dest);

	//////////
	// Read in an standard C++ string.
	//
	// [out] outString - destination string to store extraction results 
	//
	TStream&    operator>>(std::string &outString);
	
	//////////
	// This allows manipulator functions to work.
	//
	TStream&    operator>>(TStream& (*_f)(TStream &));
        
	//////////
	// TString class handles string to int conversions.
	//
	// [out] dest - an integer to store the extraction
	//
	TStream&    operator>>(int16 &dest);
	
	//////////
	// TString class handles string to int conversions.
	//
	// [out] dest - an integer to store the extraction
	//
	TStream&    operator>>(int32 &dest);

	//////////
	// TString class handles string to int conversions.
	//
	// [out] dest - an unsigned integer to store the extraction
	//
	TStream&    operator>>(uint32 &dest);
	
	//////////
	// TString class handles string to double conversions.
	//
	// [out] dest - an integer to store the extraction
	//
	TStream&    operator>>(double &dest);
	
	//////////
	// Assumes there are 4 numbers to grab from the input stream.
	//
	// [out] r - TRect to store the 4 numbers
	//
	TStream&    operator>>(TRect &r);
	
	//////////
	// Assumes there are 2 numbers to grab from the input stream.
	//
	// [out] pt - TPoint to store the 2 numbers
	//
	TStream&    operator>>(TPoint &pt);

	//////////
	// Read in an RGBA hexadecimal color in the format '#xRRGGBBAA'.
	//
	// [out] outColor - The specified color.
	//
	TStream&    operator>>(GraphicsTools::Color &outColor);
	
protected:
	//////////
	// Current positon.
	//
	unsigned int    pos;
	
	//////////
	// Is this given character a whitespace character?
	//
	// [in] ch - the character
	// [out] return - true if it is a whitespace char, false otherwise
	//
	bool			whitespace(char ch);	

public:
	//////////
	// Return the current stream position.
	//
	uint32			GetPos(void) { return (pos); }

	//////////
	// Tests to see if the character at 'position' is escaped using a '\'.
	//
	bool            inEscape(int32 position);

	//////////
	// Tests to see if the character at the current position is escaped
	// using a '\'.
	// 
    bool            inEscape(void);
};

//////////
// An input manipulator which calls 'scanopen' on the underlying
// stream.  Call it as:
//   stream >> open;
//
inline TStream& open(TStream &src) { src.scanopen(); return src; }

//////////
// An input manipulator which calls 'scanclose' on the underlying
// stream.  Call it as:
//   stream >> close;
//
inline TStream& close(TStream &src) { src.scanclose(); return src; }

//////////
// An input manipulator which calls 'discard' on the underlying
// stream.  Call it as:
//   stream >> discard;
//
inline TStream& discard(TStream &src) { src.discard(); return src; }

//////////
// An input manipulator which parses either (1) percentage values
// of the form "(pcent 20)" or (2) absolute values of the form "4".
// Call it as:
//   int result;
//   stream >> ValueOrPercent(10, result);
// When passed "(pcent 20)", this will return 2.  When passed "4", this
// will return "4".
//
class ValueOrPercent
{
	int32 mBaseValue;
	int32 *mOutputValue;

public:
	ValueOrPercent(int32 baseValue, int32 *outputValue)
		: mBaseValue(baseValue), mOutputValue(outputValue) {}

	friend TStream &operator>>(TStream &, const ValueOrPercent &);
};

END_NAMESPACE_FIVEL

#endif // TStream_h

/*
 $Log$
 Revision 1.2  2002/05/15 11:05:17  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.1.4.4  2002/05/02 05:49:04  emk
 Small MacOS compilation fixes.

 Revision 1.1.4.3  2002/05/01 11:34:02  emk
 Added support for passing a "(pcent ...)" argument to "defstyle" to
 specify leading as a percentage of the base font size (and cleaned up
 a few minor test suite issues).

 Revision 1.1.4.2  2002/04/23 11:28:57  emk
 Now that the variable manager has been merged, TStream no longer needs a callback function to look up variables in a portable fashion.

 Revision 1.1.4.1  2002/04/19 11:20:13  emk
 Start of the heavy typography merging work.  I'm doing this on a branch
 so I don't cause problems for any of the other developers.

 Alpha-blend text colors.

 Merged Mac and Windows versions of several files into the Common directory.
 Not all of these work on Mac and/or Windows yet, but they're getting there.
 Primary sources for the merged code are:

   Win/FiveL/LVersion.h -> Common/TVersion.h
   Win/FiveL/LStream.h -> Common/TStream.h
   Mac/Source/CStream.cp -> Common/TStream.cpp
   Mac/Source/CStreamTests.cp -> Common/TStreamTests.cpp

 TStream changes:

   * The TStream code now uses a callback to variable values.  This will
     probably go away once Variable and CVariable get merged.
   * Input operators for std::string and GraphicTools::Color.

 Isolated Windows-specific code in TLogger.*, in preparation for a big merge.

   * Added a portable function to set up logging.
   * Fixed the logging code to use the portable FileSystem library.
   * Made FatalError actually quit the application.

 Turned off the FiveL namespace on FIVEL_PLATFORM_OTHER, so we can debug
 with GDB, which has a few minor but painful namespace issues.

 TString changes:

   * Made sure we can convert from std::string to a TString.
   * Added some more assertions.
   * Fixed bug in various operator= methods which would allow the string's
     internal data pointer to be NULL.
   * Changed operator[] and operator() arguments to be 'int' instead of
     'int32' to avoid nasty compiler warnings.

 Typography::Style changes:

   * Added a "ShadowOffset" field that specifies the offset of the
     drop shadow.
   * Added an operator== for testing.
   * Added a ToggleFaceStyle method for toggling specified face style bits.

 Typography::StyledText changes:

   * Added a method to append a single character.

 Other Typography changes:

   * Made FaceStyle an int, not an enum, so we can do bit math with it.
   * Added assertions to made sure you can't extract a StyledText iterator
     until you've called EndConstruction.

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.3  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
