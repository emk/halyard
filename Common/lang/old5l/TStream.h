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

#if !defined (_LStream_h_)
#define _LStream_h_

#include "TString.h"
#include "TRect.h"
#include "TPoint.h"
//#include "Graphics.h"

/*-----------------------------------------------------------------

CLASS
    LStream

	An input class based on TString that allows >> style setting.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class LStream : public TString 
{

    public:
        //////////
		// Constructor.
		//
		LStream();

		//////////
		// Constructor.  Construct given initial input buffer size.
		//
		// [in] newsize - initial input buffer size
		//
        LStream(const int newsize);
        
		//////////
		// Constructor.  Construct given initial input buffer.
		//
		// [in] s - initial input buffer
		//
		LStream(const char *s);
        
		//////////
		// Constructor.  Construct given initial input buffer.
		//
		// [in] other - initial input buffer
		//
		LStream(const TString &other);
        
		//////////
		// Constructor.  Wrap around another LStream.
		//
		// [in] other - another LStream to wrap around
		//
		LStream(const LStream &other);

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
		// [out] return - true if we have reached the end of the stream, false otherwise
		//
		int  	eof() { return (pos >= m_Length); }
        
		//////////
		// Is there more data for this field?
		// Used by parsing functions with optional commands to see if there is more there.
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
		TString copystr(unsigned int startPos, unsigned int numChars);

        //////////
		// Increment the position mark to the next whitespace character or
		// parenthesis; in other words, one character past the current word.
		//
		void    scanword();
        
		//////////
		// Scan until we hit an open parenthesis, and then set the position mark
		// to the first character after the parenthesis. Since we ignore everything
		// prior to the open parenthesis comments can be added to the 5L source
		// anywhere as long as they are not within commands. (They may be within CARD and
		// MACRODEF commands, however.)
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
		// Basic extraction operator. Most others just use this and then convert the type.
		//
		// [out] dest - destination string to store extraction results 
		//
		LStream&    operator>>(TString &dest);
        
		//////////
		// This allows manipulator functions to work.
		//
		LStream&    operator>>(LStream& (*_f)(LStream &));
        
		//////////
		// TString class handles string to int conversions.
		//
		// [out] dest - an integer to store the extraction
		//
		LStream&    operator>>(int16 &dest);
        
		//////////
		// TString class handles string to int conversions.
		//
		// [out] dest - an integer to store the extraction
		//
		LStream&    operator>>(int32 &dest);
        
		//////////
		// TString class handles string to double conversions.
		//
		// [out] dest - an integer to store the extraction
		//
		LStream&    operator>>(double &dest);
        
		//////////
		// Assumes there are 4 numbers to grab from the input stream.
		//
		// [out] r - TRect to store the 4 numbers
		//
		LStream&    operator>>(TRect &r);
        
		//////////
		// Assumes there are 2 numbers to grab from the input stream.
		//
		// [out] pt - TPoint to store the 2 numbers
		//
		LStream&    operator>>(TPoint &pt);

	protected:
        //////////
		// Current Positon.
		//
		unsigned int    pos;
        
		//////////
		// Is this given character a whitespace character?
		//
		// [in] ch - the character
		// [out] return - true if it is a whitespace char, false otherwise
		//
		int				whitespace(char ch);

};

inline LStream& open(LStream &src) { src.scanopen(); return src; }
inline LStream& close(LStream &src) { src.scanclose(); return src; }
inline LStream& discard(LStream &src) { src.discard(); return src; }

#endif // _LStream_h_

/*
 $Log$
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
