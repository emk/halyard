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
//
// TString.h : 
//

#if !defined (_TString_h_)
#define _TString_h_

#include <iostream.h>
#include <string>

#include "TCommon.h"
#include "TObject.h"

BEGIN_NAMESPACE_FIVEL

//////////
// Description of String contents
//
typedef enum {TDateString, TNumberString, TStringString} StringType;

//////////
// Minimum amount to resize a TString
//
static const unsigned long MIN_STRING_RESIZE = 4;

//////////
// Maximum ammount of space allowed before shrinking the storage for a TString
//
static const long MAX_STRING_RESIZE_DIFF = 128;

//////////
// Size of TString inBuffer
//
static const long INBUFFER_SIZE = 256;

//////////
// A buffer used in streaming data to/from a TString
//
extern char inBuffer[INBUFFER_SIZE];

//////////
// Size of the buffer used by internal calls to snprintf.  This must
// be big enough to hold the maximum allowable double value, as
// formatted by %f.  According to Dave Peticolas:
// "64-bit double values...can be as high as 10e+308,
// resulting in a %f formatted string with around 310 [ed.- 320!] digits."
// We still ASSERT for snprintf overflow, just in case this assumption
// is wrong.
//
static const int SNPRINTF_BUFFER_SIZE = 512;


/*-----------------------------------------------------------------

CLASS
    TString

	A string class with dynamic storage and many useful utility methods.  

AUTHOR
    Chuck Officer<br>
	Sean Sharp<br>

------------------------------------------------------------------*/
class TString : public TObject 
{
	public:
		//////////
		// Default Constructor. 
		//
		TString();

		//////////
		// Constructor. 
		//
		// [in] inSize - a minimum initial capacity(size) for the string
		//
		TString(const uint32 inSize);

		//////////
		// Constructor.  Construct from a char string. 
		//
		// [in] inStr - character string
		//
		TString(const char *inStr);

		//////////
		// Constructor.  Construct from a char array and a length. 
		//
		// [in] inStr - character array (NULL termination not required)
		// [in] inLength - the number of characters to use
		//
		TString(const char *inStr, uint32 inLength);
		
		//////////
		// Copy Constructor.  Construct from another TString. 
		//
		// [in] inStr - a TString to copy from
		//
		TString(const TString &inStr);

		//////////
		// Conversion from std::string to TString. 
		//
		// [in] inStr - a std::string to copy from
		//
		TString(const std::string &inStr);

		//////////
		// Destructor. 
		//
		virtual ~TString();

		//////////
		// Get the length (current # of chars) of the string.  
		//
		// [out] return - the length of the string
		//
		uint32	Length() const;
		
		//////////
		// Get the size (current capacity) of the string.
		//
		// [out return - the size of the string
		//
		uint32	Size() const;
		
		//////////
		// Update information about the string.  Recomputes the length based on 
		// the length of the internal character string.
		//
		void	Update();

		//////////
		// Get the type of the string. 
		//
		// [out] return - the StringType of this string
		//
		StringType	Type();

		//////////
		// Return true if inStr is the same as this. If inCaseSens is true,
		// do a case sensitive comparison, otherwise do a case insensitive comparison. 
		//
		// [in] inStr - another string to compare against
		// [in_optional] inCaseSens - case sensitive comparison? (default true)
		// [out] return - true if the strings are equal, false otherwise
		//
		bool	Equal(const char *inStr, bool inCaseSens = true);
		
		//////////
		// Compare to another string. 
		//
		// [in] inStr - another string to compare against
		// [in_optional] inCaseSens - case sensitive comparison? (default true)
		// [out] return - < 0 if this string is less than inStr<br> 
		//					0 if this string is identical to inStr<br> 
		//				  > 0 if this string is greater than inStr<br> 
		//
		int		Compare(const char *inStr, bool inCaseSens = true) const;
		
		//////////
		// This is like Compare except the string checks to see if it
		// is a number. If it is, it compares itself to inStr
		// numerically rather than alphabetically. 
		//
		// [in] inStr - another string to compare against
		// [in_optional] inCaseSens - case sensitive comparison? (default true)
		// [out] return - < 0 if this string is less than inStr<br> 
		//					0 if this string is identical to inStr<br> 
		//				  > 0 if this string is greater than inStr<br> 
		//
		int		TypeCompare(TString &inStr, bool inCaseSens = false);

		//////////
		// Less than operator. 
		//
		// [in] inStr - another string to compare against
		// [out] return - true (!= 0) if the string is less than inStr, 
		//				  false (== 0) otherwise
		//
		friend int operator < (const TString &inStr1, const TString &inStr2)
			{ return inStr1.Compare(inStr2) < 0; }
		friend int operator < (const TString &inStr1, const char *inStr2)
			{ return inStr1.Compare(inStr2) < 0; }
		friend int operator < (const char *inStr1, const TString &inStr2)
			{ return TString(inStr1).Compare(inStr2) < 0; }
		
		//////////
		// Greater than operator. 
		//
		// [in] inStr - another string to compare against
		// [out] return - true (!= 0) if the string is greater than inStr, 
		//				  false (== 0) otherwise
		//
		friend int operator > (const TString &inStr1, const TString &inStr2)
			{ return inStr1.Compare(inStr2) > 0; }
		friend int operator > (const TString &inStr1, const char *inStr2)
			{ return inStr1.Compare(inStr2) > 0; }
		friend int operator > (const char *inStr1, const TString &inStr2)
			{ return TString(inStr1).Compare(inStr2) > 0; }

		//////////
		// Less than or equal operator. 
		//
		// [in] inStr - another string to compare against
		// [out] return - true (!= 0) if the string is less than or equal to inStr, 
		//				  false (== 0) otherwise
		//
		friend int operator <= (const TString &inStr1, const TString &inStr2)
			{ return inStr1.Compare(inStr2) <= 0; }
		friend int operator <= (const TString &inStr1, const char *inStr2)
			{ return inStr1.Compare(inStr2) <= 0; }
		friend int operator <= (const char *inStr1, const TString &inStr2)
			{ return TString(inStr1).Compare(inStr2) <= 0; }
		
		//////////
		// Greater than or equal operator. 
		//
		// [in] inStr - another string to compare against
		// [out] return - true (!= 0) if the string is greater than or equal to inStr, 
		//				  false (== 0) otherwise
		//
		friend int operator >= (const TString &inStr1, const TString &inStr2)
			{ return inStr1.Compare(inStr2) >= 0; }
		friend int operator >= (const TString &inStr1, const char *inStr2)
			{ return inStr1.Compare(inStr2) >= 0; }
		friend int operator >= (const char *inStr1, const TString &inStr2)
			{ return TString(inStr1).Compare(inStr2) >= 0; }
		
		//////////
		// Equal operator. 
		//
		// [in] inStr - another string to compare against
		// [out] return - true (!= 0) if the string is equal to inStr, 
		//				  false (== 0) otherwise
		//
		friend int operator == (const TString &inStr1, const TString &inStr2)
			{ return inStr1.Compare(inStr2) == 0; }
		friend int operator == (const TString &inStr1, const char *inStr2)
			{ return inStr1.Compare(inStr2) == 0; }
		friend int operator == (const char *inStr1, const TString &inStr2)
			{ return TString(inStr1).Compare(inStr2) == 0; }
		
		//////////
		// Not Equal operator. 
		//
		// [in] inStr - another string to compare against
		// [out] return - true (!= 0) if the string is not equal to inStr, 
		//				  false (== 0) otherwise
		//
		friend int operator != (const TString &inStr1, const TString &inStr2)
			{ return inStr1.Compare(inStr2) != 0; }
		friend int operator != (const TString &inStr1, const char *inStr2)
			{ return inStr1.Compare(inStr2) != 0; }
		friend int operator != (const char *inStr1, const TString &inStr2)
			{ return TString(inStr1).Compare(inStr2) != 0; }

		//////////
		// Does this string start with the given character string? 
		//
		// [in] inStr - a character string to compare against
		// [in_optional] inCaseSens - case sensitive comparison? (default true)
		// [out] return - true if this string starts with inStr, false otherwise
		//
		bool	StartsWith(const char *inStr, bool inCaseSens = true);
		
		//////////
		// Does this string end with the given character string? 
		//
		// [in] inStr - a character string to compare against
		// [in_optional] inCaseSens - case sensitive comparison? (default true)
		// [out] return - true if this string ends with inStr, false otherwise
		//
		bool	EndsWith(const char inCh, bool inCaseSens = true);
		
		//////////
		// Does this string contain the given character string? 
		//
		// [in] inStr - a character string to compare against
		// [in_optional] inCaseSens - case sensitive comparison? (default true)
		// [out] return - true if this string contains inStr, false otherwise
		//
		bool	Contains(const char *inStr, bool inCaseSens = true);
		
		//////////
		// Find the given character in the string. 
		//
		// [in] inCh - the character to search for
		// [in_optional] inStartPos - position in the string to start search (default 0)
		// [in_optional] inCaseSens - case sensitive comparison? (default true)
		// [out] return - the position in the string where the first occurence of
		//				  the character was found, -1 if not found
		//
		int32	Find(const char inCh, int32 inStartPos = 0, bool inCaseSens = true);
		
		//////////
		// Find the given character string in the string. 
		//
		// [in] inStr - the characterstring to search for
		// [in_optional] inStartPos - position in the string to start search (default 0)
		// [in_optional] inCaseSens - case sensitive comparison? (default true)
		// [out] return - the position in the string where the first occurence of
		//				  the character string was found, -1 if not found
		//
		int32	Find(const char *inStr, int32 inStartPos = 0, bool inCaseSens = true);

		//////////
		// Is the string empty? 
		//
		// [out] return - true if the string is empty, false otherwise
		//
		bool	IsEmpty();
		
		//////////
		// Empty the string. 
		//
		void	Empty();

		//////////
		// Trim spaces and tabs from the right end of the string. 
		//
		void	RTrim(void);
		
		//////////
		// Trim spaces and tabs from the left end of the string. 
		//
		void	LTrim(void);
		
		//////////
		// Chop a single character off the right end of the string. 
		//
		void	RChop(void);
		
		//////////
		// Chop a single character off the left end of the string. 
		//
		void	LChop(void);

		//////////
		// Returns a substring starting at inStartPos and running for inLen characters.
		//
		// [in] inStart - starting position of the substring
		// [in_optional] inLen - length of substring (default stop at end of string)
		// [out] return - a substring of this string
		//
		TString	Mid(uint32 inStart, int32 inLen = -1);
		
		//////////
		// Change a portion of a string (useful when dealing with large strings) 
		//
		// [in] inStart - starting position of the portion to be changed
		// [in] inLen - length of the portion to be changed
		// [in] str - string used to replace the specified portion
		//
		void	Set(uint32 inStart, uint32 inLen, const TString &str);

		//////////
		// Make the string all lower case. 
		//
		void	MakeLower();
		
		//////////
		// Make the string all upper case. 
		//
		void	MakeUpper();
		
		//////////
		// Get the string as lower case.  
		//
		// [out] return - the string as a lower case string
		//
		TString	GetLower() const;
		
		//////////
		// Get the string as upper case.  
		//
		// [out] return - the string as an upper case string
		//
		TString	GetUpper() const;

		//////////
		// Set the minimum string resize value used when expanding strings. 
		//
		// [in] size - new min resize value
		//
		void	SetMinResize(uint32 size);
		
		//////////
		// Change the size of the string. Always change the size by, at least, 
		// MIN_STRING_RESIZE. This helps with character by character appends. 
		//
		// [in] size - preferred resize value
		//
		void	Resize(uint32 size);
		
		//////////
		// Returns the character at the given postion. 
		//
		// [in] inPos - position
		// [out] return - the character
		//
		char	operator [] (int inPos) const;
		
		//////////
		// Returns the character at the given postion. 
		//
		// [in] inPos - position
		// [out] return - the character
		//
		char	operator () (int inPos) const;

		//////////
		// Get the character string associated with this TString. 
		//
		// [out] return - char string pointer
		//
		const char	*GetString() const;
		
		//////////
		// Get the character string associated with this TString. 
		//
		// [out] return - char string pointer
		//
		char		*GetBuffer();

		//////////
		// Recast as a character string (same as GetString()). 
		//
		// [out] return - char string pointer
		//
		operator const char *() const;
		
		//////////
		// Recast as an int16.  Evaluates to 0 if the cast is invalid. 
		//
		operator int16();
		
		//////////
		// Recast as an uint16.  Evaluates to 0 if the cast is invalid. 
		//
		operator uint16();
		
		//////////
		// Recast as an int32.  Evaluates to 0 if the cast is invalid. 
		//
		operator int32();
		
		//////////
		// Recast as an uint32.  Evaluates to 0 if the cast is invalid. 
		//
		operator uint32();
		
		//////////
		// Recast as an double.  Evaluates to 0 if the cast is invalid. 
		//
		operator double();

		//////////
		// Set to another string. 
		//
		// [in] inStr - another string
		//
		TString &operator = (const char *inStr);
		
		//////////
		// Set to another string. 
		//
		// [in] inStr - another string
		//
		TString &operator = (const TString &inStr);
		
		//////////
		// Set to an int32. 
		//
		// [in] inNum - an int32
		//
		TString &operator = (const int32 inNum);
		
		//////////
		// Set to a uint32. 
		//
		// [in] inNum - a uint32
		//
		TString &operator = (const uint32 inNum);

		//////////
		// Set to an int16. 
		//
		// [in] inNum - an int16
		//
		TString &operator = (const int16 inNum);

		//////////
		// Set to a uint16. 
		//
		// [in] inNum - a uint16
		//
		TString &operator = (const uint16 inNum);
		
		//////////
		// Set to a double. 
		//
		// [in] inNum - a double
		//
		TString &operator = (const double inNum);
		
		//////////
		// Append a character string. 
		//
		// [in] inStr - a char string
		//
		TString &operator += (const char *inStr);
		
		//////////
		// Append another TString to this one. 
		//
		// [in] inStr - a TString
		//
		TString &operator += (const TString &inStr);
		
		//////////
		// Append a character. 
		//
		// [in] inCh - a character
		//
		TString &operator += (const char inCh);
		
		//////////
		// Append a long. 
		//
		// [in] inNum - a long
		//
		TString &operator += (const long inNum);

		//////////
		// Concatenate two strings.
		//
		// [in] inStr1 - first string
		// [in] inStr2 - second string
		// [out] return - a new TString which is the concatenation of the two strings
		//
		friend TString operator + (const TString &inStr1, const TString &inStr2);
		
		// **** stream input/output methods ****
        
		//////////
		// Output the TString to a stream. 
		//
		// [in] inStream - the output stream
		// [in] inStr - the TString
		//
		friend ostream & operator << (ostream &inStream, const TString &inStr);
        
		//////////
		// Stream data from an input stream into a TString . 
		//
		// [in] inStream - the input stream
		// [in] inStr - the TString
		//
		friend istream & operator >> (istream &inStream, TString &inStr);

	protected:
		//////////
		// Minimum amount to grow by when resizing the string. 
		//
		uint32		m_MinResize;
		
		//////////
		// Current length of the string. 
		//
		uint32		m_Length;
		
		//////////
		// Size (capacity) of the string. 
		//
		uint32		m_Size;
		
		//////////
		// Char array to hold the string contents. 
		//
		char		*m_String;

		//////////
		// Is a number stored in the string?
		//
		// [out] return - true if the string is being used to store a number, 
		//				  false otherwise	
		//
		bool		IsNumber();
		
		//////////
		// Is a date stored in the string?
		//
		// [out] return - true if the string is being used to store a date, 
		//				  false otherwise	
		//
		bool		IsDate();

		//////////
		// Portably determine whether a call to snprintf failed, given
		// its return value and the size of the buffer it was using.
		// See the source code if you want to know why this is tricky.
		//
		// [in] inSnprintfRetval - number to convert
		// [in] inBufferSize - char string to store number as a string
		// [out] return - true if snprintf failed
		//
		static bool IsSnprintfError(int inSnprintfRetval, int inBufferSize);

		//////////
		// Convert a double to a char string.
		//
		// [in] inNum - number to convert
		// [out] return - the number as a string
		//
		static TString DoubleToString(double inNum);

		//////////
		// Convert a signed integer to a char string.
		//
		// [in] inNum - number to convert
		// [out] return - the number as a string
		//
		static TString IntToString(int32 inNum);

		//////////
		// Convert an unsigned integer to a char string.
		//
		// [in] inNum - number to convert
		// [out] return - the number as a string
		//
		static TString UIntToString(uint32 inNum);
};

END_NAMESPACE_FIVEL

#endif // _TString_h_

/*
 $Log$
 Revision 1.5  2002/05/15 11:05:17  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.4.4.3  2002/04/30 07:57:24  emk
 3.3.2.5 - Port Win32 code to use the 20Kloc of Common code that now
 exists.  The (defstyle ...) command should work, but (textaa ...) isn't
 available yet.

 Next up: Implement the (textaa ...) command and the low-level
 GraphicsTools::Image::DrawBitMap.

 Revision 1.4.4.2  2002/04/22 05:22:33  emk
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

 Revision 1.4.4.1  2002/04/19 11:20:13  emk
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

 Revision 1.4  2002/03/07 20:36:18  emk
 TString bug fixes & new constructor.

   - Fixed copy constructor bug where input string is empty
   - Implemented a full set of comparison operators as friends
     (not methods), so the compiler doesn't convert the
     right hand argument of (char*) == (TString) to a char*.
   - Added constructor which takes char* and length
   - Test cases for new features

 Revision 1.3  2002/03/04 15:16:14  hamon
 Added support for compiler's namespaces. Namespaces are only enabled on macintosh.

Moved OS specific configuration to TPlatform.h

Changes by Elizabeth and Eric, okayed by Eric.

 Revision 1.2  2002/02/27 16:38:21  emk
 Cross-platform code merge!

 * Merged back in support for the Macintosh platform.  This is an ongoing
   process, and we'll need to do more work.

 * Separated out platform specific configuration with big block comments.

 * Merged in a few changes from KBTree which appeared to fix bugs.

 * Merged in IntToString, UIntToString, DoubleToString changes from the
   Macintosh engine, and improved the error handling.  NOTE: doubles now
   print using "%f" (the way the Mac engine always did it).  This means
   that "tstr = 0.0" will set 'tstr' to "0.000000", not "0" (as it
   did in the Win32 engine).

 This code might not build on Windows.  As soon as I hear from ssharp
 that he's updated the project files, I'll test it myself.

 Revision 1.1  2001/09/24 15:11:00  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.2  2000/05/11 12:54:53  chuck
 v 2.01 b2

 Revision 1.1  2000/04/06 17:06:10  chuck
 Initial check-in

*/
