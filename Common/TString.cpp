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
// TString.cpp : 
//

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

#include "THeader.h"
#include "TString.h"

USING_NAMESPACE_FIVEL

#ifdef WIN32
#define snprintf _snprintf
#endif

//
//  TString - Default constructor.
//
TString::TString() : TObject()
{
    m_Length = 0;
    m_Size = 0;
    m_String = NULL;
	m_MinResize = MIN_STRING_RESIZE;

    Resize(1);
}

//
//  TString - Construct with a fixed array size.
//
TString::TString(const uint32 inSize) : TObject()
{
    m_Length = 0;
    m_Size = 0;
    m_String = NULL;
	m_MinResize = MIN_STRING_RESIZE;

    Resize(inSize);
}

//
//  TString - Construct from a string.
//
TString::TString(const char *inStr) : TObject()
{
    m_Length = 0;
    m_Size = 0;
    m_String = NULL;
	m_MinResize = MIN_STRING_RESIZE;

    if (inStr != NULL) 
    {
        m_Length = strlen(inStr);
        Resize(m_Length + 1);
        strcpy(m_String, inStr);
    }
	else
	{
		Resize(1);
	}
}

//
//  TString - Construct from a character array.
//
TString::TString(const char *inStr, uint32 inLength)
{
	ASSERT(inStr != NULL);

    m_Length = inLength;
    m_Size = 0;
    m_String = NULL;
	m_MinResize = MIN_STRING_RESIZE;

	Resize(m_Length + 1);
	strncpy(m_String, inStr, inLength);
	m_String[m_Length] = '\0';
}

//
//  TString - Construct from a TString.
//
TString::TString(const TString &inStr) : TObject()
{
    m_Length = 0;
    m_Size = 0;
    m_String = 0;
	m_MinResize = MIN_STRING_RESIZE;

    if (inStr.m_Length > 0) 
    {
        m_Length = inStr.m_Length;
        Resize(m_Length + 1);
        strcpy(m_String, inStr.m_String);
    }
	else
	{
		Resize(1);
	}
}

//
//	~TString - Destructor.
//
TString::~TString()
{
    if (m_String != NULL) 
		delete [] m_String;
}

//
//	Empty - Empty the string.
//
void TString::Empty(void)
{
	*this = "";
}

//
//  operator = - Set to another string.
//
TString &TString::operator=(const char *inStr)
{
    delete [] m_String;
    m_String = NULL;
    m_Size = 0;
    m_Length = 0;

    if (inStr) 
    {
        m_Length = strlen(inStr);
        Resize(m_Length + 1);
        strcpy(m_String, inStr);
    }

    return (*this);
}

//
//  operator = - Set to another TString.
//
TString &TString::operator=(const TString &inStr)
{
    if (&inStr == this) 
    	return (*this);

    delete [] m_String;
    m_String = NULL;
    m_Size = 0;
    m_Length = 0;

    if (inStr.m_Length) 
    {
        m_Length = inStr.m_Length;
        Resize(m_Length + 1);
        strcpy(m_String, inStr.m_String);
    }

    return (*this);
}

//
//  operator = - Set to an int32.
//
TString &TString::operator=(const int32 inNum)
{
	*this = IntToString(inNum);
	return *this;
}

//
//  operator = - Set to an uint32.
//
TString &TString::operator=(const uint32 inNum)
{
	*this = UIntToString(inNum);
	return *this;
}

//
//  operator = - Set to an int16.
//
TString &TString::operator=(const int16 inNum)
{
	*this = IntToString(inNum);
	return *this;
}

//
//  operator = - Set to a uint16.
//
TString &TString::operator=(const uint16 inNum)
{
	*this = UIntToString(inNum);
	return *this;
}

//  Set to a double.
//
TString &TString::operator=(const double inNum)
{
	*this = DoubleToString(inNum);
	return *this;
}

//
//  operator = - Append a string.
//
TString &TString::operator+=(const char *inStr)
{
    if (inStr) 
    {
        m_Length += strlen(inStr);
        Resize(m_Length + 1);
        strcat(m_String, inStr);
    }

    return *this;
}

//
// operator += - Append an int32.
//
TString &TString::operator+=(const int32 inNum)
{
	*this += IntToString(inNum);
	return *this;
}

//
//	Length - return the length of the string.
//
uint32 TString::Length() const
{
	return (m_Length); 
}

//
//	Size - return the amount of memory allocated to the string (not
//		necessarily the same as the length).
//
uint32 TString::Size() const
{
	return (m_Size);
}

//
//	IsEmpty - Return true if the string is empty (0 length), false
//		otherwise.
//
bool TString::IsEmpty()
{
	if (m_Length == 0)
		return (true);
	return (false);
}

//
//	GetString - same as cast.
//
const char *TString::GetString() const
{
	return (m_String);
}

//
//	GetBuffer - Return a pointer to the char buffer.
//
char *TString::GetBuffer()
{
	return (m_String);
}

//
//	Update - Update our information about the string.
//
void TString::Update()
{
	m_Length = strlen(m_String);
}

//
//	operator const char * - Cast operator.
//
TString::operator const char *() const
{
	return (m_String);
}

//
//	Equal - Return true if inStr is the same as this. If
//		inCaseSens is true, do a case sensitive comparison,
//		otherwise do a case insensitive comparison.
//
bool TString::Equal(const char *inStr, bool inCaseSens /* = true */)
{
	return (Compare(inStr, inCaseSens) == 0);
}

//
//  operator += - Append a character.
//
TString &TString::operator+=(const char inCh)
{
    m_Length++;
	
	// Only resize if necessary
	if (m_Length >= m_Size)
		Resize(m_Length + 1);

    m_String[m_Length - 1] = inCh;
    m_String[m_Length] = 0;

    return (*this);
}

//
//	operator += - Append an TString.
//
TString &TString::operator+=(const TString &inStr)
{
    if (inStr.m_Length) 
    {
        m_Length += inStr.m_Length;
        
		// Only resize if necessary
		if (m_Length >= m_Size)
			Resize(m_Length + 1);

        strcat(m_String, inStr.m_String);
    }

    return (*this);
}

//
// Set the minimum string resize value used when expanding strings
//
void TString::SetMinResize(uint32 size)
{
	if (size > MIN_STRING_RESIZE)
		m_MinResize = size;
}

//
//	Resize - Change the size of the string. Always change the
//		the size by, at least, MIN_STRING_RESIZE. This helps with
//		character by character appends. 
//
void TString::Resize(uint32 inSize)
{
	char		*newStr = NULL;
	uint32		newSize = 0;

    if (inSize > m_Size) 
    {
		// grow the string
        newSize = Max(inSize, m_Size + m_MinResize);  
    }
	else if ((inSize + MAX_STRING_RESIZE_DIFF) < m_Size)
	{
		// shrink the string
		newSize = inSize;
	}

	if (newSize != 0)
	{
		m_Size = newSize;

		newStr = new char[m_Size];
		ASSERT(newStr != NULL);		// Make sure the memory was allocated

        if (newStr != NULL)
		{
			newStr[0] = 0;
			if (m_String != NULL) 
			{
				strcpy(newStr, m_String);
				delete [] m_String;
			}
			m_String = newStr;
		}
	}
}

//
//	RTrim - Trim spaces from the right end.
//
void TString::RTrim(void)
{
	int32	pos;
	int32	charsTrimmed = 0;

	if (IsEmpty())
		return;

	pos = m_Length - 1;
	while ((pos >= 0) and (m_String[pos] == ' ' || m_String[pos] == '\t'))
	{
		m_String[pos] = '\0';
		pos--;
		charsTrimmed++;
	}
	
	m_Length -= charsTrimmed;

	if (charsTrimmed >= MAX_STRING_RESIZE_DIFF)
		Resize(m_Length + 1);
}

//
//	LTrim - Trim spaces from the left end.
//
void TString::LTrim(void)
{
	TString	tmpStr;
	int32	charsToTrim = 0;
	uint32	pos = 0;

	if (IsEmpty())
		return;

	while ((pos < m_Length) and (m_String[pos] == ' ' || m_String[pos] == '\t'))
		charsToTrim++;

	tmpStr = this->Mid(charsToTrim);
	*this = tmpStr;
}

//
// RChop - Chop a single character off the end of the String
//
void TString::RChop(void)
{
	if (IsEmpty())
		return;

	m_String[m_Length-1] = '\0';
	m_Length--;
}

//
// LChop - Chop a single character off the beginning of the String
//
void TString::LChop(void)
{
	TString tmpStr;

	if (IsEmpty())
		return;

	tmpStr = this->Mid(1);
	*this = tmpStr;
}

void TString::MakeLower()
{
	if (not IsEmpty())
	{
#ifndef HAVE__STRLWR
		for (int i = 0; i < m_Length; i++)
			m_String[i] = tolower(m_String[i]);
#else
		_strlwr(m_String);
#endif
	}
}

void TString::MakeUpper()
{
	if (not IsEmpty())
	{
#ifndef HAVE__STRUPR
		for (int i = 0; i < m_Length; i++)
			m_String[i] = toupper(m_String[i]);
#else
		_strupr(m_String);
#endif
	}
}

TString	TString::GetLower() const
{
	TString		lower = m_String;

	lower.MakeLower();
	return (lower);
}

TString	TString::GetUpper() const
{
	TString		upper = m_String;

	upper.MakeUpper();
	return (upper);
}

//
//  Type - Determine what type of data is in the string.
//
StringType TString::Type()
{
    if (IsDate()) 
    	return (TDateString);
    else if (IsNumber()) 
    	return (TNumberString);
    else 
    	return (TStringString);
}


//	
//	Compare - Return the result of a comparison.
//
//  < 0 if m_String < inStr
//    0 if m_String = inStr
//  > 0 if m_String > inStr
//
int TString::Compare(const char *inStr, bool inCaseSens /* = true */) const
{
    if (inCaseSens) 
    	return ::strcmp(m_String, inStr);
    else 
	{
#ifndef HAVE__STRICMP
		TString		srcStr = m_String;
		TString		compStr = inStr;

		srcStr.MakeLower();
		compStr.MakeLower();

		return (::strcmp(srcStr.GetString(), compStr.GetString()));
#else
    	return ::_stricmp(m_String, inStr);
#endif
	}
}

//
//	TypeCompare -  This is like Compare except the string checks to see if it
//  is a number. If it is, it compares itself to inStr
//  based on that rather than alphabetically.
//
int TString::TypeCompare(TString &inStr, bool inCaseSens /* = false */)
{
    StringType	type1 = Type();
    StringType	type2 = inStr.Type();
    
    if ((type1 == TNumberString) and (type2 == TNumberString)) 
    {
		double		d1, d2;

        d1 = (double) *this;
        d2 = (double) inStr;
        if (d1 > d2) 
			return (1);
        else if (d1 < d2) 
			return (-1);
        else 
			return (0);
    } 
    else 
    	return (Compare(inStr, inCaseSens));
}

//
//  StartsWith - Return true if the other string matches the 
//	beginning of this string. Shorten the string and test for 
//	equality.
//
bool TString::StartsWith(const char *inStr, bool inCaseSens)
{
	TString		tmpStr;
	uint32	 	strLen = strlen(inStr);

    if (strLen > m_Length) 
    	return (false);

    tmpStr = Mid(0, strLen);
    
    return (tmpStr.Equal(inStr, inCaseSens));
}

//
//	EndsWith - Return true if the string ends with the given
//		character.
//
bool TString::EndsWith(const char inCh, bool inCaseSens /* = true */)
{
	char	compareCh;
	char	targetCh;

	if (IsEmpty())
		return (false);

	compareCh = inCh;
	if (not inCaseSens)
		compareCh = tolower(inCh);

	targetCh = m_String[m_Length - 1];
	if (not inCaseSens)
	{
		// make lower case
		if ((targetCh >= 'A') and (targetCh <= 'Z'))
			targetCh = (targetCh - 'A') + 'a';
	}

	if (targetCh == compareCh)
		return (true);

	return (false);
}

//
//	Contains - Return true if the given string can be found
//		somewhere within this string.
//
bool TString::Contains(const char *inStr, bool inCaseSens /* = true */)
{
	ASSERT(inStr != NULL);
	char	*strPtr = NULL;
	bool	retVal = false;

	if (IsEmpty())
		return (false);

	if (not inCaseSens)
	{
		TString		temp(m_String);
		TString		otherTemp(inStr);

		temp.MakeLower();
		otherTemp.MakeLower();

		strPtr = ::strstr((const char *) temp, (const char *) otherTemp);
	}
	else
		strPtr = ::strstr(m_String, inStr);

	if (strPtr != NULL)
		retVal = true;

	return (retVal);
}

//
//	Find - Find the given character.
//
int32 TString::Find(const char inCh, int32 inStartPos /* = 0 */, bool inCaseSens /* = true */)
{
	// if this is empty, nothing to look for
	if (IsEmpty())
		return (-1);

	// sanity check on start position
	if ((inStartPos < 0) or (inStartPos >= (int32) m_Length))
		return (-1);

	char	compareCh = inCh;

	if (not inCaseSens)
		compareCh = tolower(inCh);

	int		i = inStartPos;
	char	theCh;

	while ((theCh = m_String[i]) != '\0')
	{
		if (not inCaseSens)
			theCh = tolower(theCh);

		if (theCh == compareCh)
			return (i);

		i++;
	}

	// didn't find it
	return (-1);
}

//
//	Find - Find the given string.
//
int32 TString::Find(const char *inStr, int32 inStartPos /* = 0 */, bool inCaseSens /* = true */)
{
	ASSERT(inStr != NULL);

	// if this is empty, nothing to look for
	if (IsEmpty())
		return (-1);

	// sanity check on start position
	if ((inStartPos < 0) or (inStartPos >= (int32) m_Length))
		return (-1);

	TString		searchStr = inStr;

	// if the search string is longer than this string we can stop now
	if (m_Length < searchStr.Length())
		return (-1);

	TString		thisStr = m_String;

	// convert both to lower case if not case sensitive
	if (not inCaseSens)
	{
		searchStr.MakeLower();
		thisStr.MakeLower();
	}

	// last possible start position in this for search string
	int32	endPos = m_Length - searchStr.Length();

	// position in search string
	int32	searchPos;

	// position in this string
	int32	targetPos = inStartPos;
	int32	targetPos2;

	while (true)
    {
		searchPos = 0;			// start of search pattern
		targetPos2 = targetPos;	// current target position

		while ((searchStr(searchPos) == thisStr(targetPos2))
			and (searchStr(searchPos) != '\0')
			and (thisStr(targetPos2) != '\0'))
		{
			targetPos2++;
			searchPos++;
		}

		// if we are at the end of the search string then we
		//	have found it in this string
		if (searchPos >= (int32) searchStr.Length())
			return (targetPos);

		// see if we have gone past the last possible end 
		//	position in this string
		if ((targetPos2 >= (int32)thisStr.Length()) or (targetPos >= endPos))
			return (-1);

		// move one character down this string to try and match
		//	the search string again from here
		targetPos++;
	}

	return (-1);
}
//
//	operator int16 - 
//
TString::operator int16()
{
    if ((m_Length < 1) or (not IsNumber()))
    	return (0);

    return (atoi(m_String));
}

//
//	operator uint16 -
//
TString::operator uint16()
{
    if ((m_Length < 1) or (not IsNumber()))
    	return (0);

    return (atoi(m_String));
}

//
//	operator int32 -
//
TString::operator int32()
{
    if ((m_Length < 1) or (not IsNumber()) )
    	return (0);

    return (atol(m_String));
}

//
//	operator uint32 -
//
TString::operator uint32()
{
    if ((m_Length < 1) or (not IsNumber()))
    	return (0);

    return (atol(m_String));
}

//
//	operator double - 
//
TString::operator double()
{
    if ((m_Length < 1) or (not IsNumber()))
		return (0.0);

    return (atof(m_String));
}	

//
//	Mid - Return a portion of the string starting at
//		inStartPos and running for inLength characters.
//		If inLength is -1, return the rest of the string
//		from inStartPos.
//
TString TString::Mid(uint32 inStart, int32 inLen /* = -1 */)
{
    TString     tmpStr;
	int32		tmpLen = inLen;

	if (inStart > m_Length)
		return(tmpStr);

	if ((inLen == -1) or (inStart + inLen > m_Length))
		tmpLen = m_Length - inStart;
		
    tmpStr.Resize(tmpLen + 1);
    if (tmpLen > 0)
        strncpy(tmpStr.m_String, m_String + inStart, tmpLen);
    tmpStr.m_String[tmpLen] = 0;

    tmpStr.Update();

    return (tmpStr);
}

//
// Set - Change a portion of a string (useful when dealing with large strings)
//
void TString::Set(uint32 inStart, uint32 inLen, const TString &str)
{
	uint32 i;
	const char *p;
	
	// Check boundaries
	if (str.Length() != inLen || (inStart + inLen) >= m_Length)
		return;

	p = str.GetString();
	for (i=0; i < inLen; i++)
		m_String[i+inStart] = p[i];
}

//
//	operator [] - Return a character from the array. 
//
char TString::operator [] (uint32 inPos) const
{
    if (inPos >= m_Length) 
    	inPos = m_Length - 1;

    return (m_String[inPos]);
}

//
//	operator () - Same as [].
//
char TString::operator () (uint32 inPos) const
{
	if (inPos >= m_Length)
		inPos = m_Length - 1;

	return (m_String[inPos]);
}

//
//	operator + - Concatenate strings.
//
TString FIVEL_NS operator + (const TString &inStr1, const TString &inStr2)
{
    TString tmpStr;
    int     tmpLen = inStr1.m_Length + inStr2.m_Length;

    tmpStr.Resize(tmpLen + 1);
    tmpStr.m_Length = tmpLen;
    strcpy(tmpStr.m_String, inStr1.m_String);
    strcat(tmpStr.m_String, inStr2.m_String);

    return (tmpStr);
}

enum 
{
    vSTART,
    vNEGATIVE,
    vDECIMAL
};

//
//  IsNumber - Does the string qualify for what we consider a number?
//
bool TString::IsNumber()
{
    int     state = vSTART;
    char    ch;
    char    *s = m_String;

    //  NULL Strings aren't numbers per se (in terms of their type) but
    //  they should evaluate to 0. In order to do this, this routine
    //  returns false, but the actual conversion methods will return 0.
    //
    if (m_String == NULL) 
		return (false);

    //
    //  atof() can't convert strings longer than 100 chars, so
    //  if that's the case it's not a number
    //
    if (m_Length > 100)
        return (false);
 
    while ((ch = *s++) != '\0') 
    {
        if (ch != ' ')

            switch (state) 
            {
                //  Look for dash for negative. If not there fall through
                //  to look for first digit.
                case vSTART:

                    if (ch == '-') 
                    {
                        state = vNEGATIVE;
                        break;
                    }

                //  We've had the negative sign, so look for numbers.
                //  If not a number or a decimal it's bad.
                case vNEGATIVE:

                    if (isdigit(ch)) 
                    {
                        state = vNEGATIVE;
                        break;
                    }

                    if (ch == '.') 
                    {
                        state = vDECIMAL;
                        break;
                    }
                    return (false);
                    
                //  We've had the decimal point, so look for digits only.
                case vDECIMAL:

                    if (isdigit(ch))
                        break;

                    return (false);

                //  Should never get here.
                default:

                   // assert(false);
                    break;
            }
    }
    return (true);
}

//
//  IsDate - Does the string qualify as what we consider a date?
//		This routine accepts strings of the form:
//			MM/DD/YY
//
bool TString::IsDate()
{
    char    ch;
    char    *s = m_String;

    //  Month number. 1 or 2 digits.
    //
    ch = *s++;
    if (!isdigit(ch)) 
		return (false);

    ch = *s++;
    if (isdigit(ch)) 
		ch = *s++;

    //  First slash.
    //
    if (ch != '/') 
		return (false);

    //  Day number. 1 or 2 digits.
    //
    ch = *s++;
    if (!isdigit(ch)) 
		return (false);

    ch = *s++;
    if (isdigit(ch)) 
		ch = *s++;

    //  Second slash.
    //
    if (ch != '/') 
		return (false);

    //  Year. 2 or 4 digits.
    //
    ch = *s++;
    if (!isdigit(ch)) 
		return (false);

    ch = *s++;
    if (!isdigit(ch)) 
		return (false);

	// check here if only two digits for year
	if (*s == 0)
		return (true);

	ch = *s++;
    if (!isdigit(ch)) 
		return (false);

	ch = *s++;
    if (!isdigit(ch)) 
		return (false);

    return (*s == 0);
}

//
//	IsSnprintfError -  Portably determine whether a call to
//                     snprintf failed, given its return value and the
//                     size of the buffer it was using.
//
bool TString::IsSnprintfError(int inSnprintfRetval, int inBufferSize)
{
	// Old C libraries return -1 if snprintf overflows its buffer.
	// New C libraries return the number of characters which *would* have
	// been printed if the error did not occur. This is impressively vile.
	// Thank the C99 committee for this bright idea. But wait! We also
	// need to keep track of the trailing NULL.
	int maximum_allowable_string_size = inBufferSize - 1;
	return (inSnprintfRetval < 0 ||
			inSnprintfRetval >= maximum_allowable_string_size);
}

//
//	DoubleToString - Convert a double to a char string.
//
TString TString::DoubleToString(double inNum)
{
	char buffer[SNPRINTF_BUFFER_SIZE];
	int retval;

	retval = snprintf(buffer, SNPRINTF_BUFFER_SIZE, "%f", inNum);
	ASSERT(!IsSnprintfError(retval, SNPRINTF_BUFFER_SIZE));
	return TString(buffer);
}

//
//	IntToString - Convert an int to a char string.
//
TString TString::IntToString(int32 inNum)
{
	char buffer[SNPRINTF_BUFFER_SIZE];
	int retval;

	retval = snprintf(buffer, SNPRINTF_BUFFER_SIZE, "%d", inNum);
	ASSERT(!IsSnprintfError(retval, SNPRINTF_BUFFER_SIZE));
	return TString(buffer);
}

//
//	IntToString - Convert a uint to a char string.
//
TString TString::UIntToString(uint32 inNum)
{
	char buffer[SNPRINTF_BUFFER_SIZE];
	int retval;

	retval = snprintf(buffer, SNPRINTF_BUFFER_SIZE, "%u", inNum);
	ASSERT(!IsSnprintfError(retval, SNPRINTF_BUFFER_SIZE));
	return TString(buffer);
}

//
// stream I/O functions
//
ostream & FIVEL_NS operator << (ostream &inStream, const TString &inStr)
{
	inStream << inStr.m_String;
	return (inStream);
}

istream & FIVEL_NS operator >> (istream &inStream, TString &inStr)
{
//	char				endline;

	inStream >> inBuffer;

	inStr = inBuffer;

//	inStream.get(buf, bufsize,'\n');
//	inStream.get(endline);
//
//	inStr = buf;

	return (inStream);
}

/*
 $Log$
 Revision 1.6  2002/03/07 20:36:18  emk
 TString bug fixes & new constructor.

   - Fixed copy constructor bug where input string is empty
   - Implemented a full set of comparison operators as friends
     (not methods), so the compiler doesn't convert the
     right hand argument of (char*) == (TString) to a char*.
   - Added constructor which takes char* and length
   - Test cases for new features

 Revision 1.5  2002/03/04 15:30:20  hamon
 Added support for compiler's namespaces. Namespaces are only enabled on macintosh.

Moved OS specific configuration to new file TPlatform.h

Changes by Elizabeth and Eric, okayed by Eric.

 Revision 1.4  2002/02/28 11:05:05  tvw
 A few small changes needed to build FiveL Win32 with new Common library.

 Revision 1.3  2002/02/27 16:38:21  emk
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

 Revision 1.2  2002/02/19 12:35:11  tvw
 Bugs #494 and #495 are addressed in this update.

 (1) 5L.prefs configuration file introduced
 (2) 5L_d.exe will no longer be part of CVS codebase, 5L.prefs allows for
     running in different modes.
 (3) Dozens of compile-time switches were removed in favor of
     having a single executable and parameters in the 5L.prefs file.
 (4) CryptStream was updated to support encrypting/decrypting any file.
 (5) Clear file streaming is no longer supported by CryptStream

 For more details, refer to ReleaseNotes.txt

 Revision 1.1  2001/09/24 15:11:00  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.3  2000/08/08 19:03:12  chuck
 no message

 Revision 1.2  2000/05/11 12:54:53  chuck
 v 2.01 b2

 Revision 1.1  2000/04/06 17:06:10  chuck
 Initial check-in

*/
