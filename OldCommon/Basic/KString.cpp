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
// KString.cpp : 
//

#include "KHeader.h"
#include "KString.h"

#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>

static const long MIN_STRING_RESIZE = 4;
static const long MAX_STRING_RESIZE_DIFF = 20;
static const long INBUFFER_SIZE = 256;

static char inBuffer[INBUFFER_SIZE];

//
//  KString - Default constructor.
//
KString::KString() : KObject()
{
    m_Length = 0;
    m_Size = 0;
    m_String = NULL;

    Resize(1);
}

//
//  KString - Construct with a fixed array size.
//
KString::KString(const uint32 inSize) : KObject()
{
    m_Length = 0;
    m_Size = 0;
    m_String = NULL;

    Resize(inSize);
}

//
//  KString - Construct from a string.
//
KString::KString(const char *inStr) : KObject()
{
    m_Length = 0;
    m_Size = 0;
    m_String = NULL;

    if (inStr != NULL) 
    {
        m_Length = strlen(inStr);
        Resize(m_Length + 1);
        strcpy(m_String, inStr);
    }
}

//
//  KString - Construct from a KString.
//
KString::KString(const KString &inStr) : KObject()
{
    m_Length = 0;
    m_Size = 0;
    m_String = 0;

    if (inStr.m_Length > 0) 
    {
        m_Length = inStr.m_Length;
        Resize(m_Length + 1);
        strcpy(m_String, inStr.m_String);
    }
}

//
//	~KString - Destructor.
//
KString::~KString()
{
    if (m_String != NULL) 
		delete [] m_String;
}

//
//	Empty - Empty the string.
//
void KString::Empty(void)
{
	*this = "";
}

//
//  operator = - Set to another string.
//
KString &KString::operator=(const char *inStr)
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
//  operator = - Set to another KString.
//
KString &KString::operator=(const KString &inStr)
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
KString &KString::operator=(const int32 inNum)
{
	char	tmpStr[64];
	
	IntToString(inNum, tmpStr);
    *this = tmpStr;
    
    return (*this);
}

KString &KString::operator=(const uint32 inNum)
{
	char	tmpStr[64];
	
	UIntToString(inNum, tmpStr);
	*this = tmpStr;
	
	return (*this);
}

//
//  operator = - Set to an int16.
//
KString &KString::operator=(const int16 inNum)
{
	char	tmpStr[64];
	
	IntToString(inNum, tmpStr);
    *this = tmpStr;
    
    return (*this);
}

KString &KString::operator=(const uint16 inNum)
{
	char	tmpStr[64];
	
	UIntToString(inNum, tmpStr);
	*this = tmpStr;
	
	return (*this);
}

//  Set to a double.
//
KString &KString::operator=(const double inNum)
{
    char    tmpStr[64];

    DoubleToString(inNum, tmpStr);
    *this = tmpStr;

    return (*this);
}

//
//  operator = - Append a string.
//
KString &KString::operator+=(const char *inStr)
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
KString &KString::operator+=(const int32 inNum)
{
	char	tmpStr[64];

	IntToString(inNum, tmpStr);
	*this += tmpStr;

	return (*this);
}

//
//	Length - return the length of the string.
//
uint32 KString::Length() const
{
	return (m_Length); 
}

//
//	Size - return the amount of memory allocated to the string (not
//		necessarily the same as the length).
//
uint32 KString::Size() const
{
	return (m_Size);
}

//
//	IsEmpty - Return true if the string is empty (0 length), false
//		otherwise.
//
bool KString::IsEmpty()
{
	if (m_Length == 0)
		return (true);
	return (false);
}

//
//	GetString - same as cast.
//
const char *KString::GetString() const
{
	return (m_String);
}

//
//	GetBuffer - Return a pointer to the char buffer.
//
char *KString::GetBuffer()
{
	return (m_String);
}

//
//	Update - Update our information about the string.
//
void KString::Update()
{
	m_Length = strlen(m_String);
}

//
//	operator const char * - Cast operator.
//
KString::operator const char *() const
{
	return (m_String);
}

//
//	operator < - Less than operator. Return an int
//		that is not equal to 0 (true) if inStr is
//		less than this. Return 0 (false) if inStr
//		is greater than or equal to this.
//
int KString::operator < (const KString &inStr) const
{
	return (Compare(inStr) < 0);
}

//
//	operator > - Greater than operator. Return an int
//		this is not equal to 0 (true) if inStr is
//		greater than this. Return 0 (false) if inStr
//		is less than or equal to this.
//
int KString::operator > (const KString &inStr) const
{
	return (Compare(inStr) > 0);
}

//
//	operator <= -
//
int KString::operator <= (const KString &inStr) const
{
	return (Compare(inStr) <= 0);
}

//
//	operator >= -
//
int KString::operator >= (const KString &inStr) const
{
	return (Compare(inStr) >= 0);
}

//
//	operator == -
//
int KString::operator == (const KString &inStr) const
{
	return (Compare(inStr) == 0);
}

//
//	operator == -
//
int KString::operator == (const char *inStr) const
{
	return (Compare(inStr) == 0);
}

//
//	Equal - Return true if inStr is the same as this. If
//		inCaseSens is true, do a case sensitive comparison,
//		otherwise do a case insensitive comparison.
//
bool KString::Equal(const char *inStr, bool inCaseSens /* = true */)
{
	return (Compare(inStr, inCaseSens) == 0);
}

//
//  operator += - Append a character.
//
KString &KString::operator+=(const char inCh)
{
    m_Length++;
    Resize(m_Length + 1);
    m_String[m_Length - 1] = inCh;
    m_String[m_Length] = 0;

    return (*this);
}

//
//	operator += - Append an KString.
//
KString &KString::operator+=(const KString &inStr)
{
    if (inStr.m_Length) 
    {
        m_Length += inStr.m_Length;
        Resize(m_Length + 1);
        strcat(m_String, inStr.m_String);
    }

    return (*this);
}

//
//	Resize - Change the size of the string. Always change the
//		the size by, at least, MIN_STRING_RESIZE. This helps with
//		character by character appends. 
//
void KString::Resize(uint32 inSize)
{
	char		*newStr = NULL;
	uint32		newSize = 0;

    if (inSize > m_Size) 
    {
		// grow the string
        newSize = Max(inSize, m_Size + MIN_STRING_RESIZE);  
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
void KString::RTrim(void)
{
	int32	pos;
	int32	charsTrimmed = 0;

	if (IsEmpty())
		return;

	pos = m_Length - 1;
	while ((pos >= 0) and (m_String[pos] == ' '))
	{
		m_String[pos] = '\0';
		pos--;
		charsTrimmed++;
	}
	
	m_Length = strlen(m_String);

	if (charsTrimmed >= MAX_STRING_RESIZE_DIFF)
		Resize(m_Length + 1);
}

//
//	LTrim - Trim spaces from the left end.
//
void KString::LTrim(void)
{
	KString	tmpStr;
	int32	charsToTrim = 0;
	uint32	pos = 0;

	if (IsEmpty())
		return;

	while ((pos < m_Length) and (m_String[pos] == ' '))
		charsToTrim++;

	tmpStr = this->Mid(0, charsToTrim);

	*this = tmpStr;
}

void KString::MakeLower()
{
	if (not IsEmpty())
	{
		for (int i = 0; i < m_Length; i++)
			m_String[i] = tolower(m_String[i]);
	}
}

void KString::MakeUpper()
{
	if (not IsEmpty())
	{
		for (int i = 0; i < m_Length; i++)
			m_String[i] = toupper(m_String[i]);
	}
}

KString	KString::GetLower() const
{
	KString		lower = m_String;

	lower.MakeLower();
	return (lower);
}

KString	KString::GetUpper() const
{
	KString		upper = m_String;

	upper.MakeUpper();
	return (upper);
}

//
//  Type - Determine what type of data is in the string.
//
StringType KString::Type()
{
    if (IsDate()) 
    	return (KStringDate);
    else if (IsNumber()) 
    	return (KStringNumber);
    else 
    	return (KStringString);
}


//	
//	Compare - Return the result of a comparison.
//
//  < 0 if m_String < inStr
//    0 if m_String = inStr
//  > 0 if m_String > inStr
//
int KString::Compare(const char *inStr, bool inCaseSens /* = true */) const
{
    if (inCaseSens) 
    	return ::strcmp(m_String, inStr);
    else 
    {
    	KString		srcStr = m_String;
    	KString		compStr = inStr;
    	
    	srcStr.MakeLower();
    	compStr.MakeLower();
    	
    	return (::strcmp(srcStr.GetString(), compStr.GetString()));
    }
}

//
//	TypeCompare -  This is like Compare except the string checks to see if it
//  is a number. If it is, it compares itself to inStr
//  based on that rather than alphabetically.
//
int KString::TypeCompare(KString &inStr, bool inCaseSens /* = false */)
{
    StringType	type1 = Type();
    StringType	type2 = inStr.Type();
    
    if ((type1 == KStringNumber) and (type2 == KStringNumber)) 
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
bool KString::StartsWith(const char *inStr, bool inCaseSens)
{
	KString		tmpStr;
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
bool KString::EndsWith(const char inCh, bool inCaseSens /* = true */)
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
bool KString::Contains(const char *inStr, bool inCaseSens /* = true */)
{
	char	*strPtr = NULL;
	bool	retVal = false;

	if (inStr == NULL)
		return (false);
		
	if (IsEmpty())
		return (false);

	if (not inCaseSens)
	{
		KString		temp(m_String);
		KString		otherTemp(inStr);

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
int32 KString::Find(const char inCh, int32 inStartPos /* = 0 */, bool inCaseSens /* = true */)
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
int32 KString::Find(const char *inStr, int32 inStartPos /* = 0 */, bool inCaseSens /* = true */)
{
	if (inStr == NULL)
		return (-1);
		
	// if this is empty, nothing to look for
	if (IsEmpty())
		return (-1);

	// sanity check on start position
	if ((inStartPos < 0) or (inStartPos >= (int32) m_Length))
		return (-1);

	KString		searchStr = inStr;

	// if the search string is longer than this string we can stop now
	if (m_Length < searchStr.Length())
		return (-1);

	KString		thisStr = m_String;

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
		if (searchStr(searchPos) == '\0')
			return (targetPos);

		// see if we have gone past the last possible end 
		//	position in this string
		if ((thisStr(targetPos2) == '\0') or (targetPos >= endPos))
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
KString::operator int16()
{
    if ((m_Length < 1) or (not IsNumber()))
    	return (0);

    return (atoi(m_String));
}

//
//	operator uint16 -
//
KString::operator uint16()
{
    if ((m_Length < 1) or (not IsNumber()))
    	return (0);

    return (atoi(m_String));
}

//
//	operator int32 -
//
KString::operator int32()
{
    if ((m_Length < 1) or (not IsNumber()) )
    	return (0);

    return (atol(m_String));
}

//
//	operator uint32 -
//
KString::operator uint32()
{
    if ((m_Length < 1) or (not IsNumber()))
    	return (0);

    return (atol(m_String));
}

//
//	operator double - 
//
KString::operator double()
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
KString KString::Mid(uint32 inStart, int32 inLen /* = -1 */)
{
    KString     tmpStr;
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
//	operator [] - Return a character from the array. 
//
char KString::operator [] (uint32 inPos) const
{
    if (inPos >= m_Length) 
    	inPos = m_Length - 1;

    return (m_String[inPos]);
}

//
//	operator () - Same as [].
//
char KString::operator () (uint32 inPos) const
{
	if (inPos >= m_Length)
		inPos = m_Length - 1;

	return (m_String[inPos]);
}

//
//	operator + - Concatenate strings.
//
KString operator + (const KString &inStr1, const KString &inStr2)
{
    KString tmpStr;
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
bool KString::IsNumber()
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
bool KString::IsDate()
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
//	DoubleToString -
//
void KString::DoubleToString(double inNum, char *inStr)
{
	sprintf(inStr, "%f", inNum);
}

//
//	IntToString - 
//
void KString::IntToString(int32 inNum, char *inStr)
{
	sprintf(inStr, "%ld", inNum);
}

//
//	UIntToString - 
//
void KString::UIntToString(uint32 inNum, char *inStr)
{
	sprintf(inStr, "%lu", inNum);
}

//
// stream I/O functions
//
ostream & operator << (ostream &inStream, const KString &inStr)
{
	inStream << inStr.m_String;
	return (inStream);
}

istream & operator >> (istream &inStream, KString &inStr)
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
 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
