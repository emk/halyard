/*******************************

    String class for 5L.

    This class is designed to
    be portable.

*******************************/

#include "debug.h"

#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>

#include "CString.h"
#include "util.h"


//  Maximum string length. Somewhat arbitrary. Can strings be longer
//  than a segment? Parsing large strings (in the form of variables)
//  is very slow.

#define kMaxStringLength    16000
#define kMinResize          4

//  Some string operations require a good size buffer. Declare it here
//  so we don't hammer the stack. The size of this limits how large
//  a CString may be extracted from an istream.

static char     buffer[512];

/***********************

    STATIC FUNCTIONS

***********************/


/*******************

    CONSTRUCTION

*******************/

//  Default constructor.
//
CString::CString() : CObject()
{
    len = 0;
    arraySize = 0;
    ptext = 0;
    resize(1);
}

//  Construct with a fixed array size.
//
CString::CString(const uint32 newlen) : CObject()
{
    len = 0;
    arraySize = 0;
    ptext = 0;
    resize(newlen);
}

//  Construct from a string.
//
CString::CString(const char *s) : CObject()
{
    len = 0;
    arraySize = 0;
    ptext = 0;
    
    if (s != NULL) 
    {
        len = strlen(s);
        resize(len + 1);
        strcpy(ptext, s);
    }
}

//  Construct from a CString.
//
CString::CString(const CString &other) : CObject()
{
    len = 0;
    arraySize = 0;
    ptext = 0;
    
    if (other.len > 0) 
    {
        len = other.len;
        resize(len + 1);
        strcpy(ptext, other.ptext);
    }
}

/*****************

    ASSIGNMENT

*****************/

//  Set to another string.
//
CString &CString::operator=(const char *s)
{
    delete ptext;
    ptext = 0;
    arraySize = 0;
    len = 0;
    if (s != NULL) 
    {
        len = strlen(s);
        resize(len + 1);
        strcpy(ptext, s);
    }
    return (*this);
}

//  Set to another CString.
//
CString &CString::operator=(const CString &other)
{
    if (&other == this) return *this;

    delete ptext;
    ptext = 0;
    arraySize = 0;
    len = 0;
    if (other.len) 
    {
        len = other.len;
        resize(len + 1);
        strcpy(ptext, other.ptext);
    }
    return (*this);
}

//  Set to a long.
//
CString &CString::operator=(const int32 lnum)
{
	char	theStr[32];
	
	sprintf(theStr, "%ld", lnum);
	*this = theStr;
	
  //  *this = (double)lnum;
    return (*this);
}

//	 Set to an unsigned long.
CString &CString::operator=(const uint32 lnum)
{
	char	theStr[32];
	
	sprintf(theStr, "%lu", lnum);
	*this = theStr;
	
	return (*this);
}

//  Set to an int.
//
CString &CString::operator=(const int16 num)
{
	char	theStr[32];
	
	sprintf(theStr, "%d", num);
	*this = theStr;
	
//    *this = (double)num;
    return (*this);
}

CString &CString::operator=(const uint16 num)
{
	char	theStr[32];
	
	sprintf(theStr, "%u", num);
	*this = theStr;
	
	return (*this);
}

//  Set to a double.
//
CString &CString::operator=(const double dnum)
{
    char    theStr[64]; //  Twice as big as necessary, I think.

	sprintf(theStr, "%f", dnum);
    *this = theStr;

    return (*this);
}

//  Append a string.
//
CString &CString::operator+=(const char *s)
{
    if (s != NULL) 
    {
        len += strlen(s);
        resize(len + 1);
        strcat(ptext, s);
    }
    return (*this);
}


//  Append a character string of numbers that were entered as a long int
//New SEP22
//..........................STRNGTST.CPP..........................
// This program is written to test a conversion routine
// for converting longs to strings
CString &CString::operator+=(const int32 lnum)
{
    char	TheNumber[20], TempChar;
    int32	i = 0, denominator = 10000, count = 0;
    int32	k = lnum, tmp;
    
    tmp = k/denominator;

    while((tmp == 0) and (i < 4))
    {
        k -= (tmp * denominator);
        denominator /= 10;
        i++;
        tmp = k/denominator;

    }

    while(i < 5 )
    {
        TempChar = (char)(48 + tmp);
        TheNumber[count] = TempChar;
        i++;
        count++;
        
        if(denominator >= 10)
        {
            k -= (tmp * denominator);
            denominator /= 10;
            tmp = k/denominator;
        }
        
    }
    TheNumber[count] = (char)0;

//  cout << "The number is " << TheNumber << "\n";

    if (TheNumber) 
    {
        len += strlen(TheNumber);
        resize(len + 1);
        strcat(ptext, TheNumber);
    }
    return (*this);
}



//  Append a character.
//
CString &CString::operator+=(const char ch)
{
    len++;
    resize(len + 1);
    ptext[len - 1] = ch;
    ptext[len] = 0;
    return (*this);
}

//  Append a CString.
//
CString &CString::operator+=(const CString &other)
{
    if (other.len) 
    {
        len += other.len;
        resize(len + 1);
        strcat(ptext, other.ptext);
    }
    return *this;
}

//  If the size of the string ever needs to be changed, this
//  routine does it. Always resize by a certain amount. That is,
//  is the new size is 1 more than the old, resize by more so that
//  the next resize may not be necessary. (Great for character by
//  character appends.)
//
void CString::resize(uint32 newSize)
{
    //  There is a maximal string size.
    //
    if (newSize > kMaxStringLength)
    {
        prerror("Maximum string length is %u", kMaxStringLength);
        return;
    }

    //  Only do something if the current array is too small.
    //
    if (newSize > arraySize) 
    {
        arraySize = Max(newSize, arraySize + kMinResize);
        char *newp = new char[arraySize];
        if (!newp)
        { 
            prerror("Null pointer on resize!!");
            return;
        }
        newp[0] = 0;
        if (ptext != 0) 
        {
            strcpy(newp, ptext);
            delete ptext;
        }
        ptext = newp;
    }
}

/*****************

    COMPARISON

*****************/

//  Determine what type of data is in the string.
//
DataType CString::Type()
{
    if (IsDate()) 
    	return isDate;
    else if (IsNumber()) 
    	return isNumber;
    else return 
    	isString;
}

//  Check to see if a string equals something.
//
int CString::Equal(const CString &other, bool caseSens)
{
    return (Compare(other.ptext, caseSens) == 0);
}

//  As above, but for char *'s.
//
int CString::Equal(const char *other, bool caseSens)
{
    return (Compare(other, caseSens) == 0);
}

//  Return the result of a comparison. If you need to know
//  whether something is >, < or ==, this is faster because
//  you only have to compare strings once.
//
//  <0 if ptext < s
//   0 if ptext = s
//  >0 if ptext > s
//
int CString::Compare(const CString &other, bool caseSens)
{
    return Compare(other.ptext, caseSens);
}

//  Low level compare.
//
int CString::Compare(const char *other, bool caseSens)
{
    if (caseSens) 
		return strcmp(ptext, other);
    else 
		return stricmp(ptext, other);
}

//  This is like Compare except the string checks to see if it
//  is a date or a number. If it is, it compares itself to other
//  based on that rather than alphabetically.
//
int CString::TypeCompare(CString &other, bool caseSens)
{
    DataType    type1 = Type();
    DataType    type2 = other.Type();
    double      d1, d2;

	if (type1 == isNumber and type2 == isNumber) 
	{
		d1 = *this;
		d2 = other;
		
		if (d1 > d2) 
			return 1;
		else if (d1 < d2) 
			return -1;
		else 
			return 0;
    } 
    else 
    	return Compare(other, caseSens);
}

int CString::StartsWith(const CString &other, bool caseSens)
{
    return StartsWith(other.ptext, caseSens);
}

//  Return TRUE if the other string matches the beginning of this
//  string. Shorten the string and test for equality.
//
int CString::StartsWith(const char *other, bool caseSens)
{
    CString     temp;
    unsigned int slen = strlen(other);

    if (slen > len) return FALSE;

    temp = substr(0, slen);
    return temp.Equal(other, caseSens);
}

//  Decode the string based on the cipher key.
//  
void CString::Decrypt(const char cipher)
{
    unsigned int i;

    for (i = 0; i < len; i++)
        ptext[i] = (char)(ptext[i] ^ cipher);
}

/******************

    CONVERSIONS

******************/

//  Empty strings are equal to 0 numerically.
//
CString::operator int16()
{
    if (len < 1) 
    	return (0);

    if (not IsNumber())
    {
        prcaution("%s is not a number!", ptext);
        return (0);
    }

    return (atoi(ptext));
}

//  Empty strings are equal to 0 numerically.
//
CString::operator uint16()
{
    if (len < 1) 
    	return (0);

    if (not IsNumber())
    {
        prcaution("%s is not a number!", ptext);
        return (0);
    }

    return (atoi(ptext));
}

//
//
CString::operator int32()
{
    if (len < 1) 
    	return (0);

    if (not IsNumber())
    {
        prerror("%s is not a number!", ptext);
        return (0);
    }

    return (atol(ptext));
}

CString::operator uint32()
{
    if (len < 1) 
    	return (0);

    if (not IsNumber())
    {
        prerror("%s is not a number!", ptext);
        return (0);
    }

    return (atol(ptext));
}

//
//
CString::operator double()
{
    if (len < 1) 
    	return (0.0);

    if (not IsNumber())
    {
        prerror("%s is not a number!", ptext);
        return (0.0);
    }

    return (atof(ptext));
}


//  Return a portion of the string starting at subpos
//  and running for sublen characters.
//
CString CString::substr(uint32 subpos, uint32 sublen)
{
    CString     temp;

    ASSERT(subpos <= len)
    ASSERT(subpos + sublen <= len + 1)

    temp.resize(sublen + 1);
    if (sublen > 0)
        strncpy(temp.ptext, ptext + subpos, sublen);
    temp.ptext[sublen] = 0;
    temp.update();
    
    return (temp);
}

//  Return a character from the array. X runs from 0..len-1.
//
char CString::getch(uint32 x) const
{
    if (x >= len) 
    	x = len - 1;

    return (ptext[x]);
}

/***************

    DISPOSAL

***************/

CString::~CString()
{
// this makes the program CRAWL!!
//#ifdef DEBUG_5L
//	prinfo("Deleting a CString object");
///#endif

    delete ptext;   //  Works even if p = 0.
}

/***********************

    FRIEND FUNCTIONS

***********************/

//  Adding strings concatenates them.
//
CString operator + (const CString &s1, const char *s2)
{
    CString temp;
    int     newlen = s1.len + strlen(s2);

    temp.resize(newlen + 1);
    temp.len = newlen;
    strcpy(temp.ptext, s1.ptext);
    strcat(temp.ptext, s2);
    return temp;
}

//  Case sensitive comparison.
//
int operator== (const CString &s1, const char* s2)
{
    return (strcmp(s1.ptext, s2) == 0);
}

//  Allow conversion from streams. Input is limited based
//  on the size of the array. Is there a way to limit how
//  much the istream gives us?
//
//istream& operator>>(istream &source, CString &dest)
CTextFileStream& operator>>(CTextFileStream &source, CString &dest)
{
    source >> buffer;
    dest = buffer;
    return source;
}

//  Feed to ostream as a char *.
//
//ostream& operator<<(ostream &dest, CString &source)
CTextFileStream& operator<<(CTextFileStream &dest, CString &source)
{
    dest << source.GetString();
    return dest;
}

/**********************

    PRIVATE METHODS

**********************/

enum {
    vSTART,
    vNEGATIVE,
    vDECIMAL
};

//  Does the string qualify for what we consider a number?
//
int CString::IsNumber()
{
    int     state = vSTART;
    char    ch;
    char    *s = ptext;

    //  NULL Strings aren't numbers per se (in terms of their type) but
    //  they should evaluate to 0. In order to do this, this routine
    //  returns false, but the actual conversion methods will return 0.
    //
    if (ptext == NULL) 
    	return FALSE;

    //
    //  atof() can't convert strings longer than 100 chars, so
    //  if that's the case it's not a number
    //
    if (len > 100)
        return FALSE;
 
    while ((ch = *s++) > 0)
    {
        if (ch != ' ')
		{
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
                    return (FALSE);
                    
                //  We've had the decimal point, so look for digits only.
                case vDECIMAL:
                    if (isdigit(ch))
                        break;

                    return (FALSE);

                //  Should never get here.
				default:
                    ASSERT(FALSE)
                    break;
			}
		}
	}
    return (TRUE);
}

//  Does the string qualify as what we consider a date?
//
int CString::IsDate()
{
    char    ch;
    char    *s = ptext;

    //  Month number. 1 or 2 digits.
    //
    ch = *s++;
    if (!isdigit(ch)) return FALSE;

    ch = *s++;
    if (isdigit(ch)) ch = *s++;

    //  First slash.
    //
    if (ch != '/') return FALSE;

    //  Day number. 1 or 2 digits.
    //
    ch = *s++;
    if (!isdigit(ch)) return FALSE;

    ch = *s++;
    if (isdigit(ch)) ch = *s++;

    //  Second slash.
    //
    if (ch != '/') return FALSE;

    //  Year. 2 digits.
    //
    ch = *s++;
    if (!isdigit(ch)) return FALSE;

    ch = *s++;
    if (!isdigit(ch)) return FALSE;

    return (*s == 0);
}

