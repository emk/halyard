/**************************************

    CStream.cpp

    An input class for 5L.

***************************************/

/*
    Need to do:

    Handle ; comments. What if ptext[0] is a comment?

    Handle \x things.

    Feed data to destination since we'll have to skip
        stuff (comments, etc) within the string.
        But all things outside () are ignored anyhow!
        Just force comments to be outside () (but they can be
        inside (Card () ;sssss () ;ssssss) and we're fine.

    If ask for data and there's none then give signal somehow.
*/

#include "debug.h"

#include "Mac5L.h"

#include "CStream.h"
#include "CVariable.h"

#include "util.h"


//
//  Some common character types.
//
const char kSPACE = ' ';
const char kNEWLINE = NEWLINE_CHAR;
const char kTAB = '\t';
const char kRETURN = RETURN_CHAR;
const char P_OPEN = '(';
const char P_CLOSE = ')';
const char SLASH = '\\';
const char COMMENT = '#';       //Changed MAR 31

/************************

    PROTECTED METHODS

************************/

//  Is this given character a whitespace character?
//
bool CStream::whitespace(char ch)
{
    switch (ch) 
    {
        case kSPACE:
        case kNEWLINE:
        case kTAB:
        case kRETURN:
            return (TRUE);
        default:
            return (FALSE);
    }
}

/*********************

    PUBLIC METHODS

*********************/

//
//  Constructors.
//
CStream::CStream() : CString()
{
    reset();
}

CStream::CStream(const int32 newsize) : CString(newsize)
{
    reset();
}

CStream::CStream(const char *s) : CString(s)
{
    reset();
}
    
CStream::CStream(const CString &other) : CString(other)
{
    reset();
}

CStream::CStream(const CStream &other) : CString(other)
{
    pos = other.pos;
}

char CStream::curchar()  
{
    return (ptext[pos]); 
}

//
//  Return the next character in the stream. If it is a comment
//  then ignore it and the rest of the line.
//
char CStream::nextchar(void)
{
    char    ch;

    if (eof()) 
    	return (0);

    ch = ptext[++pos];
    
    if (ch == COMMENT) 
    {
        if (ptext[pos - 1] != SLASH) 
        {
			while (not eof() and (ch != kNEWLINE) and (ch != kRETURN))
                ch = ptext[++pos];
                
            if ((ch == kNEWLINE) or (ch == kRETURN))
                return (nextchar());
        }
    }
    return (ch);
}

//  Return TRUE if we haven't hit the closing ) or the
//  end of the string yet. Used by parsing functions with
//  optional commands to see if there is more there.
//
int CStream::more(void)
{
    char    ch;
    
    skipwhite();
    ch = curchar();
    return ((not eof()) and ((ch != P_CLOSE) and (prevchar() != SLASH)));
}

//
//  Reset the stream get pointer to 0. Check now to see if first
//  character is a comment and skip it.
//
void CStream::reset(void)
{
    char    ch;
    pos = 0;

    ch = ptext[pos];
    
    if (not eof() and ch == COMMENT) 
    {
        while (not eof() and (ch != kNEWLINE) and (ch != kRETURN))
            ch = ptext[++pos];
            
        if ((ch == kNEWLINE) or (ch == kRETURN))
            ch = ptext[++pos];
    }
}


//  Increment the position mark to the next non-whitespace
//  character.
//
void CStream::skipwhite(void)
{
    char    ch = curchar();

    while (not eof()) 
    {
        if (whitespace(ch))
            ch = nextchar();
        else
            return;
    }
}

//  Increment the position mark to the next whitespace
//  character or parenthesis; in other words, one character
//  past the current word.
//
void CStream::scanword(void)
{
    char    ch = curchar();

    while (not eof()) 
    {
        switch (ch) 
        {
            case P_OPEN:
            case P_CLOSE:
                if (prevchar() != SLASH)
                    return;
            default:
                if (whitespace(ch))
                    return;
                else 
                	ch = nextchar();
                break;
        }
    }
}

//
//  Scan until we hit an open parenthesis, and then set the
//  position mark to the first character after the parenthesis.
//  Since we ignore everything prior to the open parenthesis
//  comments can be added to the 5L source anywhere as long as
//  they are not within commands. (They may be within CARD and
//  MACRODEF commands, however.)
//
void CStream::scanopen(void)
{
    char    ch = curchar();

    while (not eof()) 
    {
        switch (ch) 
        {
            case P_OPEN:
                pos++;
                return;
            default:
                ch = nextchar();
                break;
        }
    }
}

//  Read a token and discard it. We don't care about it, we just
//  want to get it out of the way.
//
void CStream::discard(void)
{
    CString     junk;

    *this >> junk;
}

//
//  Scan until we find a closing parenthesis. If we find open
//  parentheses then we must find closing parentheses for them
//  too, so keep a counter going. Set the position mark to the
//  first character after the close.
//
void CStream::scanclose(void)
{
    char    ch = curchar();
    int32	dangling_opens = 1;

    while (not eof()) 
    {
        switch (ch) 
        {
            case P_OPEN:
                dangling_opens++;
                ch = nextchar();
                break;

            case P_CLOSE:
                dangling_opens--;
                if (dangling_opens < 1) 
                {
                    pos++;
                    return;
                }               // else drop into default...

            default:
                ch = nextchar();
                break;
        }
    }

    //  Error: unterminated parentheses.
    //
    prerror("unterminated parentheses, close not found.");
}

//
//  Return the given characters, substituting variable contents
//  where appropriate. Should never have to worry about white space.
//
CString CStream::copystr(uint32 startPos, uint32 numChars)
{
    CString 	original, result, vname;
    int32     	base, curpos, origlen, DEREF = 0;
    char    	ch;
    char		*s;

    ASSERT(startPos < len)
    ASSERT(startPos + numChars <= len + 1)

    original = substr(startPos, numChars);
    s = original.GetString(); 
    origlen = original.length(); 
    
    if (numChars == 0) 
    	result = " ";
    base = curpos = 0;
    
    if (numChars)  
    {
        if (*s == '&')  
        {
            DEREF = 1;
            curpos++; 
            base++;
        }   
    }
    
    while (curpos < origlen) 
    {
        //  Do we have the start of a variable name?
        //
        if ((s[curpos] == '$') and ((curpos == 0) or (s[curpos - 1] != SLASH))) 
        {
            //  Copy up until the $ sign.
            //
            result += original.substr(base, curpos - base);
            base = curpos + 1;
            
            //  Find out how long the name is. Name ends with
            //  whitespace or another $. If we end on a $, set
            //  varadjust to 1. This means we won't include the
            //  final $ in the string we copy to the variable name.
            //
            while (curpos < origlen) 
            {
                ch = s[++curpos];
                if ((ch == '$') and (s[curpos - 1] != SLASH))
                    break;
            }

            //  Append the variable contents to the result string.
            //
            vname = original.substr(base, curpos - base);
            result += gVariableManager.GetString(vname);
            
            //  Bump up the base.
            //
            base = ++curpos;
        } 
        else 
        {
            curpos++;
        }
    }

    if (base < origlen)
        result += original.substr(base, curpos - base);

    if (DEREF) 
		result = gVariableManager.GetString(result);
		
    return (result);
}

/***************************

    EXTRACTION OPERATORS

***************************/

//  Basic extraction operator. Most others just use this
//  and then convert the type.
//
CStream& CStream::operator>>(CString &dest)
{
    CString 	temp;
    int32     	startPos;
    int32     	dangling_opens = 0;
    char    	ch;

    skipwhite();
    if (eof()) 
    {
        dest = "";
        return *this;
    }
    
    ch = curchar();
    startPos = pos;
    
    //  If the first character is an open paren then return the
    //  entire contents of this set of parentheses. Otherwise go
    //  until we hit whitespace.
    //
    if ((ch == P_OPEN) and (prevchar() != SLASH)) 
    {
        startPos = ++pos;
        //skipwhite();
        scanword();
        dest = copystr(startPos, pos - startPos);
        skipwhite();
        dangling_opens = 1;

        while (not eof() and dangling_opens > 0) 
        {
            //  This is the first character of the word so
            //  parens will always be parens, never \( or \).
            //
            ch = curchar();
            switch (ch) 
            {
                case P_OPEN:
                    dangling_opens++;
                    pos++;
                    dest += P_OPEN;
                    break;

                case P_CLOSE:
                    dangling_opens--;
                    pos++;
                    if (dangling_opens > 0)
                        dest += P_CLOSE;
                    break;

                default:
                    startPos = pos;
                    scanword();
                    if (not dest.empty())
                        dest += kSPACE;
                    temp = copystr(startPos, pos - startPos);
                    dest += temp;
                    skipwhite();
                    break;
            }

        }

    } 
    else 
    {
        scanword();
        dest = copystr(startPos, pos - startPos);
    }

    return (*this);
}

//  This little guy allows manipulator functions to work.
//
CStream& CStream::operator>>(CStream& (*_f)(CStream &))
{
    return (_f (*this));
}

//  CString class handles string to int conversions.
//
CStream& CStream::operator>>(int16 &dest)
{
    CString foo;

    *this >> foo;
    dest = (int16) foo;

    return (*this);
}

//  CString class handles string to int conversions.
//
CStream& CStream::operator>>(int32 &dest)
{
    CString foo;

    *this >> foo;
    dest = (int32) foo;

    return (*this);
}

CStream& CStream::operator>>(uint32 &dest)
{
	CString foo;
	
	*this >> foo;
	dest = (uint32) foo;
	
	return (*this);
}

//  CString class handles string to int conversions.
//
CStream& CStream::operator>>(double &dest)
{
    CString foo;

    *this >> foo;
    dest = (double) foo;

    return (*this);
}

//  Assumes there are 4 numbers to grab.
//  05NOV93 new extraction op added to allow use of var. for four nums
//  New lines added to check for a variable representing four numbers
//  The if and the else statement were added to check for '$'
//  The program now checks for a variable appended to the first variable
//  name i.e. $Dingo%Dog --> $Dingo10 --> 50 50 200 120.  This allows
//  indexing of 4 digit coords.  Note had to use a second symbol other than
//  '$' because of the way 5L handles strings beginning and ending in '$' 

CStream& CStream::operator>>(CRect &r)
{
    char	ch;
    CString	temp;

    skipwhite();
    ch = curchar();
    if ((ch == '&') or (ch == '$'))  
    {
        *this >> temp;
        CStream tempstream(temp);
        tempstream >> r.left >> r.top >> r.right >> r.bottom;
    }
    else  
        *this >> r.left >> r.top >> r.right >> r.bottom;
    
    return (*this);
}




//  Assumes there are 2 numbers to grab.
//
//  New lines added to check for a variable representing two numbers
//  The if and the else statement were added to check for '$'.  See
//  previous operator for description of extra code

CStream& CStream::operator>>(CPoint &pt)
{
    char	ch;
    CString	temp;

    skipwhite();
    ch = curchar();
    if ((ch == '&') or (ch == '$'))  
    {
        *this >> temp;
        CStream tempstream(temp);
        tempstream >> pt.x >> pt.y;
    }
    else  
        *this >> pt.x >> pt.y;
    
    return (*this);
}
