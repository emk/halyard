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
// LStream.cpp :An input class for 5L.
//
//

/*
    Need to do:

    Handle ; comments. What if m_String[0] is a comment?

    Handle \x things.

    Feed data to destination since we'll have to skip
        stuff (comments, etc) within the string.
        But all things outside () are ignored anyhow!
        Just force comments to be outside () (but they can be
        inside (Card () ;sssss () ;ssssss) and we're fine.

    If ask for data and there's none then give signal somehow.
*/

#include "stdafx.h"
#include "LStream.h"
#include "LUtil.h"            
#include "Globals.h"

//
//  Some common character types.
//
const char kSPACE = ' ';
const char kNEWLINE = '\n';
const char kTAB = '\t';
const char kRETURN = '\r';
const char P_OPEN = '(';
const char P_CLOSE = ')';
const char SLASH = '\\';
const char COMMENT = '#';

/************************

    PROTECTED METHODS

************************/

//  Is this given character a whitespace character?
//
int LStream::whitespace(char ch)
{
    switch (ch) 
    {
        case kSPACE:
        case kNEWLINE:
        case kTAB:
        case kRETURN:
            return true;
        default:
            return false;
    }
}

/*********************

    PUBLIC METHODS

*********************/

//
//  Constructors.
//
LStream::LStream() : TString()
{
    reset();
}

LStream::LStream(const int newsize) : TString(newsize)
{
    reset();
}

LStream::LStream(const char *s) : TString(s)
{
    reset();
}
    
LStream::LStream(const TString &other) : TString(other)
{
    reset();
}

LStream::LStream(const LStream &other) : TString(other)
{
    pos = other.pos;
}

char LStream::curchar()  
{
    return m_String[pos]; 
}

//
//  Return the next character in the stream. If it is a comment
//  then ignore it and the rest of the line.
//
char LStream::nextchar(void)
{
    char    ch;

    if (eof()) return 0;

    ch = m_String[++pos];
    if (ch == COMMENT) 
    {
        if (m_String[pos - 1] != SLASH) 
        {
            while (!eof() && ch != kNEWLINE)
                ch = m_String[++pos];
            if (ch == kNEWLINE)
                return nextchar();
        }
    }
    return ch;
}

//  Return true if we haven't hit the closing ) or the
//  end of the string yet. Used by parsing functions with
//  optional commands to see if there is more there.
//
int LStream::more()
{
    char    ch;
    
    skipwhite();
    ch = curchar();
    return (!eof() && (ch != P_CLOSE && prevchar() != SLASH));
}

//
//  Reset the stream get pointer to 0. Check now to see if first
//  character is a comment and skip it.
//
void LStream::reset(void)
{
    char    ch;
    pos = 0;

    ch = m_String[pos];
    
    if (!eof() && ch == COMMENT) 
    {
        while (!eof() && ch != kNEWLINE)
            ch = m_String[++pos];
        if (ch == kNEWLINE)
            ch = m_String[++pos];
    }
}


//  Increment the position mark to the next non-whitespace
//  character.
//
void LStream::skipwhite()
{
    char    ch = curchar();

    while (!eof()) 
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
void LStream::scanword()
{
    char    ch = curchar();

    while (!eof()) 
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
void LStream::scanopen()
{
    char    ch = curchar();

    while (!eof()) 
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
void LStream::discard()
{
    TString     junk;

    *this >> junk;
}

//
//  Scan until we find a closing parenthesis. If we find open
//  parentheses then we must find closing parentheses for them
//  too, so keep a counter going. Set the position mark to the
//  first character after the close.
//
void LStream::scanclose()
{
    char    ch = curchar();
    int     dangling_opens = 1;

    while (!eof()) 
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
    gLog.Log("Error: unterminated parentheses, close not found.");
}

//
//  Return the given characters, substituting variable contents
//  where appropriate. Should never have to worry about white space.
//
TString LStream::copystr(unsigned int startPos, unsigned int numChars)
{
    TString		original, result, vname;
    int			base, curpos, origlen;
	bool		deref = false;
    char		ch;
    const char	*s;

    ASSERT(startPos < m_Length);
    ASSERT(startPos + numChars <= m_Length + 1);

    original = Mid(startPos, numChars);
    s = original.GetString(); 
    origlen = original.Length(); 
    if (numChars == 0) 
		result = " ";
    base = curpos = 0;

    if (numChars)  
    {
        if (*s == '&')  
        {
            deref = true;
            curpos++; 
			base++;
        }   
    }

    while (curpos < origlen) 
    {

        //  Do we have the start of a variable name?
        //
        if (s[curpos] == '$' && (curpos == 0 || s[curpos - 1] != SLASH)) 
        {

            //  Copy up until the $ sign.
            //
            result += original.Mid(base, curpos - base);
            base = curpos + 1;
            
            //  Find out how long the name is. Name ends with
            //  whitespace or another $. If we end on a $, set
            //  varadjust to 1. This means we won't include the
            //  final $ in the string we copy to the variable name.
            //
            while (curpos < origlen) 
            {
                ch = s[++curpos];
                if (ch == '$' && s[curpos - 1] != SLASH)
                    break;
            }

            //  Append the variable contents to the result string.
            //
            vname = original.Mid(base, curpos - base);
            
            //@@@9-13-96 added test for null string
            if(gVariableManager.GetString(vname))
            	result += gVariableManager.GetString(vname);
            else
            	gLog.Log("Using uninitialized variable: %s", vname);
            
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
        result += original.Mid(base, curpos - base);

    if (deref)
    {
            if (gVariableManager.GetString(result))
			    result = gVariableManager.GetString(result);
            else
            	gLog.Log("Dereferencing uninitialized variable: %s", result);
 	}    
    
    return result;
}

/***************************

    EXTRACTION OPERATORS

***************************/

//  Basic extraction operator. Most others just use this
//  and then convert the type.
//
LStream& LStream::operator>>(TString &dest)
{
    TString temp;
    int     startPos;
    int     dangling_opens = 0;
    char    ch;

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
    if (ch == P_OPEN && prevchar() != SLASH) 
    {

        startPos = ++pos;
        //skipwhite();
        scanword();
        dest = copystr(startPos, pos - startPos);
        skipwhite();
        dangling_opens = 1;

        while (!eof() && dangling_opens > 0) 
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
                    if (not dest.IsEmpty())
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

    return *this;
}

//  This little guy allows manipulator functions to work.
//
LStream& LStream::operator>>(LStream& (*_f)(LStream &))
{
    return _f(*this);
}

//  TString class handles string to int conversions.
//
LStream& LStream::operator>>(int16 &dest)
{
    TString foo;

    *this >> foo;
    dest = (int16) foo;

    return *this;
}

//  TString class handles string to int conversions.
//
LStream& LStream::operator>>(int32 &dest)
{
    TString foo;

    *this >> foo;
    dest = (int32) foo;

    return *this;
}

//  TString class handles string to int conversions.
//
LStream& LStream::operator>>(double &dest)
{
    TString foo;

    *this >> foo;
    dest = (double)foo;

    return *this;
}

//  Assumes there are 4 numbers to grab.
//  05NOV93 new extraction op added to allow use of var. for four nums
//  New lines added to check for a variable representing four numbers
//  The if and the else statement were added to check for '$'
//  The program now checks for a variable appended to the first variable
//  name i.e. $Dingo%Dog --> $Dingo10 --> 50 50 200 120.  This allows
//  indexing of 4 digit coords.  Note had to use a second symbol other than
//  '$' because of the way 5L handles strings beggining and ending in '$' 

LStream& LStream::operator>>(TRect &r)
{
    char		ch;
    TString		temp;
	int32		left, top, right, bottom;

    skipwhite();
    ch = curchar();
    if ((ch == '&') or (ch == '$')) 
    {
        *this >> temp;

        LStream		tempstream(temp);
        tempstream >> left >> top >> right >> bottom;
    }
    else  
        *this >> left >> top >> right >> bottom;

	r.Set(top, left, bottom, right);

    return (*this);
    
}

//  Assumes there are 2 numbers to grab.
//
//  New lines added to check for a variable representing two numbers
//  The if and the else statement were added to check for '$'.  See
//  previous operator for description of extra code

LStream& LStream::operator>>(TPoint &pt)
{
    char		ch;
    TString		temp;
	int32		x, y;

    skipwhite();
    ch = curchar();
    if((ch == '&') or (ch == '$'))  
    {
        *this >> temp;

        LStream tempstream(temp);
        tempstream >> x >> y;
    }
    else  
        *this >> x >> y;

	pt.Set(x, y);

    return (*this);
}

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
