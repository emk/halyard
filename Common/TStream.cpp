// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
/**************************************

    TStream.cpp

    An input class for 5L.

***************************************/

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

#include "THeader.h"
#include "TCommon.h"
#include "TLogger.h"
#include "TStream.h"
#include "TVariable.h"

#include <ctype.h>
#include <stdio.h>

USING_NAMESPACE_FIVEL

//
//  Some common character types.
//
const char kSPACE = ' ';
const char kNEWLINE = '\n';
const char kRETURN = '\r';
const char kTAB = '\t';
const char P_OPEN = '(';
const char P_CLOSE = ')';
const char SLASH = '\\';
const char COMMENT = '#';       //Changed MAR 31

// Initialize our static variable.
TStream::CallbackMakerFunc TStream::s_CallbackMaker = NULL;


/*********************

    PUBLIC METHODS

*********************/

//
//  Constructors.
//
TStream::TStream() : TString()
{
    reset();
}

TStream::TStream(const int32 newsize) : TString(newsize)
{
    reset();
}

TStream::TStream(const char *s) : TString(s)
{
    reset();
}
    
TStream::TStream(const TString &other) : TString(other)
{
    reset();
}

TStream::TStream(const TStream &other) : TString(other), TArgumentList(other)
{
    pos = other.pos;
}

char TStream::curchar()  
{
    return (m_String[pos]); 
}

//
//  Return the next character in the stream. If it is a comment
//  then ignore it and the rest of the line.
//
char TStream::nextchar(void)
{
    char    ch;

    if (eof()) 
    	return (0);

    ch = m_String[++pos];
    
    if (ch == COMMENT) 
    {
        if (not inEscape()) 
        {
			while (not eof() and (ch != kNEWLINE) and (ch != kRETURN))
                ch = m_String[++pos];
                
            if ((ch == kNEWLINE) or (ch == kRETURN))
                return (nextchar());
        }
    }
    return (ch);
}

//  Return true if we haven't hit the closing ) or the
//  end of the string yet. Used by parsing functions with
//  optional commands to see if there is more there.
//
int TStream::more(void)
{
    char    ch;
    
    skipwhite();
    ch = curchar();
    return ((not eof()) and ((ch != P_CLOSE) and (not inEscape())));
}

//
//  Reset the stream get pointer to 0. Check now to see if first
//  character is a comment and skip it.
//
void TStream::reset(void)
{
    pos = 0;
    char ch = m_String[pos];
    
    if (not eof() and ch == COMMENT) 
    {
        while (not eof() and (ch != kNEWLINE) and (ch != kRETURN))
            ch = m_String[++pos];
            
        if ((ch == kNEWLINE) or (ch == kRETURN))
            ch = m_String[++pos];
    }
}


//  Increment the position mark to the next non-whitespace
//  character.
//
void TStream::skipwhite(void)
{
    char    ch = curchar();

    while (not eof()) 
    {
        if (isspace(ch))
            ch = nextchar();
        else
            return;
    }
}

//  Increment the position mark to the next whitespace
//  character or parenthesis; in other words, one character
//  past the current word.
//
void TStream::scanword(void)
{
    char    ch = curchar();

    while (not eof()) 
    {
        switch (ch) 
        {
			case '$':
				if (inEscape())
					ch = nextchar();
				else
				{
					ch = nextchar();
					if (ch == P_OPEN)
					{
						// emk - A small, gross hack that makes 5L much more
						// powerful.  We allow words of the form:
						// "foo$(+ 2 2)bar", where "(+ ...)" is an arbitrary
						// 5L command.  This is pretty fragile and yucky,
						// but it allows us to substitute command results
						// into strings.
						scanopen();
						scanclose();
						ch = curchar();
					}
				}
				break;

            case P_OPEN:
            case P_CLOSE:
                if (not inEscape())
                    return;
            default:
                if (isspace(ch))
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
void TStream::scanopen(void)
{
    char    ch = curchar();

    while (not eof()) 
    {
        switch (ch) 
        {
            case P_OPEN:
            	if (not inEscape())
                {
                	pos++;
                	return;
                }
            default:
                ch = nextchar();
                break;
        }
    }
}

//  Read a token and discard it. We don't care about it, we just
//  want to get it out of the way.
//
void TStream::discard(void)
{
	(void) GetStringArg();
}

//
//  Scan until we find a closing parenthesis. If we find open
//  parentheses then we must find closing parentheses for them
//  too, so keep a counter going. Set the position mark to the
//  first character after the close.
//
void TStream::scanclose(void)
{
    char    ch = curchar();
    int32	dangling_opens = 1;

    while (not eof()) 
    {
        switch (ch) 
        {
            case P_OPEN:
                if (not inEscape())
                	dangling_opens++;
                ch = nextchar();
                break;

            case P_CLOSE:
                if (not inEscape())
                {
                	dangling_opens--;
                	if (dangling_opens < 1) 
                	{
                    	pos++;
                    	return;
                	}               // else drop into default...
				}
            default:
                ch = nextchar();
                break;
        }
    }

    //  Error: unterminated parentheses.
    //
    gLog.Error("unterminated parentheses, close not found.");
    gDebugLog.Error("unterminated parentheses, close not found.");
}

//
//  Return the given characters, substituting variable contents
//  where appropriate.
//
TString TStream::ExpandVariables(uint32 startPos, uint32 numChars)
{
    TString 	original, result, vname;
    int32     	base, curpos, origlen, DEREF = 0;
    char    	ch;
    const char	*s;

    ASSERT(startPos < m_Length);
    ASSERT(startPos + numChars <= m_Length + 1);

	original = Mid(startPos, numChars);
    s = original.GetString(); 
    origlen = original.Length(); 
    
    if (numChars == 0) 
    	result = "";
    base = curpos = 0;
    
    while (curpos < origlen) 
    {
        //  Do we have the start of a variable name?
        //
        if (s[curpos] == '$' && (curpos == 0 || !inEscape(startPos + curpos)))
        {
			//  Copy up until the $ sign.
			//
			result += original.Mid(base, curpos - base);
			base = curpos + 1;

			if (curpos + 1 < origlen && s[curpos + 1] == P_OPEN)
			{
				// COMMAND OUTPUT SUBSTITUTION
				//
				// This is a horrible kludge--see scanword for an
				// explanation--but it allows us to write "foo$(+ 2 2)bar"
				// and get the string "foo4bar".

				++curpos; // Skip over '$'.

				// Find the end of the command, using a little local copy
				// of scanclose (we're not allowed to call curchar or
				// nextchar, so we can't use the real scanclose).
				int cmd_begin = curpos;
				int nesting_levels = 0;
				for (bool done = false; !done; )
				{
					ASSERT(curpos < origlen);
					switch (s[curpos++])
					{
						case SLASH:
							// Skip the next character, too.
							++curpos;
							break;

						case P_OPEN:
							++nesting_levels;
							break;

						case P_CLOSE:
							--nesting_levels;
							if (nesting_levels == 0)
								done = true;
							break;

						default:
							break;
					}
				}
				int cmd_end = curpos;
				
				// Extract the text of our command.
				TString cmd_text =
					original.Mid(cmd_begin, cmd_end - cmd_begin);

				// Build and run a callback.
				TCallback *callback = MakeCallback(cmd_text);
				callback->Run();
				delete callback;

				// Insert the output of the command into our string.
				result += gVariableManager.GetString("_result");

				// Bump up the base.
				base = curpos;
			}
			else
			{
				//  REGULAR VARIABLE SUBSTITUTION
				//				
				//  Find out how long the name is. Name ends with
				//  whitespace or another $.  Set need_bump to true
				//  if and only if we should skip a trailling $ after
				//  copying the string.
				//
				bool need_bump = false;
				while (curpos < origlen) 
				{
					ch = s[++curpos];
					if (isspace(ch))
						break;
					if (ch == '$' && !inEscape(startPos + curpos))
					{
						need_bump = true;
						break;
					}
					if ((ch == P_OPEN || ch == P_CLOSE) &&
						!inEscape(startPos + curpos))
						break;
				}
				
				//  Append the variable contents to the result string.
				//
				vname = original.Mid(base, curpos - base);
				result += gVariableManager.GetString(vname);
				
				// Skip any trailing $.
				if (need_bump)
					++curpos;

				//  Bump up the base.
				//
				base = curpos;
			}
        } 
        else 
        {
            curpos++;
        }
    }

    if (base < origlen)
        result += original.Mid(base, curpos - base);

    return (result);
}

/***************************

    EXTRACTION OPERATORS

***************************/

//  This little guy allows manipulator functions to work.
//
TStream& TStream::operator>>(TStream& (*_f)(TStream &))
{
    return (_f (*this));
}

std::string TStream::GetStatement()
{
	return GetNextArg(true);
}

void TStream::GetBody(std::list<std::string> &outBody)
{
	outBody.clear();
	while (more())
	{
		std::string statement = GetStatement();
		outBody.push_back(statement);
	}
}


//  Basic extraction operator. Most others just use this
//  and then convert the type.
//
std::string TStream::GetNextArg(bool inWantRawSource)
{
	TString		dest;
    TString 	temp;
    int32     	startPos, endPos;
    int32     	dangling_opens = 0;
    char    	ch;
	
    skipwhite();
    if (eof()) 
		return "";
    
    ch = curchar();
	startPos = pos;
    
    // If the first character is an open paren then return the
    // entire contents of this set of parentheses. Otherwise go
    // until we hit whitespace.
    if ((ch == P_OPEN) and (not inEscape())) 
    {
		// We want everything up to the matching P_CLOSE character.
        ++pos;
        scanclose();
		endPos = pos;

		// Unless we're extracting the raw statement, lop off the parens.
		if (!inWantRawSource)
		{
			++startPos;
			--endPos;
		}
    } 
    else 
    {
		startPos = pos;
        scanword();
		endPos = pos;
    }
    
	if (inWantRawSource)
		dest = TString(&m_String[startPos], endPos - startPos);
	else
		dest = ExpandVariables(startPos, endPos - startPos);

    return std::string(dest.GetString());
}

std::string TStream::GetStringArg()
{
	return GetNextArg(false);
}

//  TString class handles string to int conversions.
//
int32 TStream::GetInt32Arg()
{
    TString foo = GetStringArg().c_str();
    return (int32) foo;
}

uint32 TStream::GetUInt32Arg()
{
    TString foo = GetStringArg().c_str();
	return (uint32) foo;
}

bool TStream::GetBoolArg()
{
	return GetInt32Arg() ? true : false;
}

//  TString class handles string to double conversions.
//
double TStream::GetDoubleArg()
{
    TString foo = GetStringArg().c_str();
	return (double) foo;
}

//  Assumes there are 4 numbers to grab.
//  05NOV93 new extraction op added to allow use of var. for four nums
//  New lines added to check for a variable representing four numbers
//  The if and the else statement were added to check for '$'
//  The program now checks for a variable appended to the first variable
//  name i.e. $Dingo%Dog --> $Dingo10 --> 50 50 200 120.  This allows
//  indexing of 4 digit coords.  Note had to use a second symbol other than
//  '$' because of the way 5L handles strings beginning and ending in '$' 

TRect TStream::GetRectArg()
{
	TRect	r;
    char	ch;
    int32	left, top, right, bottom;

    skipwhite();
    ch = curchar();
    if ((ch == '&') or (ch == '$'))  
    {
        TString temp = GetStringArg().c_str();
        TStream tempstream(temp);
        tempstream >> left >> top >> right >> bottom;
    }
    else
	{
		left = GetInt32Arg();
		top = GetInt32Arg();
		right = GetInt32Arg();
		bottom = GetInt32Arg();
	}
  
  	r.Set(top, left, bottom, right);
	return r;
}


//  Assumes there are 2 numbers to grab.
//
//  New lines added to check for a variable representing two numbers
//  The if and the else statement were added to check for '$'.  See
//  previous operator for description of extra code

TPoint TStream::GetPointArg()
{
	TPoint	pt;
    char	ch;
    TString	temp;
    int32	x, y;

    skipwhite();
    ch = curchar();
    if ((ch == '&') or (ch == '$'))  
    {
        TString temp = GetStringArg().c_str();
        TStream tempstream(temp);
        tempstream >> x >> y;
    }
    else
	{
        x = GetInt32Arg();
		y = GetInt32Arg();
	}
    
    pt.Set(x, y);
	return pt;
}

GraphicsTools::Color TStream::GetColorArg()
{
	std::string input = GetStringArg();
    const char *temp = input.c_str();

	// Make sure the value is of the form '0xRRGGBBAA', where RR, etc.,
	// are hexadecimal digits.
	bool ok = true;
	if (temp[0] != '0' || temp[1] != 'x' || strlen(temp) != 10)
		ok = false;
	else
	{
		// Make sure all the digits are hexadecimal.
		for (int i = 2; i < 10; ++i)
			if (!isxdigit(temp[i]))
				ok = false;
	}
	if (!ok)
		gLog.FatalError("The color \"%s\" does not have the format "
						"\"0xRRGGBBAA\"", temp);

	// Extract the actual data, and build our color.
	unsigned int hex;
	int successful_conversions = sscanf(temp + 2, "%x", &hex);
	ASSERT(successful_conversions == 1);
	return GraphicsTools::Color(hex >> 24,
								(hex >> 16) & 0xFF,
								(hex >> 8) & 0xFF,
								hex & 0xFF);
}

TCallback *TStream::GetCallbackArg()
{
	// Get our code, and put back the parens removed by the parser.
	// XXX - We should really use GetStatement here, but that would delay
	// variable evaluation until the callback is run, which would break
	// legacy scripts quite badly.
	TString code = GetStringArg().c_str();
	code = TString("(") + code + TString(")");

	// Create and return a callback object.
	return MakeCallback(code);
}

TArgumentList *TStream::GetListArg()
{
	// Extract our next string, and build a new parser for it.
	TString list = GetStringArg().c_str();
	return new TStream(list);
}

//  Tests to see if a character is escaped, given position of character
// 
//  Goes back along stream counting slashes until we either get a char
//  that's not a slash or we reach the beginning.
//  If the number of slashes is even, we are not in escape and return false
//  If the number of slashes is odd, we are in escape and return true
bool TStream::inEscape(int32 position)
{
	uint16 slashes = 0;
	
	while ((position > 0) && (m_String[--position] == SLASH))
		slashes++;
	
	if ((slashes % 2) == 0)
		return false;
	else
		return true;
}

// Tests to see if curr character is escaped Gets current character
// position and passes it to inEscape(int32 position) function
bool TStream::inEscape(void)
{
	return inEscape(GetPos());
}

// An I/O manipulator support function.  See the header for interface
// details, or a good iostream tutorial for more information on
// how manipulator objects work.
#define PERCENT_COMMAND_PREFIX ("pcent ")
TStream &FIVEL_NS operator>>(TStream &inStream, const ValueOrPercent &inVoP)
{
	// Read in our argument.
	std::string argument;
	inStream >> argument;
	TStream arg_stream(argument.c_str());

	// See if it appears to be a "(pcent ...)" command.
	size_t prefix_len = strlen(PERCENT_COMMAND_PREFIX);
	if (argument.length() > prefix_len &&
		argument.substr(0, prefix_len) == PERCENT_COMMAND_PREFIX)
	{
		// Read a percentage command.
		int32 percentage;
		arg_stream >> discard >> percentage;

		// Calculate the value and round it.
		double value = (inVoP.mBaseValue * percentage) / 100.0;
		if (value < 0)
			value -= 0.5;
		else
			value += 0.5;
		*inVoP.mOutputValue = static_cast<int>(value);
	}
	else
	{
		// Read a simple value.
		arg_stream >> *inVoP.mOutputValue;
	}

	return inStream;
}
