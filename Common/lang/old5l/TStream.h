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
#include <list>

#include "TString.h"
#include "TRect.h"
#include "TPoint.h"
#include "GraphicsTools.h"
#include "TPrimitives.h"

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
class TStream : public TString, public TArgumentList
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
	// Is there more data for this field?  (We inherit this method
	// from TArgumentList.)
	//
	bool    HasMoreArguments()
		{ return more() ? true : false; }
        
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
	// appropriate.
	//
	// [in] startPos - starting postion
	// [in] numChars - number of characters
	// [out] return - TString with variable substitutions
	//
	TString ExpandVariables(uint32 startPos, uint32 numChars);
	
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
	// This allows manipulator functions to work.
	//
	TStream&    operator>>(TStream& (*_f)(TStream &));

	//////////
	// Extract a single, unevaluated statement from the stream.
	//
	// [out] return - A single unevaluated statement.
	//
	std::string GetStatement();

	//////////
	// Extract a list of statements from the stream (until we encounter
	// a close paren or end-of-file).
	//
	// [out] outBody - The individual statements in the body, as a list.
	//
	void GetBody(std::list<std::string> &outBody);

protected:
	//////////
	// Extract an argument from the stream.
	//
	// [in] inWantRawSource - Return the source verbatim?
	// [out] return - The value of the argument.
	//
	std::string GetNextArg(bool inWantRawSource);

	//////////
	// Read in an standard C++ string.  This is the basic extraction
	// operator. Most others just call this indirectly and then
	// convert the type.
	//
	std::string GetStringArg();

	//////////
	// For old5l, GetSymbolArg is basically identical to GetStringArg.
	//
	std::string GetSymbolArg();
	
	//////////
	// TString class handles string to int conversions.
	//
	int32 GetInt32Arg();

	//////////
	// TString class handles string to int conversions.
	//
	uint32 GetUInt32Arg();
	
	//////////
	// TString class handles string to bool conversions.
	//
	bool GetBoolArg();

	//////////
	// TString class handles string to double conversions.
	//
	double GetDoubleArg();
	
	//////////
	// Assumes there are 4 numbers to grab from the input stream.
	//
	TRect GetRectArg();
	
	//////////
	// Assumes there are 2 numbers to grab from the input stream.
	//
	TPoint GetPointArg();

	//////////
	// Read in an RGBA hexadecimal color in the format '#xRRGGBBAA'.
	//
	GraphicsTools::Color GetColorArg();
	
	//////////
	// Return the next argument as either a value or a percentage.
	//
	void GetValueOrPercentArg(bool &outIsPercent, int32 &outValue);

	//////////
	// Read in a callback function.  Note that any variables will
	// be expanded when the callback function is parsed, not
	// when it is called.
	//
	// [out] return - A newly allocated TCallback* object.  The
	//                caller must delete this when finished.
	//
	TCallback *GetCallbackArg();

	//////////
	// Return the next argument as a list.
	//
	// [out] return - A newly allocated TArgumentList* object.
	//                The caller must delete this when finished.
	//
	TArgumentList *GetListArg();

protected:
	//////////
	// Current positon.
	//
	unsigned int    pos;
	
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

private:
	typedef TCallback *(*CallbackMakerFunc)(const TString &);

	static CallbackMakerFunc s_CallbackMaker;

	static TCallback *MakeCallback(const TString &inCmd)
		{ ASSERT(s_CallbackMaker != NULL); return (*s_CallbackMaker)(inCmd); }

public:
	//////////
	// Register a factory function that creates TCallback objects
	// from 5L command strings.  We implement this as a callback to
	// break any dependencies between TStream and the platform-specific
	// 5L interpreters.
	//
	// The TCallback returned from 'inMaker' will eventually be
	// destroyed using 'delete'.  So allocate it on the heap.
	//
	static void SetCallbackMaker(CallbackMakerFunc inMaker)
		{ s_CallbackMaker = inMaker; }
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
// The number of palette entries available for emulating the old 8-bit
// palettes (for backwards compatibility with old scripts).
//
#define TSTREAM_PALETTE_SIZE (256)

//////////
// The current palette entries for emulating the old 8-bit palettes.
//
extern GraphicsTools::Color gPalette[TSTREAM_PALETTE_SIZE];

END_NAMESPACE_FIVEL

#endif // TStream_h

/*
 $Log$
 Revision 1.9  2002/11/05 23:06:37  emk
 Added type information to 5L variables, and replaced (var ...) with a more
 powerful form of (define ...).  These changes should make Scheme more
 pleasant for content authors.

   * TVariable now stores type information.
   * Added SetTyped primitive, and replaced VariableExists with
     VariableInitialized.
   * Added support for "symbol" arguments to primitives.  These correspond
     to Scheme symbols, and should eventually be used when a primitive
     argument refers to a variable name (or one a small, fixed set of strings).
   * Fixed bugs in TVariable's unsigned integer handling.
   * Removed TYPE argument from call-5l-prim, engine-var, etc.
   * Renamed DEFINE-PERSISTENT-VARIABLE to DEFINE/P.

 Revision 1.8  2002/10/08 21:42:25  emk
 Palette removal, part 1:

   * All primitives which used to take palette indices now take RGB colors.
   * Old 5L: Added DEFPALETTE command for declaring palettes without
     BMP files.  This provides backwards compatibility for old code.

 I haven't removed the palette code yet, but plan to do so as soon as the
 migration is complete.

 Revision 1.7  2002/08/17 01:41:55  emk
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

 Revision 1.6  2002/08/16 16:26:22  emk
 3.5.0 - 16 Aug 2002 - emk, zeb

 Preliminary Scheme support for Windows.  The Macintosh build is broken
 until Brian updates the event loop to use a TInterpreterManager object
 and figures out how to get the mzscheme libraries building.

 Ported 3.4.1 changes forward to Windows.

 Revision 1.5  2002/08/16 15:32:18  emk
 Merged basic Win32 Scheme support from the FiveL_3_5_scheme branch.  This
 breaks the Macintosh build, but Brian will be working to fix that.  There
 isn't any support for calling TEXT or TEXTAA from Scheme yet, and
 redoscript is still flaky because of some low-level issues with
 reinitializing the Scheme interpreter.

 Revision 1.4.4.1  2002/07/29 22:40:56  emk
 Initial support for writing 5L scripts in Scheme.  A lot more work
 remains, particularly in the platform front ends...

 Revision 1.4.2.2  2002/08/14 21:04:26  emk
 3.4.1 - Minor build and comment fixes.

 Revision 1.4.2.1  2002/08/14 20:24:49  emk
 Language bugfixes/enhancements/changes for HIV Prevention Counseling.  I
 removed some deeply-buried bugs in TStream and elsewhere, so please test
 this build thoroughly.

   * New entities: &shy;, &nbsp;, and &radic;.  I've also added
     &check; and &cross;, but we don't have the necessary font support yet.
   * TStream now handles whitespace rationally.  String literals are
     parsed verbatim, and the old "randomly munge whitespace" behavior
     has been fixed.  Most of the other changes are necessary consequences
     of this change.
   * Verbatim CR, LF and TAB characters in strings will be passed through.
     This may affect screen layout.
   * The (get ...) primitive has been backported from 3.5.
   * The '&' syntax has been removed.  Instead of '&foo$bar', you should
     now write '$(get foo$bar)'.
   * Entities don't need to be escaped any more: \&amp; -> &amp;.

 Thanks to this cleanup, it was possible to implement several much-wanted
 features without too much work:

   * New primitives: WHEN, UNLESS and WHILE.
   * BODY has been renamed to BEGIN, and longer prematurely evaluates all
     the variables in nested expressions.
   * Debug log improvements.

 Revision 1.4  2002/06/21 15:41:58  emk
 3.3.8 - 5L language improvements, including nested expressions,
 return values and new primitives.

   * Expressions can now be nested: '(set x $(+ 2 $(* 3 5)))' will
     set 'x' to 17.  Nested expressions should be indented as follows:

       (set x $(+ $really_big_variable_1
                  $really_big_variable_2))

     ...that is, arguments should _stack in a column_.  I will be
     extremely anal about this if I'm reading your code.

   * '(return ...)' now takes an optional argument, which will be
     returned from the macro.  So you can define your own functions, too.

   * New primitives: +, -, *, /, truncate, float+, float-, float*,
     float/, strlen, substr, findsubstr, length, nth, haskey, getval.

 A note on Lisp naming conventions--when you create a new "data structure"
 type, you should generally name functions as follows:

   # Define a type 'pt' with members 'x' and 'y' using lists.
   (macrodef pt (return ($1 $2)))
   (macrodef pt-x (return $(nth 0 $1)))
   (macrodef pt-y (return $(nth 1 $1)))

   # Alternative implementation of a type 'pt2' using associative lists.
   # (You couldn't pass 'pt2' to a built-in command, but it's a nice small
   # example.)
   (macrodef pt2 (return (x $1 y $2)))      # (pt2 10 20) => (x 10 y 20)
   (macrodef pt2-x (return $(getval $1 x))) # (pt2-x ...) => 10
   (macrodef pt2-y (return $(getval $1 y))) # (pt2-y ...) => 20

 The function 'pt' is called the "constructor", and the functions 'pt-x' and
 'pt-y' are called "accessors".

 Revision 1.3  2002/06/20 16:32:53  emk
 Merged the 'FiveL_3_3_4_refactor_lang_1' branch back into the trunk.  This
 branch contained the following enhancements:

   * Most of the communication between the interpreter and the
     engine now goes through the interfaces defined in
     TInterpreter.h and TPrimitive.h.  Among other things, this
     refactoring makes will make it easier to (1) change the interpreter
     from 5L to Scheme and (2) add portable primitives that work
     the same on both platforms.
   * A new system for handling callbacks.

 I also slipped in the following, unrelated enhancements:

   * MacOS X fixes.  Classic Mac5L once again runs under OS X, and
     there is a new, not-yet-ready-for-prime-time Carbonized build.
   * Bug fixes from the "Fix for 3.4" list.

 Revision 1.2.6.2  2002/06/11 18:15:31  emk
 3.3.4.4 - Partial separation of primitives from interpreter, and
 various 5L language enhancements related to callbacks.

   - Finished fleshing out TArgumentList, added support for callbacks.
   - Made all built-in primitives access their arguments through the
     TArgument interface.
   - Implemented a BODY command.
   - Changed how the TOUCH, BUTTPCX and KEYBIND commands parse their
     callback arguments.  See below for details; you'll have to change
     some code.  This was necessary to move callback parsing into
     TStream's implementation of the TArgumentList interface.

 5L Language Changes
 -------------------

   * (KEYBIND ...) now takes an arbitrary command instead of a card name.
     As with TOUCH and BUTTPCX, variables are evaluated when the
     keybind is installed, not when it is invoked.  Examples:

       (keybind f (jump foo))
       (keybind a (add x 10))

   * You can now run a series of zero or more commands using (BODY cmd...).
     This should work with IF, TOUCH, BUTTPCX and KEYBIND.  Example:

       (body
         (set x 10)
         (set y 20))

     Commands such as WAIT, JUMP, NAP, etc., will not do what you expect
     unless they're the last statement in a BODY.  This is caused by the
     low-level design of the interpreter, and is non-trivial to fix.

     RETURN is also not BODY-friendly.

     When you pass a body to IF, TOUCH, BUTTPCX or KEYBIND, all the
     variables in the body will be evaluated *before* any code is run!

   * The arguments to BUTTPCX and TOUCH have been rationalized after
     consultation with Douglas.  The commands now work as follows:

       (TOUCH rect cmd [cursor [picture [point]]])
       (BUTTPCX picture point header label cmd [cursor])

     Note that the second callback has disappeared from both TOUCH and
     BUTTPCX; use BODY instead.

 Revision 1.2.6.1  2002/06/10 17:52:48  emk
 3.3.4.3 - Added a TArgumentList class in TPrimitives.  This class provides
 an abstract interface to argument list parsing, and replaces parts of
 TStream.  This will allow us to begin breaking dependencies between
 the primitives and the nasty parsing gunk in TStream.

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
