// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
//////////////////////////////////////////////////////////////////////////////
//
//   (C) Copyright 1999, Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

#if !defined (TVariable_H)
#define TVariable_H

#include "TBTree.h"

BEGIN_NAMESPACE_FIVEL

//////////
// A class for representing a 5L variable, which consists of a 
// name/value pair.  It also knows how to convert to a number of
// different formats.
//
class TVariable : public TBNode 
{
	public:
		//////////
		// Supported value types.  Internally, all values are
	    // stored as strings, but we keep track of their (alleged) types
	    // to better support programming languages which distinguish
	    // between different variable types.
	    //
		// A variable will convert its value to any type upon request,
		// for compatibility with old5l and other string-based languages.
	    //
		enum Type {
			TYPE_UNINITIALIZED = 0,
			TYPE_NULL,    // No value.
			TYPE_STRING,  // Regular string.
			TYPE_SYMBOL,  // A symbol, as in Scheme.
			TYPE_LONG,    // A 32-bit signed integer.
			TYPE_ULONG,   // A 32-bit unsigned integer.
			TYPE_DOUBLE,  // A floating point number.
			TYPE_BOOLEAN, // A boolean value.
			TYPE_POINT,   // A point.
			TYPE_RECT,    // A rectangle, right-bottom exclusive.
			TYPE_COLOR    // An RGB color.
		};

		//////////
		// Constructor.
		//
		// [in] inName - name of the variable
		// [in_optional] inValue - initial value (default NULL)
		//
		TVariable(const char *inName, const char *inValue = NULL);
		
		//////////
		// Destructor.
		//
		virtual ~TVariable() {}

		//////////
		// Return the type of the variable's current value.
		//
		Type GetType() { return mType; }

		//////////
		// Make a variable null.
		//
		void MakeNull() { mType = TYPE_NULL; mValue = ""; }

		//////////
		// Is a variable null?
		//
		bool IsNull() { return mType == TYPE_NULL ? true : false; }

		//////////
		// Get the value of this variable as a character string.
		//
		// [out] return - the value of this variable
		//
		const char	*GetString(void) { return (const char *) mValue; }

		//////////
		// Get the value of this variable as a symbol.
		//
		// [out] return - the value of this variable
		//
		const char	*GetSymbol(void) { return (const char *) mValue; }
		
		//////////
		// Get the value of this variable as a long.
		//
		// [out] return - the value of this variable
		//
		int32	GetLong(void) { return (int32) mValue; }
		
		//////////
		// Get the value of this variable as unsigned long.
		//
		// [out] return - the value of this variable
		//
		uint32	GetULong(void) { return (uint32) mValue; }
		
		//////////
		// Get the value of this variable as a double.
		//
		// [out] return - the value of this variable
		//
		double	GetDouble(void)	{ return (double) mValue; }

		//////////
		// Get the value of this variable as a boolean.
		//
		// [out] return - the value of this variable
		//
		bool	GetBoolean(void) { return (mValue == "0" ? false : true); }
		
		//////////
		// Get the value of this variable as a point.
		//
		TPoint	GetPoint();

		//////////
		// Get the value of this variable as a rect.
		//
		TRect	GetRect();

		//////////
		// Get the value of this variable as a color.
		//
		GraphicsTools::Color GetColor();

		//////////
		// Set the value of this variable.
		//
		// [in] inValue - new value
		//
		void	SetString(const char *inValue)
			{ mType = TYPE_STRING; mValue = inValue; }
		
		//////////
		// Set the value of this variable.
		//
		// [in] inValue - new value
		//
		void	SetString(const TString &inValue)
			{ mType = TYPE_STRING; mValue = inValue; }
		
		//////////
		// Set the value of this variable.
		//
		// [in] inValue - new value
		//
		void	SetSymbol(const char *inValue)
			{ mType = TYPE_SYMBOL; mValue = inValue; }
		
		//////////
		// Set the value of this variable.
		//
		// [in] inValue - new value
		//
		void	SetLong(const int32 inValue)
			{ mType = TYPE_LONG; mValue = inValue; }
		
		//////////
		// Set the value of this variable.
		//
		// [in] inValue - new value
		//
		void	SetULong(const uint32 inValue)
			{ mType = TYPE_ULONG; mValue = inValue; }
		
		//////////
		// Set the value of this variable.
		//
		// [in] inValue - new value
		//
		void	SetDouble(const double inValue)
			{ mType = TYPE_DOUBLE; mValue = inValue; }

		//////////
		// Set the value of this variable.
		//
		// [in] inValue - new value
		//
		void	SetBoolean(const bool inValue)
			{ mType = TYPE_BOOLEAN; mValue = (inValue ? "1" : "0"); }
		
		//////////
		// Set the value of this variable.
		//
		void	SetPoint(const TPoint &inValue);

		//////////
		// Set the value of this variable.
		//
		void	SetRect(const TRect &inValue);

		//////////
		// Set the value of this variable.
		//
		void	SetColor(const GraphicsTools::Color &inValue);

		//////////
		// Fill this variable with a date string.
		//
		// [in] inDate - current time in seconds elapsed since 
		//				 midnight (00:00:00), January 1, 1970 
		// [in] inDateType - desired date/time format on of:
		//				(DT_LONGDATE, DT_DATE, DT_TIME, DT_YEAR,
		//				DT_MONTH, DT_LONGMONTH, DT_DAY, DT_LONGDAY)  
		//
		void	SetDate(uint32 inDate, int32 inDateType);

		//////////
		// Assign the value of one variable to another, including type
		// information.
		//
		void    Assign(const TVariable *inVar)
			{ mType = inVar->mType; mValue = inVar->mValue; }

	protected:
		//////////
		// The type of this variable.
		//
		Type		mType;

		//////////
		// The value of this variable.
		//
		TString		mValue;
		
		//////////
		// Is this varaible read-only?
		//
		bool		mReadOnly;
};


//////////
// This class manages a binary tree of variables. Commands to
// set and get variable values pass through this manager.
//
class TVariableManager : public TBTree 
{
	public:		
		//////////
		// Get the value of a special variable.
	    //
		typedef TString (*SpecialVariableFunction)();

		//////////
		// Constructor.
		//
		TVariableManager();
		
		//////////
		// Destructor.
		//
		virtual		~TVariableManager();
        
        //////////
		// Remove all variables from the tree.
		//
		void		RemoveAll(void);
        
		//////////
	    // Register a new special variable.  'inName' must begin
	    // with an underscore.
	    //
		void RegisterSpecialVariable(const std::string &inName,
									 SpecialVariableFunction inFunction);

		//////////
		// Return the type of the specified variable's current value.
		//
		// [in] inName - name of the variable
		// [out] return - the type of the variable
		//
		TVariable::Type GetType(const char *inName);

		//////////
		// Make a variable null.
		//
		void MakeNull(const char *inName);

		//////////
		// Is a variable null?
		//
		bool IsNull(const char *inName);

		//////////
		// Get the value of the specified variable as a character string.
		//
		// [in] inName - name of the variable
		// [out] return - the value of the variable
		//
		const char 	*GetString(const char *inName);
		
		//////////
		// Get the value of the specified variable as a symbol.
		//
		// [in] inName - name of the variable
		// [out] return - the value of the variable
		//
		const char 	*GetSymbol(const char *inName);
		
		//////////
		// Get the value of the specified variable as a long.
		//
		// [in] inName - name of the variable
		// [out] return - the value of the variable
		//
		long 		GetLong(const char *inName);
		
		//////////
		// Get the value of the specified variable as a double.
		//
		// [in] inName - name of the variable
		// [out] return - the value of the variable
		//
		double		GetDouble(const char *inName);

		//////////
		// Get the value of the specified variable as a boolean.
		//
		// [in] inName - name of the variable
		// [out] return - the value of the variable
		//
		bool		GetBoolean(const char *inName);
		
		//////////
		// Get the value of the specified variable as a point.
		//
		// [in] inName - name of the variable
		// [out] return - the value of the variable
		//
		TPoint		GetPoint(const char *inName);

		//////////
		// Get the value of the specified variable as a rect.
		//
		// [in] inName - name of the variable
		// [out] return - the value of the variable
		//
		TRect		GetRect(const char *inName);

		//////////
		// Get the value of the specified variable as a color.
		//
		// [in] inName - name of the variable
		// [out] return - the value of the variable
		//
		GraphicsTools::Color GetColor(const char *inName);

		//////////
		// Find a variable by name.  If not found, create a new one
		// (unless we're told not to).
		//
		// [in] inName - name of the variable
		// [in_optional] fReading - read-only access? (default true)
		// [in_optional] fCreate - create? (default true)
		//
		TVariable	*FindVariable(const char *inName,
								  int fReading = true,
								  int fCreate = true);
		
		//////////
		// Set the value of the specified variable
		//
		// [in] inName - name of the variable
		// [in] inValue - the value
		//
		void		SetString(const char *inName, const char *inValue);
		
		//////////
		// Set the value of the specified variable
		//
		// [in] inName - name of the variable
		// [in] inValue - the value
		//
		void		SetSymbol(const char *inName, const char *inValue);
		
		//////////
		// Set the value of the specified variable
		//
		// [in] inName - name of the variable
		// [in] inValue - the value
		//
		void		SetLong(const char *inName, const int32 inValue);
		
		//////////
		// Set the value of the specified variable
		//
		// [in] inName - name of the variable
		// [in] inValue - the value
		//
		void		SetULong(const char *inName, const uint32 inValue);
		
		//////////
		// Set the value of the specified variable
		//
		// [in] inName - name of the variable
		// [in] inValue - the value
		//
		void		SetDouble(const char *inName, const double inValue);
        
		//////////
		// Set the value of the specified variable
		//
		// [in] inName - name of the variable
		// [in] inValue - the value
		//
		void		SetBoolean(const char *inName, const bool inValue);

		//////////
		// Set the value of the specified variable
		//
		// [in] inName - name of the variable
		// [in] inValue - the value
		//
		void		SetPoint(const char *inName, const TPoint &inPoint);
        
		//////////
		// Set the value of the specified variable
		//
		// [in] inName - name of the variable
		// [in] inValue - the value
		//
		void		SetRect(const char *inName, const TRect &inPoint);
        
		//////////
		// Set the value of the specified variable
		//
		// [in] inName - name of the variable
		// [in] inValue - the value
		//
		void		SetColor(const char *inName,
							 const GraphicsTools::Color &inPoint);
        
		//////////
		// Set the value of the specified variable with a date string.
		//
		// [in] inName - name of the variable
		// [in] inDate - current time in seconds elapsed since 
		//				 midnight (00:00:00), January 1, 1970 
		// [in] inDateType - desired date/time format on of:
		//				(DT_LONGDATE, DT_DATE, DT_TIME, DT_YEAR,
		//				DT_MONTH, DT_LONGMONTH, DT_DAY, DT_LONGDAY)  
		//
		void		SetDate(const char *inName, uint32 inDate,
							int32 inDateType);

		//////////
		// Set the value of the specified variable from another variable.
		//
		// [in] inName - the variable to set
		// [in] inName - the variable from which to get the value
		//
		void    	Assign(const char *inName, const TVariable *inVar);
        
		//////////
		// Get the root of the local variable tree (used by macros).
		//
		// [out] return - the root of the local variable tree
		//
		TVariable	*GetLocal();
		
		//////////
		// Set the root of a local variable tree (used by macros).
		// Add() should be used to add variables to the local tree.
		// NOTE: TVariableManager will not clean up local trees!
		// It is up to whoever makes the tree to maintain it and delete it.
		// 
		// [in] newroot - new root of the local variable tree
		//
		void		SetLocal(TVariable *newroot);

	private:
		//////////
		// Callback functions for additional special variables.
		//
		std::map<std::string,SpecialVariableFunction> mSpecials;

		//////////
		// Root of the local variable tree.
		//
		TVariable	*localroot;
		
		//////////
		// Used to store value for special variables.
		//
		TVariable	*special;

		//////////
		// Is the specified variable a "special variable"?
		//
		// [in] inName - name of the variable to check
		// [out] return - true if the variable is special, false otherwise
		//
		int			IsSpecial(const char *inName);
};

extern TVariableManager gVariableManager;

END_NAMESPACE_FIVEL

#endif // TVariable_h

/*
 $Log$
 Revision 1.8  2004/01/23 17:55:58  kwasi
 Removed all redundant includes of TValue.h and GraphicsTools.h.

 Revision 1.7  2003/12/31 00:33:01  emk
 0.0.11 - 30 Dec 2003 - emk

 TRANSPARENT OVERLAYS!  Added support for alpha-composited layers.  This
 engine will require script updates:

   * SET-ZONE-CURSOR! has been renamed to SET-ELEMENT-CURSOR!.
   * RECT objects now (more) consistently exclude their right and bottom
     edges.  This may cause off-by-one errors in existing drawing code.
   * COLOR now represents opaque alpha values as 255 and transparent values
     as 0.  This is the opposite of the previous behavior.

 Changes to Scheme Runtime:

   * Added :OVERLAY? and :ALPHA? to ZONE.  These allow you to create a
     rectangular zone with an associated drawing context (and optionally an
     alpha channel).  If :OVERLAY? is true, the zone must be rectangular.
   * Added (WITH-DRAWING-CONTEXT ZONE BODY...), which allows you to change
     the current drawing context.  Do not call IDLE within this form.
   * Added (DRAWING-CONTEXT-REXT), which returns the bounding rectangle
     for the current drawing context.
   * Added (COLOR-AT POINT), which returns the color at POINT in the
     current drawing context.
   * Fixed output routines to know about POINT, RECT and COLOR objects.
   * Support for storing POINT, RECT and COLOR objects in engine variables,
     including DEFINE/P.
   * Support for comparing POINT, RECT and COLOR objects with EQUALS?.

 Changes to Tamale:

   * Added an Overlay class.  This is basically a square zone with its
     own (possibly transparent) DrawingArea and special hit-testing logic.
   * Switched from 0 opaque to 255 opaque, for performance and consistency
     with windows.
   * Support for TPoint, TRect, GraphicsTools::Color in engine variables.
   * TRect <-> wxRect conversion functions now reliably exclude right and
     bottom edges.
   * Added CompositeInto functions to Element and DrawingArea, for use with
     alpha-compositing.
   * DrawingAreas may now have alpha-channels.
   * Added alpha-channel support to DrawingArea::Clear.
   * Modified optimized versions of FillBox and DrawPixMap to use a
     templated transfer function, and added a transfer function for using
     them with DrawingAreas with alpha channels.
   * Fixed many bugs with arguments to DrawingArea::InvalidateRect.
   * Added support for retrieving pixel values.
   * Added support for pushing and poping drawing contexts.
   * Implemented a much-more-sophisticated list of dirty regions for use
     with the compositing.
   * Idling not allowed when a drawing context is pushed.
   * Replaced our single offscreen buffer with a compositing pixmap and
     and a background pixmap.
   * Invalidate an element's location when deleting it.

 Changes to wxWindows:

   * Exported AlphaBlend from the MSW wxDC class, so we can do alpha blends
     between arbitrary DCs.
   * Removed wxBitmap::UngetRawData pre-multiplication code--there wasn't
     any matching code in wxBitmap::GetRawData, and many raw algorithms
     are much more efficient on pre-multiplied data.

 Revision 1.6  2003/06/13 10:57:30  emk
 Further use of precompiled headers; pruning of various inappropriate
 includes.

 Revision 1.5  2002/11/05 23:06:37  emk
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

 Revision 1.4  2002/10/15 18:06:05  emk
 3.5.8 - 15 Oct 2002 - emk

 Engine:

   * The Windows engine now reloads scripts in the same fashion as the
     Mac engine--if a load fails, you get a chance to retry it.
   * Log files get flushed after every line.
   * The LOG and SCHEMEIDLE primitives are no longer logged, to reduce
     clutter in Debug.log.
   * Fixed tons of bugs in places where the Windows engine assumed it
     had a TInterpreter object, but didn't (i.e., lots of "sInstance"
     assertions are gone).
   * Added support for measuring text without drawing it.
   * Added support for checking whether an engine variable is initialized.
   * Made sure LCursor initializes mForceShow.

 Revision 1.3  2002/07/15 15:56:32  zeb
 3.3.13 - 15 July 2002 - zeb, emk
   * Language change: (IF cond true_cmd false_cmd) now takes arbitrary
     expressions for 'cond'.  The following new primitives have
     been added: AND, OR, NOT, contains, =, <>, <, >, <=, >=.
   * Added a new (LOG filename msg) command, which allows the programmer
     to write to "5L", "debug" and "MissingMedia" logs.
   * Major logging improvements: All primitives are now automatically
     logged in a standard format (bug #1003).
   * Adjusting of coordinates using origin is now logged.
   * Callbacks are now logged in a much more useful fashion.
   * Old arithmetic primitives now return a value (add, sub, div).
   * Added MakeQuotedString to TTemplateUtils and wrote a matching test suite.

 Revision 1.2  2002/05/15 11:05:18  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.1.4.1  2002/04/22 05:22:33  emk
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
