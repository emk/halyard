// -*- Mode: C++; tab-width: 4; -*-
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

#include <map>
#include <string>

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
		// Get the value of this variable as a character string.
		//
		// [out] return - the value of this variable
		//
		const char	*GetString(void) { return (const char *) mValue; }
		
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
		// Set the value of this variable.
		//
		// [in] inValue - new value
		//
		void	SetString(const char *inValue) { mValue = inValue; }
		
		//////////
		// Set the value of this variable.
		//
		// [in] inValue - new value
		//
		void	SetString(const TString &inValue) { mValue = inValue; }
		
		//////////
		// Set the value of this variable.
		//
		// [in] inValue - new value
		//
		void	SetLong(const long inValue) { mValue = inValue; }
		
		//////////
		// Set the value of this variable.
		//
		// [in] inValue - new value
		//
		void	SetDouble(const double inValue)	{ mValue = inValue; }
		
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

	protected:
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
		// Get the value of the specified variable as a character string.
		//
		// [in] inName - name of the variable
		// [out] return - the value of the variable
		//
		const char 	*GetString(const char *inName);
		
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
		// Find a variable by name.  If not found, create a new one.
		//
		// [in] inName - name of the variable
		// [in_optional] fReading - read-only access? (default true)
		//
		TVariable	*FindVariable(const char *inName, int fReading = true);
		
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
		void		SetLong(const char *inName, const long inValue);
		
		//////////
		// Set the value of the specified variable
		//
		// [in] inName - name of the variable
		// [in] inValue - the value
		//
		void		SetDouble(const char *inName, const double inValue);
        
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
