// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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
// TVariable.cpp : Code for handling variables within 5L.
//
//

#include "TDateUtil.h"
#include "TVariable.h"
#include "TLogger.h"

//#include "LFiles.h"
//#include "Card.h"
//#include "Globals.h"

USING_NAMESPACE_FIVEL

TVariableManager FIVEL_NS gVariableManager;

//  Initialize the variable.
TVariable::TVariable(const char *inName, const char *inValue) : TBNode(inName)
{
    if (inValue != NULL)
        mValue = inValue; 
	else
		mValue = (int32) 0;
}

void TVariable::SetDate(uint32 inDate, int32 inDateType)
{
	TString theDate;
	::SetDate(theDate, inDate, inDateType);
	SetString(theDate.GetString());
}


//=========================================================================
//  Variable Manager Routines
//=========================================================================

//  Initialize the local variable tree.
TVariableManager::TVariableManager() : TBTree()
{
    localroot = NULL;

    //  The special variable is not in the tree so its name won't
    //  conflict with a 5L variable called "Special".
    special = new TVariable("Special");
}

//  Normally the local tree will never exist. But in case we
//  exit in the middle of a macro (due to syntax, etc) we should
//  clean up.
TVariableManager::~TVariableManager()
{ 
	RemoveAll();

	if (localroot != NULL)
		localroot->RemoveAll();

	if (special != NULL)
		delete special;
}

void
TVariableManager::RegisterSpecialVariable(const std::string &inName,
										  SpecialVariableFunction inFunc)
{
	mSpecials.insert(std::pair<std::string,SpecialVariableFunction>(inName,
																	inFunc));
}

//	RemoveAll - Wipe out all the nodes.
void TVariableManager::RemoveAll(void)
{
	if (localroot != NULL)
	{
		localroot->RemoveAll();
		localroot = NULL;
	}
	
	// don't delete special - there is no reason to
	TBTree::RemoveAll();
}
	

// Function: TVariableManager::IsSpecial
//
//  Parameter name
// Return:
//  1 if special, 0 else.
// Comments:
//  Determine if the given variable is a "special" variable. If it is
//  set the value of the special instance variable to the proper value.
//  Return whether or not the name is a special variable.
//  
//  Special variables:
//
//      date        Current date in form 08/21/93
//      longdate    Current date in form January 22, 1992
//      time        Current time in form 12:34pm
//
int TVariableManager::IsSpecial(const char *name)
{
    TString     vname(name);
    TString     str;

    vname.MakeLower();
    
    // do a quick check to see if it could be a special variable
	if (not vname.StartsWith("_", false))
    	return (false);

	if (vname.Equal("_date"))
    {
    	::GetDate(str, df_DATE);
		special->SetString(str);
        return true;
    } 
	else if (vname.Equal("_longdate")) 
    {
    	::GetDate(str, df_LONGDATE);
        special->SetString(str);
        return true;
    } 
    else if (vname.Equal("_time")) 
    {
    	::GetDate(str, df_TIME);
        special->SetString(str);
        return true;
    } 
    else if (vname.Equal("_seconds"))
    {
    	::GetDate(str, df_SECONDS);
        special->SetString(str);
        return true;
    } 
	else
	{
		std::map<std::string,SpecialVariableFunction>::iterator found =
			mSpecials.find(std::string(vname));
		if (found != mSpecials.end())
		{
			special->SetString((*found->second)());
			return true;
		}
	}

    return false;
}

void TVariableManager::MakeNull(const char *inName)
{
    TVariable *var = FindVariable(inName, true);
	var->MakeNull();
}

bool TVariableManager::IsNull(const char *inName)
{
    TVariable *var = FindVariable(inName, true);
	return var->IsNull();
}

// Return value of "name" as a string.
const char *TVariableManager::GetString(const char *name)
{
    TVariable *var = FindVariable(name, true);
    return (var->GetString());
}

// Return value of "name" as a long.
long TVariableManager::GetLong(const char *name)
{
    TVariable *var = FindVariable(name, true);
    return var->GetLong();
}

// Return value of "name" as a double float.
double TVariableManager::GetDouble(const char *name)
{
    TVariable *var = FindVariable(name, true);
    return var->GetDouble();
}


// Function: TVariableManager::FindVariable
//
//  Parameter name
//  Parameter fReading
// Return:
//  Variable name "name" or new variable created.
// Comments:
//  Search the tree for the variable. If it's not there create it.
//  We always create variables if they don't exist. This is so a
//  variable may be set in the command line (or not set in the
//  command line) and still used in the script.
//
TVariable *TVariableManager::FindVariable(const char *name, int fReading)
{
    TVariable *var;

    //  First see if it is a special variable. If it is and fReading is
    //  false, complain because these are read-only variables. Otherwise
    //  return the special variable, which IsSpecial has set.
    if (IsSpecial(name)) 
    {
        if (fReading)
			return (special);
		else
		{
            gLog.FatalError("Error: $%s is a read-only variable.", name);
		}
            
    }

    //  Search the local tree.
    if (localroot) 
    {
        var = (TVariable*) localroot->Find(name);
        if (var) 
        	return var;
    }

    //  Now check the global tree. It's ok to fail; we'll create the
    //  variable if it's not there.
    var = (TVariable*) Find(name);
    if (var == NULL) 
    { 
 		if (fReading)
 			gDebugLog.Log("Getting variable <%s> before it has been set.",
						  name);
        var = new TVariable(name);
        Add(var); 
    
		var->SetString("0");
    }

    return var;
}

// Set 'name' to 'data'.
void TVariableManager::SetString(const char *name, const char *data)
{
    TVariable *var = FindVariable(name, false);
    var->SetString(data);
}

// Set 'name' to 'data'.
void TVariableManager::SetLong(const char *name, const long data)
{
    TVariable *var = FindVariable(name, false);
    var->SetLong(data);
}

// Set 'name' to 'data'.
void TVariableManager::SetDouble(const char *name, const double data)
{
    TVariable *var = FindVariable(name, false);
    var->SetDouble(data);
} 

// Set 'name' to 'data'.
void TVariableManager::SetDate(const char *name, uint32 date, int32 date_type)
{
	TVariable *var = FindVariable(name, false);
	var->SetDate(date, date_type);
}


// Function: TVariableManager::GetLocal
//
//  Parameter (null)
// Return:
//  root of local tree (for $1 etc. in a macro)
// Comments:
//  Methods to manage the local tree used by macros.
//  NOTE: TVariableManager will not clean up local trees!
//  It is up to whoever makes the tree to maintain it
//  and delete it.
//
TVariable *TVariableManager::GetLocal()
{
    return localroot;
}

void TVariableManager::SetLocal(TVariable *newlocal)
{
    localroot = newlocal;
}


/*
 $Log$
 Revision 1.5  2002/07/15 15:56:32  zeb
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

 Revision 1.4  2002/05/29 09:38:53  emk
 Fixes for various "crash on exit" bugs in 5L.

   * Fixed lots of bugs in TBTree, mostly in the code for removing nodes.
     TBTree should now work more or less correctly.
   * Removed the broken reference counting logic in TIndex and TIndexFile.
   * Made FatalError call abort(), not exit(1), so the destructors for
     (possibly corrupt) global variables will not be called.

 This code may break either the Windows or Mac build; I'll try to fix things
 right away.

 Revision 1.3  2002/05/15 11:05:17  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.2.2.2  2002/04/30 07:57:24  emk
 3.3.2.5 - Port Win32 code to use the 20Kloc of Common code that now
 exists.  The (defstyle ...) command should work, but (textaa ...) isn't
 available yet.

 Next up: Implement the (textaa ...) command and the low-level
 GraphicsTools::Image::DrawBitMap.

 Revision 1.2.2.1  2002/04/22 05:22:33  emk
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

 Revision 1.2  2002/02/19 12:35:12  tvw
 Bugs #494 and #495 are addressed in this update.

 (1) 5L.prefs configuration file introduced
 (2) 5L_d.exe will no longer be part of CVS codebase, 5L.prefs allows for
     running in different modes.
 (3) Dozens of compile-time switches were removed in favor of
     having a single executable and parameters in the 5L.prefs file.
 (4) CryptStream was updated to support encrypting/decrypting any file.
 (5) Clear file streaming is no longer supported by CryptStream

 For more details, refer to ReleaseNotes.txt

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.5  2000/08/08 19:03:41  chuck
 no message

 Revision 1.4  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.3  1999/11/02 17:16:37  chuck
 2.00 Build 8

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
