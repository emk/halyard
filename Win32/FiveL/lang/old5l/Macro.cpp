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
// Macro.cpp : 
//

#include "stdafx.h"

#include "Macro.h"
#include "LUtil.h"
#include "Globals.h"
#include "TWin5LInterpreter.h"

MacroManager gMacroManager;

Macro::Macro(TIndexFile *inFile, const char *name, long p1, long p2) 
	: Card(inFile, name, p1, p2)
{
    m_Running = false;
	m_Return = false;
}

/***********************************************************************
 * Function: Macro::Execute
 *
 *  Parameter (null)
 * Return:
 *
 * Comments:
 *  A little different from Card::Execute... we don't record
 *  CurrentCard/CurrentCommand, and we don't flush the m_Script
 *  once we're done.
 ***********************************************************************/
void Macro::Execute()
{
    if (m_Running)
	{
        gLog.Log("Error: Can't call macro %s recursively.", Key());
		return;
	}

    m_Running = true;
	m_Return = false;

    SetScript();
    m_Script.reset();
 
 	gDebugLog.Log("macro <%s>", Key());
 	gDebugLog.Log("<%s>", m_Script.GetString());

    m_Script >> open >> discard >> discard;   //  Remove "(macrodef NAME"

    //  Do commands until we jump somewhere or we hit the closing paren.
    //
    while ((m_Script.more()) 
		and (not m_Return)
		and (not gCardManager.Jumping())) 
    {
        DoCommand();
    }
    
    m_Running = false;
	m_Return = false;
}

void Macro::Return()
{
	if (m_Running)
		m_Return = true;

	gDebugLog.Log("return: from macro <%s>", Key());
}

/***********************************************************************
 * Function: MacroManager::MakeNewIndex
 *
 *  Parameter name
 *  Parameter start
 *  Parameter end
 * Return:
 *
 * Comments:
 *  Creates new macro entry into the macro tree
 ***********************************************************************/
void MacroManager::MakeNewIndex(TIndexFile *inFile, const char *name, long start, long end)
{
    Macro   *newMacro;

    newMacro = new Macro(inFile, name, start, end);

	if (newMacro->SetScript())
		Add(newMacro);
}


/*
 $Log$
 Revision 1.4  2002/06/20 16:32:55  emk
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

 Revision 1.3.6.1  2002/06/06 05:47:30  emk
 3.3.4.1 - Began refactoring the Win5L interpreter to live behind an
 abstract interface.

   * Strictly limited the files which include Card.h and Macro.h.
   * Added TWin5LInterpreter class.
   * Made as much code as possible use the TInterpreter interface.
   * Fixed a few miscellaneous build warnings.

 Revision 1.3  2002/05/15 11:05:33  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.2.2.1  2002/04/30 07:57:31  emk
 3.3.2.5 - Port Win32 code to use the 20Kloc of Common code that now
 exists.  The (defstyle ...) command should work, but (textaa ...) isn't
 available yet.

 Next up: Implement the (textaa ...) command and the low-level
 GraphicsTools::Image::DrawBitMap.

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

 Revision 1.4  2000/05/11 12:54:54  chuck
 v 2.01 b2

 Revision 1.3  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
