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

Macro::Macro(IndexFile *inFile, const char *name, long p1, long p2) 
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
void MacroManager::MakeNewIndex(IndexFile *inFile, const char *name, long start, long end)
{
    Macro   *newMacro;

    newMacro = new Macro(inFile, name, start, end);

	if (newMacro->SetScript())
		Add(newMacro);
}


/*
 $Log$
 Revision 1.1.2.1  2002/03/13 15:06:56  emk
 Merged changed from 3.1.1 -> 3.2.1 into the 3.2.0.1 codebase,
 because we want these in the stable engine.  Highlights:

   1) FiveL.prefs file support.
   2) Removal of -D command line flag.

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
