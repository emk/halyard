// -*- Mode: C++; tab-width: 4; -*-
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
// Card.cpp : Execute commands.
//
// (see header file for additional documentation)

#include "stdafx.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "TCommon.h" 

#include "TDeveloperPrefs.h"
#include "TRect.h"
#include "Card.h"
#include "Macro.h"
#include "LUtil.h"
#include "LDiskUtils.h"
#include "Globals.h"
#include "FiveL.h"
#include "Config.h"
#include "Graphics.h"
#include "LHttp.h"
#include "FileSystem.h"
#include "Header.h"
#include "TStyleSheet.h"
#include "TWin5LInterpreter.h"
#include "TCommonPrimitives.h"
#include "TWinPrimitives.h"
#include "TException.h"

using FileSystem::GetDataFilePath;
using FileSystem::Path;

CardManager gCardManager;

//  Card - Constructor. Cards have a default origin of (0, 0)
//
Card::Card(TIndexFile *inFile, const char *name, long p1, long p2) 
	: TIndex(inFile, name, p1, p2)
{
	gOrigin.SetOrigin(0, 0);
}

//
//	Start - Get the card ready for execution.
//
void Card::Start(void)
{
	gDebugLog.Log("Start card <%s>", Name());

	gNeedsRefresh = true;
	
	SetScript();				// load the card's code into memory
	m_Script.reset();			// reset it to the start
	gOrigin.SetOrigin(0, 0);	// reset the origin
	m_stopped = false;

	gDebugLog.Log("<%s>", m_Script.GetString());

	// toss (card name
	m_Script >> open >> discard >> discard;
}

void Card::Stop(void)
{
}	

//
//	Execute - Execute commands in the card until
//		we pause or there aren't any more commands
//		or we are jumping somewhere.
//
void Card::Execute(void)
{ 
	if (m_Script.more())
	{
		while ((not gCardManager.Paused())
		   and (m_Script.more())
		   and (not m_stopped)
		   and (not gCardManager.Napping())
		   and (not gCardManager.Jumping()))
		{
			DoCommand();
		}
    }
    
    if (not m_Script.more())
    {
    	if (gNeedsRefresh)
    	{
    		gView->Draw();
    		gNeedsRefresh = false;
    		
    		gCursorManager.CheckCursor();
    	}
    }
}

// Evaluate a given command. Trim opening paren and read opword.
// Opword determines what we parse and then we call the appropriate
// routine DoTheCommand...
void Card::DoCommand(void)
{
	TString     opword;
	
	m_Script >> open;
	m_Script >> opword;
	opword.MakeLower();

	try
	{
		if (opword == (char *)"if") DoIf();
		else if (opword == (char *)"body") DoBody();
		else if (opword == (char *)"return") DoReturn();
		else if (opword == (char *)"exit") DoExit();
		else if (gPrimitiveManager.DoesPrimitiveExist(opword.GetString()))
			gPrimitiveManager.CallPrimitive(opword.GetString(), m_Script);
		else DoMacro(opword);
	}
	catch (std::exception &e)
	{
		// XXX - We need a "quit unless debugging" error.
		gLog.Error("Internal error: %s.", e.what());
    }
    catch (...)
    {
		gLog.Error("Unknown internal error.");
	}

	// Find the closing paren for our command.
	// XXX - If an error occurs, we may not always find this close
	// parentheses.  This could make the interpreter puke pretty badly.
	// But this works often enough that it's worth trying.
	m_Script >> close;
}

// Execute a single command, perhaps in response to a touch zone or
// a timeout. Save the old m_Script, do the one command, and restore
// the m_Script.
//
// theCommand should look like "(jump aCard)", ie both parens need to
// be there.
void Card::OneCommand(TString &theCommand)
{
    TStream     saveScript(m_Script);
	try
	{
		m_Script = theCommand;
		DoCommand();
	}
	catch (...)
	{
		m_Script = saveScript;
		throw;
	}
	m_Script = saveScript;
}

/************************

    PROTECTED METHODS

************************/

enum EvalMode 
{
    FirstTime,
    And,
    Or
};

/***********************************************************************
 * Function: Card::Evaluate
 *
 *  Parameter conditional (blah AND blah OR etc..)
 * Return:
 *
 * Comments:
 *  Evaluate the given conditional and determine whether or not
 *  it is true.
 ***********************************************************************/
int Card::Evaluate(TStream& conditional)
{
    int         globalRes, localRes, result;
    EvalMode    mode = FirstTime;
	TString		cond;
    TString     op;
    TString     modeStr;
    TString     str1, str2;

    globalRes = localRes = false;

	cond = (const char *) conditional;

    while (conditional.more()) 
    {
        conditional >> str1 >> op >> str2;

        //  Returns <0, 0, or >0.
        //
		op.MakeLower();
		if (op.Equal("contains"))
        {
			if (str1.Contains(str2, false))
				localRes = true;
			else
				localRes = false;        
        }
		else
		{        
	        result = str1.TypeCompare(str2);
	
	        if (op == (char *)"=") 
	        	localRes = (result == 0);
	        else if (op == (char *)"<>") 
	        	localRes = (result != 0);
	        else if (op == (char *)">") 
	        	localRes = (result > 0);
	        else if (op == (char *)">=") 
	        	localRes = (result >= 0);
	        else if (op == (char *)"<") 
	        	localRes = (result < 0);
	        else if (op == (char *)"<=") 
	        	localRes = (result <= 0);
	        else
			{
				gLog.Log("Error: bad If statement <%s>", (const char *) cond);
	            gLog.Log("Error: IF: unknown operator %s.", (const char *) op);
			}
        }
        
        switch (mode) 
        {
            case FirstTime:
                globalRes = localRes;
                break;
            case And:
                globalRes = globalRes && localRes;
                break;
            case Or:
                globalRes = globalRes || localRes;
                break;
        }

        if (conditional.more()) 
        {
            conditional >> modeStr;
            modeStr.MakeLower();
            if (modeStr == (char *)"and") 
            {
                if (mode == Or) 
				{
					gLog.Log("Error: bad If statement <%s>", (const char *) cond);
                	gLog.Log("Error: IF: can't mix ANDs and ORs.");
				}
                mode = And;
            } 
            else if (modeStr == (char *)"or") 
            {
                if (mode == And)
				{
					gLog.Log("Error: bad If statement <%s>", (const char *) cond);
                	gLog.Log("Error: IF: can't mix ANDs and ORs.");
				}
                mode = Or;
            } 
            else
			{
				gLog.Log("Error: bad If statement <%s>", (const char *) cond);
                gLog.Log("Error: IF: expected AND or OR here, not %s.", (const char *) modeStr);
			}
        }
    }
    return globalRes;
}

/*-------------------------------------------------------------------
    (MACRONAME <VAR>...)

    Call the macro by name.
    VAR are an optional number of local variables that vary depending
    upon the particular macrodef.
---------------------------------------------------------------------*/
void Card::DoMacro(TString &name)
{
    Macro       *theMacro;
    TString     vname, contents;
    int32       vnum;
    TVariable    *local, *temp, *oldlocal;

    theMacro = (Macro *) gMacroManager.Find(name);
    
    if (theMacro == NULL)
    {
		gLog.Log("Error: Unknown macro/opword %s.", (const char *) name);
		return;
	}

    //  Get the local variables, if passed.
    //
    local = 0;
    vnum = 0;
    while (m_Script.more()) 
    {
        //  Variables are named 1, 2, 3...
        //
        vname = ++vnum;
        m_Script >> contents;

        temp = new TVariable(vname, contents);

        if (local == 0) 
        	local = temp;
        else 
        	local->Add(temp);
    }

    //  Save old local tree and set current local tree to ours.
    //
    oldlocal = gVariableManager.GetLocal();
    gVariableManager.SetLocal(local);

    //  Run the macro using our co-ordinate system, but make
	//  sure the macro can't change our co-ordinate system.
    //
	TPoint oldorigin = gOrigin.GetOrigin();
    theMacro->Execute();
	gOrigin.SetOrigin(oldorigin);

    //  Restore old local tree and delete ours.
    //
    gVariableManager.SetLocal(oldlocal);
	if (vnum > 0)
    	local->RemoveAll();
}

/*-----------------------------------------------------------------
    (IF (CONDITIONAL) (true_CMD) <(false_CMD)>)

    Evaluate the conditional expression and execute the appropriate
    command based on the value of the expression. Only numbers may
    be compared. It's important that the conditional statement be
    enclosed in parentheses and that the operator (>, <, =) be
    separated from the operands by a space.
-------------------------------------------------------------------*/
void Card::DoIf()
{
    TStream     conditional;

    m_Script >> conditional;
    conditional.reset();

    if (Evaluate(conditional))
        DoCommand();
    else 
    {
        //  Skip true_CMD.
        m_Script >> open >> close;
        if (m_Script.more()) 
        	DoCommand();
    }
}

/*-----------------------------------------------------------------
    (BODY cmd...)

    Evaluate zero or more commands in sequence.  The BODY command
    can be used to pass a list of commands as an argument to the
    IF, BUTTPCX and TOUCH commands.
-------------------------------------------------------------------*/
void Card::DoBody()
{
	while (m_Script.more())
	{
		// Extract our command and put back the parentheses removed
		// by the parser.  This a kludge.
		TString cmd;
		m_Script >> cmd;
		cmd = TString("(") + cmd + TString(")");

		// Execute the command.
		OneCommand(cmd);
	}
}

//
//	DoReturn - Stop processing and return from the current script
//
void Card::DoReturn()
{
	Return();
}

void Card::Return()
{
	gDebugLog.Log("return: in card <%s>, stop processing", Key());
	m_stopped = true;
}

/*-------------------
    (EXIT)

    Exit the program.
---------------------*/
void Card::DoExit()
{
    int16 WhichSide = 0;
    
    if (m_Script.more()) 
    	m_Script >> WhichSide;
    
    gCardManager.DoExit(WhichSide);
}

/***************************

    CARD MANAGER METHODS

***************************/

CardManager::CardManager() : TIndexManager()
{ 
	m_HaveJump = false;
	m_Napping = false; 
	m_Paused = false;
	m_TimeoutTime = 0;
	m_NapTime = 0;
    m_CurrentCard = NULL;
    m_JumpCard = NULL; 
    m_TimeoutCard = NULL;
    m_ExitNow = false;
    m_ExitScript = 0;
	m_ReDoScript = false;
}

void CardManager::RemoveAll(void)
{
	m_HaveJump = false;
	m_Napping = false; 
	m_Paused = false;
	m_TimeoutTime = 0;
	m_NapTime = 0;
    m_CurrentCard = NULL;
    m_JumpCard = NULL; 
    m_TimeoutCard = NULL;
    m_ExitNow = false;
    m_ExitScript = 0;
	m_ReDoScript = false;

 	m_CardList.RemoveAll();   
	TIndexManager::RemoveAll();
}

void CardManager::Idle(void)
{ 
	TVariable	*theAfterVar;
	TVariable	*theBeforeVar;
	Card		*theCard;
	int32		index;
	
	if (m_Paused)
		return;
			
	if (m_Napping)
	{
		// see if it is time to wakeup
		if (m_NapTime < ::timeGetTime())
		{ 
			gDebugLog.Log("CardManager: wake up from nap");
			m_NapTime = 0;
			m_Napping = false;
		}
		else
			return;		// nothing else to do
	}
		 
	if (m_TimeoutTime > 0)
	{
		// see if the timeout timer has expired
		if (m_TimeoutTime < ::timeGetTime())
		{
			m_HaveJump = true;
			m_JumpCard = m_TimeoutCard;
			
			m_TimeoutTime = 0;
			m_TimeoutCard = NULL;
		}
	}

	if (m_HaveJump)
	{
		// process the jump
		if (m_CurrentCard != NULL)
		{ 
			m_PrevCard = m_CurrentCard->Name();
						
			m_CurrentCard->Stop();
		}
		
		// a jump kills the timeout timer
		m_TimeoutTime = 0;
		m_TimeoutCard = NULL; 
		
		// a jump will kill video
		if (gVideoManager.Playing())
			gVideoManager.Kill(); 
			
		// and non-looping audio
		if (gAudioManager.Playing())
			gAudioManager.Kill(0, false);
		
		m_CurrentCard = m_JumpCard;

		// cbo - set the before and after variables
		theBeforeVar = gVariableManager.FindVariable("_BeforeCard", false);
		theBeforeVar->SetLong(0);
		theAfterVar = gVariableManager.FindVariable("_AfterCard", false);
		theAfterVar->SetLong(0);

		index = m_CurrentCard->GetIndex();
		if (m_CardList.ValidIndex(index))
		{
			if (m_CardList.ValidIndex(index - 1))
			{
				theCard = (Card *) m_CardList.Item(index - 1);
				if (theCard != NULL)
					theBeforeVar->SetString(theCard->Name());
			}

			if (m_CardList.ValidIndex(index + 1))
			{
				theCard = (Card *) m_CardList.Item(index + 1);
				if (theCard != NULL)
					theAfterVar->SetString(theCard->Name());
			}
		}
			
		m_JumpCard = NULL;
		m_HaveJump = false;
		
		// load the current card to get the ball rolling
		m_CurrentCard->Start();
	}
	
	if (m_CurrentCard != NULL)
	{
		// process some commands
		m_CurrentCard->Execute();
	}
	
	// see if we need to leave
	if (m_ExitNow)
	{
		SwitchScripts(m_ExitScript);
	}

	// see if we need to do redoscript
	if (m_ReDoScript)
	{
		m_ReDoScript = false;
		ReDoScript(m_ReDoCardName);
	}
}

void CardManager::DoExit(int32 inScript)
{
	m_ExitNow = true;
	m_ExitScript = inScript;
}

void CardManager::OneCommand(TString &inCmd)
{
	if (m_CurrentCard != NULL)
		m_CurrentCard->OneCommand(inCmd);
}

void CardManager::WakeUp(void)
{
	gDebugLog.Log("CardManager: WakeUp");
	m_Paused = false;
}

void CardManager::Pause(void)
{
	gDebugLog.Log("CardManager: Pause");
	m_Paused = true;
}

void CardManager::Timeout(const char *inCardName, int32 inTime)
{
	if (inTime > 0)
	{
		m_TimeoutCard = GetCard(inCardName);
    
    	if (m_TimeoutCard != NULL)
    	{
    		m_TimeoutTime = inTime * 1000L;
    		m_TimeoutTime += ::timeGetTime();
    	}
    }
}

void CardManager::Nap(int32 inTime)
{
	if (inTime > 0)
	{
		gDebugLog.Log("CardManager: Nap for <%ld> tenths", inTime);
		m_Napping = true;
		m_NapTime = inTime * 100L; 
		m_NapTime += ::timeGetTime();
	}
}

void CardManager::KillNap(void)
{
	gDebugLog.Log("CardManager: Kill the nap");

	m_Napping = false;
	m_NapTime = 0;
}

const char *CardManager::CurCardName(void)
{
    if (m_CurrentCard != NULL)
		return ((const char *) (m_CurrentCard->Name()));
	return (NULL);
}

const char *CardManager::PrevCardName(void)
{
	return (m_PrevCard.GetString());
}

void CardManager::JumpToCardByName(const char *inName)
{
	Card	*theCard = NULL;
	
	theCard = GetCard(inName);
	if (theCard != NULL)
		JumpToCard(theCard);
	else
		gLog.Log("Trying to jump to <%s>, couldn't find it", inName);
}

void CardManager::JumpToCard(Card *inCard)
{

	// Make sure inCard is a valid Card
	//ASSERT
	
	if (inCard != NULL)
	{
		m_JumpCard = inCard;
		m_HaveJump = true;
		
		m_Paused = false;		// when jump always wake up manager
	}
	else
		gDebugLog.Log("Trying to jump to a null card");
}

void CardManager::DoReDoScript(TString &inCardName)
{
	m_ReDoScript = true;
	m_ReDoCardName = inCardName;
}

void CardManager::MakeNewIndex(TIndexFile *inFile, const char *inName, 
							   int32 inStart, int32 inEnd)
{
    Card    *newCard;  
    int32	index;

    newCard = new Card(inFile, inName, inStart, inEnd);

	//  when redoscript functionality is enabled, read the script into memory so that 
	//	the file can be changed without invalidating the index information
	if (gDeveloperPrefs.GetPref(REDOSCRIPT) == REDOSCRIPT_ON)
		newCard->SetScript();

    Add(newCard); 
    
    // add the card to our list of cards
	index = m_CardList.Add(newCard);
	newCard->SetIndex(index);
}

/*
 $Log$
 Revision 1.7  2002/06/20 16:32:54  emk
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

 Revision 1.6.2.8  2002/06/19 19:18:08  emk
 Make the refactored TCommonPrimitives code work on Windows.

 Revision 1.6.2.7  2002/06/12 19:42:46  emk
 3.3.4.6 - Fixed bug where the origin didn't get restored after each macro
 call.  (This bug was introduced in 3.3.4.5.)

 Revision 1.6.2.6  2002/06/12 19:03:03  emk
 3.3.4.5 - Moved Do* commands from Card.{h,cpp} to TWinPrimitives.{h,cpp},
 and broke the remaining dependencies between these primitive commands and
 the current 5L interpreter.  The TInterpreter and TPrimitives interfaces
 are now quite mature.

 *** Please beat very, very hard on this build.  I don't anticipate
 further changes to the Windows engine for a while. ***

 REMOVED COMMANDS: kill (use still), loadpick (use loadpic)
 NEEDS TESTING: origin w/macros, other uses of origin.  5L now
   sets the origin to 0,0 whenever it begins a new card, which
   should produce behavior identical to the old system, unless
   I've overlooked something.
 NEEDS TESTING: make sure all the commands are available, and
   have the right names.  I've checked this a dozen times
   by eye, but I might have overlooked something.

 The only remaining dependencies between the interpreter and the rest of 5L
 are in the Header and TStyleSheet classes.  I'm postponing this last bit
 of cleanup until after 3.4.  Up next: Repeat the 3.3.4.{1-5} changes for
 the Macintosh.

 Revision 1.6.2.5  2002/06/11 18:15:42  emk
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

 Revision 1.6.2.4  2002/06/05 20:42:38  emk
 3.3.4.2 - Broke Win5L dependencies on TIndex file by moving various pieces
 of code into TWin5LInterpreter.  Windows 5L now accesses the interpreter
 through a well-defined API.  Changes:

   * Removed many direct and indirect #includes of TIndex.h.
   * Added a TInterpreter method ReloadScript, which can be called by the
     higher-level ReDoScript command.
   * Checked in some files which should have been included in the 3.3.4.1
     checkin--these files contain the initial refactorings of Card and Macro
     callsites to go through the TInterpreter interface.

 Up next: Refactor various Do* methods out of Card and into a procedural
 database.

 Revision 1.6.2.3  2002/06/06 05:47:30  emk
 3.3.4.1 - Began refactoring the Win5L interpreter to live behind an
 abstract interface.

   * Strictly limited the files which include Card.h and Macro.h.
   * Added TWin5LInterpreter class.
   * Made as much code as possible use the TInterpreter interface.
   * Fixed a few miscellaneous build warnings.

 Revision 1.6.2.2  2002/06/05 08:50:52  emk
 A small detour - Moved responsibility for script, palette and data directories
 from Config.{h,cpp} to FileSystem.{h,cpp}.

 Revision 1.6.2.1  2002/06/05 07:05:30  emk
 Began isolating the 5L-language-specific code in Win5L:

   * Created a TInterpreter class, which will eventually become the
     interface to all language-related features.
   * Moved ssharp's developer preference support out of Config.{h,cpp}
     (which are tighly tied to the language) and into TDeveloperPrefs.{h,cpp},
     where they will be isolated and easy to port to other platforms.

 Revision 1.6  2002/05/29 13:58:17  emk
 3.3.4 - Fixed various crash-on-exit problems (including those in TBTree,
 TIndex and TLogger::FatalError), and reverted the Win32 _INCR_Y code
 to the behavior that shipped with Genetics.

 Revision 1.5  2002/05/15 11:05:33  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.4.2.3  2002/05/15 09:23:55  emk
 3.3.2.8 - Last typography branch checkin before merge.

 * Fixed (wait ...) bug which caused all (wait ...) commands to wait
 until the end of the movie.

 * (buttpcx ...) now uses anti-aliased text.

 * Miscellaneous other tweaks and fixes--just getting things into shape
 for the merge.

 Revision 1.4.2.2  2002/05/01 03:27:07  emk
 3.3.2.6 - First Windows engine with (textaa ...) command.

 - Implemented a primitive, slow Image::DrawPixMap command that uses
 ::GetPixel and ::SetPixel to do alpha blending (shudder).  Strangely
 enough, it's about as fast as the somewhat optimized Mac routines.
 Anyone got a good GDI book?

 - Fixed several assertion failures.

 Known problems:

 - Occasional assertion failure on exit.  The reference-counting on
 TIndexFile claims it's getting dereferenced too many times.  This is
 an old bug; all the TBTree and TBNode classes are pretty dodgy.

 - Assertion failure on "Special Variables" screen in 5Ltest.  This is
 caused by overlong lines.

 Revision 1.4.2.1  2002/04/30 07:57:30  emk
 3.3.2.5 - Port Win32 code to use the 20Kloc of Common code that now
 exists.  The (defstyle ...) command should work, but (textaa ...) isn't
 available yet.

 Next up: Implement the (textaa ...) command and the low-level
 GraphicsTools::Image::DrawBitMap.

 Revision 1.4  2002/02/28 15:31:06  tvw
 Fixes subtraction to detect the type of number being subtracted.

 Revision 1.3  2002/02/28 15:13:18  tvw
 Fixes addition to detect the type of number being added.

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

 Revision 1.1  2001/09/24 15:11:00  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.10  2000/08/08 19:03:40  chuck
 no message

 Revision 1.9  2000/05/11 12:54:53  chuck
 v 2.01 b2

 Revision 1.8  2000/04/07 17:05:15  chuck
 v 2.01 build 1

 Revision 1.7  2000/02/02 15:15:32  chuck
 no message

 Revision 1.6  1999/11/16 13:46:31  chuck
 no message

 Revision 1.5  1999/11/02 17:16:37  chuck
 2.00 Build 8

 Revision 1.4  1999/10/27 19:42:40  chuck
 Better cursor management

 Revision 1.3  1999/10/22 20:29:09  chuck
 New cursor management.

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
