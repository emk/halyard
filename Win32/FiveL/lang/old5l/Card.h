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

#if !defined (_Card_h_)
#define _Card_h_
 
#include "TArray.h"
#include "TString.h"
#include "TRect.h"
#include "TPoint.h"
//#include "TDictionary.h"
#include "TIndex.h"

// globals
static bool	gNeedsRefresh;

/*-----------------------------------------------------------------

CLASS
    Card

	A Card is the fundamental unit of 5L.  A Card contains code and
	jumps between Cards makeup the program flow.

GLOBAL VARIABLES
	static bool	gNeedsRefresh - true when the graphics view needs a refresh

AUTHOR
	Chuck Officer<br>
	Sean Sharp

-----------------------------------------------------------------*/

class Card : public TIndex 
{
    public:
		//////////
		// Constructor.  Cards have a default origin of (0, 0)
		//
		// [in] infile - index file associated with this Card
		// [in_optional] name - name of the Card (default NULL)
		// [in_optional] p1 - starting index (in infile) (default 0)
		// [in_optional] p2 - ending index (in infile) (default 0)
		//
        Card(TIndexFile *inFile, const char *name = NULL, long p1 = 0, long p2 = 0);
        
		//////////
		// Get the card ready for execution
		//
		//
        void			Start(void);
        
		//////////
		// DEPRECATED
		//
		//
		void			Stop(void);
		
		//////////
		// Execute commands in the card until we pause or there aren't any more commands
		// or we are jumping somewhere.
		//
		//
        virtual void    Execute(void);
		
		//////////
		// Stop processing and return from the current script
		//
		virtual void	Return(void);
        
		//////////
		// Are we in the process of returning from this script?
		//
		virtual bool	IsReturning() { return m_stopped; }
        
		//////////
		// Evaluate a given command. Trim opening paren and read opword.
		// Opword determines what we parse and then we call the appropriate
		// routine DoTheCommand...
		//
        virtual void    DoCommand(TStream &inStream);
        
		//////////
		// Execute a single command, perhaps in response to a touch zone or
		// a timeout. Save the old m_Script, do the one command, and restore
		// the m_Script.
		//
		// [in] theCmd - should look like "(jump aCard)", ie both parens
		//               need to be there.
		//
		virtual void    OneCommand(const TString &theCmd);
        
	    //////////
	    // Run a list of commands.
		//
		// [in] inBody - The commands to run, with all parens.
	    //
		void			RunBody(const std::list<std::string> &inBody);

		//////////
		// Get the index of this Card
		//
		// [out] return - the index of this Card
		//
		int32			GetIndex(void) { return (m_index); }
		
		//////////
		// Set the index for this Card
		//
		// [in] index - the index
		//
		void			SetIndex(int32 index) { m_index = index;  }

    protected:
		//////////
		// Evaluate the given conditional and determine whether or not it
	    // is true.
		//
		// [in] inFormName - The name of form containing the conditional
		//                   (e.g., "if", "when", etc.).
		// [in] inConditional - The conditional statement to evaluate.
		// [out] return - The result of the conditional.
		//
        bool     EvaluateCondition(const char *inFormName,
								   const char *inConditional);

		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoMacro(TString &name, TStream &inArgs);

		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoIf(TStream &inArgs);

		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoBegin(TStream &inArgs);
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoWhen(TStream &inArgs);
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoUnless(TStream &inArgs);
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoWhile(TStream &inArgs);
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoReturn(TStream &inArgs);

		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoExit(TStream &inArgs);

	private:
        //////////
		// Local coordinate system for this card.
		//
		TPoint	m_origin;   
        
		//////////
		// Index where this card is located.
		//
		int32	m_index;
		
		//////////
		// Has execution of this card been halted?
		//
		bool	m_stopped;
};


/*-----------------------------------------------------------------

CLASS
    CardManager

	Manages the Card objects and the program flow between them.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class CardManager : public TIndexManager 
{
	public:
        
		//////////
		// Constructor.  Initialize the manager.
		//
		//
		CardManager();
        
        //////////
		// Toss all the cards in the tree and reset things.
		//
		//
		void			RemoveAll(void);				
        
		//////////
		// Processing time for card manager.
		//
		//
		void			Idle(void);
        
		//////////
		// Exit the current script.
		//
		// [in] inScript - Jump to another script on exit if >0 
		//					(specified in config file) 
		//
		void			DoExit(int32 inScript);
        				
        //////////
		// Create an index for a new Card and insert it into the card list. 
		//
		// [in] inFile - index file associated with the new Card
		// [in] inName - name of the new Card
		// [in] inStart - starting index (in inFile)
		// [in] inEnd - ending index (in inFile)
		//
		virtual void 	MakeNewIndex(TIndexFile *inFile, const char *inName, 
							long inStart, long inEnd);

        //////////
		// Get name of current Card.
		//
		// [out] return - the name of the current card.
		//
		const char 		*CurCardName(void);
        
		//////////
		// Get name of the previous Card.
		//
		// [out] return - the name of the previous card.
		//
		const char		*PrevCardName(void);
        
        //////////
		// Used by touch zones for executing a command in response to a button being hit.
		//
		// [in] inCmd - a single command to execute (e.g. "(jump aCard)" )
		//
		void			OneCommand(TString &inCmd);
        
        //////////
		// Pause the CardManager.  Use WakeUp() to resume.
		//
		//
		void			Pause(void);
        
		//////////
		// Wakeup from a Pause().
		//
		//
		void			WakeUp(void);
        
		//////////
		// Is the CardManager paused?
		//
		// [out] return - true if paused, false otherwise
		//
		bool			Paused(void) { return (m_Paused); }
        		
        //////////
		// Set the timeout timer. 
		//
		// [in] inName - name of Card to jump to after timeout.
		// [in] inTime - time in seconds (e.g. timeout in 30 sec)
		//
		void			Timeout(const char *inName, int32 inTime);
        
		//////////
		// Set the nap timer.
		//
		// [in] inTime - time in 1/10 seconds (e.g. 20 = timeout in 2 sec)
		//
		void			Nap(int32 inTime);        
        
		//////////
		// Is the CardManager napping?
		//
		// [out] return - true if napping, false otherwise
		//
		bool			Napping(void) { return (m_Napping); }
        
		//////////
		// Kill the nap timer.
		//
		void			KillNap(void);
       
        
		//////////
		// Get a Card by name.
		//
		// [in] inName - name of the Card
		// [out] return - pointer to a Card object
		//
		Card			*GetCard(const char *inName) { return ((Card *) Find(inName)); }
 
		//////////
		// Reload the current script (compiled into DEBUG builds only). 
		//
		// [in] inCardName - name of the Card to jump to after reload.
		//
		void			DoReDoScript(TString &inCardName);

		//////////
		// Jump to a card given its name.
		//
		// [in] inName - name of the Card to jump to.
		//
        void			JumpToCardByName(const char *inName);

        //////////
		// Jump to a Card by reference.
		//
		// [in] inCard - the Card to jump to.
		//
		void			JumpToCard(Card *inCard);
        
		//////////
		// Is the CardManager in the process of jumping?
		//
		// [out] return - true if jumping, false otherwise.
		//
		bool			Jumping(void) { return (m_HaveJump); }

    protected:
        //////////
		// Do we have a jump?
		//
		bool			m_HaveJump;
        
		//////////
		// Are we napping?
		//
		bool			m_Napping;
        
		//////////
		// Are we paused?
		//
		bool			m_Paused;
        
		//////////
		// Exit this script now?
		//
		bool			m_ExitNow;
        
		//////////
		// ID of script to jump to on exit, 0 to exit completely.
		//
		int32			m_ExitScript;

		//////////
		// If true, re-parse the current script.
		//
		bool			m_ReDoScript;
		
		//////////
		// Name of the card to jump to after re-parsing the script.
		//
		TString			m_ReDoCardName;
		
        //////////
		// Time which the timeout timer times out.
		//
		DWORD			m_TimeoutTime;
        
		//////////
		// Nap time.
		//
		DWORD			m_NapTime;
        
        //////////
		// Current Card.
		//
		Card    		*m_CurrentCard;
        
		//////////
		// Card to jump to if there is a jump.
		//
		Card			*m_JumpCard;
        
		//////////
		// Name of the previous Card.
		//
		TString			m_PrevCard;
        
		//////////
		// Card to jump to if a timeout occurs.
		//
        Card			*m_TimeoutCard;
 
		//////////
		// Our list of Card objects.
		//
 		TArray			m_CardList;

		//TDictionary<int, 20>	m_CommandMap;
};

//////////
// Global Card Manager
//
extern CardManager gCardManager;

#endif // _Card_h_

/*
 $Log$
 Revision 1.4.2.2  2002/08/14 22:30:10  emk
 3.4.1 - Bugfix: Commands with bodies now check for the "returning" flag
 correctly, even if they're within macros (Macro has its own return system
 which works slightly differently from Card's).

 Revision 1.4.2.1  2002/08/14 20:24:50  emk
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

 Revision 1.4  2002/06/20 16:32:54  emk
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

 Revision 1.3.6.3  2002/06/12 19:03:03  emk
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

 Revision 1.3.6.2  2002/06/11 18:15:43  emk
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

 Revision 1.2.2.2  2002/05/01 03:27:07  emk
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

 Revision 1.1  2001/09/24 15:11:00  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.7  2000/08/08 19:03:40  chuck
 no message

 Revision 1.6  2000/05/11 12:54:54  chuck
 v 2.01 b2

 Revision 1.5  2000/04/07 17:05:15  chuck
 v 2.01 build 1

 Revision 1.4  1999/11/16 13:46:32  chuck
 no message

 Revision 1.3  1999/10/27 19:42:40  chuck
 Better cursor management

 Revision 1.2  1999/09/24 19:57:18  chuck
 Initial revision

*/
