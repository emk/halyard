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
#include "Index.h"

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

class Card : public Index 
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
        Card(IndexFile *inFile, const char *name = NULL, long p1 = 0, long p2 = 0);
        
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
		//
		virtual void	Return(void);
        
		//////////
		// Evaluate a given command. Trim opening paren and read opword.
		// Opword determines what we parse and then we call the appropriate
		// routine DoTheCommand...
		//
		//
        virtual void    DoCommand();
        
		//////////
		// Execute a single command, perhaps in response to a touch zone or
		// a timeout. Save the old m_Script, do the one command, and restore
		// the m_Script.
		//	
		//
		// [in] theCmd - should look like "(jump aCard)", ie both parens need to
		//				 be there.
		//
		virtual void    OneCommand(TString &theCmd);
        
		//////////
		// Adjust the global rect to local coordinates based on origin.
		//
		// [in] r - new rect
		//
		virtual void    AdjustRect(TRect *r);
        
		//////////
		// Adjust the global point to local coordinates based on origin
		//
		// [in] pt - new point
		//
		virtual void    AdjustPoint(TPoint *pt);

        //////////
		// Sets the card's local coordinate system.
		//
		// [in] loc - set the origin to this location
		//
		virtual void    SetOrigin(TPoint &loc);
		
		//////////
		// Sets the card's local coordinate system.
		//
		// [in] inX - X-coordinate
		// [in] inY - Y-coordinate
		//
		virtual void	SetOrigin(int16 inX, int16 inY);
        
		//////////
		// Offsets the card's local coordinate system by the amount given.
		//
		// [in] delta - amount to offset the origin
		//
		virtual void    OffsetOrigin(TPoint &delta);

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
		// Evaluate the given conditional and determine whether or not it is true.
		//
		// [in] conditional - the conditional input
		// [out] return - result of the conditional
		//
        int     Evaluate(LStream& conditional);

        //////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoAdd();
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoAudio();
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoAudioKill(void);
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoAudioVolume(void);
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoAudioWait(void);
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoBackground();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoBeep();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoBlippo();
        
		//void    DoBlueramp();

		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
        void    DoBox();
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoBrowse();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void  	DoButtpcx();
      	
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoCheckDisc();
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoCheckUrl();
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoCheckVol();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoClose();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoCTouch();
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoCursor();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoDiv();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoEjectDisc();

		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
        void    DoExit();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoFade();
        
		//void    DoFadeLock();
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoGlobalFiles();		//SS
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoHighlight();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoHidemouse();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoIf();

        //////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoInput();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoJump();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoKey();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoKeybind();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoKill();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoLine();

		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
        void    DoLoadpal();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoLoadpic();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoLock();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoLookup();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoMacro(TString &name);

		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
        void    DoMicro();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoNap();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoOpen();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoOrigin();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoOval();
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoPause();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoPlay();

		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoPlayQTFile();
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoPlayQTLoop();
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoQuitQTLoop();
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoPlayQTRect();
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoPreloadQTFile();

		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
        void    DoPrint();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoRead();
#ifdef DEBUG
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoReDoScript();
#endif
        //////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoResume();
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoReturn();
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoResetOrigin();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoRewrite();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoRnode();

		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
        void    DoScreen();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoSearch();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoSet();
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoSetWindowTitle();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoShowmouse();
		
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void	DoStill();	        
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoSub();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoText();

		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
        void    DoTimeout();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoTouch();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoUnblippo();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoUnlock();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoVideo();

		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
        void    DoWait();
        
		//////////
		// Refer to 5L Scriptor's Guide for details on this 5L command.
		//
		void    DoWrite();

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
class CardManager : public IndexManager 
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
		virtual void 	MakeNewIndex(IndexFile *inFile, const char *inName, 
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
#ifdef DEBUG
		void			DoReDoScript(TString &inCardName);
#endif

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

#ifdef DEBUG
		//////////
		// DEBUG ONLY - If true, re-parse the current script.
		//
		bool			m_ReDoScript;
		
		//////////
		// DEBUG ONLY - Name of the card to jump to after re-parsing the script.
		//
		TString			m_ReDoCardName;
#endif
		
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

#endif // _Card_h_

/*
 $Log$
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
