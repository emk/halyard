//
//	CCard.h - The card class. Cards receive processing time (they
//				are LPeriodicals) when they are active and execute
//				commands.
//
//				A card can have two timers going at any given time.
//				The napTimer will suspend the execution of commands
//				until it has timed out.
//				The timeoutTimer does not suspend execution but will
//				jump to another card when it has timed out.
//
//				It is (in theory) possible to deactivate one card
//				to execute commands on another and then reactivate
//				the original card and pick up where it left off.
//				This is not currently being used.
//		
//			Enhancements:
//				1. Make it possible to backtrack through the
//				commands on the current card (through cards too?).
//

#ifndef _H_CCARD
#define _H_CCARD

#include "THeader.h"

#include "TIndex.h"
#include "TRect.h"
#include "CTimer.h"
#include "TArray.h"

BEGIN_NAMESPACE_FIVEL

class CCard : public TIndex
{
    private:

        TPoint  mOrigin;
        bool	mPaused;
		bool	mActive;
		bool	mDoingOne;
		bool	mResumeMovie;
		bool	mStopped;
		
		// cbo 
		int32	mIndex;
		
		CTimer	*mTimeoutTimer;
		CTimer	*mNapTimer;
		
    public:
    			CCard(TIndexFile *inFile, const char *inName = NULL,
    					int32 inStart = 0, int32 inEnd = 0);
        		//CCard(const char *name = 0, int32 p1 = 0, int32 p2 = 0);
				~CCard();
						
        void	SpendTime(void);
		void  	Execute(void);
        
        void	Start(void);
        void	Stop(void);
		
		void	WakeUp(void);
		bool	Napping(void) { return ((mNapTimer != NULL) and (mPaused)); }
        bool	Paused(void) { return (mPaused); }
  
  		int32	Index(void) { return (mIndex); }      
        void	SetIndex(int32 index) { mIndex = index; }
        
        void    DoCommand();
        void    OneCommand(TString &theCmd);
        void    AdjustRect(TRect *r);
        void    AdjustPoint(TPoint *pt);

        void    SetOrigin(TPoint &loc);
        void	SetOrigin(int32 inX, int32 inY);
        void    OffsetOrigin(TPoint &delta);

    protected:

        int16   Evaluate(TStream& conditional);
		
		// utility methods
		void    UpdateSpecialVariablesForGraphic(TRect bounds);

        void    DoAdd();
        // new audio commands
        void	DoAudio(void);
        void 	DoAudioKill(void);
        void	DoAudioPlay(void);
        void	DoAudioVolume(void);
        void	DoAudioWait(void);
        // end of new audio commands
		void	DoBackground();
        void    DoBeep();
        void    DoBlippo();
        void    DoBlueramp();
        void    DoBox();
        void    DoBrowse();
        void  	DoButtpcx();
		void	DoCheckVol();
        void    DoClose();
        void    DoCTouch(); 
        void	DoCursor();
#ifdef DEBUG
        void	DoDebug();	// cbo_fix
#endif
        void    DoDiv();
        void	DoEjectDisc();
        void    DoExit();
        void    DoFade();
        void    DoHighlight();
        void  	DoHidemouse();
        void    DoIf();
        void    DoInitldp();

        void    DoInput();
        void    DoJump();
        void    DoKey();
        void    DoKeybind();
        void	DoKill();
        void    DoLine();

        void    DoLoadpal();
        void    DoLoadpic();
        void    DoLock();
        void    DoLookup();
        void    DoMacro(TString &name);

        void    DoMicro();
        void    DoNap();
        void    DoOpen();
        void    DoOrigin();
        void    DoOval();
		void	DoPause();
        void    DoPlay();
		void	DoPlayQTFile();
		void	DoPlayQTLoop();
		void	DoPlayQTRect();
		void	DoPreloadQTFile();
        void    DoPrint();
        void	DoQTPause();
        void    DoRead();
#ifdef DEBUG
		void	DoReDoScript();
#endif
		void	DoRefresh();
		void	DoResetOrigin();		
        void    DoResume();
        void	DoReturn();
        void    DoRewrite();
        void    DoRnode();

        void    DoScreen();
        void    DoSearch();
        void    DoSet();
        void    DoShowmouse();
		void	DoStill();
        void    DoSub();
        void    DoText();
        void	DoTextAA();
        void    DoTimeout();
        void    DoTouch();
        void    DoUnblippo();
        void    DoUnlock();
        void    DoVideo();
        void    DoWait();
        void    DoWrite();

};

class CCardManager : public TIndexManager 
{
    private:
    	bool			mExitNow;
    	bool			mHaveJump;
    	int16			mExitSide;
        CCard    		*mCurrentCard;
        CCard			*mJumpCard;
        TString			mPrevCard;
        
        TArray			mCardList;	// cbo - list of cards in order
#ifdef DEBUG
		bool			mReDoScript;
		TString			mReDoCard;
#endif

    public:
        				CCardManager();

		virtual void	MakeNewIndex(TIndexFile *inFile, const char *inName,
									int32 inStart, int32 inEnd);
        
        virtual void	RemoveAll();	// cbo - 
       
	    virtual const char 	*CurCardName(void);
	    const char		*PrevCardName(void);
	    const char		*BeforeCardName(void);
	    const char		*AfterCardName(void);
	    
        CCard			*GetCurCard(void);		// need??
        CCard			*GetCard(const char *cardName) { return ((CCard *) Find(cardName)); }
		
		void			DoOneCommand(TString &theCommand);
		
		void			CurCardSpendTime(void);
		void			CurCardKill(void);
		void			CurCardWakeUp(void);
		bool			CurCardNapping(void);
		bool			CurCardPaused(void);
		void			DoExit(int16 inSide);
#ifdef DEBUG
		void			DoReDoScript(TString &cardName);
#endif
		
        void			JumpToCardByName(const char *newCardName, bool comeBack);
        void			JumpToCard(CCard *newCard, bool comeBack);
		bool			Jumping(void) { return (mHaveJump); }
		
		//////////
		// A SpecialVariableFunction to get the _curcard variable.
		//
		static TString	ReadSpecialVariable_curcard();
		
		//////////
		// A SpecialVariableFunction to get the _prevcard variable.
		//
		static TString	ReadSpecialVariable_prevcard();
};

extern CCardManager gCardManager;
END_NAMESPACE_FIVEL
#endif
