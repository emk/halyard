// -*- Mode: C++; tab-width: 4; -*-
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
		
		void	Nap(int32 inTenths);
		void	WakeUp(void);
		bool	Napping(void) { return ((mNapTimer != NULL) and (mPaused)); }
		void	Timeout(int32 inSeconds, const char *inCardName);
        void	Pause(void) { mPaused = true; }
        bool	Paused(void) { return (mPaused); }
  
  		int32	Index(void) { return (mIndex); }      
        void	SetIndex(int32 index) { mIndex = index; }
        
        void    DoCommand(TStream &inScript);
        void    OneCommand(const TString &theCmd);
		void	RunBody(const std::list<std::string> &inBody);

    protected:
        bool    EvaluateCondition(const char *inFormName,
								  const char *inConditional);
        void    DoIf(TStream &inArgs);
        void    DoBegin(TStream &inArgs);
        void    DoWhen(TStream &inArgs);
        void    DoUnless(TStream &inArgs);
        void    DoWhile(TStream &inArgs);
        void    DoExit(TStream &inArgs);
        void	DoReturn(TStream &inArgs);
		void	DoMacro(TString &name, TStream &inArgs);
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
	    
		bool			CardManagerReady() { return mCurrentCard != NULL; }
        CCard			*GetCurCard(void);		// need??
        CCard			*GetCard(const char *cardName) { return ((CCard *) Find(cardName)); }
		
		void			DoOneCommand(const TString &theCommand);
		
		void			CurCardSpendTime(void);
		void			CurCardKill(void);
		void			CurCardWakeUp(void);
		bool			CurCardNapping(void);
		bool			CurCardPaused(void);
		void			DoExit(int16 inSide);
#ifdef DEBUG
		void			DoReDoScript(const TString &cardName);
#endif
		
        void			JumpToCardByName(const char *newCardName, bool comeBack);
        void			JumpToCard(CCard *newCard, bool comeBack);
		bool			Jumping(void) { return (mHaveJump); }
};

extern CCardManager gCardManager;
END_NAMESPACE_FIVEL
#endif
