// -*- Mode: C++; tab-width: 4; -*-
/**********************************************

    CCard class. This is the class that knows
    how to execute commands.

***********************************************/

#include "THeader.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "TLogger.h"
#include "TRect.h"
#include "TPoint.h"

#include "CMac5LApp.h"
#include "CCard.h"
#include "TVariable.h"
#include "CResource.h"
#include "CMacroManager.h"
#include "CHeader.h"
#include "CFiles.h"
#include "CPicture.h"
#include "CPlayerView.h"
#include "CMoviePlayer.h"
#include "CPlayerText.h"
#include "CPlayerBox.h"
#include "CPlayerLine.h"
#include "CPlayerOval.h"
#include "CPlayerPict.h"
#include "CPlayerView.h"
#include "CPlayerInput.h"
#include "CTouchZone.h"
#include "CModule.h"
#include "TDateUtil.h"
#include "GraphicsTools.h"
#include "TStyleSheet.h"
#include "TException.h"
#include "TMac5LInterpreter.h"
#include "TPrimitives.h"
#include "TCommonPrimitives.h"

#include "gamma.h"

// cbo_debug
//#include <profiler.h>

#include <UInternetConfig.h>

USING_NAMESPACE_FIVEL

/**************

    GLOBALS

**************/

CCardManager FIVEL_NS gCardManager;

static Boolean gNeedsRefresh = false;


/*******************

    CARD METHODS

*******************/

//
//  CCard - Initialize a card. This will happen when the m_Script is read
//			in from disk so don't activate the card yet.
//
CCard::CCard(TIndexFile *inFile, const char *inName /* = NULL */,
				int32 inStart /* = 0 */, int32 inEnd /* = 0  */)
	: TIndex(inFile, inName, inStart, inEnd)
{
	mPaused = false;
	mActive = false;
	mDoingOne = false;
	mResumeMovie = false;
	
	mIndex = -1;
	
	mTimeoutTimer = NULL;
	mNapTimer = NULL;
	
	gOrigin.SetOrigin(0, 0);
}

CCard::~CCard()
{
}

//
//	Start - Start the card. This will initialize everything. 
//
void CCard::Start(void)
{
	gDebugLog.Log("Start card <%s>", Name());
	gPlayerView->AdjustMyCursor();

	mPaused = false;
	mResumeMovie = false;
	mStopped = false;
	
	SetScript();					// load in the m_Script from the file
	m_Script.reset();					// reset the m_Script
	gOrigin.SetOrigin(0, 0);			
	mActive = true;

	// dump the card out to the debug file
	gDebugLog.Log("%s", m_Script.GetString());
	
	m_Script >> open >> discard >> discard;	// remove leading junk
	gNeedsRefresh = true;		// Should this be here or below
}

//
//	Stop - Stop the card. Can't reactivate from this, have to start
//			over from the beginning of the card.
//
void CCard::Stop(void)
{
	// A movie can't play without its card.
	if (gMovieManager.Playing())
		gMovieManager.Kill();
	
	mActive = false;
	
	if (mTimeoutTimer != NULL)
	{
		delete mTimeoutTimer;
		mTimeoutTimer = NULL;
	}
	
	if (mNapTimer != NULL)
	{
		delete mNapTimer;
		mNapTimer = NULL;
	}
}

//
//	SpendTime - Processing time for the card.
//
void CCard::SpendTime(void)
{
	if (mActive)
		Execute();
}

//
//	Execute - Execute commands on the card.
//
void CCard::Execute(void)
{
	CCard	*jumpCard;
	
	if (mTimeoutTimer != NULL)
	{
		if (mTimeoutTimer->HitTimer())
		{
			gDebugLog.Log("Hit timeout timer, jumping to card");
			jumpCard = (CCard *) mTimeoutTimer->GetUserData();
			if (jumpCard != NULL)
				gCardManager.JumpToCard(jumpCard, false);
				
			delete mTimeoutTimer;
			mTimeoutTimer = NULL;
		}
	}
	
	if (mNapTimer != NULL)
	{
		if (mNapTimer->HitTimer())
		{
			gDebugLog.Log("Hit the end of the nap, resuming");
			mPaused = false;
			
			delete mNapTimer;
			mNapTimer = NULL;
			
			if (mResumeMovie)
			{
				gPlayerView->DoResume(false);
				mResumeMovie = false;
			}
		}
	}
	
	if (not mPaused)
	{
		if (m_Script.more())
		{
			// cbo_debug
			//ProfilerSetStatus(true);
			
			// Process all commands as long as we haven't paused or don't jump
			//	somewhere else.
			while ((mActive) 
				and (m_Script.more()) 
				and (not mPaused)
				and (not mStopped)				// return command stops execution
				and (not gCardManager.Jumping()))
					DoCommand(m_Script);
					
			// cbo_debug
			//ProfilerSetStatus(false);
		}
		else if (gNeedsRefresh)
		{
			gPlayerView->AdjustMyCursor();
			
			gPlayerView->Draw(nil);
			gNeedsRefresh = false;
			
			if (not mDoingOne)
				gPlayerView->ProcessEvents(true);		// allow touchzones and keybinds
		}
	}
}

void CCard::Nap(int32 inTenths)
{
	Pause();
	if (!mNapTimer)
		mNapTimer = new CTimer(inTenths * 100, NULL);
}

//
//	WakeUp
//	
void CCard::WakeUp(void)
{
	mPaused = false;
}

void CCard::Timeout(int32 inSeconds, const char *inCardName)
{
	// Look up the card to jump to after our timeout.
	CCard *card = gCardManager.GetCard(inCardName);
	if (!card)
		throw TException(__FILE__, __LINE__, "Timeout card does not exist");

	// Install our timer.
	if (!mTimeoutTimer)
		mTimeoutTimer = new CTimer(inSeconds * 1000, (void*) card);
}


/***********************************************************************
 * Function: CCard::DoCommand
 *
 *  Parameter (null)
 * Return:
 *
 * Comments:
 *  Evaluate a given command. Trim opening paren and read opword.
 *  Opword determines what we parse and then we call the appropriate
 *  routine DoTheCommand...
 ***********************************************************************/
void CCard::DoCommand(TStream &inScript)
{
    TString     opword;

    inScript >> open >> opword;
    opword.MakeLower();
	
	try
	{
		if (opword == (char *)"if") DoIf(inScript);
		else if (opword == (char *)"begin") DoBegin(inScript);
		else if (opword == (char *)"when") DoWhen(inScript);
		else if (opword == (char *)"unless") DoUnless(inScript);
		else if (opword == (char *)"while") DoWhile(inScript);
		else if (opword == (char *)"return") DoReturn(inScript);
		else if (opword == (char *)"exit") DoExit(inScript);
		else if (gPrimitiveManager.DoesPrimitiveExist(opword.GetString()))
			gPrimitiveManager.CallPrimitive(opword.GetString(), inScript);
		else DoMacro(opword, inScript);
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
	inScript >> close;
}

/***********************************************************************
 * Function: CCard::OneCommand
 *
 *  Parameter theCommand
 * Return:
 *
 * Comments:
 *  Execute a single command, perhaps in response to a touch zone or
 *  a timeout.
 *
 *  theCommand should look like "(jump aCard)", ie both parens need to
 *  be there.
 ***********************************************************************/
void CCard::OneCommand(const TString &theCommand)
{
	StValueRestorer<bool> restore_DoingOne(mDoingOne);
	mDoingOne = true;

	TStream script(theCommand);
	DoCommand(script);
}

// Run a list of commands.
void CCard::RunBody(const std::list<std::string> &inBody)
{
	for (std::list<std::string>::const_iterator iter = inBody.begin();
		 iter != inBody.end() && !mStopped && !gCardManager.Jumping();
		 ++iter)
	{
		OneCommand(iter->c_str());
	}
}


/************************

    PROTECTED METHODS

************************/


/*************************

    5L UTILITY METHODS

*************************/
bool CCard::EvaluateCondition(const char *inFormName,
							  const char *inConditional)
{
	TStream conditional_stream(inConditional);
	bool result;
	conditional_stream >> result;
	gDebugLog.Log("*** %s: %s -> %s", inFormName, inConditional,
				  result ? "true" : "false");
	return result;
}

/*-------------------------------------------------------------------
    (MACRONAME <X Y> <VAR>...)

    Call the macro by name. X, Y is the mOrigin to use for the macro.
    VAR are an optional number of local variables that vary depending
    upon the particular macrodef.
---------------------------------------------------------------------*/
void CCard::DoMacro(TString &name, TStream &inArgs)
{
    TIndex		*theMacro;
    TString		vname, contents;
    int16		vnum;
    TVariable	*local, *temp;

    theMacro = (TIndex *) gMacroManager.Find(name);
	
	if (theMacro == NULL)
	{
        gLog.Caution("Couldn't find macro/command <%s>.", (const char *) name);
        gDebugLog.Caution("Couldn't find macro/command <%s>.",
						  (const char *) name);
        return;
	}

    //
    //  Get the local variables, if passed.
    //
    local = 0;
    vnum = 0;
    
    TString arg_string = "Macro arguments:";
    while (inArgs.more()) 
	{
        //  Variables are named 1, 2, 3...
        //
        vname = ++vnum;
        inArgs >> contents;

		arg_string += (TString(" $") + TString::IntToString(vnum) +
					   TString(" = <") + contents + TString(">"));
        temp = new TVariable(vname, contents);

        if (local == 0) 
			local = temp;
        else 
			local->Add(temp);
    }
    if (vnum > 0)
		gDebugLog.Log("%s", arg_string.GetString());

	// Save our card script, local variable tree and origin.
	TStream saveScript = m_Script;
	TVariable *oldlocal = gVariableManager.GetLocal();
	TPoint saveOrigin = gOrigin.GetOrigin();
	try
	{
		//  Save old local tree and set current local tree to ours.
		gVariableManager.SetLocal(local);
		
		// We have already saved this card's m_Script, now set it to 
		// the macro and execute the commands on it. 
		m_Script = theMacro->GetScript();
	
		m_Script.reset();					// start at the beginning

		// dump the macro
		gDebugLog.Log("macro: %s", m_Script.GetString());	 
	
		m_Script >> open >> discard >> discard; // toss (macrodef name
		
		mStopped = false;
		while (mActive            // used to only check for m_Script.more()
			   && m_Script.more()
			   && !mStopped   // stop on return
			   // Don't check mPaused here because we could be waiting for audio
			   && !gCardManager.Jumping())
		{
			DoCommand(m_Script);
		}
	}
	catch (...)
	{
		// Unstop the main card; restore our saved script, origin
		// and local variable tree; delete the local variable tree
		// we just used.  MATCHES CLEANUP CODE BELOW.
		mStopped = false;
		m_Script = saveScript;
		gOrigin.SetOrigin(saveOrigin);
		gVariableManager.SetLocal(oldlocal);
		if (vnum > 0) 
			local->RemoveAll();

		// Rethrow our exception.
		throw;
	}

	// Unstop the main card; restore our saved script, origin
	// and local variable tree; delete the local variable tree
	// we just used.  MATCHES CLEANUP CODE ABOVE.
	mStopped = false;
	m_Script = saveScript;
	gOrigin.SetOrigin(saveOrigin);
	gVariableManager.SetLocal(oldlocal);
	if (vnum > 0) 
		local->RemoveAll();
}

/*-----------------------------------------------------------------
    (IF (CONDITIONAL) (true_CMD) <(false_CMD)>)

    Evaluate the conditional expression and execute the appropriate
    command based on the value of the expression.
-------------------------------------------------------------------*/
void CCard::DoIf(TStream &inArgs)
{
	std::string condition = inArgs.GetStatement();
	std::string then_statement = inArgs.GetStatement();
	bool have_else_statement = false;
	std::string else_statement;
	if (inArgs.more())
	{
		else_statement = inArgs.GetStatement();
		have_else_statement = true;
	}
	
	if (EvaluateCondition("if", condition.c_str()))
		OneCommand(then_statement.c_str());
	else if (have_else_statement)
		OneCommand(else_statement.c_str());
}

/*-----------------------------------------------------------------
    (BEGIN body...)

    Evaluate zero or more commands in sequence.  The BEGIN command
    can be used to pass a list of commands as an argument to the
    IF, BUTTPCX and TOUCH commands.
-------------------------------------------------------------------*/
void CCard::DoBegin(TStream &inArgs)
{
	std::list<std::string> body;
	inArgs.GetBody(body);
	RunBody(body);
}

/*-----------------------------------------------------------------
    (WHEN cond body...)

    Evaluate body if cond is true.
-------------------------------------------------------------------*/
void CCard::DoWhen(TStream &inArgs)
{
	std::string condition = inArgs.GetStatement();
	std::list<std::string> body;
	inArgs.GetBody(body);

	if (EvaluateCondition("when", condition.c_str()))
		RunBody(body);
}

/*-----------------------------------------------------------------
    (UNLESS cond body...)

    Evaluate body if cond is false.
-------------------------------------------------------------------*/
void CCard::DoUnless(TStream &inArgs)
{
	std::string condition = inArgs.GetStatement();
	std::list<std::string> body;
	inArgs.GetBody(body);

	if (!EvaluateCondition("unless", condition.c_str()))
		RunBody(body);
}

/*-----------------------------------------------------------------
    (WHILE cond body...)

    Evaluate body while cond is true.
-------------------------------------------------------------------*/
void CCard::DoWhile(TStream &inArgs)
{
	std::string condition = inArgs.GetStatement();
	std::list<std::string> body;
	inArgs.GetBody(body);
	
	while (EvaluateCondition("while", condition.c_str()))
		RunBody(body);
}

/*-------------------
    (EXIT)

    Exit the program.
---------------------*/
void CCard::DoExit(TStream &inArgs)
{
	int16	theSide = 0;
	
    if (inArgs.more())
		gLog.Error("Switching scripts is no longer supported.");

	gDebugLog.Log("exit: %d", theSide);
		
	TInterpreterManager::GetInstance()->RequestQuitApplication();
}


//
//	(return) - stop execution of this card or macro
//
void CCard::DoReturn(TStream &inArgs)
{
	if (inArgs.more())
	{
		TString returnval;
		inArgs >> returnval;
		::SetPrimitiveResult(returnval.GetString());
	}

	gDebugLog.Log("return");
	mStopped = true;
}


/***************************

    CARD MANAGER METHODS

***************************/

CCardManager::CCardManager() : TIndexManager()
{
    mCurrentCard = NULL;
    mHaveJump = false;
}


/***********************************************************************
 * Function: CCardManager::CurCardName
 *
 *  Parameter (null)
 * Return:
 *   Name of "mCurrentCard" (Name() being protected...)
 * Comments:
 *
 ***********************************************************************/
const char *CCardManager::CurCardName()
{
    ASSERT(mCurrentCard != NULL);

    return (mCurrentCard->Name());
}

//
//	PrevCardName - Return the name of the previously executing card.
//
const char *CCardManager::PrevCardName()
{
	return ((const char *) mPrevCard);
}

//
//	BeforeCardName - Return the name of the card before this one in the
//		.scr file.
const char *CCardManager::BeforeCardName(void)
{
	CCard	*theCard = NULL;
	const char	*retStr = NULL;
	int32	index;
	
	ASSERT(mCurrentCard != NULL);
	
	index = mCurrentCard->Index();
	if (mCardList.ValidIndex(index))
	{
		if (mCardList.ValidIndex(index - 1))
		{
			theCard = (CCard *) mCardList.Item(index - 1);
			if (theCard != NULL)
				retStr = theCard->Name();
		}
	}
	
	return (retStr);
}

//
//	AfterCardName - Return the name of the card after this one in the
//		.scr file.
//
const char *CCardManager::AfterCardName(void)
{
	CCard	*theCard = NULL;
	const char	*retStr = NULL;
	int32	index;
	
	ASSERT(mCurrentCard != NULL);
	
	index = mCurrentCard->Index();
	if (mCardList.ValidIndex(index))
	{
		if (mCardList.ValidIndex(index + 1))
		{
			theCard = (CCard *) mCardList.Item(index + 1);
			if (theCard != NULL)
				retStr = theCard->Name();
		}
	}
	
	return (retStr);
}	
	
//
//	GetCurCard - Return the current card.
//
CCard *CCardManager::GetCurCard(void)
{
	ASSERT(mCurrentCard != NULL);
	
	return (mCurrentCard);
	
}

//
//	DoOneCommand
//
void CCardManager::DoOneCommand(const TString &theCommand)
{
	ASSERT(mCurrentCard != NULL);
	
	mCurrentCard->OneCommand(theCommand);
}

// 
//	CurCardSpendTime
//
void CCardManager::CurCardSpendTime(void)
{
	TVariable	*theAfterVar;
	TVariable	*theBeforeVar;
	CCard		*theCard;
	int32		index;
	
	if (mHaveJump)
	{
		if (mCurrentCard != NULL)
		{
			mPrevCard = mCurrentCard->Name();
			mCurrentCard->Stop();
		}	
			
		mCurrentCard = mJumpCard;
		mCurrentCard->Start();
		
		// set the before and after variables
		theBeforeVar = gVariableManager.FindVariable("_BeforeCard", FALSE);
		theBeforeVar->SetLong(0);
		
		theAfterVar = gVariableManager.FindVariable("_AfterCard", FALSE);
		theAfterVar->SetLong(0);
		
		index = mCurrentCard->Index();
		if (mCardList.ValidIndex(index))
		{
			if (mCardList.ValidIndex(index - 1))
			{
				theCard = (CCard *) mCardList.Item(index - 1);
				if (theCard != NULL)
					theBeforeVar->SetString(theCard->Name());
			}
			
			if (mCardList.ValidIndex(index + 1))
			{
				theCard = (CCard *) mCardList.Item(index + 1 );
				if (theCard != NULL)
					theAfterVar->SetString(theCard->Name());
			}
		}
		
		mHaveJump = false;
		return;			// go back and check for events
	}
	
	if (mCurrentCard != NULL)
	{	
		mCurrentCard->SpendTime();
	}
}

//
// CurCardKill
//
void CCardManager::CurCardKill(void)
{
	if (mCurrentCard != NULL)
		mCurrentCard->Stop();
		
	mCurrentCard = NULL;
	mPrevCard = "";
}

// 
//	CurCardWakeUp
//
void CCardManager::CurCardWakeUp(void)
{
	if (mCurrentCard != NULL)
	{
		mCurrentCard->WakeUp();
	}
}

//
//	CurCardNapping
//
bool CCardManager::CurCardNapping(void)
{
	if (mCurrentCard != NULL)
		return (mCurrentCard->Napping());
	
	return (false);
}

//
// CurCardPaused
//
bool CCardManager::CurCardPaused(void)
{
	if (mCurrentCard != NULL)
		return (mCurrentCard->Paused());
	return (false);
}

/***********************************************************************
 * Function: CCardManager::ProcessTopLevelForm
 *
 *  Parameter name
 *  Parameter start
 *  Parameter end
 * Return:
 *
 * Comments:
 *    Adds node "name" to CCardManager..
 ***********************************************************************/
void CCardManager::ProcessTopLevelForm(TIndexFile *inFile, const char *inName,
								       int32 inStart, int32 inEnd)
{
    CCard    	*newCard;
    int32		index;

    newCard = new CCard(inFile, inName, inStart, inEnd);
    
#ifdef DEBUG
	// when debugging, read the m_Script into memory so that the m_Script file
	//	can be modified without messing up the index information
	newCard->SetScript();
#endif
	
    Add(newCard);
    
    // add the card to our array of cards
    index = mCardList.Add(newCard);
    newCard->SetIndex(index);
}

//
//	RemoveAll - We have to toss the array of cards too.
//
void CCardManager::RemoveAll()
{
	mCardList.RemoveAll();
	TBTree::RemoveAll();
}

//
//  JumpToCardByName - Jump to a new card given its name.
//
void CCardManager::JumpToCardByName(const char *newCardName, bool comeBack)
{
	CCard	*theCard = NULL;
	
	theCard = GetCard(newCardName);
	if (theCard == NULL)
	{
		gDebugLog.Log("Trying to jump to <%s>, couldn't find it", newCardName);
		gLog.Caution("Trying to jump to <%s>, couldn't find it", newCardName);
	}
	else
		JumpToCard(theCard, comeBack);
}

//
//	JumpToCard - Jump to a new card.
//
void CCardManager::JumpToCard(CCard *newCard, bool /* comeBack */)
{
    if (newCard == NULL)
    	gLog.Caution("Trying to jump to a null card!");
    else
    {	
		mJumpCard = newCard;
		mHaveJump = true;
	}
}


