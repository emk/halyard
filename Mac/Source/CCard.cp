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

#include "gamma.h"

// cbo_debug
//#include <profiler.h>

#include <UInternetConfig.h>

#include "CMenuUtil.h"

USING_NAMESPACE_FIVEL

/**************

    GLOBALS

**************/

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
	
	SetOrigin(0, 0);
}

CCard::~CCard()
{
}

//
//	Start - Start the card. This will initialize everything. 
//
void CCard::Start(void)
{
#ifdef DEBUG
	gDebugLog.Log("Start card <%s>", Name());
#endif
	gPlayerView->AdjustMyCursor();

	mPaused = false;
	mResumeMovie = false;
	mStopped = false;
	
	SetScript();					// load in the m_Script from the file
	m_Script.reset();					// reset the m_Script
	SetOrigin(0, 0);			
	mActive = true;

#ifdef DEBUG
	// dump the card out to the debug file
	gDebugLog.Log("%s", m_Script.GetString());
#endif
	
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
#ifdef DEBUG
			gDebugLog.Log("Hit timeout timer, jumping to card");
#endif
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
#ifdef DEBUG
			gDebugLog.Log("Hit the end of the nap, resuming");
#endif
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
					DoCommand();
					
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

//
//	WakeUp
//	
void CCard::WakeUp(void)
{
	mPaused = false;
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
void CCard::DoCommand(void)
{
    TString     opword;

    m_Script >> open >> opword;
    opword.MakeLower();
	
    if (opword == (char *)"add") DoAdd();
    // new audio commands
    else if (opword.Equal("audio")) DoAudio();
    else if (opword == (char *)"audiokill") DoAudioKill();
    else if (opword == (char *)"audioplay") DoAudioPlay();
    else if (opword == (char *)"audiovolume") DoAudioVolume();
    else if (opword == (char *)"audiowait") DoAudioWait();
    // end of new audio commands
    else if (opword == (char *)"background") DoBackground();
    else if (opword == (char *)"beep") DoBeep();
    else if (opword == (char *)"blippo") DoBlippo();
    else if (opword == (char *)"blueramp") DoBlueramp();
    else if (opword == (char *)"box") DoBox();
    else if (opword == (char *)"buttpcx") DoButtpcx();
    else if (opword == (char *)"checkdisc") DoCheckDisc();
    else if (opword.Equal("checkvol")) DoCheckVol();
    else if (opword == (char *)"close") DoClose();
    else if (opword == (char *)"ctouch") DoCTouch();
    else if (opword == (char *)"cursor") DoCursor();
#ifdef DEBUG
	else if (opword == (char *)"debug") DoDebug();
#endif
    else if (opword == (char *)"div") DoDiv();
    else if (opword == (char *)"ejectdisc") DoEjectDisc();
    else if (opword == (char *)"exit") DoExit();
    else if (opword == (char *)"fade") DoFade();
    else if (opword == (char *)"highlight") DoHighlight();
    else if (opword == (char *)"hidemouse") DoHidemouse();
    else if (opword == (char *)"if") DoIf();
    else if (opword == (char *)"input") DoInput();
    else if (opword == (char *)"jump") DoJump();
    else if (opword == (char *)"key") DoKey();
    else if (opword == (char *)"keybind") DoKeybind();
    else if (opword == (char *)"kill") DoKill();
    else if (opword == (char *)"line") DoLine();
    else if (opword == (char *)"loadpal") DoLoadpal();
    else if (opword == (char *)"loadpic" || opword == (char *)"loadpick") DoLoadpic();
    else if (opword == (char *)"lock") DoLock();
    else if (opword == (char *)"lookup") DoLookup();
    else if (opword == (char *)"micro") DoMicro();
    else if (opword == (char *)"nap") DoNap();
    else if (opword == (char *)"open") DoOpen();
    else if (opword == (char *)"origin") DoOrigin();
    else if (opword == (char *)"circle") DoOval();
    else if (opword == (char *)"oval") DoOval();
    else if (opword == (char *)"pause") DoQTPause();
    else if (opword == (char *)"play") DoPlay();
    else if (opword == (char *)"playqtfile") DoPlayQTFile();
    else if (opword == (char *)"playqtloop") DoPlayQTLoop();
    else if (opword == (char *)"playqtrect") DoPlayQTRect();
    else if (opword == (char *)"preload") DoPreloadQTFile();
    else if (opword == (char *)"print") DoPrint();
   // else if (opword == (char *)"qtpause") DoQTPause();
    else if (opword == (char *)"read") DoRead();
#ifdef DEBUG
	else if (opword == (char *)"redoscript") DoReDoScript();
#endif
//	else if (opword == (char *)"refresh") DoRefresh();
	else if (opword == (char *)"resetorigin") DoResetOrigin();
    else if (opword == (char *)"resume") DoResume();
    else if (opword.Equal("return")) DoReturn();
    else if (opword == (char *)"rewrite") DoRewrite();
    else if (opword == (char *)"rnode" || opword == (char *)"rvar") DoRnode(); 
    else if (opword == (char *)"screen") DoScreen();
    else if (opword == (char *)"search") DoSearch();
    else if (opword == (char *)"set") DoSet();
    else if (opword == (char *)"showmouse") DoShowmouse();
    else if (opword == (char *)"still") DoStill();
    else if (opword == (char *)"sub") DoSub();
    else if (opword == (char *)"text") DoText();
    else if (opword == (char *)"textaa") DoTextAA();
    else if (opword == (char *)"timeout") DoTimeout();
    else if (opword == (char *)"touch") DoTouch();
    else if (opword == (char *)"unblippo") DoUnblippo();
    else if (opword == (char *)"unlock") DoUnlock();
    else if (opword == (char *)"video") DoPlayQTFile();
    else if (opword == (char *)"wait") DoWait();
    else if (opword == (char *)"write") DoWrite();
    else if (opword == (char *)"browse") DoBrowse();
    else DoMacro(opword);

    m_Script >> close;
}

/***********************************************************************
 * Function: CCard::OneCommand
 *
 *  Parameter theCommand
 * Return:
 *
 * Comments:
 *  Execute a single command, perhaps in response to a touch zone or
 *  a timeout. Save the old m_Script, do the one command, and restore
 *  the m_Script.
 *
 *  theCommand should look like "(jump aCard)", ie both parens need to
 *  be there.
 ***********************************************************************/
void CCard::OneCommand(TString &theCommand)
{
    TStream     saveScript(m_Script);

	saveScript = m_Script;
	mDoingOne = true;
	
    m_Script = theCommand;
    DoCommand();
    
    m_Script = saveScript;

    mDoingOne = false;
}

/***********************************************************************
 * Function: CCard::AdjustRect
 *
 *  Parameter r
 * Return:
 *
 * Comments:
 *  Adjust the global rect to local coordinates based on mOrigin.
 ***********************************************************************/
void CCard::AdjustRect(TRect *r)
{
	r->Offset(mOrigin);
}

/***********************************************************************
 * Function: CCard::AdjustPoint
 *
 *  Parameter pt
 * Return:
 *
 * Comments:
 *  Adjust the global point to local coordinates based on mOrigin.
 ***********************************************************************/
void CCard::AdjustPoint(TPoint *pt)
{
	pt->Offset(mOrigin);
}

/***********************************************************************
 * Function: CCard::SetOrigin
 *
 *  Parameter loc
 * Return:
 *
 * Comments:
 *  Sets the card's local coordinate system.
 ***********************************************************************/
void CCard::SetOrigin(TPoint &loc)
{
    mOrigin = loc;
    gVariableManager.SetLong("_originx", mOrigin.X());
    gVariableManager.SetLong("_originy", mOrigin.Y());
}
void CCard::SetOrigin(int32 inX, int32 inY)
{
	TPoint	newOrigin(inX, inY);
	SetOrigin(newOrigin);
}

/***********************************************************************
 * Function: CCard::OffsetOrigin
 *
 *  Parameter delta
 * Return:
 *
 * Comments:
 *      Offsets the card's local coordinate system by the amount given.
 ***********************************************************************/
void CCard::OffsetOrigin(TPoint &delta)
{
	TPoint newOrigin(mOrigin);
	
	newOrigin.Offset(delta);
	
	SetOrigin(newOrigin);
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
 * Function: CCard::Evaluate
 *
 *  Parameter conditional (blah AND blah OR etc..)
 * Return:
 *
 * Comments:
 *  Evaluate the given conditional and determine whether or not
 *  it is true.
 ***********************************************************************/
int16 CCard::Evaluate(TStream& conditional)
{
    int16		globalRes, localRes, result;
    EvalMode	mode = FirstTime;
    TString     op;
    TString     modeStr;
    TString     str1, str2;
#ifdef DEBUG
	TString		origStr1, origStr2, origOp;
#endif

    globalRes = localRes = false;

    while (conditional.more()) 
	{
        conditional >> str1 >> op >> str2;
#ifdef DEBUG
		origStr1 = str1;
		origStr2 = str2;
		origOp = op;
#endif
		// See if op is contains first.
		if (op.Equal("contains", false))
		//if (op == (char *)"contains")
		{
			//char *res = nil;
			
			if (str1.Contains(str2, false))
				localRes = true;
			else
				localRes = false;
				
			//res = strstr(str1.GetString(), str2.GetString());
			//if (res != nil)
			//	localRes = true;
			//else
			//	localRes = false;
		}
		else
		{
	        //  Returns <0, 0, or >0.
	        //
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
	            gLog.Error("IF: unknown operator %s.", (const char *) op);
	            return (globalRes);
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
            
			if (modeStr == (char *) "and") 
			{
                if (mode == Or) 
					gLog.Caution("IF: can't mix ANDs and ORs.");
                mode = And;
                
                // one false makes a whole bunch of ANDed things false
                if (not globalRes)
                	goto end;
            } 
			else if (modeStr == (char *) "or") 
			{
                if (mode == And) 
					gLog.Caution("IF: can't mix ANDs and ORs.");
                mode = Or;
                
                // one true makes a whole bunch of ORed things true
                if (globalRes)
                	goto end;	
            } 
			else
			{
                gLog.Caution("IF: expected AND or OR here, not %s.", (const char *) modeStr);
                mode = And;
            }
        }
    }

end:    
#ifdef DEBUG
	if (globalRes)
		gDebugLog.Log("if (%s %s %s) -> true", origStr1.GetString(), origOp.GetString(), origStr2.GetString());
	else
		gDebugLog.Log("if (%s %s %s) -> false", origStr1.GetString(), origOp.GetString(), origStr2.GetString());
#endif
		
    return (globalRes);
}

/*************************

    5L UTILITY METHODS

*************************/
/*----------------------------------------------------------------
  	Log _Graphic_X and _Graphic_Y for variable manager
  	Requires TRect bounds to already be offset from origin.
  	_Graphic_X and _Graphic_Y hold bottom & right pixel coordinates 
  	of latest drawn graphic that called this.
------------------------------------------------------------------*/
void CCard::UpdateSpecialVariablesForGraphic(TRect bounds)
{
	Rect sides = bounds.GetRect();
	gVariableManager.SetLong("_Graphic_X", (short) sides.right);
	gVariableManager.SetLong("_Graphic_Y", (short) sides.bottom);
}

/*************************

    5L COMMAND METHODS

*************************/

/*----------------------------------------------
    (ADD VARIABLE AMOUNT)

    Adds the given amount to the given variable.
    
    cbo - This originally was written to use floats.
------------------------------------------------*/
void CCard::DoAdd()
{
	TString		theVarName;
	int32		theAmount;
	int32		theOrigValue;
	int32		theResValue;

    m_Script >> theVarName >> theAmount;
    
	// cbo_fix - we don't have fcvt like Windows does
   // sum = gVariableManager.GetDouble(vname);
	theOrigValue = gVariableManager.GetLong(theVarName);
    theResValue = theOrigValue + theAmount;

    //gVariableManager.SetDouble(vname, sum);
    gVariableManager.SetLong(theVarName, theResValue);

#ifdef DEBUG
	gDebugLog.Log("add: %s <%ld> + <%ld> = <%ld>", 
		(const char *) theVarName, theOrigValue, theAmount, theResValue);
#endif
}

//
//	DoAudio - 
//
void CCard::DoAudio(void)
{
	TString		audio_file;
	TString		flag;
	int32		offset = 0;
	int32		volume = 100;
	int32		fade_time = 0;
	bool		do_loop = false;
	bool		do_kill = false;
	
	m_Script >> audio_file;
	
	if (m_Script.more())
		m_Script >> offset;
	if (m_Script.more())
		m_Script >> volume;
	if (m_Script.more())
		m_Script >> fade_time;
		
	while (m_Script.more())
	{
		m_Script >> flag;
		flag.MakeLower();
		if (flag.Equal("kill"))
			do_kill = true;
		else if (flag.Equal("loop"))
			do_loop = true;
	}
	
	if (do_loop)
		gMovieManager.PlayLoop(audio_file.GetString(), fade_time);
	else
		gMovieManager.Play(audio_file.GetString(), offset, true, NULL);
}	

//
//	DoAudioKill - Kill audio clips. If fade_time is > 0, use it to fade the volume
//		out over that many tenths of a second. If loop is present, kill loops too.
//
//		(audiokill [fade_time] [loop])
//
//			default value:
//				fade_time		0
//				loop			no
//
void CCard::DoAudioKill(void)
{
	TString		loop_flag;
	int32		fade_time = 0;
	bool		do_kill_loops = FALSE;
	
	if (m_Script.more())
		m_Script >> fade_time;
		
	if (m_Script.more())
	{
		m_Script >> loop_flag;
		loop_flag.MakeLower();
		
		if (loop_flag == (char *) "loop")
			do_kill_loops = true;
		else
			gLog.Caution("Bad flag to audiokill command <%s>", loop_flag.GetString());
	}
}

//
//	DoAudioPlay - Play the audio clip.
//
//		(audioplay clip_name [offset] [volume] [fade_time] [kill] [loop])
//
//			default values:
//				offset		0
//				volume		100
//				fade_time	0
//				kill		no
//				loop		no
//
void CCard::DoAudioPlay(void)
{
	TString		audio_file;
	TString		flag;
	int32		the_offset = 0;
	int32		the_volume = 100;
	int32		the_fade_time = 0;
	bool		do_loop = false;
	bool		do_kill = false;
	
	m_Script >> audio_file;
	
	if (m_Script.more())
		m_Script >> the_offset;
	if (m_Script.more())
		m_Script >> the_volume;
	if (m_Script.more())
		m_Script >> the_fade_time;
	
	while (m_Script.more())
	{
		m_Script >> flag;
		flag.MakeLower();
		
		if (flag == (char *) "kill")
			do_kill = true;
		else if (flag == (char *) "loop")
			do_loop = true;
		else
			gLog.Caution("Bad flag to audioplay command <%s>", flag.GetString());
	}
}

//
//	DoAudioVolume - Set the volume for all playing audio clips. The volume
//		is given as a number between 0 (off) and 100 (full volume). The 
//		optional fade_time parameter is used to fade down or up to the volume
//		over that many tenths of a second.
//
//		(audiovolume volume [fade_time])
//
//			default values:
//				fade_time	0
//
void CCard::DoAudioVolume(void)
{
	int32		the_volume;
	int32		the_fade_time = 0;
	
	m_Script >> the_volume;
	
	if (m_Script.more())
		m_Script >> the_fade_time;
		
}

//
//	DoAudioWait - Wait for a particular frame of the playing audio clip.
//
//		(audiowait frame)
//
void CCard::DoAudioWait(void)
{
	int32		the_frame;
	
	m_Script >> the_frame;
	
}	
	
	

/*---------------------------------------------------------------
    (BACKGROUND picfile x1 y1 x2 y2)

	Draw the pic in the bounding rectangle (x1 y1 x2 y2)
-----------------------------------------------------------------*/
void CCard::DoBackground()
{
    TString     TempCmd, picname;
    TRect     	loc;
    Rect		macLoc;
    
    m_Script >> picname >> loc;
    
    AdjustRect(&loc);
    macLoc = loc.GetRect();

#ifdef DEBUG
	gDebugLog.Log("background: <%s>, L<%d>, T<%d>, R<%d>, B<%d>", 
		picname.GetString(), macLoc.left, macLoc.top, macLoc.right, macLoc.bottom);
#endif

    gPlayerView->SetBackPic(picname, macLoc);
}

/*---------------------------------------------------------------
    (BEEP <FREQ> <DURATION>)

    The computer beeps. Optional parameters control the frequency
    (pitch) of the beep and its duration, in tenths of seconds.
-----------------------------------------------------------------*/
 void CCard::DoBeep()
{
    int16     freq = 1500;        //  Hz
    int16     duration = 80;      //  Milliseconds

    if (m_Script.more()) 
		m_Script >> freq;
    
    if (m_Script.more()) 
	{
        m_Script >> duration;
        duration *= 100;
    }

#ifdef DEBUG
	gDebugLog.Log("beep: freq <%d>, duration <%d>", freq, duration);
#endif
    
 	// cbo_fix - we can do better than this
	SysBeep(30);
}
/*---------------------------------------------------------------
    (BLIPPO)

    Copy the current display screen to the offscreen buffer while
    still allowing drawing on screen. Use unblippo to restore the
    saved screen.
-----------------------------------------------------------------*/
void CCard::DoBlippo()
{
#ifdef DEBUG
	gDebugLog.Log("blippo: ");
#endif

	gPlayerView->Blippo();
}

/*----------------------------------------------------------------
    (BLUERAMP one two three four)
    Puts the stupid blueramp inside the given rectangle.
 ----------------------------------------------------------------*/
void CCard::DoBlueramp()
{
#ifdef CBO_FIX
    TRect bounds;

    m_Script >> bounds;
    Blueramp(bounds);
#endif
}

/*----------------------------------------------------------------
    (BOX LEFT TOP RIGHT BOTTOM FILL COLOR)

    Draws a box of the given color at the coordinates specified.
    If FILL == "FILL", the box is filled in. Otherwise,
    only the outline of the box is drawn.
------------------------------------------------------------------*/
void CCard::DoBox()
{
	CPlayerBox	*boxPtr;
    TRect		bounds;
    Rect		macBounds;
    int16		color, lineThickness = 1;
    TString		fill;
    Boolean		theFill = false;


    m_Script >> bounds >> fill >> color;
    if (m_Script.more()) 
    	m_Script >> lineThickness;

    AdjustRect(&bounds);
    macBounds = bounds.GetRect();
    
    fill.MakeLower();
    if (fill == (char *) "fill")
    	theFill = true;

#ifdef DEBUG
	if (theFill)
		gDebugLog.Log("box: L <%d>, T <%d>, R <%d>, B <%d>, color <%d>", 
			bounds.Left(), bounds.Top(), bounds.Right(), bounds.Bottom(), color);
	else
		gDebugLog.Log("box: L <%d>, T <%d>, R <%d>, B <%d>", 
			bounds.Left(), bounds.Top(), bounds.Right(), bounds.Bottom());
#endif
    
    boxPtr = new CPlayerBox(macBounds, theFill, lineThickness, color);
	if (boxPtr != nil)
		delete boxPtr;
}


/*---------------------------------------------------------------
   DoBrowse launches a browser and goes to the given URL.
   It opens the default browser according to the settings in 
   InternetConfig.
-----------------------------------------------------------------*/
void CCard::DoBrowse()
{
	
	TString theURL;
	
	m_Script >> theURL;
	
	long startSel = 0;
   	long endSel = theURL.Length();
  	
    if (PP::UInternetConfig::PP_ICAvailable())	
    {
    	#ifdef DEBUG
    		gDebugLog.Log("PP_IC Available");
    	#endif	
    
    	// Theoretically our suspend event should show the menu bar without problems. 
    	// However, this doesn't work. Internet explorer's menu bar will be screwed up 
    	// if Internet Explorer was not previously running. 
    	// Therefore we need to show the menu bar here, before we call PP_ICLaunchURL. 
    	gMenuUtil.ShowMenuBar();
        PP::UInternetConfig::PP_ICLaunchURL("\p", (char *) theURL.GetString(), endSel, &startSel, &endSel);
    	
    }
    else
    {
    	#ifdef DEBUG
    		gDebugLog.Log("PP_IC Not available");
		#endif
	}
}




/*---------------------------------------------------------------
    (BUTTPCX PCXFILE X Y header text Command <second Command>)
    Puts a button with text "text" on the screen and activates a corresponding
    touchzone.  It takes care of the picture and hilited picture display.
    OPTIONAL:  set command to be executed prior to the jump.
-----------------------------------------------------------------*/

void CCard::DoButtpcx()
{
    TRect       bounds;
    TPoint      buttLoc;
	const char		*theHeadName = nil;
    CPicture    *thePicture = nil;
    TString     theHeaderName, picname, theCommand, cmdText, Text, scmdText;
    TString     secondCommand;
    TString		cursorType;
    CursorType	cursor = HAND_CURSOR;
    CursorType	tmpCursor = UNKNOWN_CURSOR;

    m_Script >> picname >> buttLoc >> theHeaderName >> Text >> cmdText;

	if (m_Script.more() and (m_Script.curchar() != '('))
	{
		m_Script >> cursorType;
		
		tmpCursor = gCursorManager.FindCursor(cursorType);
		if (tmpCursor != UNKNOWN_CURSOR)
	    	cursor = tmpCursor;
	    else
	    	gLog.Caution("Unknown cursor type: %s", cursorType.GetString());
	}
	
   	scmdText = "";
    if (m_Script.more())  
	{
        m_Script >> secondCommand;      // OPTIONAL: (command <params +>)...
        scmdText= "(";
        scmdText += secondCommand;
        scmdText += ")";
    }
	
    AdjustPoint(&buttLoc);
	
    theCommand = "(";       // due to parser's stripping of parens.
    theCommand += cmdText;
    theCommand += ")";

	if (not picname.IsEmpty())
	{
		thePicture = gPictureManager.GetPicture(picname);
		
		if (thePicture != NULL)
		{
			bounds = thePicture->GetBounds();
			bounds.Offset(buttLoc);
		}
	}
	
	if (not theHeaderName.IsEmpty())
		theHeadName = theHeaderName.GetString();

#ifdef DEBUG
	gDebugLog.Log("buttpcx: <%s>, X<%d>, Y<%d>, header <%s>, text <%s>, cmd <%s>, cmd2 <%s>",
			(const char *) picname, buttLoc.X(), buttLoc.Y(), (const char *) theHeadName, 
			(const char *) Text, (const char *) cmdText, (const char *) scmdText);
#endif
	
	UpdateSpecialVariablesForGraphic(bounds);

	if (thePicture != NULL)
		new CTouchZone(bounds, theCommand, thePicture, buttLoc, (const char *) Text, cursor, 
				(const char *) theHeadName, scmdText);
#ifdef DEBUG
	else
		gDebugLog.Log("no picture, no touch zone");
#endif
	
	// cbo_test - try this
	//gPlayerView->AdjustMyCursor();
}

//
//	CheckDisc - Check the CD. If the correct CD is in the drive, do nothing (fall through).
//		If the wrong CD is in there, jump to the wrong_disc card, if no CD is in the drive
//		jump to the no_disc card.
//
//		(checkdisc volume_name wrong_disc no_disc)
//
void CCard::DoCheckDisc()
{
	TString		vol_name;
	TString		wrong_disc;			// where to jump if wrong disc 
	TString		no_disc;			// where to jump if no disc
	TString		jump_card;			// which one of these two to jump to
	int32		check_disc;
	bool		do_jump = FALSE;
	
	m_Script >> vol_name >> wrong_disc >> no_disc;
	
#ifdef DEBUG
	gDebugLog.Log("checkdisc: <%s>, if fail <%s>", 
		vol_name.GetString(), wrong_disc.GetString());
#endif

	// turn vol_name into a volume name
	vol_name += ":";
	
	check_disc = gVariableManager.GetLong("_NoCheckDisc");
	if (check_disc == 0)
	{
		// first just see if the volume is already mounted
		if (gModMan->VolMounted(vol_name))
		{
			// nothing else to do
#ifdef DEBUG
			gDebugLog.Log("Volume is already mounted (might not be a CD)");
#endif			
		}
		else if (gModMan->CDInDrive())
		{
			if (not gModMan->VolMounted(vol_name))
			{
#ifdef DEBUG
				gDebugLog.Log("Wrong disc in CD player");
#endif
				jump_card = wrong_disc;
				do_jump = TRUE;
			}
		}
		else
		{
#ifdef DEBUG
			gDebugLog.Log("No disc in CD player");
#endif
			jump_card = no_disc;
			do_jump = TRUE;
		}	

		// if we found the disc we were looking for, set the 
		// current CD so it will find movies
		if (not do_jump)
		{			
#ifdef DEBUG
			gDebugLog.Log("Disc found, setting current CD to <%s>", vol_name.GetString());
#endif
			gModMan->SetCurCD(vol_name);
		}
	}
#ifdef DEBUG
	else
		gDebugLog.Log("_NoCheckDisc is true, no jumping");
#endif
	
	if (do_jump)
	{
		gPlayerView->ProcessEvents(false);	// stop processing keys and touch zones
	
    	gCardManager.JumpToCardByName((const char *) jump_card, false);
    
    	gPlayerView->Draw(nil);				// refresh the screen so see everything before jumping		
	}	
}

//
//	DoCheckVol - Check if the volume is mounted and return the path to it.
//		(checkvol vol_name var_to_get_path [card_to_jump_if_no_volume])
//
void CCard::DoCheckVol()
{
	TString		vol_name;
	TString		real_path_var;
	TString		no_volume;
	
	m_Script >> vol_name >> real_path_var;
	
	if (m_Script.more())
		m_Script >> no_volume;
		
#ifdef DEBUG
	gDebugLog.Log("checkvol: <%s>, put path into <%s>, if volume not found <%s>",
		vol_name.GetString(), real_path_var.GetString(), no_volume.GetString());
#endif

	gVariableManager.SetLong(real_path_var.GetString(), 0);
	
	if (gModMan->VolMounted(vol_name))
	{
		if (not vol_name.EndsWith(':'))
			vol_name += ":";
			
		gVariableManager.SetString(real_path_var.GetString(), vol_name);
	}
	else if (not no_volume.IsEmpty())
	{
#ifdef DEBUG
		gDebugLog.Log("checkvol: failed, jumping to <%s>",
			no_volume.GetString());
#endif

		gPlayerView->ProcessEvents(false);	// stop processing keys and touch zones
		gCardManager.JumpToCardByName(no_volume.GetString(), false);
    	gPlayerView->Draw(nil);				// refresh the screen so see everything before jumping
	}
}
	

/*----------------------------------------------------------------------
    (CLOSE FILENAME)

    Close the given text file.
------------------------------------------------------------------------*/
void CCard::DoClose()
{
    TString     filename;

    m_Script >> filename;

#ifdef DEBUG
	gDebugLog.Log("close: file <%s>", filename.GetString());
#endif

    gFileManager.Close(filename);
}

/*-------------------------------------------------------------
    (CTOUCH)

    Clear all current touch zones. Typically called immediately
    before setting new touch zones.
---------------------------------------------------------------*/
 void CCard::DoCTouch()
{
    int16	left, top;

    if (m_Script.more())  
    {
        m_Script >> left >> top;

#ifdef DEBUG
	gDebugLog.Log("ctouch: at left <%d>, top <%d>", left, top);
#endif   

        gPlayerView->CTouch(left, top);
    }
    else 
    {
#ifdef DEBUG
	gDebugLog.Log("ctouch: all");
#endif   
    	gPlayerView->CTouch();
    }

	// cbo_test - put this back in because we took it out in wait and nap
	gPlayerView->AdjustMyCursor();   	
}

//
//	DoCursor - Change the cursor.
//
void CCard::DoCursor()
{
	CursorType	theCursor = ARROW_CURSOR;
	CursorType	tmpCursor;
	TString		cursorStr;
	bool		forceShow = false;
	
	if (m_Script.more())
	{
		m_Script >> cursorStr;
		
		tmpCursor = gCursorManager.FindCursor(cursorStr);
		if (tmpCursor != UNKNOWN_CURSOR)
		{
			theCursor = tmpCursor;
			forceShow = true;
		}
		else
			gLog.Caution("Unknown cursor type: %s", cursorStr.GetString());
	}
	
	gCursorManager.ChangeCursor(theCursor);
	gCursorManager.ForceShow(forceShow);
}

//
//	DoDebug - Drop into the debugger
//
#ifdef DEBUG
void CCard::DoDebug()
{
	// drop into debugger
	BreakToSourceDebugger_();
}
#endif

/*--------------------------------------------------------
        (DIV X Y)

        X <- X/Y,  X will be truncated to int16.
 ---------------------------------------------------------*/
void CCard::DoDiv()
{
    TString		theVarName;
    double		theAmount;
    int32		theOrigValue;
    int32		theResValue;

    m_Script >> theVarName >> theAmount;


	theOrigValue = gVariableManager.GetLong(theVarName);

    if (theAmount == 0.0)
    {
		gLog.Caution("Division by zero: %s <%ld> / <%f>", 
			(const char *) theVarName, theOrigValue, theAmount);
			
		theResValue = 0;
    }
    else
		theResValue = (int32) (theOrigValue / theAmount);
   
    gVariableManager.SetLong(theVarName, theResValue);
    
#ifdef DEBUG
	gDebugLog.Log("div: %s <%ld> by <%f> = <%ld>", 
		(const char *) theVarName, theOrigValue, theAmount, theResValue);
#endif
}

//
//	(ejectdisc) - Eject whatever CD is in the drive.
//
void CCard::DoEjectDisc()
{
	gModMan->EjectCD();
}

/*-------------------
    (EXIT)

    Exit the program.
---------------------*/
void CCard::DoExit()
{
	int16	theSide = 0;
	
	if (m_Script.more())
		m_Script >> theSide;

#ifdef DEBUG
	gDebugLog.Log("exit: %d", theSide);
#endif
		
	gCardManager.DoExit(theSide);
}

/*---------------------------------------------------------------
    (FADE DIR <STEPS>)

    Either FADE IN or FADE OUT. This ramps the palette in or out.
-----------------------------------------------------------------*/
void CCard::DoFade()
{
    TString     direction;
    int16       steps = 1;

    m_Script >> direction;
    if (m_Script.more()) 
    	m_Script >> steps;

#ifdef DEBUG
	gDebugLog.Log("fade: %s, steps <%d>", direction.GetString(), steps);
#endif

	// cbo_hack - try making the fades a bit faster
//	if (steps >= 10)
//		steps -= (steps / 3);
	
    direction.MakeLower();
    if (direction == (char *) "in")
    {
    	gPlayerView->Draw(nil);		// refresh the screen first
		DoGFade(true, steps, true);
	}
    else if (direction == (char *) "out")
		DoGFade(false, steps, true);
    else
        gLog.Caution("Fade in or out, but don't fade %s", (const char *) direction);
}

/*---------------------------------------------------------
    (HIGHLIGHT PICNAME)

    Will highlight a picture already displayed on screen by
    drawing PICNAMEH.PCX, pausing, and then redrawing
    PICNAME.PCX. Both of these pictures are matted.
-----------------------------------------------------------*/
void CCard::DoHighlight()
{
#ifdef CBO_FIX
    TString     picName;
    Picture     *thePicture;
    TPoint       pt;

    m_Script >> picName;

    thePicture = gPictureManager.GetPicture(picName);
    thePicture->GetLoc(&pt);
    thePicture->Hilite(pt);
#endif
}

/*----------------------------------------------------------------
    (HIDEMOUSE)
    As expected, hides the mouse. WARNING!  unknown behv. when TouchScreen!!
 ---------------------------------------------------------------*/
void CCard::DoHidemouse()
{
	gCursorManager.HideCursor();
}
/*-----------------------------------------------------------------
    (IF (CONDITIONAL) (TRUE_CMD) <(FALSE_CMD)>)

    Evaluate the conditional expression and execute the appropriate
    command based on the value of the expression. Only numbers may
    be compared. It's important that the conditional statement be
    enclosed in parentheses and that the operator (>, <, =) be
    separated from the operands by a space.
-------------------------------------------------------------------*/
void CCard::DoIf()
{
    TStream     conditional;
	
    m_Script >> conditional;

    conditional.reset();

    if (Evaluate(conditional)) 
    {
        DoCommand();
    } 
    else 
    {
        //  Skip TRUE_CMD.
        m_Script >> open >> close;
        if (m_Script.more()) 
        	DoCommand();
    }
}

/*---------------------------------------------------------------------
    (INPUT STYLE VARIABLE MASK X Y <REQUIRE>)

    Allow user input from the keyboard. The text they input is saved
    in the given variable. It appears onscreen in the given header
    style, with upper left corner X, Y. The mask is an input mask which
    controls the kind of characters the user may input. (See GX Text
    for more details about the mask.)

    If REQUIRE = true then the entire mask must be filled. Otherwise
    any input, however short, is accepted.
-----------------------------------------------------------------------*/
void CCard::DoInput()
{
    TString     theVarName, mask, style, required;
    TRect		bounds;
    Rect		macBounds;
    int16       fRequire = false;

    m_Script >> style >> theVarName >> mask >> bounds;

    if (m_Script.more()) 
    {
        m_Script >> required;
        required.MakeLower();
        if (required == (char *) "true") 
        	fRequire = true;
    }

   	AdjustRect(&bounds);
   	macBounds = bounds.GetRect();

#ifdef DEBUG
	gDebugLog.Log("input: into <%s>, style <%s>, mask <%s>, L <%d>, T <%d>, R <%d>, B <%d>, require <%s>",
			theVarName.GetString(), style.GetString(), mask.GetString(), 
			bounds.Left(), bounds.Top(), bounds.Right(), bounds.Bottom(),
			required.GetString());
#endif
   	
	mPaused = true;
   	gPlayerView->ProcessEvents(false);
   	
	DoCPlayerInput(theVarName, style, mask, macBounds, fRequire);
	
	gPlayerView->Draw(nil);
}

/*-------------------------
    (JUMP JUMPCARD)

    Jump to the given card.
---------------------------*/
void CCard::DoJump()
{
    TString     jumpCard;

    m_Script >> jumpCard;

#ifdef DEBUG
	gDebugLog.Log("jump: to <%s>", (const char *) jumpCard);
#endif
    
	gPlayerView->ProcessEvents(false);	// stop processing keys and touch zones
	
    gCardManager.JumpToCardByName(jumpCard, false);
    
    gPlayerView->Draw(nil);				// refresh the screen so see everything before jumping
}

/*--------------------------------------------------------
    (KEY COLOR)

    Switch the overlay mode and set the keycolor to COLOR.
----------------------------------------------------------*/
void CCard::DoKey()
{
#ifdef CBO_FIX
    int16     newKeyColor;

    m_Script >> newKeyColor;
    //CheckColorIW(&newKeyColor);
    gVideoManager->overlay(newKeyColor);

#endif
}

/*-----------------------------------------------------------
    (KEYBIND CHAR <LINKCARD>)

    Bind the given character to the linkcard so that pressing
    ALT-CHAR jumps to the linkcard. If no linkcard is given,
    then remove the binding associated with CHAR.
-------------------------------------------------------------*/
void CCard::DoKeybind()
{
    TString 	keyEquiv; 
    TString		linkCard;
	CCard		*theCard;
	char		theChar;
	
    m_Script >> keyEquiv;

    if (m_Script.more())
        m_Script >> linkCard;
        
    keyEquiv.MakeLower();
    if (keyEquiv == (const char *) "esc")
    	theChar = 0x1B;					// the Escape key
    else
    	theChar = keyEquiv(0);

#ifdef DEBUG
	gDebugLog.Log("keybind: key <%c>: Jump to card <%s>", keyEquiv(0), linkCard.GetString());
#endif

	theCard = (CCard *) gCardManager.Find(linkCard.GetString());
	
	if (theCard != NULL)
    	gPlayerView->AddKeyBinding(theChar, theCard);
#ifdef DEBUG
	else
		gDebugLog.Caution("ERROR: Trying to keybind to non-existant card <%s>!", linkCard.GetString());
#endif
}

//
//	DoKill - Kill whatever movie is playing.
//
void CCard::DoKill()
{
	if (gMovieManager.Playing())
	{
		gMovieManager.Kill();
#ifdef DEBUG
		gDebugLog.Log("kill: the movie be dead");
#endif
	}
#ifdef DEBUG
	else
		gDebugLog.Log("kill: nothing to kill");
#endif
	
}

/*--------------------------------------------------------------
    (LINE X1 Y1 X2 Y2 COLOR <THICKNESS>)

    Draw a line of given color and thickness (default is 1) from
    x1, y1 to x2, y2.
----------------------------------------------------------------*/
void CCard::DoLine()
{
	CPlayerLine	*linePtr;
    TPoint  	a, b;
    int16   	color, thickness = 1;
    Rect		theRect;

    m_Script >> a >> b >> color;
    
    if (m_Script.more())
    	m_Script >> thickness;

    AdjustPoint(&a);
    AdjustPoint(&b);
    
    //
    // make sure horizontal and vertical lines don't have
    //			funny numbers when thickness > 1
    if ((b.X() == (a.X() + thickness)) or (b.X() == (a.X() - thickness)))
    	b.SetX(a.X());
    if ((b.Y() == (a.Y() + thickness)) or (b.Y() == (a.Y() - thickness)))
    	b.SetY(a.Y());
    
    ::SetRect(&theRect, a.X(), a.Y(), b.X(), b.Y());
    linePtr = new CPlayerLine(theRect, thickness, color);
	if (linePtr != nil)
		delete linePtr;
}

/*-------------------------------------------------------------
    (LOADPAL PICTURE)

    Set the screen palette to the palette of the given picture.
---------------------------------------------------------------*/
void CCard::DoLoadpal()
{
	CPalette	*thePal = nil;
    TString 	palname;
    TString		flag;
    bool		noload = false;
    bool		lock = false;
    bool		unlock = false;

	m_Script >> palname;

	palname.MakeLower();

#ifdef DEBUG
	gDebugLog.Log("loadpal: <%s>", palname.GetString());
#endif

	while (m_Script.more())
	{
		m_Script >> flag;
		flag.MakeLower();
		
		if (flag == (char *) "noload")
			noload = true;
		else if (flag == (char *) "lock")
			lock = true;
		else if (flag == (char *) "unlock")
			unlock = true;
		else
			gLog.Caution("Bad flag to loadpal command <%s>", flag.GetString());
	}
	
	thePal = gPaletteManager.GetPalette(palname);
	
	if (thePal != nil)
	{
		if (lock)
			thePal->Lock(true);
		else if (unlock)
			thePal->Lock(false);
			
		if (not noload)
			gPaletteManager.SetPalette(thePal, true);
	}
#ifdef DEBUG
	else
		gDebugLog.Log("Couldn't find palette <%s>", palname.GetString());
#endif
}

/*---------------------------------------------------------------------
    (LOADPIC PICTURE X Y <FLAGS...>)
-----------------------------------------------------------------------*/
void CCard::DoLoadpic()
{
	CPlayerPict	*pictPtr;
    TString     TempCmd, picname, flag;
    CPicture    *thePicture = NULL;
    CPalette	*thePalette = NULL;
    TPoint	    loc;
    bool       	matte = false;
    bool		noshow = false;
    bool		lock = false;
    bool		unlock = false;
    bool		do_pal = false;
    
    m_Script >> picname >> loc;
	picname.MakeLower();
	
    AdjustPoint(&loc);	

#ifdef DEBUG
	gDebugLog.Log("loadpic: <%s>, X <%d>, Y <%d>", picname.GetString(), loc.X(), loc.Y());
#endif
		
   while (m_Script.more()) 
    {
        m_Script >> flag;
        flag.MakeLower();
        
        if (flag == (char *) "noshow")
            noshow = true;
        else if (flag == (char *) "pal")
        	do_pal = true;
        else if (flag == (char *) "matte") 
        	matte = true;  
        else if (flag == (char *) "lock")
        	lock = true;
        else if (flag == (char *) "unlock")
        	unlock = true;
        else
			gLog.Caution("Bad flag to loadpic command <%s>", flag.GetString());
    }

	thePicture = gPictureManager.GetPicture(picname);
	
	// GetBounds returns rect with (0,0,width, height)
	// We need to offset this before calling UpdateGraphicsForSpecialVariables.
	TRect bounds; 
	bounds = thePicture->GetBounds();	
	bounds.Offset(loc);
	UpdateSpecialVariablesForGraphic(bounds);

	if (thePicture != nil)
	{
		if (lock)
			thePicture->Lock(true);
		else if (unlock)
			thePicture->Lock(false);
	}
	
	if (do_pal and (thePicture != NULL))
	{
		thePalette = gPaletteManager.GetPalette(picname);
		
		if (thePalette != nil)
			gPaletteManager.SetPalette(thePalette, true);
	}
    
	if ((not noshow) and (thePicture != NULL))
	{
    	pictPtr = new CPlayerPict(thePicture, loc, matte);
		if (pictPtr != NULL)
			delete pictPtr;
	}
}

/*--------------------------------------------------------
    (LOCK <CLEAR>)

    CLEAR   If given, clear the offscreen buffer. Otherwise,
            copy the current screen contents offscreen.

    Lock the screen so that all drawing takes place in the
    offscreen buffer.
----------------------------------------------------------*/
void CCard::DoLock()
{
    TString     clear;
    bool		doClear = false;

    if (m_Script.more())
    {
        m_Script >> clear;
        clear.MakeLower();
    }

    if (clear == (char *)"clear")
		doClear = true;
    else 
		doClear = false;

#ifdef DEBUG
	gDebugLog.Log("lock: Clear: <%d>", doClear);
#endif
		
	gPlayerView->Lock(doClear);
}

/*------------------------------------------------------------------
    (LOOKUP FILENAME FIELD1 <FIELD2> ... <FIELDN>)

    FILENAME    The text file to lookup the record in.

    FIELD1..N   The fields which must match (case not important) the
                record.

    Assumes the text file is a tab delimited, return marked database
    and tries to find the record that matches the first N fields. If
    it succeeds, the file pointer is positioned after the first N
    fields and the scriptor can start doing (read..until tab) calls.
    If it fails, the pointer is at end of file.
--------------------------------------------------------------------*/
void CCard::DoLookup()
{
    TString     searchString, param, filename;
    int16       numFields = 0;

    m_Script >> filename;

    //  Append all the fields together into a search string that looks
    //  like "field1 TAB field2 TAB ... fieldN"
    //
    while (m_Script.more()) 
    {
        m_Script >> param;
        if (numFields > 0)
            searchString += '\t';
        numFields++;
        searchString += param;
    }

#ifdef DEBUG
	gDebugLog.Log("lookup: file <%s>, search <%s>", filename.GetString(), searchString.GetString());
#endif
    
    gFileManager.Lookup(filename, searchString, numFields);
}

/*-------------------------------------------------------------------
    (MACRONAME <X Y> <VAR>...)

    Call the macro by name. X, Y is the mOrigin to use for the macro.
    VAR are an optional number of local variables that vary depending
    upon the particular macrodef.
---------------------------------------------------------------------*/
void CCard::DoMacro(TString &name)
{
	TStream		saveScript(m_Script);
    TIndex		*theMacro;
    TString		vname, contents;
    int16		vnum;
    TVariable	*local, *temp, *oldlocal;

    theMacro = (TIndex *) gMacroManager.Find(name);
	
	if (theMacro == NULL)
	{
        gLog.Caution("Couldn't find macro/command <%s>.", (const char *) name);
        return;
	}

    //
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

    //
    //  Save old local tree and set current local tree to ours.
    //
    oldlocal = gVariableManager.GetLocal();
    gVariableManager.SetLocal(local);

	//
	// We have already saved this card's m_Script, now set it to 
	//	the macro and execute the commands on it. 
	//
	m_Script = theMacro->GetScript();
	
	m_Script.reset();					// start at the beginning

#ifdef DEBUG
	// dump the macro
	gDebugLog.Log("macro: %s", m_Script.GetString());	
#endif 
	
	m_Script >> open >> discard >> discard; // toss (macrodef name
	
	// Save the origin so we can reset it when the macro is done.
	TPoint		saveOrigin(mOrigin);

	mStopped = false;
			
	while ((mActive) 							// used to only check for m_Script.more()
		and (m_Script.more()) 
		and (not mStopped)						// stop on return
		and (not gCardManager.Jumping()))		// don't check mPaused here because we could be waiting for audio
	{
		DoCommand();
	}
    mStopped = false;
    
    m_Script = saveScript;			// restore the original m_Script
   		
    //
    //  Restore old local tree and delete ours.
    //
    gVariableManager.SetLocal(oldlocal);
    if (vnum > 0) 
		local->RemoveAll(local);
		
	// restore old origin
	SetOrigin(saveOrigin);
		
    delete local;
}

/*-------------------------------------------------------------------
    (MICRO EFFECT)

    Switch to graphics only (micro) mode. The effect defines how this
    transition is accomplished.
---------------------------------------------------------------------*/
void CCard::DoMicro()
{
#ifdef DONT_DO_THIS
	TString     strEffect;
    FXType      theEffect;

    m_Script >> strEffect;

    theEffect = StringToEffect(strEffect);
	// Not quite sure what the spec says to do here. I think all we need to do is
	// the same as an unlock(), but may want to gfade down & up if from movie.
	
	gPlayerView->Micro(theEffect);
#endif
}

/*------------------------------------------------
    (NAP TIME)

    Pause execution for TIME tenths of seconds.
    The user can abort a int32 nap via the ESC key.
--------------------------------------------------*/
void CCard::DoNap()
{
    int32    tenths;

    m_Script >> tenths;
    tenths *= 100;      // Convert to milliseconds
	
	if (mNapTimer == NULL)
	{
		mPaused = true;
		mNapTimer = new CTimer(tenths, NULL);

#ifdef DEBUG
		gDebugLog.Log("nap: %d", tenths);
		//gDebugLog.Log("Refreshing Card (DoNap)");
#endif

		// cbo_test - took this out to prevent flashing
		gPlayerView->AdjustMyCursor();
		
		gPlayerView->Draw(nil);
		gPlayerView->ProcessEvents(true);
	}
}

/*----------------------------------------------------------------
    (OPEN FILENAME KIND)

    Open a text file. KIND specifies the kind of access one
    will have to the file. It may be APPEND (write only, appending
    to the end of the file), NEW (write only, overwriting
    anything in the file), or READONLY.
------------------------------------------------------------------*/
void CCard::DoOpen()
{
    TString     filename, kind;
    char		*slashPtr;
    FileKind    fKind = fReadOnly;

    m_Script >> filename >> kind;
    kind.MakeLower();

    if (kind == (char *)"append") 
    	fKind = fWriteAppend;
    else if (kind == (char *)"new") 
    	fKind = fWriteNew;
    else if (kind == (char *)"readonly") 
    	fKind = fReadOnly;
    else
        gLog.Caution("Unknown open file kind: %s", (const char *)kind);
    
    // Filenames can look DOS like.     
    slashPtr = strstr(filename.GetString(), "\\");
    if (slashPtr != NULL)
    	*slashPtr = ':';

#ifdef DEBUG
	gDebugLog.Log("open: file <%s>", filename.GetString());
#endif

    gFileManager.Open(filename, fKind);
}

/*------------------------------------------------------------
    (ORIGIN DX DY)

    Move the local coordinates for this particular card (or
    macro) by the delta values given. This change is an offset
    from whatever the current coordinates are. There is no
    way to set the absolute coordinates for a macro or card!
--------------------------------------------------------------*/
void CCard::DoOrigin()
{
    TPoint   delta;

    m_Script >> delta;

    OffsetOrigin(delta);
}

/*----------------------------------------------------------------
    (CIRCLE x y Radius COLOR)

    Draws an oval of the given color at the coordinates specified.
    If FILL == "FILL", the oval is filled in. Otherwise,
    only the outline of the oval is drawn.
------------------------------------------------------------------*/

void CCard::DoOval()
{
	CPlayerOval	*ovalPtr;
    TRect		bounds;
    Rect		macBounds;
    int16		color, lineThickness = 1;
    TString		fill;
    Boolean		theFill = false;


    m_Script >> bounds >> fill >> color;

#ifdef DEBUG
	gDebugLog.Log("oval: L <%d>, T <%d>, R <%d>, B <%d>", 
			bounds.Left(), bounds.Top(), bounds.Right(), bounds.Bottom());
#endif

    AdjustRect(&bounds);
    macBounds = bounds.GetRect();
    
    fill.MakeLower();
    if (fill == (char *) "fill")
    	theFill = true;
    
    ovalPtr = new CPlayerOval(macBounds, theFill, lineThickness, color);
	if (ovalPtr != nil)
		delete ovalPtr;
}


void CCard::DoPause()
{
#ifdef CBO_FIX
    int32    tenths = 0L;

    m_Script >> tenths;

    gVideoManager->pause(tenths);
#endif
}

/*-----------------------------------------------------------
    (PLAY FRAME1 FRAME2 TRACK <SPEED>)

    Begin playing the segment from FRAME1..FRAME2 with the
    given audio track (use 0 for none and 3 for stereo).
    An optional speed in fps can be given; the particular
    videodisk player will choose as close a speed as possible
    (since not all players support the same range of fps's).
-------------------------------------------------------------*/
void CCard::DoPlay()
{
#ifdef CBO_FIX
    int32        frame1, frame2, speed = 30;
    AudioState  track;
    int16 trackNum;

    m_Script >> frame1 >> frame2 >> trackNum;
    if (m_Script.more()) {
        speed = 30;  // fps = 30fps
    }

    switch (trackNum) {
        case 0:
            track = audNONE;
            break;
        case 1:
            track = audONE;
            break;
        case 2:
            track = audTWO;
            break;
        case 3:
            track = audSTEREO;
            break;
        case 4:
            track = audONE;
            break;
        default:
            gLog.Caution("Illegal audio track parameter %ld.", trackNum);
    }
    gVideoManager->audio(track);
    gVideoManager->clip(frame1, frame2, 30); //Override speed!!

#endif
}

/*-----------------------------------------------------------
    (playqtfile file [frame] [pal] [origin])
    
    Play a QuickTime file. Frame is the frame offset to be
    used with subsequent wait commands and corresponds to
    a laser disc frame (nothing to do with QuickTime). Pal
    is the palette to use (name.pic).

-------------------------------------------------------------*/
void CCard::DoPlayQTFile()
{
	TString			theQTFile;
	TString			thePal;
	TPoint			movieOrigin(0, 0);
	const char		*thePalStr = NULL;
	int32			theOffset = 0;
	bool			audioOnly = false;

	m_Script >> theQTFile;

	if (m_Script.more())
		m_Script >> theOffset;

	if (m_Script.more())
		m_Script >> thePal;
		
	if (m_Script.more())
	{
		m_Script >> movieOrigin;
		
		AdjustPoint(&movieOrigin);
		gMovieManager.SetOrigin(movieOrigin);
	}
	
	if (theQTFile.Contains(".a2", false))
		audioOnly = true;
    		
	if (not thePal.IsEmpty())
	{
		thePal.MakeLower();
		thePalStr = thePal.GetString();
	}

#ifdef DEBUG
	if (audioOnly)
	{
		gDebugLog.Log("playqtfile: <%s>, offset <%ld>",
			(const char *) theQTFile, theOffset);
	}
	else
	{
		gDebugLog.Log("playqtfile: <%s>, offset <%ld>, pal <%s>",
			(const char *) theQTFile, theOffset, (const char *) thePal);
	}
#endif
				
	gMovieManager.Play(theQTFile.GetString(), theOffset, 
		audioOnly, thePalStr);
}

//
//	PlayQTLoop - Loop the given audio file.
//
//		(playqtloop file [fade])
//
void CCard::DoPlayQTLoop()
{
	TString		theQTFile;
	int32		theFadeTime = 0;
	bool		audioOnly = false;
	
	m_Script >> theQTFile;
	
	if (m_Script.more())
		m_Script >> theFadeTime;
	
    if (strstr(theQTFile.GetString(), ".a2")) 
		audioOnly = true;

	if (not audioOnly)
		gLog.Caution("playqtloop can only be used with audio files!");
	else
	{
		
#ifdef DEBUG
		gDebugLog.Log("playqtloop: <%s> <%ld>", theQTFile.GetString(), theFadeTime);
#endif
		gMovieManager.PlayLoop(theQTFile.GetString(), theFadeTime);
	}
}	

//
//	PlayQTRect - Give a point which will be used to create a rect for the
//			next PlayQTFile command.
//
//		(playqtrect X Y>
//
void CCard::DoPlayQTRect()
{
	TString		theQTFile;
	TString		thePal;
	TPoint		thePT;
	
	m_Script >> thePT;
	
	AdjustPoint(&thePT);
		
#ifdef DEBUG
	gDebugLog.Log("playqtrect: X <%d>, Y <%d>", thePT.X(), thePT.Y());
#endif

	gMovieManager.SetOrigin(thePT);
}
	
void CCard::DoPreloadQTFile()
{
	TString 	theQTFile;
	TString		syncFlag;
	int32		tenths = 0;
	bool		audioOnly = false;
	bool		doSync = false;

    m_Script >> theQTFile;
    
    if (m_Script.more())
    	m_Script >> tenths;
    	
    if (m_Script.more())
    {
    	m_Script >> syncFlag;
    	
    	if (syncFlag.Equal("sync", false))
    		doSync = true;
#ifdef DEBUG
		else
			gDebugLog.Log("perload: bad flag <%s>, looking for sync", syncFlag.GetString());
#endif
	}
    
    if (strstr(theQTFile.GetString(), ".a2"))
    	audioOnly = true;

#ifdef DEBUG
	gDebugLog.Log("preload: <%s>, tenths <%d>, %s", 
		theQTFile.GetString(), tenths, (doSync ? "sync" : "async"));
#endif
	
	if ((tenths > 0) and (mNapTimer == NULL)) 
	{
		mPaused = true;
		mNapTimer = new CTimer(tenths, NULL);

		// cbo_test - took this out to prevent flashing
		gPlayerView->AdjustMyCursor();
		
		gPlayerView->Draw(nil);
		gPlayerView->ProcessEvents(true);
	}
    
    // don't actually preroll	
	//gMovieManager.Preroll(theQTFile.GetString(), audioOnly, doSync);    
}

/*-----------------------------------------------------------------
    (PRINT JUSTIFICATION TEXT)

    Prints the given text with either CENTER or LEFT justification.

    !!! This is currently disabled. Don't bother updating printer
        support until we get a project that needs it.
-------------------------------------------------------------------*/
void CCard::DoPrint()
{
#ifdef CBO_FIX
    TString     just;
    TString     text;

    m_Script >> just >> text;

    just.MakeLower();
    if (just == (char *)"center") 
    {
        // Call centered print routine.
    } 
    else 
    {
        // Call default print routine.
    }

#endif
}

/*-----------------------------------------------------------------
    (READ FILENAME VARIABLE <UNTIL DELIM>)

    Read data from a text file and put it into the variable.
    Normally this will read the next word as defined by whitespace.
    Use the UNTIL DELIM construct to read until some other
    delimiter.

    Valid delimiters are TAB, RETURN, EOF, or any single character.
-------------------------------------------------------------------*/
void CCard::DoRead()
{
    TString         filename, vname, delimstr;
    unsigned char   delim;
    TString         res;

    m_Script >> filename >> vname;

    if (m_Script.more()) 
    {
        m_Script >> discard >> delimstr;
        delimstr.MakeLower();
        
        if (delimstr == (char *)"tab") 
        	delim = '\t';
        else if (delimstr == (char *)"return") 
        	delim = NEWLINE_CHAR;
        else if (delimstr == (char *)"eof") 
        	delim = 0;
        else 
        	delim = delimstr(0);

        gFileManager.ReadUntil(filename, res, delim);
    } 
    else 
    	gFileManager.Read(filename, res);

#ifdef DEBUG
	gDebugLog.Log("read: var <%s>, value <%s>", vname.GetString(), res.GetString());
#endif
    	
    gVariableManager.SetString(vname.GetString(), res.GetString());
}

#ifdef DEBUG
//
//	DoReDoScript
//
void CCard::DoReDoScript()
{
	TString		theCard;
	
	m_Script >> theCard;
	
	gDebugLog.Log("redoscript: <%s>", (const char *) theCard);
	gCardManager.DoReDoScript(theCard);	
}
#endif

//
//	DoRefresh - Refresh the screen.
//
void CCard::DoRefresh(void)
{
	//gPlayerView->Refresh();

}

//
//	DoResetOrigin - Reset the origin or set it to something new.
//
void CCard::DoResetOrigin(void)
{
	TPoint		newOrigin(0, 0);
	
	if (m_Script.more())
		m_Script >> newOrigin;
		
	SetOrigin(newOrigin);
}

/*---------------------------------------------------------------
    (RESUME)

    If the user touched a touch zone and mPaused a playing segment
    (audio or video) then this command will resume playback.
-----------------------------------------------------------------*/
void CCard::DoResume()
{
#ifdef DEBUG
	gDebugLog.Log("resume");
#endif

	gPlayerView->DoResume(false);
}

//
//	(return) - stop execution of this card or macro
//
void CCard::DoReturn()
{
#ifdef DEBUG
	gDebugLog.Log("return");
#endif

	mStopped = true;
}

/*----------------------------------------------------------------------
    (REWRITE FILENAME FIELD1 <FIELD2> ... <FIELDN>)

    FILENAME    The text file which contains the record to be rewritten.

    FIELD1..N   The fields which define the record.

    Given a file that is open for appending, this command will look up
    the given record, as specified by the fields. It will move that
    record to the end of the file and position the file pointer at the
    end of the file (appending) so that the specific data may be
    written.
------------------------------------------------------------------------*/
void CCard::DoRewrite()
{
    TString		searchString, param, filename;
    int16		numFields = 0;

    m_Script >> filename;

    //  Append all the fields together into a search string that looks
    //  like "field1 TAB field2 TAB ... fieldN"
    //
    while (m_Script.more()) 
    {
        m_Script >> param;
        if (numFields > 0)
            searchString += '\t';
        numFields++;
        searchString += param;
    }

#ifdef DEBUG
	gDebugLog.Log("rewrite: file <%s>, look for <%s>", filename.GetString(), searchString.GetString());
#endif

    gFileManager.Rewrite(filename, searchString, numFields);
}

/*---------------------------------------------------------------
    (RNODE PICTURE.PCX || FONT.GFT)       10MAR94
    DISABLED!!
----------------------------------------------------------------*/
void CCard::DoRnode()
{
#ifdef CBO_FIX
    TString NodeKey;

    m_Script >> NodeKey;
   //   gHeaderManager.Remove(NodPt);
   // ZapNode(NodeKey);

#endif
}

//
//	DoQTPause - 
//
//		(pause time)
//
void CCard::DoQTPause()
{
	int32	tenths;
	
	m_Script >> tenths;
	tenths *= 100;			// to milliseconds
	
	if (gMovieManager.Playing())
	{
#ifdef DEBUG
		gDebugLog.Log("pause: %ld milliseconds", tenths);
#endif
		gPlayerView->DoPause(false);
		
		mPaused = true;
		mResumeMovie = true;		// resume movie when we wake up
		
		mNapTimer = new CTimer(tenths, NULL);
		
		// cbo_test - took this out to prevent flashing
		gPlayerView->AdjustMyCursor();
		gPlayerView->Draw(nil);
		gPlayerView->ProcessEvents(true);
	}
#ifdef DEBUG
	else
		gDebugLog.Log("pause: nothing playing");
#endif
	
}	

/*---------------------------------------------------------------
    (SCREEN COLOR)

    A fast way to fill the entire screen with a particular color.
-----------------------------------------------------------------*/
void CCard::DoScreen()
{
    int16 color;

    m_Script >> color;

#ifdef DEBUG
	gDebugLog.Log("screen: <%d>", color);
#endif

    gPlayerView->ColorCard(color);
}

/*-----------------------------------
    (SEARCH FRAME [FLAG])

    Search to the given frame number.
        If FLAG present (anything) then wait for completion.
-------------------------------------*/
void CCard::DoSearch()
{
#ifdef CBO_FIX
    int32    frame;

    m_Script >> frame;

    gVideoManager->search(frame);


#endif
}

/*---------------------------------------
    (SET VARIABLE NEWVALUE)

    Sets the variable to the given value.
-----------------------------------------*/
void CCard::DoSet()
{
    TString     	vname;
    TString			value;
    TString			flag;
    uint32			date;
    int32			date_type;
	
    m_Script >> vname >> value;
    
    if (m_Script.more())
    {
    	m_Script >> flag;
    	
    	flag.MakeLower();
    	
    	if (flag == (char *) "longdate")
    		date_type = DT_LONGDATE;
    	else if (flag == (char *) "date")
    		date_type = DT_DATE;
    	else if (flag == (char *) "time")
    		date_type = DT_TIME;
    	else if (flag == (char *) "year")
    		date_type = DT_YEAR;
    	else if (flag == (char *) "month")
    		date_type = DT_MONTH;
    	else if (flag == (char *) "longmonth")
    		date_type = DT_LONGMONTH;
    	else if (flag == (char *) "day")
    		date_type = DT_DAY;
    	else if (flag == (char *) "longday")
    		date_type = DT_LONGDAY;
    	else
    		gLog.Caution("Bad flag to set command <%s>", flag.GetString());

		date = (uint32) value;
		
    	gVariableManager.SetDate(vname.GetString(), date, date_type);

#ifdef DEBUG
		gDebugLog.Log("set date: <%s> to <%s>", (const char *) vname, gVariableManager.GetString(vname.GetString()));
#endif  	
    }
    else
    {
#ifdef DEBUG
		gDebugLog.Log("set: <%s> to <%s>", (const char *) vname, (const char *) value);
#endif

    	gVariableManager.SetString(vname.GetString(), value.GetString());
    }
}


/*---------------------------------------------------------------------
    (SHOWMOUSE)
    Shows the mouse (shouldn't be needed, maybe only in conjunction w/ hide~)
 ---------------------------------------------------------------------*/
void CCard::DoShowmouse()
{
	gCursorManager.ShowCursor();
}

/*----------------------------------------------
    (STILL)

    Pause the video playback.
------------------------------------------------*/
void CCard::DoStill()
{
#ifdef DEBUG
	gDebugLog.Log("still");
#endif

	gPlayerView->DoPause(false);
}

/*----------------------------------------------
    (SUB VARIABLE AMOUNT)

    Subtract the given amount from the variable.
    
    cbo - This originally was written to use floats.
------------------------------------------------*/
void CCard::DoSub()
{
	TString 	theVarName;
	uint32		theAmount;
	uint32		theOrigValue;
	uint32		theResValue;

    m_Script >> theVarName >> theAmount;


    theOrigValue = gVariableManager.GetLong(theVarName.GetString());
    theResValue = theOrigValue - theAmount;

    gVariableManager.SetLong(theVarName, theResValue);

#ifdef DEBUG
	gDebugLog.Log("sub: %s <%ld> - <%ld> = <%ld>", 
		(const char *) theVarName, theOrigValue, theAmount, theResValue);
#endif
}

/*--------------------------------------------------------------
    (TEXT HEADER LEFT TOP RIGHT BOTTOM TEXTSTRING)

    Display the given textstring, using the given header style,
    within the given rect. Note that the bottom of the rectangle
    is elastic... it will actually be as much or as little as
    necessary to display all the text.
----------------------------------------------------------------*/
void CCard::DoText()
{
	CPlayerText	*textPtr;
	TRect		bounds;
	TString 	header, text;

    m_Script >> header >> bounds >> text;

    AdjustRect(&bounds);

#ifdef DEBUG
	gDebugLog.Log("text: header <%s>, text <%s>", header.GetString(), text.GetString());
#endif
    
    textPtr = new CPlayerText(header.GetString(), bounds, text.GetString(), 0, 0);
	if (textPtr != nil)
		delete textPtr;
}

/*--------------------------------------------------------------
    (TEXTNEW STYLESHEET LEFT TOP RIGHT BOTTOM TEXTSTRING)

    Display the given textstring, using the given header style,
    within the given rect. Note that the bottom of the rectangle
    is elastic... it will actually be as much or as little as
    necessary to display all the text.
----------------------------------------------------------------*/
void CCard::DoTextAA()
{
	TRect		bounds;
	std::string style, text;

    m_Script >> style >> bounds >> text;

    AdjustRect(&bounds);
	gDebugLog.Log("textaa: style <%s>, text <%s>",
				  style.c_str(), text.c_str());

	try
	{
		gStyleSheetManager.Draw(style, text,
								GraphicsTools::Point(bounds.Left(),
													 bounds.Top()),
								bounds.Right() - bounds.Left(),
								gPlayerView);
	}
	catch (std::exception &error)
	{
		gDebugLog.Error("ERROR: %s", error.what());
	}
	catch (...)
	{
		gDebugLog.Error("ERROR: Unknown exception");
	}
}
        
        
/*-----------------------------------------------------------
    (TIMEOUT DELAY CARD)

    If the user doesn't respond in DELAY seconds, jump to the
    given card.
-------------------------------------------------------------*/
void CCard::DoTimeout()
{
	CCard		*theCard;
    TString 	cardName;
    int32     	secs = 0;

    m_Script >> secs >> cardName;
	theCard = gCardManager.GetCard(cardName.GetString());

	secs *= 1000;		// to get it into milliseconds

#ifdef DEBUG
	gDebugLog.Log("timeout: delay <%ld>, jump to <%s>", secs, (const char *) cardName);
#endif
	
	if (mTimeoutTimer == NULL)
		mTimeoutTimer = new CTimer(secs, (void *) theCard);
}

/*--------------------------------------------------------------
    (TOUCH LEFT TOP RIGHT BOTTOM CARD CURSOR <PICT <X Y>>)

    Create a touch zone bounded by the given rectangle. Touching
    this touch zone will make the program go to card CARD. If a
    PICT is specified than that picture will highlight in
    reponse to the touch. If there are more than one copy of
    a given picture on the screen, specify an X and Y coordinate
    for the one you want to highlight.
----------------------------------------------------------------*/
void CCard::DoTouch()
{
    char        ch_sep;
    TRect       bounds;
    TPoint		loc;
    CPicture	*thePicture = NULL;
    TString     theCommand, SecondCommand;
    TString     cmdText, scmdText;
    TString     picname;
    TString		cursorType;
    CursorType	cursor = HAND_CURSOR;
    CursorType	tmpCursor;

    m_Script >> bounds >> cmdText;

#ifdef DEBUG
	gDebugLog.Log("touch: L <%d>, T <%d>, R <%d>, B <%d>", bounds.Left(), bounds.Top(), bounds.Right(), bounds.Bottom());
#endif

    theCommand = "(";
    theCommand += cmdText;
    theCommand += ")";

    AdjustRect(&bounds);

	if (m_Script.more() and (m_Script.curchar() != '('))
	{
		m_Script >> cursorType;

		tmpCursor = gCursorManager.FindCursor(cursorType);
		if (tmpCursor != UNKNOWN_CURSOR)
			cursor = tmpCursor;
		else
	    {
	    	// unknown cursor, assume it must be a picture name, we know it can't be
	    	//	the second command as it didn't start with an open paren
	    	picname = cursorType;
	    	if (m_Script.more()) 
			{
				m_Script >> loc;
				AdjustPoint(&loc);
			}
			else 
				loc = bounds.TopLeft();
		}
	}
			    
    if (m_Script.more()) 
    {
        ch_sep = m_Script.curchar();

        if (ch_sep == '(')  
        {
            m_Script >> SecondCommand;
            scmdText = "(";
            scmdText += SecondCommand;
            scmdText += ")";
        }
        
		if (m_Script.more())
		{
			// Get the name of the picture
			m_Script >> picname;
	
			// Get it's offset (if specified), otherwise just use the topLeft of the bounds
			if (m_Script.more()) 
			{
				m_Script >> loc;
				
				AdjustPoint(&loc);
			}
			else 
				loc = bounds.TopLeft();
		}
    }
    
    if (not picname.IsEmpty())
    	thePicture = gPictureManager.GetPicture(picname);
    
    new CTouchZone(bounds, theCommand, thePicture, loc, cursor, scmdText);
    
   // gPlayerView->AdjustMyCursor();
}

/*----------------------------------------------------------
    (UNBLIPPO <EFFECT> <DELAY>)

    Copies the offscreen buffer to the display with a given
    special effect. An optional delay will allow the user to
    control the speed of this effect. DELAY is the length in
    tenths of seconds that the effect should take.
------------------------------------------------------------*/
void CCard::DoUnblippo()
{
    TString	effect;
    int32   delay = 0;
    FXType	theEffect = kFXNone;

    if (m_Script.more())
    {
        m_Script >> effect;
	        theEffect = StringToEffect(effect);

        if (m_Script.more())
        	m_Script >> delay;
    }

#ifdef DEBUG
	gDebugLog.Log("unblippo: Effect: <%d>  Delay: <%ld>", theEffect, delay);
#endif

	if (gPlayerView->BlippoAvailable())
		gPlayerView->UnBlippo(theEffect, delay);
#ifdef DEBUG
	else
        gDebugLog.Caution("Unblippo: No Blippo bits available!");
#endif
}


/*----------------------------------------------------------
    (UNLOCK <EFFECT> <DELAY>)

    Copies the offscreen buffer to the display with a given
    special effect. An optional delay will allow the user to
    control the speed of this effect. DELAY is the length in
    tenths of seconds that the effect should take.
------------------------------------------------------------*/
void CCard::DoUnlock()
{
    TString	effect;
    int32   delay = 0;
    FXType	theEffect = kFXNone;

    if (m_Script.more()) 
    {
        m_Script >> effect;
	        theEffect = StringToEffect(effect);

        if (m_Script.more())
        	m_Script >> delay;
    }

#ifdef DEBUG
	gDebugLog.Log("unlock: Effect: <%d>  Delay: <%ld>", theEffect, delay);
#endif

	gPlayerView->UnLock(theEffect, delay);
}

/*------------------------------------------------------
    (VIDEO EFFECT)

    Switch the video only mode with the given effect.

    Note: this effect is accomplished, in most cases, by
    temporarily switching to overlay (key) mode for the
    effect.
--------------------------------------------------------*/
void CCard::DoVideo()
{
#ifdef CBO_FIX
    TString effect;
    Effect  theEffect;

    m_Script >> effect;
//    theEffect = StringToEffect(effect);
//    gVideoManager->video(theEffect);
	pfadeup(true, true);

#endif
}


/*-----------------------------------------------------------
    (WAIT <FRAME>)

    Wait until the videodisc player reaches the given frame
    number, or the end of the currently playing segment if
    no frame number is given.

    This is a busy wait. See LVideo.cpp for information about
    what the system does while waiting for the frame.
-------------------------------------------------------------*/
void CCard::DoWait()
{
    int32    frame = 0;

    if (m_Script.more()) 
		m_Script >> frame;

#ifdef DEBUG
	gDebugLog.Log("wait: %ld", frame);
#endif

	// have to check if we are playing something first to be sure
	//		WakeCard will actually do something and we sould pause
	if (gMovieManager.Playing())
	{
		// if we are playing audio, blast the gworld to the screen
		if (gMovieManager.AudioPlaying())
		{
			// cbo_test - took this out because was causing flashing
			gPlayerView->AdjustMyCursor();

			gPlayerView->Draw(nil);
			
			gPlayerView->ProcessEvents(true);
		}

		gMovieManager.WakeCard(frame);
		mPaused = true;
	}
#ifdef DEBUG
	else
	{
		if ((frame == 0) and (gModMan->NoVolume()))
		{
			gDebugLog.Log("wait: no volume or movie, pausing");
			
			//::SysBeep(30);
			mPaused = true;
		}
		else
			gDebugLog.Log("wait: nothing to wait for");
	}
#endif
}

/*-----------------------------------------------------------
    (WRITE FILENAME DATA)

    Write the given data to the file.
-------------------------------------------------------------*/
void CCard::DoWrite()
{
    TString     filename, data;

    m_Script >> filename >> data;

#ifdef DEBUG
	gDebugLog.Log("write: file <%s>, data <%s>", filename.GetString(), data.GetString());
#endif

    gFileManager.Write(filename, data);
}

/***************************

    CARD MANAGER METHODS

***************************/

CCardManager::CCardManager() : TIndexManager()
{
    mCurrentCard = NULL;
    mExitNow = false;
    mHaveJump = false;
    mExitSide = 0;
#ifdef DEBUG
	mReDoScript = false;
#endif
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
void CCardManager::DoOneCommand(TString &theCommand)
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
		
		if (mExitNow)
		{
			mExitNow = false;
			
			gTheApp->DoExit(mExitSide);
		}
#ifdef DEBUG
		if (mReDoScript)
		{
			mReDoScript = false;
			
			gTheApp->ReDoScript(mReDoCard);
		}
#endif
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
	mExitNow = false;
}

//
//	DoExit
//
void CCardManager::DoExit(int16 inSide)
{
	mExitNow = true;
	mExitSide = inSide;
	
	if (mCurrentCard != nil)
		mCurrentCard->Stop();		// no more executing commands
}

#ifdef DEBUG
void CCardManager::DoReDoScript(TString &cardName)
{
	mReDoScript = true;
	mReDoCard = cardName;
}
#endif

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
 * Function: CCardManager::MakeNewIndex
 *
 *  Parameter name
 *  Parameter start
 *  Parameter end
 * Return:
 *
 * Comments:
 *    Adds node "name" to CCardManager..
 ***********************************************************************/
void CCardManager::MakeNewIndex(TIndexFile *inFile, const char *inName,
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
		gLog.Caution("Trying to jump to <%s>, couldn't find it", newCardName);
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

TString CCardManager::ReadSpecialVariable_curcard()
{
	return gCardManager.CurCardName();
}

TString CCardManager::ReadSpecialVariable_prevcard()
{
	return gCardManager.PrevCardName();
}

