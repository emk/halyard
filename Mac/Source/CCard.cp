/**********************************************

    CCard class. This is the class that knows
    how to execute commands.

***********************************************/

#include "debug.h"

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "Mac5L.h"

#include "CMac5LApp.h"
#include "CCard.h"
#include "CPrefs.h"
#include "CVariable.h"
#include "CRect.h"
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

#include "util.h"
#include "stack.h"
#include "gamma.h"

// cbo_debug
//#include <profiler.h>


/**************

    GLOBALS

**************/

static Boolean gNeedsRefresh = false;


/*******************

    CARD METHODS

*******************/

//
//  CCard - Initialize a card. This will happen when the script is read
//			in from disk so don't activate the card yet.
//
CCard::CCard(const char *name, int32 p1, int32 p2) : CIndex(name, p1, p2)
{
	mPaused = false;
	mActive = false;
	mDoingOne = false;
	mResumeMovie = false;
	
	mIndex = -1;
	
	mTimeoutTimer = NULL;
	mNapTimer = NULL;
	
    mOrigin.Set(0, 0);
}

CCard::~CCard()
{
}

//
//	Start - Start the card. This will initialize everything. 
//
void CCard::Start(void)
{
#ifdef DEBUG_5L
	prinfo("Start card <%s>", Name());
#endif
	gPlayerView->AdjustMyCursor();

	mPaused = false;
	mResumeMovie = false;
	
	SetScript();					// load in the script from the file
	script.reset();					// reset the script
	mOrigin.Set(0, 0);				
	mActive = true;

#ifdef DEBUG_5L
	// dump the card out to the debug file
	prinfo("script: %s", script.GetString());
#endif
	
	script >> open >> discard >> discard;	// remove leading junk
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
#ifdef DEBUG_5L
			prinfo("Hit timeout timer, jumping to card");
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
#ifdef DEBUG_5L
			prinfo("hit the end of the nap, resuming");
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
		if (script.more())
		{
			// cbo_debug
			//ProfilerSetStatus(true);
			
			// Process all commands as long as we haven't paused or don't jump
			//	somewhere else.
			while ((mActive) 
				and (script.more()) 
				and (not mPaused)
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
    CString     opword;

    script >> open >> opword;
    opword.makelower();
	
    if (opword == (char *)"add") DoAdd();
    // new audio commands
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
    else if (opword == (char *)"close") DoClose();
    else if (opword == (char *)"ctouch") DoCTouch();
#ifdef DEBUG_5L
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
    else if (opword == (char *)"pause") DoQTPause();
    else if (opword == (char *)"play") DoPlay();
    else if (opword == (char *)"playqtfile") DoPlayQTFile();
    else if (opword == (char *)"playqtloop") DoPlayQTLoop();
    else if (opword == (char *)"playqtrect") DoPlayQTRect();
    else if (opword == (char *)"preload") DoPreloadQTFile();
    else if (opword == (char *)"print") DoPrint();
   // else if (opword == (char *)"qtpause") DoQTPause();
    else if (opword == (char *)"read") DoRead();
#ifdef DEBUG_5L
	else if (opword == (char *)"redoscript") DoReDoScript();
#endif
//	else if (opword == (char *)"refresh") DoRefresh();
    else if (opword == (char *)"resume") DoResume();
    else if (opword == (char *)"rewrite") DoRewrite();
    else if (opword == (char *)"rnode" || opword == (char *)"rvar") DoRnode(); 
    else if (opword == (char *)"screen") DoScreen();
    else if (opword == (char *)"search") DoSearch();
    else if (opword == (char *)"set") DoSet();
    else if (opword == (char *)"showmouse") DoShowmouse();
    else if (opword == (char *)"still") DoStill();
    else if (opword == (char *)"sub") DoSub();
    else if (opword == (char *)"text") DoText();
    else if (opword == (char *)"timeout") DoTimeout();
    else if (opword == (char *)"touch") DoTouch();
    else if (opword == (char *)"unblippo") DoUnblippo();
    else if (opword == (char *)"unlock") DoUnlock();
    else if (opword == (char *)"video") DoVideo();
    else if (opword == (char *)"wait") DoWait();
    else if (opword == (char *)"write") DoWrite();
    else DoMacro(opword);

    script >> close;
}

/***********************************************************************
 * Function: CCard::OneCommand
 *
 *  Parameter theCommand
 * Return:
 *
 * Comments:
 *  Execute a single command, perhaps in response to a touch zone or
 *  a timeout. Save the old script, do the one command, and restore
 *  the script.
 *
 *  theCommand should look like "(jump aCard)", ie both parens need to
 *  be there.
 ***********************************************************************/
void CCard::OneCommand(CString &theCommand)
{
    CStream     saveScript(script);

	saveScript = script;
	mDoingOne = true;
	
    script = theCommand;
    DoCommand();
    
    script = saveScript;

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
void CCard::AdjustRect(CRect *r)
{
    r->left += mOrigin.x;
    r->right += mOrigin.x;
    r->top += mOrigin.y;
    r->bottom += mOrigin.y;
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
void CCard::AdjustPoint(CPoint *pt)
{
    pt->x += mOrigin.x;
    pt->y += mOrigin.y;
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
void CCard::SetOrigin(CPoint &loc)
{
    mOrigin = loc;
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
void CCard::OffsetOrigin(CPoint &delta)
{
    mOrigin.x += delta.x;
    mOrigin.y += delta.y;
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
int16 CCard::Evaluate(CStream& conditional)
{
    int16		globalRes, localRes, result;
    EvalMode	mode = FirstTime;
    CString     op;
    CString     modeStr;
    CString     str1, str2;

    globalRes = localRes = false;

    while (conditional.more()) 
	{
        conditional >> str1 >> op >> str2;

		// See if op is contains first.
		if (op == (char *)"contains")
		{
			char *res = nil;
			
			res = strstr(str1.GetString(), str2.GetString());
			if (res != nil)
				localRes = true;
			else
				localRes = false;
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
	            prerror("IF: unknown operator %s.", (char *) op);
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
            modeStr.makelower();
            
			if (modeStr == (char *) "and") 
			{
                if (mode == Or) 
					prcaution("IF: can't mix ANDs and ORs.");
                mode = And;
                
                // one false makes a whole bunch of ANDed things false
                if (not globalRes)
                	return (globalRes);
            } 
			else if (modeStr == (char *) "or") 
			{
                if (mode == And) 
					prcaution("IF: can't mix ANDs and ORs.");
                mode = Or;
                
                // one true makes a whole bunch of ORed things true
                if (globalRes)
                	return(globalRes);
            } 
			else
			{
                prcaution("IF: expected AND or OR here, not %s.", (char *) modeStr);
                mode = And;
            }
        }
    }
    return globalRes;
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
	CString		theVarName;
	int32		theAmount;
	int32		theOrigValue;
	int32		theResValue;

    script >> theVarName >> theAmount;

    
	// cbo_fix - we don't have fcvt like Windows does
   // sum = gVariableManager.GetDouble(vname);
	theOrigValue = gVariableManager.GetLong(theVarName);
    theResValue = theOrigValue + theAmount;

    //gVariableManager.SetDouble(vname, sum);
    gVariableManager.SetLong(theVarName, theResValue);

#ifdef DEBUG_5L_SCRIPT
	prinfo("add: %s <%ld> + <%ld> = <%ld>", 
		(char *) theVarName, theOrigValue, theAmount, theResValue);
#endif
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
	CString		loop_flag;
	int32		fade_time = 0;
	bool		do_kill_loops = FALSE;
	
	if (script.more())
		script >> fade_time;
		
	if (script.more())
	{
		script >> loop_flag;
		loop_flag.makelower();
		
		if (loop_flag == (char *) "loop")
			do_kill_loops = true;
		else
			prcaution("Bad flag to audiokill command <%s>", loop_flag.GetString());

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
	CString		audio_file;
	CString		flag;
	int32		the_offset = 0;
	int32		the_volume = 100;
	int32		the_fade_time = 0;
	bool		do_loop = false;
	bool		do_kill = false;
	
	script >> audio_file;
	
	if (script.more())
		script >> the_offset;
	if (script.more())
		script >> the_volume;
	if (script.more())
		script >> the_fade_time;
	
	while (script.more())
	{
		script >> flag;
		flag.makelower();
		
		if (flag == (char *) "kill")
			do_kill = true;
		else if (flag == (char *) "loop")
			do_loop = true;
		else
			prcaution("Bad flag to audioplay command <%s>", flag.GetString());
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
	
	script >> the_volume;
	
	if (script.more())
		script >> the_fade_time;
		
}

//
//	DoAudioWait - Wait for a particular frame of the playing audio clip.
//
//		(audiowait frame)
//
void CCard::DoAudioWait(void)
{
	int32		the_frame;
	
	script >> the_frame;
	
}	
	
	

/*---------------------------------------------------------------
    (BACKGROUND picfile x1 y1 x2 y2)

	Draw the pic in the bounding rectangle (x1 y1 x2 y2)
-----------------------------------------------------------------*/
void CCard::DoBackground()
{
    CString     TempCmd, picname;
    CRect     	loc;
    Rect		macLoc;
    
    script >> picname >> loc;
    
    AdjustRect(&loc);
	loc.MakeMacRect(&macLoc);

#ifdef DEBUG_5L_SCRIPT
	prinfo("background: <%s>, L<%d>, T<%d>, R<%d>, B<%d>", 
		(char *) picname, macLoc.left, macLoc.top, macLoc.right, macLoc.bottom);
#endif

    gPlayerView->SetBackPic((char *) picname, macLoc);
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

    if (script.more()) 
		script >> freq;
    
    if (script.more()) 
	{
        script >> duration;
        duration *= 100;
    }

#ifdef DEBUG_5L_SCRIPT
	prinfo("beep: freq <%d>, duration <%d>", freq, duration);
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
#ifdef DEBUG_5L_SCRIPT
	prinfo("blippo: ");
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
    CRect bounds;

    script >> bounds;
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
    CRect		bounds;
    Rect		macBounds;
    int16		color, lineThickness = 1;
    CString		fill;
    Boolean		theFill = false;


    script >> bounds >> fill >> color;
    if (script.more()) 
    	script >> lineThickness;

#ifdef DEBUG_5L_SCRIPT
	prinfo("box: L <%d>, T <%d>, R <%d>, B <%d>", bounds.left, bounds.top, bounds.right, bounds.bottom);
#endif

    AdjustRect(&bounds);
    bounds.MakeMacRect(&macBounds);
    
    fill.makelower();
    if (fill == (char *) "fill")
    	theFill = true;
    
    boxPtr = new CPlayerBox(macBounds, theFill, lineThickness, color);
	if (boxPtr != nil)
		delete boxPtr;
}

/*---------------------------------------------------------------
    (BUTTPCX PCXFILE X Y header text Command <second Command>)
    Puts a button with text "text" on the screen and activates a corresponding
    touchzone.  It takes care of the picture and hilited picture display.
    OPTIONAL:  set command to be executed prior to the jump.
-----------------------------------------------------------------*/

void CCard::DoButtpcx()
{
    CRect       bounds1;
	Rect		macBounds;
    CPoint      buttLoc;
	Point		macLoc;
	char		*theHeadName = nil;
    CPicture    *thePicture = nil;
    CString     theHeaderName, picname, theCommand, cmdText, Text, scmdText;
    CString     secondCommand;
    CString		cursorType;
    CursorType	cursor = HAND_CURSOR;

    script >> picname >> buttLoc >> theHeaderName >> Text >> cmdText;

	if (script.more())
		script >>	cursorType;
		
	cursor = gCursorManager.FindCursor(cursorType);
	
   	scmdText = "";
    if (script.more())  
	{
        script >> secondCommand;      // OPTIONAL: (command <params +>)...
        scmdText= "(";
        scmdText += secondCommand;
        scmdText += ")";
    }
	
    AdjustPoint(&buttLoc);
   	buttLoc.MakeMacPt(&macLoc);
	
    theCommand = "(";       // due to parser's stripping of parens.
    theCommand += cmdText;
    theCommand += ")";

	if (not picname.empty())
	{
		thePicture = GetPicture(picname.GetString(), true);
		
		if (thePicture != NULL)
		{
			macBounds = thePicture->GetPictureRect();
			::OffsetRect(&macBounds, macLoc.h, macLoc.v);
		}
	}
		
	if (not theHeaderName.empty())
		theHeadName = theHeaderName.GetString();

#ifdef DEBUG_5L_SCRIPT
	prinfo("buttpcx: <%s>, X<%d>, Y<%d>, header <%s>, text <%s>, cmd <%s>, cmd2 <%s>",
			(char *) picname.GetString(), macLoc.h, macLoc.v, (char *) theHeadName, 
			(char *) Text, (char *) cmdText, (char *) scmdText);
#endif
		
	if (thePicture != NULL)
		new CTouchZone(macBounds, theCommand, thePicture, macLoc, Text, cursor, 
				theHeadName, scmdText);
	
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
	CString		vol_name;
	CString		wrong_disc;			// where to jump if wrong disc 
	CString		no_disc;			// where to jump if no disc
	char		*jump_card;			// which one of those two to jump to
	int32		check_disc;
	bool		do_jump = FALSE;
	
	script >> vol_name >> wrong_disc >> no_disc;
	
#ifdef DEBUG_5L_SCRIPT
	prinfo("checkdisc: <%s>, if fail <%s>", 
		vol_name.GetString(), wrong_disc.GetString());
#endif

	// turn vol_name into a volume name
	vol_name += ":";
	
	check_disc = gVariableManager.GetLong("_NoCheckDisc");
	if (check_disc == 0)
	{
		// right now, this will always return true
		if (gModMan->CDInDrive())
		{
			if (not gModMan->VolMounted(vol_name))
			{
#ifdef DEBUG_5L_SCRIPT
				prinfo("Wrong disc in CD player");
#endif
				jump_card = wrong_disc.GetString();
				do_jump = TRUE;
			}
		}
		else
		{
#ifdef DEBUG_5L_SCRIPT
			prinfo("No disc in CD player");
#endif
			jump_card = no_disc.GetString();
			do_jump = TRUE;
		}	

		// if we found the disc we were looking for, set the 
		// current CD so it will find movies
		if (not do_jump)
		{			
#ifdef DEBUG_5L_SCRIPT
			prinfo("Disc found, setting current CD to <%s>", vol_name.GetString());
#endif
			gModMan->SetCurCD(vol_name.GetString());
		}
	}
#ifdef DEBUG_5L_SCRIPT
	else
		prinfo("_NoCheckDisc is true, no jumping");
#endif
	
	if (do_jump)
	{
		gPlayerView->ProcessEvents(false);	// stop processing keys and touch zones
	
    	gCardManager.JumpToCardByName(jump_card, false);
    
    	gPlayerView->Draw(nil);				// refresh the screen so see everything before jumping		
	}	
}

/*----------------------------------------------------------------------
    (CLOSE FILENAME)

    Close the given text file.
------------------------------------------------------------------------*/
void CCard::DoClose()
{
    CString     filename;

    script >> filename;

#ifdef DEBUG_5L_SCRIPT
	prinfo("close: file <%s>", filename.GetString());
#endif

    gFileManager.Close(filename.GetString());
}

/*-------------------------------------------------------------
    (CTOUCH)

    Clear all current touch zones. Typically called immediately
    before setting new touch zones.
---------------------------------------------------------------*/
 void CCard::DoCTouch()
{
    int16	left, top;

    if (script.more())  
    {
        script >> left >> top;

#ifdef DEBUG_5L_SCRIPT
	prinfo("ctouch: at left <%d>, top <%d>", left, top);
#endif   

        gPlayerView->CTouch(left, top);
    }
    else 
    {
#ifdef DEBUG_5L_SCRIPT
	prinfo("ctouch: all");
#endif   
    	gPlayerView->CTouch();
    }

	// cbo_test - put this back in because we took it out in wait and nap
	gPlayerView->AdjustMyCursor();   	
}

//
//	DoDebug - Drop into the debugger
//
#ifdef DEBUG_5L
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
    CString		theVarName;
    double		theAmount;
    int32		theOrigValue;
    int32		theResValue;

    script >> theVarName >> theAmount;


	theOrigValue = gVariableManager.GetLong(theVarName);

    if (theAmount == 0.0)
    {
		prcaution("Division by zero: %s <%ld> / <%f>", 
			(char *) theVarName, theOrigValue, theAmount);
			
		theResValue = 0;
    }
    else
		theResValue = (int32) (theOrigValue / theAmount);
   
    gVariableManager.SetLong(theVarName, theResValue);
    
#ifdef DEBUG_5L_SCRIPT
	prinfo("div: %s <%ld> by <%f> = <%ld>", 
		(char *) theVarName, theOrigValue, theAmount, theResValue);
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
	
	if (script.more())
		script >> theSide;

#ifdef DEBUG_5L_SCRIPT
	prinfo("exit: %d", theSide);
#endif
		
	gCardManager.DoExit(theSide);
}

/*---------------------------------------------------------------
    (FADE DIR <STEPS>)

    Either FADE IN or FADE OUT. This ramps the palette in or out.
-----------------------------------------------------------------*/
void CCard::DoFade()
{
    CString     direction;
    int16       steps = 1;

    script >> direction;
    if (script.more()) 
    	script >> steps;

#ifdef DEBUG_5L_SCRIPT
	prinfo("fade: %s, steps <%d>", direction.GetString(), steps);
#endif

	// cbo_hack - try making the fades a bit faster
//	if (steps >= 10)
//		steps -= (steps / 3);
	
    direction.makelower();
    if (direction == (char *) "in")
    {
    	gPlayerView->Draw(nil);		// refresh the screen first
		DoGFade(true, steps, true);
	}
    else if (direction == (char *) "out")
		DoGFade(false, steps, true);
    else
        prcaution("Fade in or out, but don't fade %s", (char *)direction);
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
    CString     picName;
    Picture     *thePicture;
    CPoint       pt;

    script >> picName;

    thePicture = GetPicture(picName);
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
	gPlayerView->ShowMouse(false);
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
    CStream     conditional;

    script >> conditional;
    conditional.reset();

#ifdef DEBUG_5L_SCRIPT
	prinfo("if: <%s>", (char *) conditional);
#endif

    if (Evaluate(conditional)) 
    {
#ifdef DEBUG_5L_SCRIPT
		prinfo("if -> true");
#endif
        DoCommand();
    } 
    else 
    {
#ifdef DEBUG_5L_SCRIPT
		prinfo("if -> false");
#endif
        //  Skip TRUE_CMD.
        script >> open >> close;
        if (script.more()) 
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
    CString     theVarName, mask, style, required;
   // CPoint      loc;
    CRect		bounds;
    Rect		macBounds;
   // Point		macLoc;
    int16       fRequire = false;

    script >> style >> theVarName >> mask >> bounds;

    if (script.more()) 
    {
        script >> required;
        required.makelower();
        if (required == (char *) "true") 
        	fRequire = true;
    }

   // AdjustPoint(&loc);
   	//loc.MakeMacPt(&macLoc);
   	AdjustRect(&bounds);
   	bounds.MakeMacRect(&macBounds);

#ifdef DEBUG_5L_SCRIPT
	prinfo("input: into <%s>, style <%s>, mask <%s>, L <%d>, T <%d>, R <%d>, B <%d>, require <%s>",
			(char *) theVarName, (char *) style, (char *) mask, bounds.left, bounds.top, bounds.right, bounds.bottom,
			(char *) required);
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
    CString     jumpCard;

    script >> jumpCard;

#ifdef DEBUG_5L_SCRIPT
	prinfo("jump: to <%s>", (char *) jumpCard);
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

    script >> newKeyColor;
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
    CString 	keyEquiv; 
    CString		linkCard;
	CCard		*theCard;
	char		theChar;
	
    script >> keyEquiv;

    if (script.more())
        script >> linkCard;
        
    keyEquiv.makelower();
    if (keyEquiv == (const char *) "esc")
    	theChar = 0x1B;					// the Escape key
    else
    	theChar = keyEquiv.getch(0);

#ifdef DEBUG_5L_SCRIPT
	prinfo("keybind: key <%c>: Jump to card <%s>", keyEquiv.getch(0), linkCard.GetString());
#endif

	theCard = (CCard *) gCardManager.FindNode(linkCard.GetString(), true);
	
	if (theCard != NULL)
    	gPlayerView->AddKeyBinding(theChar, theCard);
#ifdef DEBUG_5L_SCRIPT
	else
		prcaution("Trying to keybind to non-existant card <%s>!", (char *) linkCard);
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
#ifdef DEBUG_5L_SCRIPT
		prinfo("kill: the movie be dead");
#endif
	}
#ifdef DEBUG_5L_SCRIPT
	else
		prinfo("kill: nothing to kill");
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
    CPoint  	a, b;
    int16   	color, thickness = 1;
    Rect		theRect;

    script >> a >> b >> color;
    
    if (script.more())
    	script >> thickness;

    AdjustPoint(&a);
    AdjustPoint(&b);
    
    //
    // make sure horizontal and vertical lines don't have
    //			funny numbers when thickness > 1
    if ((b.x == (a.x + thickness)) or (b.x == (a.x - thickness)))
    	b.x = a.x;
    if ((b.y == (a.y + thickness)) or (b.y == (a.y - thickness)))
    	b.y = a.y;
    
    ::SetRect(&theRect, a.x, a.y, b.x, b.y);
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
    CString 	palname;
    CString		flag;
    bool		noload = false;
    bool		lock = false;
    bool		unlock = false;

	script >> palname;

	palname.makelower();

#ifdef DEBUG_5L_SCRIPT
	prinfo("loadpal: <%s>", palname.GetString());
#endif

	while (script.more())
	{
		script >> flag;
		flag.makelower();
		
		if (flag == (char *) "noload")
			noload = true;
		else if (flag == (char *) "lock")
			lock = true;
		else if (flag == (char *) "unlock")
			unlock = true;
		else
			prcaution("Bad flag to loadpal command <%s>", flag.GetString());
	}
	
	thePal = GetPalette(palname.GetString());
	
	if (thePal != nil)
	{
		if (lock)
			thePal->Lock(true);
		else if (unlock)
			thePal->Lock(false);
			
		if (not noload)
			thePal->SetPalette(true);
	}
#ifdef DEBUG_5L
	else
		prinfo("Couldn't find palette <%s>", palname.GetString());
#endif
}

/*---------------------------------------------------------------------
    (LOADPIC PICTURE X Y <FLAGS...>)
-----------------------------------------------------------------------*/
void CCard::DoLoadpic()
{
	CPlayerPict	*pictPtr;
    CString     TempCmd, picname, flag;
    CPicture    *thePicture = NULL;
    CPalette	*thePalette = NULL;
    Rect		macBounds;
    CPoint	    loc;
    Point		macLoc;
    bool       	matte = false;
    bool		noshow = false;
    bool		lock = false;
    bool		unlock = false;
    bool		do_pal = false;
    
    script >> picname >> loc;
	picname.makelower();
	
    AdjustPoint(&loc);
	loc.MakeMacPt(&macLoc);
	

#ifdef DEBUG_5L_SCRIPT
	prinfo("loadpic: <%s>, X <%d>, Y <%d>", picname.GetString(), macLoc.h, macLoc.v);
#endif
		
    while (script.more()) 
    {
        script >> flag;
        flag.makelower();
        
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
			prcaution("Bad flag to loadpic command <%s>", flag.GetString());
    }

	thePicture = GetPicture(picname.GetString(), matte);

	if (thePicture != nil)
	{
		if (lock)
			thePicture->Lock(true);
		else if (unlock)
			thePicture->Lock(false);
	}
	
	if (do_pal and (thePicture != NULL))
	{
		thePalette = GetPalette(picname.GetString());
		
		if (thePalette != nil)
			thePalette->SetPalette(true);
	}
    
	if ((not noshow) and (thePicture != NULL))
	{
		macBounds = thePicture->GetPictureRect();
		::OffsetRect(&macBounds, macLoc.h, macLoc.v);

    	pictPtr = new CPlayerPict(thePicture, macBounds, matte);
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
    CString     clear;
    bool		doClear = false;

    if (script.more())
    {
        script >> clear;
        clear.makelower();
    }

    if (clear == (char *)"clear")
		doClear = true;
    else 
		doClear = false;

#ifdef DEBUG_5L_SCRIPT
	prinfo("lock: Clear: <%d>", doClear);
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
    CString     searchString, param, filename;
    int16       numFields = 0;

    script >> filename;

    //  Append all the fields together into a search string that looks
    //  like "field1 TAB field2 TAB ... fieldN"
    //
    while (script.more()) 
    {
        script >> param;
        if (numFields > 0)
            searchString += '\t';
        numFields++;
        searchString += param;
    }

#ifdef DEBUG_5L_SCRIPT
	prinfo("lookup: file <%s>, search <%s>", filename.GetString(), searchString.GetString());
#endif
    
    gFileManager.Lookup(filename.GetString(), searchString, numFields);
}

/*-------------------------------------------------------------------
    (MACRONAME <X Y> <VAR>...)

    Call the macro by name. X, Y is the mOrigin to use for the macro.
    VAR are an optional number of local variables that vary depending
    upon the particular macrodef.
---------------------------------------------------------------------*/
void CCard::DoMacro(CString &name)
{
	CStream		saveScript(script);
    CIndex		*theMacro;
    CString		vname, contents;
    int16		vnum;
    CVariable	*local, *temp, *oldlocal;

    theMacro = (CIndex *) gMacroManager.FindNode(name, true);
	
	if (theMacro == NULL)
	{
        prcaution("Couldn't find macro/command <%s>.", (char *) name);
        return;
	}

    //
    //  Get the local variables, if passed.
    //
    local = 0;
    vnum = 0;
    while (script.more()) 
	{
        //  Variables are named 1, 2, 3...
        //
        vname = ++vnum;
        script >> contents;

        temp = new CVariable(vname, contents);

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
	// We have already saved this card's script, now set it to 
	//	the macro and execute the commands on it. 
	//
	script = theMacro->GetScript();
	
	script.reset();					// start at the beginning

#ifdef DEBUG_5L
	// dump the macro
	prinfo("macro: %s", script.GetString());	
#endif 
	
	script >> open >> discard >> discard; // toss (macrodef name
	
	// Save the origin so we can reset it when the macro is done.
	CPoint		saveOrigin(mOrigin);
		
	while ((mActive) 							// used to only check for script.more()
		and (script.more()) 
		and (not gCardManager.Jumping()))		// don't check mPaused here because we could be waiting for audio
			DoCommand();
    	
    script = saveScript;			// restore the original script
   		
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
	CString     strEffect;
    FXType      theEffect;

    script >> strEffect;

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

    script >> tenths;
    tenths *= 100;      // Convert to milliseconds
	
	if (mNapTimer == NULL)
	{
		mPaused = true;
		mNapTimer = new CTimer(tenths, NULL);

#ifdef DEBUG_5L_SCRIPT
		prinfo("nap: %d", tenths);
		prinfo("Refreshing Card (DoNap)");
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
    CString     filename, kind;
    char		*slashPtr;
    FileKind    fKind = fReadOnly;

    script >> filename >> kind;
    kind.makelower();

    if (kind == (char *)"append") 
    	fKind = fWriteAppend;
    else if (kind == (char *)"new") 
    	fKind = fWriteNew;
    else if (kind == (char *)"readonly") 
    	fKind = fReadOnly;
    else
        prcaution("Unknown open file kind: %s", (char *)kind);
    
    // Filenames can look DOS like.     
    slashPtr = strstr(filename.GetString(), "\\");
    if (slashPtr != NULL)
    	*slashPtr = ':';

#ifdef DEBUG_5L_SCRIPT
	prinfo("open: file <%s>", filename.GetString());
#endif

    gFileManager.Open(filename.GetString(), fKind);
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
    CPoint   delta;

    script >> delta;

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
    CRect		bounds;
    Rect		macBounds;
    int16		color, lineThickness = 1;
    CString		fill;
    Boolean		theFill = false;


    script >> bounds >> fill >> color;

#ifdef DEBUG_5L_SCRIPT
	prinfo("oval: L <%d>, T <%d>, R <%d>, B <%d>", bounds.left, bounds.top, bounds.right, bounds.bottom);
#endif

    AdjustRect(&bounds);
    bounds.MakeMacRect(&macBounds);
    
    fill.makelower();
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

    script >> tenths;

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

    script >> frame1 >> frame2 >> trackNum;
    if (script.more()) {
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
            prerror("Illegal audio track parameter %ld.", trackNum);
    }
    gVideoManager->audio(track);
    gVideoManager->clip(frame1, frame2, 30); //Override speed!!

#endif
}

/*-----------------------------------------------------------
    (PLAYQTFILE FILE [FRAME] [PAL])
    (PLAYQTLOOP FILE)
    
    Play a QuickTime file. Frame is the frame offset to be
    used with subsequent wait commands and corresponds to
    a laser disc frame (nothing to do with QuickTime). Pal
    is the palette to use (name.pic).
    
    PlayQTLoop is used to loop an audio file. It cannot
    be used with movies (for the time being).

-------------------------------------------------------------*/
void CCard::DoPlayQTFile()
{
	CString			theQTFile;
	CString			thePal;
	char			*thePalStr = NULL;
	int32			theOffset = 0;
	int32			theStartOffset = 0;
	bool			audioOnly = false;

	script >> theQTFile;

	if (script.more())
		script >> theOffset;

    if (strstr(theQTFile.GetString(), ".a2")) 
		audioOnly = true;

	if (audioOnly)
	{
		if (script.more())
		{
			script >> theStartOffset;
#ifdef DEBUG_5L_SCRIPT
			prinfo("Got an offset of <%ld>", theStartOffset);
#endif
		}
	}
	else
	{
		if (script.more())
			script >> thePal;
	}	
    
		
	if (not thePal.empty())
	{
		thePal.makelower();
		thePalStr = thePal.GetString();
	}

#ifdef DEBUG_5L_SCRIPT
	if (audioOnly)
	{
		prinfo("playqtfile: <%s>, offset <%ld>, start <%ld>",
			(char *) theQTFile, theOffset, theStartOffset);
	}
	else
	{
		prinfo("playqtfile: <%s>, offset <%ld>,pal <%s>",
			(char *) theQTFile, theOffset, (char *) thePal);
	}
#endif
		
	gMovieManager.Play(theQTFile.GetString(), theOffset, 
		audioOnly, thePalStr, theStartOffset);
}

//
//	PlayQTLoop - Loop the given audio file.
//
//		(playqtloop file [fade])
//
void CCard::DoPlayQTLoop()
{
	CString		theQTFile;
	int32		theFadeTime = 0;
	bool		audioOnly = false;
	
	script >> theQTFile;
	
	if (script.more())
		script >> theFadeTime;
	
    if (strstr(theQTFile.GetString(), ".a2")) 
		audioOnly = true;

	if (not audioOnly)
		prcaution("playqtloop can only be used with audio files!");
	else
	{
		
#ifdef DEBUG_5L_SCRIPT
		prinfo("playqtloop: <%s> <%ld>", (char *) theQTFile, theFadeTime);
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
	CString		theQTFile;
	CString		thePal;
	CPoint		thePT;
	Point		macPoint;
	
	script >> thePT;
	
	AdjustPoint(&thePT);
	thePT.MakeMacPt(&macPoint);
		
#ifdef DEBUG_5L_SCRIPT
	prinfo("playqtrect: X <%d>, Y <%d>", thePT.x, thePT.y);
#endif

	gMovieManager.SetOrigin(macPoint);
}
	

void CCard::DoPreloadQTFile()
{
	CString 	theQTFile;
	bool		audioOnly = false;

    script >> theQTFile;
    
    if (strstr(theQTFile.GetString(), ".a2"))
    	audioOnly = true;

#ifdef DEBUG_5L_SCRIPT
	prinfo("preload: <%s>, audio", (char *) theQTFile, audioOnly);
#endif
    	
	gMovieManager.Preroll(theQTFile.GetString(), audioOnly);    
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
    CString     just;
    CString     text;

    script >> just >> text;

    just.makelower();
    if (just == (char *)"center") {
        // Call centered print routine.
    } else {
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
    CString         filename, vname, delimstr;
    unsigned char   delim;
    CString         res;

    script >> filename >> vname;

    if (script.more()) 
    {
        script >> discard >> delimstr;
        delimstr.makelower();
        
        if (delimstr == (char *)"tab") 
        	delim = '\t';
        else if (delimstr == (char *)"return") 
        	delim = NEWLINE_CHAR;
        else if (delimstr == (char *)"eof") 
        	delim = 0;
        else 
        	delim = delimstr.getch(0);

        gFileManager.ReadUntil(filename.GetString(), res, delim);
    } 
    else 
    	gFileManager.Read(filename.GetString(), res);

#ifdef DEBUG_5L_SCRIPT
	prinfo("read: var <%s>, value <%s>", vname.GetString(), res.GetString());
#endif
    	
    gVariableManager.SetString(vname.GetString(), res.GetString());
}

#ifdef DEBUG_5L
//
//	DoReDoScript
//
void CCard::DoReDoScript()
{
	CString		theCard;
	
	script >> theCard;
	
	prinfo("redoscript: <%s>", (char *) theCard);
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

/*---------------------------------------------------------------
    (RESUME)

    If the user touched a touch zone and mPaused a playing segment
    (audio or video) then this command will resume playback.
-----------------------------------------------------------------*/
void CCard::DoResume()
{
#ifdef DEBUG_5L_SCRIPT
	prinfo("resume");
#endif

	gPlayerView->DoResume(false);
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
    CString		searchString, param, filename;
    int16		numFields = 0;

    script >> filename;

    //  Append all the fields together into a search string that looks
    //  like "field1 TAB field2 TAB ... fieldN"
    //
    while (script.more()) 
    {
        script >> param;
        if (numFields > 0)
            searchString += '\t';
        numFields++;
        searchString += param;
    }

#ifdef DEBUG_5L_SCRIPT
	prinfo("rewrite: file <%s>, look for <%s>", filename.GetString(), searchString.GetString());
#endif

    gFileManager.Rewrite(filename.GetString(), searchString, numFields);
}

/*---------------------------------------------------------------
    (RNODE PICTURE.PCX || FONT.GFT)       10MAR94
    DISABLED!!
----------------------------------------------------------------*/
void CCard::DoRnode()
{
#ifdef CBO_FIX
    CString NodeKey;

    script >> NodeKey;
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
	
	script >> tenths;
	tenths *= 100;			// to milliseconds
	
	if (gMovieManager.Playing())
	{
#ifdef DEBUG_5L_SCRIPT
		prinfo("pause: %ld milliseconds", tenths);
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
#ifdef DEBUG_5L_SCRIPT
	else
		prinfo("pause: nothing playing");
#endif
	
}	

/*---------------------------------------------------------------
    (SCREEN COLOR)

    A fast way to fill the entire screen with a particular color.
-----------------------------------------------------------------*/
void CCard::DoScreen()
{
    int16 color;

    script >> color;

#ifdef DEBUG_5L_SCRIPT
	prinfo("screen: <%d>", color);
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

    script >> frame;

    gVideoManager->search(frame);


#endif
}

/*---------------------------------------
    (SET VARIABLE NEWVALUE)

    Sets the variable to the given value.
-----------------------------------------*/
void CCard::DoSet()
{
    CString     	vname;
    CString			value;
    CString			flag;
    uint32			date;
    int32			date_type;
	
    script >> vname >> value;
    
    if (script.more())
    {
    	script >> flag;
    	
    	flag.makelower();
    	
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
    		prcaution("Bad flag to set command <%s>", flag.GetString());

		date = (uint32) value;
		
    	gVariableManager.SetDate(vname.GetString(), date, date_type);

#ifdef DEBUG_5L_SCRIPT
		prinfo("set date: <%s> to <%s>", (char *) vname, gVariableManager.GetString(vname.GetString()));
#endif  	
    }
    else
    {
#ifdef DEBUG_5L_SCRIPT
		prinfo("set: <%s> to <%s>", (char *) vname, (char *) value);
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
	gPlayerView->ShowMouse(true);
}

/*----------------------------------------------
    (STILL)

    Pause the video playback.
------------------------------------------------*/
void CCard::DoStill()
{
#ifdef DEBUG_5L_SCRIPT
	prinfo("still");
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
	CString theVarName;
	int32		theAmount;
	int32		theOrigValue;
	int32		theResValue;

    script >> theVarName >> theAmount;


    theOrigValue = gVariableManager.GetLong(theVarName.GetString());
    theResValue = theOrigValue - theAmount;

    gVariableManager.SetLong(theVarName, theResValue);

#ifdef DEBUG_5L_SCRIPT
	prinfo("sub: %s <%ld> - <%ld> = <%ld>", 
		(char *) theVarName, theOrigValue, theAmount, theResValue);
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
	CRect		bounds;
	Rect		macBounds;
	CString 	header, text;

    script >> header >> bounds >> text;

    AdjustRect(&bounds);
    bounds.MakeMacRect(&macBounds);

#ifdef DEBUG_5L_SCRIPT
	prinfo("text: header <%s>, text <%s>", header.GetString(), text.GetString());
	prinfo("text in rect: L <%d>, T <%d>, R <%d>, B <%d>", bounds.left, bounds.top, bounds.right, bounds.bottom);
#endif
    
    textPtr = new CPlayerText(header.GetString(), macBounds, text.GetString(), 0, 0);
	if (textPtr != nil)
		delete textPtr;
}

/*-----------------------------------------------------------
    (TIMEOUT DELAY CARD)

    If the user doesn't respond in DELAY seconds, jump to the
    given card.
-------------------------------------------------------------*/
void CCard::DoTimeout()
{
	CCard		*theCard;
    CString 	cardName;
    int32     	secs = 0;

    script >> secs >> cardName;
	theCard = gCardManager.GetCard(cardName.GetString());

	secs *= 1000;		// to get it into milliseconds

#ifdef DEBUG_5L_SCRIPT
	prinfo("timeout: delay <%ld>, jump to <%s>", secs, (char *) cardName);
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
    CRect       bounds;
    Rect		macBounds;
    CPoint		loc;
    Point      	macLoc;
    CPicture	*thePicture = NULL;
    CString     theCommand, SecondCommand;
    CString     cmdText, scmdText;
    CString     picname;
    CString		cursorType;
    CursorType	cursor = HAND_CURSOR;

    script >> bounds >> cmdText;

#ifdef DEBUG_5L_SCRIPT
	prinfo("touch: L <%d>, T <%d>, R <%d>, B <%d>", bounds.left, bounds.top, bounds.right, bounds.bottom);
#endif

    theCommand = "(";
    theCommand += cmdText;
    theCommand += ")";

    AdjustRect(&bounds);
    bounds.MakeMacRect(&macBounds);

	if (script.more())
		script >> cursorType;

	cursor = gCursorManager.FindCursor(cursorType);
			    
    if (script.more()) 
    {
        ch_sep = script.curchar();

        if (ch_sep == '(')  
        {
            script >> SecondCommand;
            scmdText = "(";
            scmdText += SecondCommand;
            scmdText += ")";
        }
        
		if (script.more())
		{
			// Get the name of the picture
			script >> picname;
	
			// Get it's offset (if specified), otherwise just use the topLeft of the bounds
			if (script.more()) 
			{
				script >> loc;
				loc.MakeMacPt(&macLoc);
			}
			else 
				macLoc = topLeft(macBounds);
		}
    }
    
    if (not picname.empty())
    	thePicture = GetPicture(picname.GetString(), true);
    
    new CTouchZone(macBounds, theCommand, thePicture, macLoc, cursor, scmdText);
    
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
    CString	effect;
    int32   delay = 0;
    FXType	theEffect = kFXNone;

    if (script.more())
    {
        script >> effect;
	        theEffect = StringToEffect(effect);

        if (script.more())
        	script >> delay;
    }

#ifdef DEBUG_5L_SCRIPT
	prinfo("unblippo: Effect: <%d>  Delay: <%ld>", theEffect, delay);
#endif

	if (gPlayerView->BlippoAvailable())
		gPlayerView->UnBlippo(theEffect, delay);
	else
        prcaution("Unblippo: No Blippo bits available!");
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
    CString	effect;
    int32   delay = 0;
    FXType	theEffect = kFXNone;

    if (script.more()) 
    {
        script >> effect;
	        theEffect = StringToEffect(effect);

        if (script.more())
        	script >> delay;
    }

#ifdef DEBUG_5L_SCRIPT
	prinfo("unlock: Effect: <%d>  Delay: <%ld>", theEffect, delay);
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
    CString effect;
    Effect  theEffect;

    script >> effect;
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

    if (script.more()) 
		script >> frame;

#ifdef DEBUG_5L_SCRIPT
	prinfo("wait: %ld", frame);
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
#ifdef DEBUG_5L
	else
	{
		if ((frame == 0) and (gModMan->NoVolume()))
		{
			prinfo("wait: no volume or movie, pausing");
			
			//::SysBeep(30);
			mPaused = true;
		}
		else
			prinfo("wait: nothing to wait for");
	}
#endif
}

/*-----------------------------------------------------------
    (WRITE FILENAME DATA)

    Write the given data to the file.
-------------------------------------------------------------*/
void CCard::DoWrite()
{
    CString     filename, data;

    script >> filename >> data;

#ifdef DEBUG_5L_SCRIPT
	prinfo("write: file <%s>, data <%s>", filename.GetString(), data.GetString());
#endif

    gFileManager.Write(filename.GetString(), data);
}

/***************************

    CARD MANAGER METHODS

***************************/

CCardManager::CCardManager() : CIndexManager()
{
    mCurrentCard = NULL;
    mExitNow = false;
    mHaveJump = false;
    mExitSide = 0;
#ifdef DEBUG_5L
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
char *CCardManager::CurCardName()
{
    ASSERT(mCurrentCard)

    return (mCurrentCard->Name());
}

//
//	PrevCardName - Return the name of the previously executing card.
//
char *CCardManager::PrevCardName()
{
	return ((char *) mPrevCard);
}

//
//	BeforeCardName - Return the name of the card before this one in the
//		.scr file.
char *CCardManager::BeforeCardName(void)
{
	CCard	*theCard = NULL;
	char	*retStr = NULL;
	int32	index;
	
	ASSERT(mCurrentCard)
	
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
char *CCardManager::AfterCardName(void)
{
	CCard	*theCard = NULL;
	char	*retStr = NULL;
	int32	index;
	
	ASSERT(mCurrentCard)
	
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
	ASSERT(mCurrentCard)
	
	return (mCurrentCard);
	
}

//
//	DoOneCommand
//
void CCardManager::DoOneCommand(CString &theCommand)
{
	ASSERT(mCurrentCard)
	
	mCurrentCard->OneCommand(theCommand);
}

// 
//	CurCardSpendTime
//
void CCardManager::CurCardSpendTime(void)
{
	CVariable	*theAfterVar;
	CVariable	*theBeforeVar;
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
#ifdef DEBUG_5L
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

#ifdef DEBUG_5L
void CCardManager::DoReDoScript(char *cardName)
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
void CCardManager::MakeNewIndex(char *name, int32 start, int32 end)
{
    CCard    	*newCard;
    int32		index;

    newCard = new CCard(name, start, end);

    AddNode(newCard);
    
    // add the card to our array of cards
    index = mCardList.Add(newCard);
    newCard->SetIndex(index);
}

//
//	ZapTree - We have to toss the array of cards too.
//
void CCardManager::ZapTree()
{
	mCardList.RemoveAll();
	CBTree::ZapTree();
}

//
//  JumpToCardByName - Jump to a new card given its name.
//
void CCardManager::JumpToCardByName(const char *newCardName, bool comeBack)
{
	CCard	*theCard = NULL;
	
	theCard = GetCard(newCardName);
	if (theCard == NULL)
		prcaution("trying to jump to <%s>, couldn't find it", newCardName);
	else
		JumpToCard(theCard, comeBack);
}

//
//	JumpToCard - Jump to a new card.
//
void CCardManager::JumpToCard(CCard *newCard, bool /* comeBack */)
{
    if (newCard == NULL)
    	prcaution("Trying to jump to a null card!");
    else
    {	
		mJumpCard = newCard;
		mHaveJump = true;
	}
}
