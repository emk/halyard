// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "stdafx.h"

// Needed for RegisterWindowsPrimitives.
#include "TCommon.h"
#include "TPrimitives.h"
#include "TCommonPrimitives.h"
#include "TWinPrimitives.h"

// Needed to implement the primitives.
#include "TDeveloperPrefs.h"
#include "TRect.h"
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
#include "TException.h"

using FileSystem::GetDataFilePath;
using FileSystem::Path;

USING_NAMESPACE_FIVEL


//=========================================================================
//  RegisterWindowsPrimitives
//=========================================================================
//  Install our Windows-specific primitive functions.

void FIVEL_NS RegisterWindowsPrimitives()
{
	REGISTER_5L_PRIMITIVE(Audio);
	REGISTER_5L_PRIMITIVE(AudioKill);
	REGISTER_5L_PRIMITIVE(AudioVolume);
	REGISTER_5L_PRIMITIVE(AudioWait);
	REGISTER_5L_PRIMITIVE(Background);
	REGISTER_5L_PRIMITIVE(Beep);
	REGISTER_5L_PRIMITIVE(Blippo);
	//REGISTER_5L_PRIMITIVE(Blueramp); - Disabled
	REGISTER_5L_PRIMITIVE(Box);
	REGISTER_5L_PRIMITIVE(Browse);
	REGISTER_5L_PRIMITIVE(Buttpcx);
	REGISTER_5L_PRIMITIVE(CheckDisc);
	REGISTER_5L_PRIMITIVE(CheckUrl);
	REGISTER_5L_PRIMITIVE(CheckVol);
	REGISTER_5L_PRIMITIVE_WITH_NAME("circle", Oval);
	REGISTER_5L_PRIMITIVE(Close);
	REGISTER_5L_PRIMITIVE(CTouch);
	REGISTER_5L_PRIMITIVE(Cursor);
	REGISTER_5L_PRIMITIVE(EjectDisc);
	REGISTER_5L_PRIMITIVE(Fade);
	//REGISTER_5L_PRIMITIVE(FadeLock); - Disabled
	REGISTER_5L_PRIMITIVE(GlobalFiles);
	REGISTER_5L_PRIMITIVE(Highlight);
	REGISTER_5L_PRIMITIVE(Hidemouse);
	REGISTER_5L_PRIMITIVE(Input);
	REGISTER_5L_PRIMITIVE(Jump);
	REGISTER_5L_PRIMITIVE(Key);
	REGISTER_5L_PRIMITIVE(Keybind);
	//REGISTER_5L_PRIMITIVE_WITH_NAME("kill", still); - Never finished
	REGISTER_5L_PRIMITIVE(Line);
	REGISTER_5L_PRIMITIVE(Loadpal);
	REGISTER_5L_PRIMITIVE(Loadpic);
	//REGISTER_5L_PRIMITIVE_WITH_NAME("loadpick", Loadpic); - Deprecated
	REGISTER_5L_PRIMITIVE(Lock);
	REGISTER_5L_PRIMITIVE(Lookup);
	REGISTER_5L_PRIMITIVE(Micro);
	REGISTER_5L_PRIMITIVE(Nap);
	REGISTER_5L_PRIMITIVE(Open);
	REGISTER_5L_PRIMITIVE(Origin);
	REGISTER_5L_PRIMITIVE(Oval);
	REGISTER_5L_PRIMITIVE(Pause);
	//REGISTER_5L_PRIMITIVE(Play); - I have no idea where this came from
	REGISTER_5L_PRIMITIVE(PlayQTFile);
	//REGISTER_5L_PRIMITIVE(PlayQTLoop); - Disabled
	//REGISTER_5L_PRIMITIVE(QuitQTLoop); - Disabled
	REGISTER_5L_PRIMITIVE(PlayQTRect);
	REGISTER_5L_PRIMITIVE_WITH_NAME("preload", PreloadQTFile);
	REGISTER_5L_PRIMITIVE(Print);
	REGISTER_5L_PRIMITIVE(Read);
	REGISTER_5L_PRIMITIVE(ReDoScript);
	REGISTER_5L_PRIMITIVE(Resume);
	REGISTER_5L_PRIMITIVE(Rewrite);
	//REGISTER_5L_PRIMITIVE(Rnode); - Disabled
	//REGISTER_5L_PRIMITIVE_WITH_NAME("rvar", Rnode); - Disabled
	REGISTER_5L_PRIMITIVE(Screen);
	//REGISTER_5L_PRIMITIVE(Search);
	REGISTER_5L_PRIMITIVE(SetWindowTitle);
	REGISTER_5L_PRIMITIVE(Showmouse);
	REGISTER_5L_PRIMITIVE(Still);
	REGISTER_5L_PRIMITIVE(Text);
	REGISTER_5L_PRIMITIVE(TextAA);
	REGISTER_5L_PRIMITIVE(Timeout);
	REGISTER_5L_PRIMITIVE(Touch);
	REGISTER_5L_PRIMITIVE(TouchActivate);
	REGISTER_5L_PRIMITIVE(TouchCoords);
	REGISTER_5L_PRIMITIVE(TouchCount);
	REGISTER_5L_PRIMITIVE(Unblippo);
	REGISTER_5L_PRIMITIVE(Unlock);
	REGISTER_5L_PRIMITIVE_WITH_NAME("video", PlayQTFile);
	REGISTER_5L_PRIMITIVE(Wait);
	REGISTER_5L_PRIMITIVE(Write);
}


//=========================================================================
//  Implementation of Windows Primitives
//=========================================================================

//
//	DoAudio - Play an audio clip.
//
//	(audio clip_name [offset] [volume] [fade_time] [kill] [loop])
//
//		default values:
//			offset 		0
//			volume 		100
//			fade_time 	0
//			kill		true
//			loop		false
//
DEFINE_5L_PRIMITIVE(Audio)
{
	TString		audio_file;
	TString		flag;
	int32		the_offset = 0;
	int32		the_volume = 100;
	int32		the_fade_time = 0;
	bool		do_loop = false;
	bool		do_kill = false;
	
	inArgs >> audio_file;
	
	if (inArgs.HasMoreArguments())
		inArgs >> the_offset;
	if (inArgs.HasMoreArguments())
		inArgs >> the_volume;
	if (inArgs.HasMoreArguments())
		inArgs >> the_fade_time;
	
	while (inArgs.HasMoreArguments())
	{
		inArgs >> flag;
		flag.MakeLower();
		
		if (flag == (char *) "kill")
			do_kill = true;
		else if (flag == (char *) "loop")
			do_loop = true;
	}
	
	gAudioManager.Play(audio_file, the_offset, the_volume,
			the_fade_time, do_loop, do_kill);
}

//
//	AudioKill - Kill all playing audio clips. Optionally give the
//		fade time in tenths of a second.
//
//	(audiokill [fade_time] [loop])
//
//		defaults values:
//			fade_time	0
//			loop		false
//
DEFINE_5L_PRIMITIVE(AudioKill)
{
	TString		loop_flag;
	int32		fade_time = 0;
    bool		do_kill_loops = false;
    
	if (inArgs.HasMoreArguments())
		inArgs >> fade_time;
	
	if (inArgs.HasMoreArguments())
	{
		inArgs >> loop_flag;
		loop_flag.MakeLower();
		
		if (loop_flag == (char *) "loop")
			do_kill_loops = true;
	}
		
	gAudioManager.Kill(fade_time, do_kill_loops);
}

//
//	AudioVolume - Set the volume of the playing clips. The volume is 
//		given as a number between 0 (off) and 100 (full volume).
//
//	(audiovolume volume [fade_time])
// 
//		default values:
//			fade_time	0
//
DEFINE_5L_PRIMITIVE(AudioVolume)
{
	int32		the_volume;
	int32		the_fade_time = 0;
	
	inArgs >> the_volume;
	
	if (inArgs.HasMoreArguments())
		inArgs >> the_fade_time;
	
	gAudioManager.Volume(the_volume, the_fade_time);
}

//
//	AudioWait - Wait for a particular frame of the playing clip.
//
//	(audiowait frame)
//
DEFINE_5L_PRIMITIVE(AudioWait)
{
	int32		the_frame;
	
	inArgs >> the_frame;
	
	gAudioManager.Wait(the_frame);
}

/*---------------------------------------------------------------
    (BACKGROUND picfile x1 y1 x2 y2)

	Draw the pic in the bounding rectangle (x1 y1 x2 y2)
-----------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Background)
{
    TString     picname;
    LPicture    *thePicture;
    TRect		loc;
    
    inArgs >> picname >> loc;

    thePicture = gPictureManager.GetPicture(picname);
    thePicture->Draw(loc);
}

/*---------------------------------------------------------------
    (BEEP <FREQ> <DURATION>)

    The computer beeps. Optional parameters control the frequency
    (pitch) of the beep and its duration, in tenths of seconds.
-----------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Beep)
{
    int16     freq = 1500;        //  Hz
    int16     duration = 80;      //  Milliseconds

    if (inArgs.HasMoreArguments()) 
    	inArgs >> freq;
    if (inArgs.HasMoreArguments()) 
    {
        inArgs >> duration;
        duration *= 100;
    }

    Beep(freq, duration);
}
/*---------------------------------------------------------------
    (BLIPPO)

    Copy the current display screen to the offscreen buffer while
    still allowing drawing on screen. Use unblippo to restore the
    saved screen.
-----------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Blippo)
{
	gView->Blippo();
}

/*----------------------------------------------------------------
    (BLUERAMP one two three four)
    Puts the stupid blueramp inside the given rectangle.
 ----------------------------------------------------------------*/
/*
DEFINE_5L_PRIMITIVE(Blueramp)
{
    TRect bounds;

    inArgs >> bounds;
//    Blueramp(bounds);
}
*/

/*----------------------------------------------------------------
    (BOX LEFT TOP RIGHT BOTTOM FILL COLOR)

    Draws a box of the given color at the coordinates specified.
    If FILL == "FILL", the box is filled in. Otherwise,
    only the outline of the box is drawn.
------------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Box)
{
    TRect    	bounds;
    TRect		dirtyRect;
    int16     	color, lineThickness = 1;
    TString 	fill;
    int			do_fill = false;

    
    inArgs >> bounds >> fill >> color;
    if (inArgs.HasMoreArguments()) 
    	inArgs >> lineThickness;

    gOrigin.AdjustRect(&bounds);
    fill.MakeLower();
    if (fill == (char *) "fill")
    	do_fill = true;
    
    // when filling, ignore line thickness	
    if ((lineThickness == 1) or (do_fill))
		DrawRect(bounds, color, do_fill);
    	//DrawRect(bounds.left, bounds.top, bounds.right, bounds.bottom, color, do_fill);
	else
	{
		dirtyRect = bounds;
		dirtyRect.SetBottom(dirtyRect.Bottom() + lineThickness);
		dirtyRect.SetRight(dirtyRect.Right() + lineThickness);
		gView->DirtyRect(&dirtyRect);
		
		DrawLine(bounds.Left(), bounds.Top(), bounds.Right(), bounds.Top(), color, lineThickness);
		DrawLine(bounds.Right(), bounds.Top(), bounds.Right(), bounds.Bottom(), color, lineThickness);
		DrawLine(bounds.Left(), bounds.Bottom(), bounds.Right(), bounds.Bottom(), color, lineThickness);
		DrawLine(bounds.Left(), bounds.Top(), bounds.Left(), bounds.Bottom(), color, lineThickness);
	}
}

//
//	DoBrowse - Opens the given URL in the default browser
//
DEFINE_5L_PRIMITIVE(Browse)
{
	TString		theUrl;

	inArgs >> theUrl;

	if (gBrowserTool.GoToUrl(theUrl))
		PutInBackground();
	else
	{
		gLog.Caution("Can't open a web browser for <%s>.", theUrl.GetString());
		::SetPrimitiveError("nobrowse", "Can't open a web browser.");
	}
}

/*---------------------------------------------------------------
    (BUTTPCX PICT X Y header text Command Cursor <second Command>)
    Puts a button with text "text" on the screen and activates a corresponding
    touchzone.  It takes care of the picture and hilited picture display.
    OPTIONAL:  set command to be executed prior to the jump.
-----------------------------------------------------------------*/

DEFINE_5L_PRIMITIVE(Buttpcx)
{
    TRect		bounds1, bounds;
    TPoint		buttLoc;
    LPicture	*thePicture;
    TString     HeaderName, picname, theCommand, Text;
	TCallback	*callback;
    LTouchZone  *z;
    int         dl, fontHeight;
	TString		cursorStr;
	CursorType	cursor = HAND_CURSOR;
	CursorType	tmpCursor = UNKNOWN_CURSOR;

    inArgs >> picname >> buttLoc >> HeaderName >> Text >> callback;

	if (inArgs.HasMoreArguments())
	{
		inArgs >> cursorStr;

		tmpCursor = gCursorManager.FindCursor(cursorStr);
		if (tmpCursor != UNKNOWN_CURSOR)
			cursor = tmpCursor;
		else
			gLog.Caution("Unknown cursor type: <%s>", (const char *) cursorStr);
	}

    gOrigin.AdjustPoint(&buttLoc);

    thePicture = gPictureManager.GetPicture(picname);

	if (!thePicture->HaveGraphic() || !thePicture->HaveInfo()) 
	{
		gLog.Caution("Missing graphic for button: %s", picname.GetString());
		return;
	}

	gDebugLog.Log("Draw picture at X <%d>, Y <%d>", 
		buttLoc.X(), buttLoc.Y());

    thePicture->Draw(buttLoc, true);
	bounds = thePicture->GetBounds();
	bounds.Offset(buttLoc);

	gDebugLog.Log("Picture bounds: T <%d> L <%d>, B <%d>, R <%d>",
		bounds.Top(), bounds.Left(), bounds.Bottom(), bounds.Right());
	UpdateSpecialVariablesForGraphic(bounds);

	bounds1 = bounds;
    dl = bounds1.Bottom() - bounds1.Top();      //...and text...
    fontHeight = gStyleSheetManager.GetLineHeight(HeaderName);

    dl -= fontHeight;
    dl /= 2;
	bounds1.OffsetTop(dl);
	bounds1.OffsetBottom(dl);

	gDebugLog.Log("Draw text at T <%d> L <%d>, B <%d>, R <%d>",
		bounds1.Top(), bounds1.Left(), bounds1.Bottom(), bounds1.Right());

	gStyleSheetManager.DoText(HeaderName, bounds1, Text, gView);

	// Install our touchzone.
	z = new LTouchZone(bounds, callback, cursor, thePicture, 
					   buttLoc, (const char *) Text, HeaderName);
    gTouchZoneManager.Add(z);
    
	gCursorManager.CheckCursor();
	gView->Draw();
}

//
//	CheckDisc - See if the given disc is in a CD drive.
//
//		(checkdisc volume_name wrong_disc no_disc)
//
DEFINE_5L_PRIMITIVE(CheckDisc)
{
	TString			vol_name;
	TString			no_disc;
	TString			wrong_disc;
	TString			jump_card;
	bool			do_jump = false;

	inArgs >> vol_name >> wrong_disc >> no_disc;

	if ((not do_jump) and (gConfigManager.PlayMedia()))
	{ 
		if (not CDInDrive())
		{
			do_jump = true;
			jump_card = no_disc;
		}

		if (not do_jump)
		{
			// now we either are sure to have a CD in the drive
			//	or we are on a 16 bit system
			//	- check if the correct volume is mounted
			CDStatus	status;

			status = VolIsMounted(gConfigManager.MediaDrive(),
				vol_name);
			if (status == CDMountedWrongVolume)
			{
				do_jump = true;
				jump_card = wrong_disc;
			}
			else if (status == CDNotMounted)
			{
				do_jump = true;
				jump_card = no_disc;
			}
		}
	}
	
	if (do_jump)
	{ 
	    gDebugLog.Log("CheckDisc: failed, jumping to <%s>", (const char *) jump_card);
	    TInterpreter::GetInstance()->JumpToCardByName(jump_card.GetString());
    	gView->Draw();
    }
}

//
//	DoCheckUrl - Check the validity of the given URL, returns 0 on success
//		Optionally sets a variable to the Http reply value	
//		_ERROR is set to the errorCode
//		(checkurl url [var_to_set])
//
DEFINE_5L_PRIMITIVE(CheckUrl)
{
	TString		url;
	TString		var_name;
	long		errorCode;

	inArgs >> url;

	if (inArgs.HasMoreArguments())
	{
		inArgs >> var_name;
		gVariableManager.SetString(var_name, "0");
	}

	LHttpError error;
	error = gHttpTool.CheckURL(url, var_name);

	switch (error)
	{
		case LHTTP_OK:
			errorCode = 0;
			break;
		case LHTTP_URLNotFound:
			errorCode = -2;
			break;
		default:
			errorCode = -3;
			break;
	}

	gVariableManager.SetLong("_ERROR", errorCode);
}

//
//	DoCheckVol - 
//		(checkvol volume_name real_path [no_volume])
//
DEFINE_5L_PRIMITIVE(CheckVol)
{
	TString		vol_name;
	TString		real_path_var;
	TString		no_volume;
	TString		path_to_vol;

	inArgs >> vol_name >> real_path_var;

	if (inArgs.HasMoreArguments())
		inArgs >> no_volume;

	gVariableManager.SetLong(real_path_var, 0);

	if (FindMountPoint(vol_name, path_to_vol))
	{
		// found it, set variable
		gVariableManager.SetString(real_path_var.GetString(), 
			path_to_vol);
	}
	else if (not no_volume.IsEmpty())
	{
		// didn't find it, jump to no_volume card
		gDebugLog.Log("checkvol: failed, jumping to <%s>",
			no_volume.GetString());

		TInterpreter::GetInstance()->JumpToCardByName(no_volume.GetString());
		gView->Draw();
	}
}

/*----------------------------------------------------------------------
    (CLOSE FILENAME)

    Close the given text file.
------------------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Close)
{
	std::string	filename;

	inArgs >> filename;
    Path path = FileSystem::GetDataFilePath(filename);
    gFileManager.Close(path.ToNativePathString().c_str());
}

/*-------------------------------------------------------------
    (CTOUCH)

    Clear all current touch zones. Typically called immediately
    before setting new touch zones.
---------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(CTouch)
{
    int16	left, top;

    if (inArgs.HasMoreArguments())  
    {
        inArgs >> left >> top;
        gTouchZoneManager.Clear(left, top);
    }
    else 
    {   
    	gTouchZoneManager.Clear();
    }
    
    gCursorManager.CheckCursor();
}

/*--------------------------------------------------------
        (Cursor [cursorName])

		Change the cursor, if no cursorName provided, changes to the default 
		cursor.
 ---------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Cursor)
{
	CursorType	theCursor = ARROW_CURSOR;
	CursorType	tmpCursor;
	TString		cursorStr;
	bool		forceShow = false;
	
	if (inArgs.HasMoreArguments())
	{
		inArgs >> cursorStr;
		
		tmpCursor = gCursorManager.FindCursor(cursorStr);
		if (tmpCursor != UNKNOWN_CURSOR)
		{
			theCursor = tmpCursor;
			forceShow = true;
		}
		else
		{
			gLog.Caution("Unknown cursor type: %s", cursorStr.GetString());
			gDebugLog.Log("Unknown cursor type: %s", cursorStr.GetString());
		}
	}
	
	gCursorManager.ChangeCursor(theCursor);
	gCursorManager.ForceShow(forceShow);
}

/*--------------------------------------------------------
        (Ejectdisc)

        Eject a CD from the CDROM drive.
 ---------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(EjectDisc)
{  
	gVideoManager.Kill();
	gAudioManager.Kill();	

	EjectCD();
}

/*---------------------------------------------------------------
    (FADE DIR <TIME>)

    Either FADE IN or FADE OUT. Time is in tenths of seconds.
-----------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Fade)
{
    TString     direction;
    long		fadeTime = 0;
    bool		fadeIn = false;

    inArgs >> direction;
    
    if (inArgs.HasMoreArguments()) 
    	inArgs >> fadeTime;

    direction.MakeLower();

    if (direction == (char *) "in")
    	fadeIn = true;   
    else if (direction == (char *) "out")
    	fadeIn = false; 
    else
    {
        gLog.Log("Error: Can fade in or out, but can't fade %s.", (const char *) direction);
		return;
	} 

	gView->Fade(fadeIn, fadeTime, true);
}

/*---------------------------------------------------------------
    (GLOBALFILES FILE1 [FILE2]...[FILEN])

    Append file(s) to the list of global files.  Global files are
	those that may be accessed my multiple FiveL users.
-----------------------------------------------------------------*/
#ifdef USE_BUNDLE
DEFINE_5L_PRIMITIVE(GlobalFiles)
{
	TString gFile;
	TString fileList;
	int numFields = 0;

	//  Make a comma separated list
    //
    while (inArgs.HasMoreArguments()) 
    {
        inArgs >> gFile;
        if (numFields > 0)
            fileList += ',';
        numFields++;
        fileList += gFile;
    }

	if (!fileList.IsEmpty())
		gFileManager.AddGlobalFiles(fileList);
}
#endif

/*---------------------------------------------------------
    (HIGHLIGHT PICNAME)

    Will highlight a picture already displayed on screen by
    drawing PICNAMEH.PCX, pausing, and then redrawing
    PICNAME.PCX. Both of these pictures are matted.
-----------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Highlight)
{
    TString		picName;
    LPicture		*thePicture;
    TPoint	pt;

    inArgs >> picName;

    thePicture = gPictureManager.GetPicture(picName);
	pt = thePicture->GetOrigin();
    thePicture->Hilite(pt); 
}

/*----------------------------------------------------------------
    (HIDEMOUSE)
    As expected, hides the mouse. WARNING!  unknown behv. when TouchScreen!!
 ---------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Hidemouse)
{
}

/*---------------------------------------------------------------------
    (INPUT STYLE VARIABLE MASK L T R B <REQUIRE>)

    Allow user input from the keyboard. The text they input is saved
    in the given variable. It appears onscreen in the given header
    style, with upper left corner X, Y. The mask is an input mask which
    controls the kind of characters the user may input. (See GX Text
    for more details about the mask.)

    If REQUIRE = true then the entire mask must be filled. Otherwise
    any input, however short, is accepted.
-----------------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Input)
{
    TString		vname, mask, style, required;
    TRect		bounds;
    int			fRequire = false;

    inArgs >> style >> vname >> mask >> bounds;

    if (inArgs.HasMoreArguments()) 
    {
        inArgs >> required;
        required.MakeLower();
		if (required.Equal("true"))
        	fRequire = true;
    }
    
    gView->Draw();
    
    gOrigin.AdjustRect(&bounds);
	gInputManager.Start(vname, style, mask, bounds);
}

/*-------------------------
    (JUMP JUMPCARD)

    Jump to the given card.
---------------------------*/
DEFINE_5L_PRIMITIVE(Jump)
{
    TString     jumpcard;

    inArgs >> jumpcard;
    
    TInterpreter::GetInstance()->JumpToCardByName(jumpcard);
    
    gView->Draw();
}

/*--------------------------------------------------------
    (KEY COLOR)

    Switch the overlay mode and set the keycolor to COLOR.
----------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Key)
{
    int16     newKeyColor;

    inArgs >> newKeyColor;
}

/*-----------------------------------------------------------
    (KEYBIND CHAR <LINKCARD>)

    Bind the given character to the linkcard so that pressing
    ALT-CHAR jumps to the linkcard. If no linkcard is given,
    then remove the binding associated with CHAR.
-------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Keybind)
{
    TString		keyEquiv;
	TCallback	*callback = NULL;
    char		theChar;
    
	inArgs >> keyEquiv;
    if (inArgs.HasMoreArguments())
		inArgs >> callback;
    
    keyEquiv.MakeLower();
	if (keyEquiv.Equal("esc"))
    	theChar = 0x1B;
    else
    	theChar = keyEquiv(0);
  
	if (callback)
		gCommandKeyManager.AddCommandKey(theChar, callback);
	else
		gCommandKeyManager.RemoveCommandKey(theChar);
}

/*--------------------------------------------------------------
    (LINE X1 Y1 X2 Y2 COLOR <THICKNESS>)

    Draw a line of given color and thickness (default is 1) from
    x1, y1 to x2, y2.
----------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Line)
{
    TPoint	a, b;
    int16		color, thickness = 1;

    inArgs >> a >> b >> color;
    if (inArgs.HasMoreArguments()) 
    	inArgs >> thickness;

    gOrigin.AdjustPoint(&a);
    gOrigin.AdjustPoint(&b);
    DrawLine(a.X(), a.Y(), b.X(), b.Y(), color, thickness);
}

/*-------------------------------------------------------------
    (LOADPAL PICTURE)

    Set the screen palette to the palette of the given picture.
---------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Loadpal)
{
    TString 	palname; 
    TString		flag;
    LPalette	*thePal = NULL;
    bool		noload = false;
    bool		lock = false;
    bool		unlock = false;

    inArgs >> palname;
    palname.MakeLower();
    
	while (inArgs.HasMoreArguments())
	{
		inArgs >> flag;
		flag.MakeLower(); 
		
		if (flag.Equal("noload"))
			noload = true;
		else if (flag.Equal("lock"))
			lock = true;
		else if (flag.Equal("unlock"))
			unlock = true;
		else
			gLog.Log("Bad flag to loadpal command <%s>", (const char *) flag);
	}

    thePal = gPaletteManager.GetPalette(palname);
 
 	if (thePal == NULL)
 	{
		gDebugLog.Log("Couldn't find palette <%s>", palname.GetString());
		gLog.Log("Can't find graphic <%s>, please reinstall.", palname.GetString());
		return;
	}
	else
	{
		if (lock)
			thePal->Lock();
		else if (unlock)
			thePal->Unlock();
			
		if (not noload)
			gPaletteManager.SetPalette(thePal, true);
	}
}

/*---------------------------------------------------------------------
    (LOADPIC PICTURE X Y <FLAGS...>)

	Display the given picture at the given location (x, y). As mentioned
	above, there are several flags which modify this command's behavior. 
	Standard behavior is to load the picture from the graphics library on
	disk and then display it on screen using the current palette.

    Flags:
		noshow  - Load into memory only, do not display.
		pal		- Display the picture in its own pallete.	
		matte	- Draw only colored (non-white) pixels.
		lock	- Lock the pic in memory
		unlock	- Unlock a pic, allows it to be purged.
-----------------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Loadpic)
{
    TString		picname;
    TString		flag;
    LPicture	*thePicture;
    LPalette	*thePal;
    TPoint		loc;
    bool		matte = false;
    bool		noshow = false;
    bool		lock = false;
    bool		unlock = false;
    bool		do_pal = false;
    bool		have_pic = true;
    
    inArgs >> picname >> loc; 
    picname.MakeLower();
    
    gOrigin.AdjustPoint(&loc);

    while (inArgs.HasMoreArguments()) 
    {
		inArgs >> flag;
        flag.MakeLower();
        
        if (flag.Equal("noshow"))
        	noshow = true;
        else if (flag.Equal("pal"))
        	do_pal = true;
        else if (flag.Equal("matte"))
        	matte = true;
        else if (flag.Equal("lock"))
        	lock = true;
        else if (flag.Equal("unlock"))
        	unlock = true;
        else
        	gDebugLog.Log("Bad flag to loadpic command <%s>", flag.GetString());
	} 

	thePicture = gPictureManager.GetPicture(picname);

	if (thePicture == NULL)
	{
		gDebugLog.Log("loadpic: can't find <%s>.", picname.GetString());
		gLog.Log("Error: Can't find graphic <%s>. Please reinstall.", (char *) picname.GetString());
		return;
	}

	if (thePicture != NULL)
	{
		// GetBounds returns rect with (0,0,width, height).  We need to
		// offset this before calling UpdateGraphicsForSpecialVariables.
		TRect bounds = thePicture->GetBounds();	
		bounds.Offset(loc);
		UpdateSpecialVariablesForGraphic(bounds);
		Rect sides = bounds.GetRect();
	
		gDebugLog.Log("loadpic: <%s>, <L T R B> %d %d %d %d",
					  picname.GetString(), loc.X(), loc.Y(),
					  sides.right, sides.bottom);
	}

    if (thePicture != NULL)
    {    	
    	if (lock)
    		thePicture->Lock();
    	else if (unlock)
    		thePicture->Unlock();
    }
        	
    if (do_pal)
    {
		thePal = gPaletteManager.GetPalette(picname);
        		
    	if (thePal != NULL)
    		gPaletteManager.SetPalette(thePal, true);
    }
        
    if ((thePicture != NULL) and (not noshow))
    	thePicture->Draw(loc, matte);
}

/*--------------------------------------------------------
    (LOCK <CLEAR>)

    CLEAR   If given, clear the offscreen buffer. Otherwise,
            copy the current screen contents offscreen.

    Lock the screen so that all drawing takes place in the
    offscreen buffer.
----------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Lock)
{
    TString     clear;
    bool		doClear = false;

    if (inArgs.HasMoreArguments()) 
    {
        inArgs >> clear;
        clear.MakeLower();
        
        if (clear.Equal("true"))
        	doClear = true;
    }
    
    gView->Lock(doClear);
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
DEFINE_5L_PRIMITIVE(Lookup)
{
    TString     searchString, param, filename;
    int         numFields = 0;

    inArgs >> filename;

    //  Append all the fields together into a search string that looks
    //  like "field1 TAB field2 TAB ... fieldN"
    //
    while (inArgs.HasMoreArguments()) 
    {
        inArgs >> param;
        if (numFields > 0)
            searchString += '\t';
        numFields++;
        searchString += param;
    }

	Path path = FileSystem::GetDataFilePath(filename.GetString());

    gFileManager.Lookup(path.ToNativePathString().c_str(), searchString, numFields);
}

/*-------------------------------------------------------------------
    (MICRO EFFECT)

    Switch to graphics only (micro) mode. The effect defines how this
    transition is accomplished.
---------------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Micro)
{
}

/*------------------------------------------------
    (NAP TIME)

    Pause execution for TIME tenths of seconds.
    The user can abort a long nap via the ESC key.
--------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Nap)
{
    int32    tenths;

    inArgs >> tenths;

    gCursorManager.CheckCursor();
    gView->Draw();
    TInterpreter::GetInstance()->Nap(tenths);
}

/*----------------------------------------------------------------
    (OPEN FILENAME KIND)

    Open a text file. KIND specifies the kind of access one
    will have to the file. It may be APPEND (write only, appending
    to the end of the file), NEW (write only, overwriting
    anything in the file), or READONLY.
------------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Open)
{ 
    TString     filename, kind;
    FileKind    fKind; 
    bool		open_file = true;
    
    inArgs >> filename >> kind;
    kind.MakeLower();

    if (kind.Equal("append"))
    	fKind = fWriteAppend;
    else if (kind.Equal("new"))
    	fKind = fWriteNew;
    else if (kind.Equal("readonly"))
    	fKind = fReadOnly;
    else
	{
        gLog.Log("Error: Unknown open file kind: %s.", (const char *) kind);
		return;
	}

	Path path = GetDataFilePath(filename.GetString());
    gVariableManager.SetString("_ERROR", "0");
    
/*	Does not seem to work, also leaves an empty file when non-existent -SS
	
	//check for existence of file:
    if ((fKind == fReadOnly) || (fKind == fWriteAppend))
    {
		ifstream theFile;

		theFile.open(tempname, ios::binary);    
        if (!theFile.is_open())
    	{
        	gVariableManager.SetString("_ERROR", "-1");
			gDebugLog.Log("open: file doesn't exist, setting _ERROR to -1");
			open_file = false;
        }
        else
        	theFile.close();
    }	
*/    
    // if not new and we couldn't find it, don't try to open it
    if (open_file)
    	gFileManager.Open(path.ToNativePathString().c_str(), fKind);
}

/*----------------------------------------------------------------
    (CIRCLE x y Radius COLOR)

    Draws an oval of the given color at the coordinates specified.
    If FILL == "FILL", the oval is filled in. Otherwise,
    only the outline of the oval is drawn.
------------------------------------------------------------------*/

DEFINE_5L_PRIMITIVE(Oval)
{
	TRect		bounds;
	int16     	color, x,y, radius;
	TString 	fill;

	inArgs >> x >> y >> radius >> color;

	DrawCircle(x, y, radius, color);
}


DEFINE_5L_PRIMITIVE(Pause)
{
    long    tenths = 0L;

    inArgs >> tenths;

    gVideoManager.Pause(tenths);
    gAudioManager.Pause(tenths);
    
    gCursorManager.CheckCursor();
    gView->Draw();
}


/*-----------------------------------------------------------
    (playqtfile file [frame] [pal] [origin])

    Begin playing the QuickTime file from FRAME1..FRAME2.
    
    3/13/96 Changed to (PLAYQTFILE FILE [FRAME] [pal])
    where frame is a global offset for succeeding wait
    statements that depend on a laser disc frame and not
    a QT frame (i.e., non-zero based plays and waits).
    pal loads the palette from the graphics file of the name
    file.pic and uses it as the global video palette.
-------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(PlayQTFile)
{ 
	LPalette	*thePal;
	TString 	QTfile;
	TString 	PalFile;
	TPoint		movieOrigin(0, 0);
    int32		theOffset = 0;
    bool		audio_only = false;
    bool		have_pal = false;
    
    inArgs >> QTfile;

    if (inArgs.HasMoreArguments())
        inArgs >> theOffset;

    if (inArgs.HasMoreArguments()) 
    {
        inArgs >> PalFile; 
        have_pal = true;
    }   
	
	if (inArgs.HasMoreArguments())
	{
		inArgs >> movieOrigin;

		if ((movieOrigin.X() >= 0) and (movieOrigin.Y() >= 0))
		{
			gDebugLog.Log("playqtfile: set origin to T <%d>, L <%d>",
				movieOrigin.Y(), movieOrigin.X());

			gVideoManager.SetOrigin(movieOrigin);
		}
	}

	if (QTfile.Contains(".a2", false))
    	audio_only = true;
    
    if ((not audio_only) and (not gVideoManager.HaveOrigin()))
	{
		gDebugLog.Log("playing full screen movie, fade out");
		if (gView->BitDepth() > 8)
			gView->BlackScreen();
		else
			gView->Fade(false, 2, false);
			
		gView->ClearScreen(0); 
	}

	// set the palette if we have one	
	if (have_pal)
	{
		char	*chptr;
		
		// strip off the extension
		chptr = ::strstr(PalFile.GetBuffer(), ".");
		if (chptr != NULL)
		{
			*chptr = '\0';
			PalFile.Update();
		}
		
		thePal = gPaletteManager.GetPalette(PalFile);
		if (thePal != NULL)
			gPaletteManager.SetPalette(thePal, false);
	}
    
    if (not audio_only)
    	gVideoManager.Play(QTfile, theOffset);
    else
    	gAudioManager.Play(QTfile, theOffset, 100, 0, false, true);
}

/*-----------------------------------------------------------
    (PLAYQTLOOP AUDIOFILE [FADEIN])

	9-25-96 Looping audio files with fade-in
-------------------------------------------------------------*/

DEFINE_5L_PRIMITIVE(PlayQTLoop)
{
	TString		QTfile;
    long		theFadeTime = 0;
    bool		audio_file;
	
    inArgs >> QTfile;
    if (inArgs.HasMoreArguments())
        inArgs >> theFadeTime;

	if (QTfile.Contains(".a2", false))
    	audio_file = true;
    else
    {
		gDebugLog.Log("playqtloop: not an audio file <%s>", QTfile.GetString());
    	return;
    }
    
    gAudioManager.Play(QTfile, 0, 100, theFadeTime, true, true); 
}

/*-----------------------------------------------------------
    (QUITQTLOOP [FADEOUT])

	9-25-96 Kills looping audio files with fade-out
-------------------------------------------------------------*/

DEFINE_5L_PRIMITIVE(QuitQTLoop)
{
	int32	fadeTime = 0;
	 
	if (not gAudioManager.Looping())
		return;
    
    if (inArgs.HasMoreArguments())
        inArgs >> fadeTime;

	gAudioManager.Kill(fadeTime, true);
}

/*-----------------------------------------------------------
    (PLAYQTRECT X1 Y1)

	Sets the origin for the movie display. This applies only
	to the next movie, then is reset. Uses a global grMovieOrigin,
	where x,y = -1 if the default origin is to be used (centers
	the movie on the screen).
	
	Setting the origin also disables autofading of graphics
	and drawing black at the beginning and end of movies. This
	allows graphics to appear and persist with movies on the
	screen.
	
	Coordinates
-------------------------------------------------------------*/

DEFINE_5L_PRIMITIVE(PlayQTRect)
{
    TPoint vidLoc;
    
    inArgs >> vidLoc;

    ASSERT((vidLoc.X() >= 0) and (vidLoc.X() <= 640));
    ASSERT((vidLoc.Y() >= 0) and (vidLoc.Y() <= 480));
    
    gVideoManager.SetOrigin(vidLoc);
}

//
//	(PRELOAD QTFILE [NAP] [SYNC])
//
//	Preload a Quicktime file into memory.			
//
DEFINE_5L_PRIMITIVE(PreloadQTFile)
{
	TString		QTfile;
	TString		syncFlag;
	int32		tenths = 0;
	int32		nap_time = 0;
	int32		start_time = 0;
	int32		end_time = 0;
	bool		audio_file = false;
	bool		doSync = false;

    inArgs >> QTfile;

	if (inArgs.HasMoreArguments())
		inArgs >> tenths;

	if (inArgs.HasMoreArguments())
	{
		inArgs >> syncFlag;

		if (syncFlag.Equal("sync", false))
			doSync = true;
		else
			gDebugLog.Log("preload: bad flag <%s>, looking for sync", syncFlag.GetString());
	}
   
	if (QTfile.Contains(".a2", false))
		audio_file = true;

//	start_time = ::timeGetTime();
//
//	if (tenths > 0)
//	{
//		nap_time = tenths * 100L;
//
//		// figure out the time the nap should stop
//		nap_time += start_time;
//	}

	if (not QTfile.Equal("0"))
	{
		if (audio_file)
		{
			gAudioManager.Preroll(QTfile, tenths, doSync);
			gVariableManager.SetString("_PreloadedVideo", QTfile.GetString());
		}
		else
		{
			gVideoManager.Preroll(QTfile, tenths, doSync);
			gVariableManager.SetString("_PreloadedAudio", QTfile.GetString());
		}
	}

//	end_time = ::timeGetTime();
//
//	gDebugLog.Log("preload: the preload took <%d> milliseconds", end_time - start_time);
//
//	if (tenths > 0)
//	{
//		// see if we have passed our nap time
//		if (nap_time > end_time)
//		{
//			// have to wait some more
//			tenths = (nap_time - end_time) / 100;
//
//			gDebugLog.Log("preload: going to nap for <%d> tenths", tenths);
//
//			gCursorManager.CheckCursor();
//			gView->Draw();
//			TInterpreter::GetInstance()->Nap(tenths);
//		}
//	}
}
                                       
DEFINE_5L_PRIMITIVE(Print)
{
}

/*-----------------------------------------------------------------
    (READ FILENAME VARIABLE <UNTIL DELIM>)

    Read data from a text file and put it into the variable.
    Normally this will read the next word as defined by whitespace.
    Use the UNTIL DELIM construct to read until some other
    delimiter.

    Valid delimiters are TAB, RETURN, EOF, or any single character.

    TODO - Replace the bogus "UNTIL" argument with a keyword
	argument, or remove it entirely.
-------------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Read)
{
	TString 		tempname;
    TString         filename, vname, until, delimstr;
    unsigned char   delim;
    TString         res;

    inArgs >> filename >> vname;
 
	Path path = GetDataFilePath(filename.GetString());

    if (inArgs.HasMoreArguments()) 
    {
        inArgs >> until >> delimstr;
        delimstr.MakeLower();
        if (delimstr.Equal("tab")) 
        	delim = '\t';
        else if (delimstr.Equal("return")) 
        	delim = '\n';
        else if (delimstr.Equal("eof"))
        	delim = 0;
        else 
        	delim = delimstr(0);

        gFileManager.ReadUntil(path.ToNativePathString().c_str(), res, delim);
        
    } 
	else
	{
		gFileManager.Read(path.ToNativePathString().c_str(), res);
	}

    gVariableManager.SetString(vname, res);
}

//
//	ReDoScript 
//
DEFINE_5L_PRIMITIVE(ReDoScript)
{
	TString		theCard;
	inArgs >> theCard;

	// make sure redoscript functionality is enabled
	if (gDeveloperPrefs.GetPref(REDOSCRIPT) == REDOSCRIPT_OFF)
		return;
	
	ReDoScript(theCard);
}

/*---------------------------------------------------------------
    (RESUME)

    If the user touched a touch zone and paused a playing segment
    (audio or video) then this command will resume playback.
-----------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Resume)
{
    gVideoManager.Resume();
    gAudioManager.Resume();
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
DEFINE_5L_PRIMITIVE(Rewrite)
{ 
    TString     searchString, param, filename;
    int         numFields = 0;

    inArgs >> filename;

    //  Append all the fields together into a search string that looks
    //  like "field1 TAB field2 TAB ... fieldN"
    //
    while (inArgs.HasMoreArguments()) 
    {
        inArgs >> param;
        if (numFields > 0)
            searchString += '\t';
        numFields++;
        searchString += param;
    }

	Path path = GetDataFilePath(filename.GetString());

    gFileManager.Rewrite(path.ToNativePathString().c_str(), searchString, numFields);
}

/*---------------------------------------------------------------
    (SCREEN COLOR)

    A fast way to fill the entire screen with a particular color.
-----------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Screen)
{
    int16 color;

    inArgs >> color; 
    
    gView->ClearScreen(color);
}

/*-----------------------------------
    (SEARCH FRAME [FLAG])

    Search to the given frame number.
        If FLAG present (anything) then wait for completion.
-------------------------------------*/
//DEFINE_5L_PRIMITIVE(Search)
//{
//    long    frame;
//
//    inArgs >> frame;
//
//}

/*---------------------------------------------------------------------
    (SETWINDOWTITLE TITLE)

    Set the application window title
 ---------------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(SetWindowTitle)
{
	TString theTitle;

	inArgs >> theTitle;
	::SetWindowText(hwndApp, theTitle.GetString());
}

/*---------------------------------------------------------------------
    (SHOWMOUSE)

    Shows the mouse (shouldn't be needed, maybe only in conjunction w/ hide~)
 ---------------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Showmouse)
{
}

DEFINE_5L_PRIMITIVE(Still)
{
	gVideoManager.Pause(0);
	gAudioManager.Pause(0);
}

/*--------------------------------------------------------------
    (TEXT HEADER LEFT TOP RIGHT BOTTOM COLOR SHADOW TEXTSTRING)

    Display the given textstring, using the given header style,
    within the given rect. Note that the bottom of the rectangle
    is elastic... it will actually be as much or as little as
    necessary to display all the text.
----------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Text)
{
    TRect		bounds;
    TString 	header, text;

    inArgs >> header >> bounds >> text;
    
    gOrigin.AdjustRect(&bounds);

    gHeaderManager.DoText(header, bounds, text, 0, 0);
}

/*--------------------------------------------------------------
    (TEXTAA STYLESHEET LEFT TOP RIGHT BOTTOM TEXTSTRING)

    Display the given textstring, using the given stylesheet,
    within the given rect. Note that the bottom of the rectangle
    is elastic... it will actually be as much or as little as
    necessary to display all the text.
----------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(TextAA)
{
	TRect		bounds;
	std::string style, text;

    inArgs >> style >> bounds >> text;

    gOrigin.AdjustRect(&bounds);

	try
	{
		gStyleSheetManager.Draw(style, text,
								GraphicsTools::Point(bounds.Left(),
													 bounds.Top()),
								bounds.Right() - bounds.Left(),
								gView);
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
DEFINE_5L_PRIMITIVE(Timeout)
{
    TString 	cardName;
    int32     	secs;

    inArgs >> secs >> cardName;
    
    TInterpreter::GetInstance()->Timeout(cardName.GetString(), secs);
}

/*--------------------------------------------------------------
    (TOUCH LEFT TOP RIGHT BOTTOM CMD [CURSOR [PICT [X Y]]])

    Create a touch zone bounded by the given rectangle. Touching
    this touch zone will make the program go to card CARD. If a
    PICT is specified than that picture will highlight in
    reponse to the touch. If there are more than one copy of
    a given picture on the screen, specify an X and Y coordinate
    for the one you want to highlight.
----------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Touch)
{
    TRect       bounds;
    TPoint      loc;
	TCallback	*callback;
    TString     picname;
    LPicture    *thePicture = 0;
    LTouchZone  *theZone;
	CursorType	cursor = HAND_CURSOR;
	CursorType	tmpCursor = UNKNOWN_CURSOR;
	TString		cursorStr;

    inArgs >> bounds >> callback;
 
    gOrigin.AdjustRect(&bounds);

	// Get our cursor, if any.
	if (inArgs.HasMoreArguments())
	{
		inArgs >> cursorStr;

		tmpCursor = gCursorManager.FindCursor(cursorStr);
		if (tmpCursor != UNKNOWN_CURSOR)
			cursor = tmpCursor;
	}

	// Get our picture-related arguments.
    if (inArgs.HasMoreArguments()) 
    {
        inArgs >> picname;
        thePicture = gPictureManager.GetPicture(picname);
        if (inArgs.HasMoreArguments()) 
        {
            inArgs >> loc;
            gOrigin.AdjustPoint(&loc);
        } 
        else 
			loc = thePicture->GetOrigin();
    }

    theZone = new LTouchZone(bounds, callback, cursor, thePicture, loc);
    gTouchZoneManager.Add(theZone);
    
    gCursorManager.CheckCursor();
}

/*----------------------------------------------------------
    (TOUCHACTIVATE INDEX)

    Simulate a click of the touchzone INDEX, counting from
    zero.
------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(TouchActivate)
{
	int32 index;
	inArgs >> index;
	if (0 <= index && index < gTouchZoneManager.GetTouchZoneCount())
		gTouchZoneManager.ActivateTouchZone(index);
	else
		::SetPrimitiveError("badtouch", "No such touchzone.");
}

/*----------------------------------------------------------
    (TOUCHCOORDS INDEX)

    Return the coordinates of the touchzone INDEX, counting
    from zero.
------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(TouchCoords)
{
	int32 index;
	inArgs >> index;
	if (0 <= index && index < gTouchZoneManager.GetTouchZoneCount())
	{
		TPoint coords = gTouchZoneManager.GetTouchZoneCoordinates(index);
		::SetPrimitiveResult(coords);
	}
	else
		::SetPrimitiveError("badtouch", "No such touchzone.");
}	

/*----------------------------------------------------------
    (TOUCHCOUNT)

    Return the number of touchzones (and buttpcxs) currently
	displayed.
------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(TouchCount)
{
	::SetPrimitiveResult((int32) gTouchZoneManager.GetTouchZoneCount());
}

/*----------------------------------------------------------
    (UNBLIPPO <EFFECT> <DELAY>)

    Copies the offscreen buffer to the display with a given
    special effect. An optional delay will allow the user to
    control the speed of this effect. DELAY is the length in
    tenths of seconds that the effect should take.
------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Unblippo)
{
    TString		effect;
    long		delay = 0;
    
    if (inArgs.HasMoreArguments())
    {
    	inArgs >> effect;
    	if (inArgs.HasMoreArguments())
    		inArgs >> delay;
    }
    
    gView->UnBlippo(effect, delay);
}


/*----------------------------------------------------------
    (UNLOCK <EFFECT> <DELAY>)

    Copies the offscreen buffer to the display with a given
    special effect. An optional delay will allow the user to
    control the speed of this effect. DELAY is the length in
    tenths of seconds that the effect should take.
------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Unlock)
{
    TString 	effect;
    long    	delay = 0;

    if (inArgs.HasMoreArguments()) 
    {
        inArgs >> effect;
        if (inArgs.HasMoreArguments()) 
        	inArgs >> delay;
    }

	gView->Unlock(effect, delay);
}

/*------------------------------------------------------
    (VIDEO EFFECT)

    Switch the video only mode with the given effect.

    Note: this effect is accomplished, in most cases, by
    temporarily switching to overlay (key) mode for the
    effect.
--------------------------------------------------------*/
//DEFINE_5L_PRIMITIVE(Video)
//{
//    TString effect;
//
//    inArgs >> effect;
//}


/*-----------------------------------------------------------
    (WAIT <FRAME>)

	Suspends the execution of the 5L script until
	the specified frame is reached by the videodisc player.
-------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Wait)
{
    long    frame = 0L;

    if (inArgs.HasMoreArguments()) 
    	inArgs >> frame;

    if (gVideoManager.Playing())
    	gVideoManager.Wait(frame);
    else if (gAudioManager.PlayNoLoop())
    	gAudioManager.Wait(frame);
    
    gCursorManager.CheckCursor();
    gView->Draw();
}

/*-----------------------------------------------------------
    (WRITE FILENAME DATA)

    Write the given data to the file.
-------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Write)
{
	TString 	tempname;
    TString     filename, data;

    inArgs >> filename >> data;

	Path path = GetDataFilePath(filename.GetString());

    gFileManager.Write(path.ToNativePathString().c_str(), data);
}

