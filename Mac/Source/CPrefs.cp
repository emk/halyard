/*************************************************

    CPreferences

    A little class that interprets command
    line switches and that other classes and
    modules can query to determine things about
    the environment.

**************************************************/

#include "CPrefs.h"
#include <string.h>
#include "util.h"
#include <stdlib.h>

extern int      gTransKey;

const int16 	gDisplayType = 0;
const int16  	gScreenWidth = 640;
const int16  	gScreenHeight = 480;
const int16  	gPaletteSize = 0;

//
//  Constructor.
//
CPreferences::CPreferences()
{
    //
    //  Set default values here.
    //
    fDemo = FALSE;
    fEncrypt = TRUE;
    fMouse = FALSE;
    fCheckDisc = TRUE;
    fPrint = TRUE;
    fHelp = FALSE;
    
    videoController = None;
    vidDisplay = VGA;

    //
    //  Determine timeout value from TIMEOUT variable set in
    //  autoexec.bat. @@@KM--should be done in an ini file.
    //
//    char *environStr = getenv("TIMEOUT");
//    if (environStr != NULL) timeout = atoi(environStr);
//    else 
	timeout = 30;
}

/***********************************************************************
 * Function: CPreferences::SetSwitch
 *
 *  Parameter sw
 * Return:
 *
 * Comments:
 *  This method is passed one of the command line switches.
 *  Look at it and set the appropriate field.
 ***********************************************************************/
void CPreferences::SetSwitch(char *sw)
{
    //  Check the value of the switch and set values accordingly.
    //  Switches can override previous switches; the last switch
    //  wins. (i.e. -sony -visage will select Visage)
    //

 	strlower(sw);

    if (strcmp(sw, "iml") == 0) 
	{
        fEncrypt = FALSE;
        fDemo = TRUE;
    } 
	else if (sw[0] == 'k')
	{
        sw++;
       gTransKey = atoi(sw);
    } 
	else if (sw[0] == 'v') 
	{
        //
        //  It's a video controller... make sure InfoWindows is set
        //  to EGA. The others are all VGA unless overridden by the
        //  -ega switch.
        //

        sw++;

        if (strcmp(sw, "none") == 0) 
		{
            videoController = None;
            fCheckDisc = FALSE;
            fMouse = TRUE;
        } 
		else if (strcmp(sw, "sony") == 0) 
			videoController = Sony;
        else if (strcmp(sw, "visage") == 0) 
			videoController = Visage;
        else if (strcmp(sw, "videologic") == 0) 
			videoController = Videologic;
        else if (strncmp(sw, "info", 4) == 0) 
		{
            videoController = InfoWindows;
            vidDisplay = EGA;
        } 
		else 
			prerror("Unknown video controller: %s", sw);
    } 
	else if (strcmp(sw, "mouse") == 0)
        fMouse = TRUE;
    else if (strcmp(sw, "noprint") == 0)
        fPrint = FALSE;
    else if (strcmp(sw, "ega") == 0)
        vidDisplay = EGA;
    else if (strcmp(sw, "nocheck") == 0)
        fCheckDisc = FALSE;
    else if (strcmp(sw, "check") == 0) //Added 09FEB94 to allow setting this
        fCheckDisc = TRUE;
    else if (strcmp(sw, "demo") == 0)
        fDemo = TRUE;
    else if (strcmp(sw, "help") == 0)
        fHelp = TRUE;
    else 
		prerror("Unknown switch: %s", sw);
}
