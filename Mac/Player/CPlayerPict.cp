/* =================================================================================
	CPlayerPict.cp	
   ================================================================================= */

#include <iostream>
#include <Palettes.h>
#include <LStream.h>
#include <UDrawingState.h>

#include "Mac5L.h"
#include "CPlayerView.h"
#include "CPlayerPict.h"


/* ---------------------------------------------------------------------------------
		¥ CPlayerPict()
		
		Wrapper for drawing the picture in the offscreen world. Always purge
		the picture after drawing. If it is locked, nothing will happen.
   --------------------------------------------------------------------------------- */

CPlayerPict::CPlayerPict(CPicture		*inPict,
						 const Rect		&inBounds,
						 const Boolean	inMatte)
{
	if (inPict == NULL)
		return;
	
	// Get the offscreen gworld from the card view, and draw into it.
	CGWorld *theGWorld = gPlayerView->GetGWorld();
	GWorldPtr macGWorld = theGWorld->GetMacGWorld();

	theGWorld->BeginDrawing();		// Draw to offscreem GWorld
	inPict->DrawPic(topLeft(inBounds), BITMAP(macGWorld), inMatte);
	theGWorld->EndDrawing();

	// cbo_mem
	// cbo - only do this if we don't want the resource tree to 
	//		manage memory for us
	//inPict->Purge();				// free memory for the picture
}

// ---------------------------------------------------------------------------------
//		¥ ~CPlayerPict
// ---------------------------------------------------------------------------------

CPlayerPict::~CPlayerPict()
{
}
