/* =================================================================================
	CPlayerPict.cp	
   ================================================================================= */

#include "THeader.h"

#include <iostream>
#include <Palettes.h>
#include <LStream.h>
#include <UDrawingState.h>

#include "CPlayerView.h"
#include "CPlayerPict.h"

USING_NAMESPACE_FIVEL
/* ---------------------------------------------------------------------------------
		¥ CPlayerPict()
		
		Wrapper for drawing the picture in the offscreen world. Always purge
		the picture after drawing. If it is locked, nothing will happen.
   --------------------------------------------------------------------------------- */

CPlayerPict::CPlayerPict(CPicture		*inPict,
							TPoint		&inPt,
						 	bool		inMatte)
{
	if (inPict == NULL)
		return;
	
	// Get the offscreen gworld from the card view, and draw into it.
	CGWorld *theGWorld = gPlayerView->GetGWorld();
	GWorldPtr macGWorld = theGWorld->GetMacGWorld();

	theGWorld->BeginDrawing();		// Draw to offscreem GWorld
	inPict->Draw(inPt, macGWorld, inMatte);
	theGWorld->EndDrawing();

	inPict->Purge();				// free memory for the picture
}

// ---------------------------------------------------------------------------------
//		¥ ~CPlayerPict
// ---------------------------------------------------------------------------------

CPlayerPict::~CPlayerPict()
{
}
