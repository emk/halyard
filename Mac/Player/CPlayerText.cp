/* =================================================================================
	CPlayerText.cp	
	
	Draw text on the screen. Combines 'LPane' and 'CText' objects.
   ================================================================================= */

#include <iostream>
#include <TextEdit.h>
#include <string.h>
#include <Palettes.h>
#include <LStream.h>
#include <UDrawingState.h>

#include "Mac5L.h"
#include "CPlayerView.h"
#include "CPlayerText.h"
#include "CRect.h"


/* ---------------------------------------------------------------------------------
		¥ CPlayerText(&Header, BoundsRect, text)
		
		Construct the text display object. The header passes down to the CText
		class, and it contains the formatting info - the pane is only used
		as a display context.
   --------------------------------------------------------------------------------- */

CPlayerText::CPlayerText(
	const char		*inHeader,		// Name of header format to use.
	const Rect		&inBounds,		// Bouding rect (relative to PlayerView's rect)
	const char 		*inText,		// The 'raw' string (including format chars)
	const int8		/* ignore */,
	const int8		/* ignore2 */)		// Ignore these (for compatibility)
	: CText(inHeader, inBounds, inText)	// Invoke text object constructor
{
	// Get the offscreen gworld from the card view, and draw into it.
	CGWorld *theGWorld;
	theGWorld = gPlayerView->GetGWorld();
	theGWorld->BeginDrawing();		// Draw to offscreem GWorld
	DrawSelf();
	theGWorld->EndDrawing();
	
//	::InvalRect(&inBounds);
}

// ---------------------------------------------------------------------------------
//		¥ ~CPlayerText
// ---------------------------------------------------------------------------------

CPlayerText::~CPlayerText()
{
}

void
CPlayerText::DrawSelf()
{
	StColorPenState savePenState;
	StColorPenState::Normalize();
	
	DrawText();
}
