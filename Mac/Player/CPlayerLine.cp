/* =================================================================================
	CPlayerLine.cp	
   ================================================================================= */

#include "THeader.h"

#include <iostream>
#include <Palettes.h>
#include <LStream.h>
#include <UDrawingState.h>

#include "CPlayerView.h"
#include "CPlayerBox.h"
#include "CPlayerLine.h"
#include "CPalette.h"

/* ---------------------------------------------------------------------------------
		¥ CPlayerLine(&Rect, Fill, LIneWidth, ColorIdx)
		
		Constructor for a custom Player Pane class. Sets up a SPaneInfo struct to
		call the custom pane's constructor.
   --------------------------------------------------------------------------------- */
USING_NAMESPACE_FIVEL

CPlayerLine::CPlayerLine(
	const Rect		&inBounds,		// Bouding rect (relative to PlayerView's rect)
	const int8		inLineWidth,	// Frame's line width, in pixels
	const int16		inColorIdx)		// Index into CLUT
{
	// Assign our data.
	mLineWidth 	= inLineWidth;
	mColorIdx 	= inColorIdx;
	
	// Set the LineFrame rect to the bounds. Need to do this because we will
	// expand the frame in order for PowerPlant to recognize it as a Rect (if
	// 2 sides overlap it thinks it's never in the update rgn).
	mLineFrame	= inBounds;

	// Get the offscreen gworld from the card view, and draw into it.
	CGWorld *theGWorld;
	theGWorld = gPlayerView->GetGWorld();
	theGWorld->BeginDrawing();		// Draw to offscreem GWorld
	DrawSelf();
	theGWorld->EndDrawing();
	
//	::InvalRect(&inBounds);
}

// ---------------------------------------------------------------------------------
//		¥ ~CPlayerLine
// ---------------------------------------------------------------------------------

CPlayerLine::~CPlayerLine()
{
}

// ---------------------------------------------------------------------------------
//		¥ DrawSelf
// ---------------------------------------------------------------------------------

void
CPlayerLine::DrawSelf()
{
	PP::StColorPenState savePenState;
	PP::StColorPenState::Normalize();
	
	// Set the pen state for color, width. Get the rgb color from
	// the color index.
	::PmForeColor(mColorIdx);	
	::PenSize(mLineWidth, mLineWidth);
	
	::MoveTo(mLineFrame.left, mLineFrame.top);
	::LineTo(mLineFrame.right, mLineFrame.bottom);
}
