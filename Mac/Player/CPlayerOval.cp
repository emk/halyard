/* =================================================================================
	CPlayerOval.cp	
   ================================================================================= */

#include "THeader.h"

#include <iostream>
#include <Palettes.h>
#include <LStream.h>
#include <UDrawingState.h>

#include "CPlayerView.h"
#include "CPlayerOval.h"
#include "CPalette.h"

/* ---------------------------------------------------------------------------------
		¥ CPlayerOval(&Rect, Fill, LIneWidth, Color)
   --------------------------------------------------------------------------------- */
USING_NAMESPACE_FIVEL
using GraphicsTools::Color;

CPlayerOval::CPlayerOval(
	const Rect		&inBounds,		// Bouding rect (relative to PlayerView's rect)
	const Boolean 	inFill,			// TRUE if rect to be filled (solid)
	const int8		inLineWidth,	// Frame's line width, in pixels
	const Color 	inColor)		// Color
{
	// Assign our data.
	mFill 		= inFill;
	mLineWidth 	= inLineWidth;
	mColor	 	= inColor;
	mBounds		= inBounds;
	
	// Get the offscreen gworld from the card view, and draw into it.
	CGWorld *theGWorld;
	theGWorld = gPlayerView->GetGWorld();
	theGWorld->BeginDrawing();		// Draw to offscreem GWorld
	DrawSelf();
	theGWorld->EndDrawing();
	
//	::InvalRect(&mBounds);
}

// ---------------------------------------------------------------------------------
//		¥ ~CPlayerOval
// ---------------------------------------------------------------------------------

CPlayerOval::~CPlayerOval()
{
}

// ---------------------------------------------------------------------------------
//		¥ DrawSelf
// ---------------------------------------------------------------------------------

void
CPlayerOval::DrawSelf()
{
	PP::StColorPenState savePenState;
	PP::StColorPenState::Normalize();
	
	// Set the pen state for color, width.
	RGBColor color = gPlayerView->GetColor(mColor);
	::RGBForeColor(&color);	
	::PenSize(mLineWidth, mLineWidth);
	
	// If 'mFill' is TRUE, we want to fill the box. Otherwise, just frame it.
	if (mFill)
	{
		::PenMode(patCopy);
		Pattern black;
		::FillOval( &mBounds, PP::UQDGlobals::GetBlackPat(&black));
	}
	else
		::FrameOval( &mBounds );
}
