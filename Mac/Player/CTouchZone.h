// -*- Mode: C++; tab-width: 4; -*-
/* =================================================================================
	CTouchZone.h	
	
	TouchZone pane type. Does not draw anything, but is added to the command
	chain to accept clicks and/or hotkeys.
	
	This is the header for a custom Player Pane class. To define a new class:
	- declare the base class in the class header
	- define the class ID
	- define the approriate constructors. The 'SPaneInfo' constructor will need
	  to have the same parameters as the base classes SPaneInfo constructor.
	- Define destructor
	- Define any data members
	- Override at least DrawSelf().
   ================================================================================= */

#pragma once

#include "TStyleSheet.h"

#include "TString.h"
#include "TInterpreter.h"
#include "CPicture.h"
#include "CCursor.h"

#include <LMouseTracker.h>
BEGIN_NAMESPACE_FIVEL

class CTouchZone : public PP::LButton, public PP::LCommander, public PP::LMouseTracker 
{
	public:
		enum { class_ID = 'PlTz' };	// Class ID - needs to be unique & not all lower-case
	
		// Standard constructor
		CTouchZone(TRect &r, TCallback *cmd, CPicture *inPict, TPoint &loc, 
				   const CursorType = HAND_CURSOR);
	
		// This constructor is used for the 'Buttpcx' command
		CTouchZone(TRect &r, TCallback *cmd, CPicture *inPict,
				   TPoint &loc, const char *text,
				   const CursorType = HAND_CURSOR,
				   const char *stylesheet = nil);
					
		// Destructor
		virtual 	~CTouchZone();
		
		// Mouse tracking functions.
		void        AdjustMouseSelf(Point               inPortPt,
									const EventRecord&  /* inMacEvent */,
									RgnHandle           /* outMouseRgn */);
		
		bool		IsButtPcx(void) { return (not mNormalTouch);}
		virtual void	HotSpotResult(SInt16	inHotSpot);
		virtual void	HotSpotAction(SInt16, Boolean, Boolean);
		char		FirstChar(void) { return ((mText != "") ? *mText: 0); }
		CursorType	GetCursor(void) { return (mCursor); }
		
	private:
		TCallback	*mCallback;
		TPoint       mPictLoc;
		CPicture	*mPicture;		// pict & hilite pict that we use in the touchzone
		bool		mNormalTouch;	// to distinguish touchzones from buttpcx's
		CursorType	mCursor;
		
		// Used for drawing text.
		TString		mStyleSheet;
		TRect		mBounds;
		TString		mText;
		
	protected:
		void		SetupZone(TRect &r, TCallback *cmd, CPicture *inPict,
							  TPoint &loc,
							  const CursorType cursor = HAND_CURSOR);
			
		virtual Boolean PointIsInFrame(SInt32 inHoriz, SInt32	inVert) const
		{
			return (LPane::PointIsInFrame(inHoriz, inVert));
		}
		
		virtual void	FinishCreateSelf(); 
		virtual void	DrawSelf();
};
END_NAMESPACE_FIVEL
