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

#include "KHeader.h"

#include "CText.h"
#include "KString.h"
#include "CPicture.h"
#include "CCursor.h"

#include <LMouseTracker.h>

class CTouchZone : public LButton, public LCommander, public CText, public LMouseTracker 
{
	public:
		enum { class_ID = 'PlTz' };	// Class ID - needs to be unique & not all lower-case
	
		// Standard constructor
		CTouchZone(KRect &r, KString &cmd, CPicture *inPict, KPoint &loc, 
					const CursorType = HAND_CURSOR, const KString &SecondCmd = nil);
	
		// This constructor is used for the 'Buttpcx' command
		CTouchZone(KRect &r, KString &cmd, CPicture *inPict, KPoint &loc, const char *text,
					const CursorType = HAND_CURSOR,
					const char *header = nil, const KString &secCmd = nil);
					
		// Destructor
		virtual 	~CTouchZone();
		
		// Mouse tracking functions.
		void		MouseEnter(Point inPortPt, const EventRecord& inMacEvent);
		void		MouseLeave();
		void		MouseWithin(Point inPortPt, const EventRecord& inMacEvent);
		bool		IsButtPcx(void) { return (not mNormalTouch);}
		virtual void	HotSpotResult(Int16	inHotSpot);
		virtual void	HotSpotAction(Int16, Boolean, Boolean);
		char		FirstChar(void) { return ((mText != nil) ? *mText: 0); }
		CursorType	GetCursor(void) { return (mCursor); }
		
	private:
		KString     mCommand;
		KString     mSecondCommand;  //for second command support.
		KPoint       mPictLoc;
		CPicture	*mPicture;		// pict & hilite pict that we use in the touchzone
		bool		mNormalTouch;	// to distinguish touchzones from buttpcx's
		CursorType	mCursor;
		
	protected:
		void		SetupZone(KRect &r, KString &cmd, CPicture *inPict, KPoint &loc, 
							const KString &SecondCmd = nil, const CursorType cursor = HAND_CURSOR);
			
		virtual Boolean PointIsInFrame(Int32 inHoriz, Int32	inVert) const
		{
			return (LPane::PointIsInFrame(inHoriz, inVert));
		}
		
		virtual void	FinishCreateSelf(); 
		virtual void	DrawSelf();
};
