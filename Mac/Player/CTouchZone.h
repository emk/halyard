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

#include "CText.h"
#include "CString.h"
#include "CPicture.h"

#include <LMouseTracker.h>

class CTouchZone : public LButton, public LCommander, public CText, public LMouseTracker 
{
	public:
		enum { class_ID = 'PlTz' };	// Class ID - needs to be unique & not all lower-case
	
		// Standard constructor
		CTouchZone(Rect &r, CString &cmd, CPicture *inPict, Point &loc, const CString &SecondCmd = nil);
	
		// This constructor is used for the 'Buttpcx' command
		CTouchZone(Rect &r, CString &cmd, CPicture *inPict, Point &loc, char *text,
					const char *header = nil, const CString &secCmd = nil);
					
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
		
	private:
		CString     mCommand;
		CString     mSecondCommand;  //for second command support.
		Point       mPictLoc;
		CPicture	*mPicture;		// pict & hilite pict that we use in the touchzone
		bool		mNormalTouch;	// to distinguish touchzones from buttpcx's
		
	protected:
		void			SetupZone(Rect &r, CString &cmd, CPicture *inPict, Point &loc, const CString &SecondCmd = nil);
		
		
	
		virtual Boolean PointIsInFrame(Int32 inHoriz, Int32	inVert) const
		{
			return (LPane::PointIsInFrame(inHoriz, inVert));
		}
		
		virtual void	FinishCreateSelf(); 
		virtual void	DrawSelf();
};
