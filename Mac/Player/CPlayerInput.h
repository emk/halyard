/* =================================================================================
	CPlayerInput.h	
	
	Draw a box in the player view. Can define bounds, fill (frames if no fill),
	line width & color. Base class: LPane. 
	
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

#include "KString.h"

class CPlayerInput : public LEditField {
public:
	enum { class_ID = 'PlBx' };	// Class ID - needs to be unique & not all lower-case

	// Standard constructor. Will call the SPaneInfo constructor
	CPlayerInput(	const	KString inVarName,
							KString	inStyle,
							KString	inMask,
							Rect	inBounds,
							bool	inRequired);
				
	// Destructor
	virtual 			~CPlayerInput();
	virtual	Boolean		FocusDraw(LPane * /* inSubPane */);
	
private:
	KString				mVarToSet;		// Name of var to receive the text
	KString				mStyle;			// Style to display in when we're done
	KString				mMask;			// Input format mask
	RGBColor			mBackColor;		// our background color
	bool				mRequired;		// TRUE if some entry is required.
	bool				mHaveBackColor;	// true if was have a background color
	
	virtual void		FinishCreateSelf();
	virtual Boolean		HandleKeyPress(const EventRecord	&inKeyEvent);
};

bool HaveInputUp(void);

void DoCPlayerInput(KString inVarName, KString inStyle,
					KString	inMask, Rect inBounds, bool inRequired);

