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

class CPlayerInput : public LEditField {
public:
	enum { class_ID = 'PlBx' };	// Class ID - needs to be unique & not all lower-case

	// Standard constructor. Will call the SPaneInfo constructor
	CPlayerInput(	const	CString inVarName,
							CString	inStyle,
							CString	inMask,
							Rect	inBounds,
							bool	inRequired);
				
	// Destructor
	virtual 			~CPlayerInput();
	virtual	Boolean		FocusDraw(LPane * /* inSubPane */);
	
private:
	CString				mVarToSet;	// Name of var to receive the text
	CString				mStyle;		// Style to display in when we're done
	CString				mMask;		// Input format mask
	bool				mRequired;	// TRUE if some entry is required.
	
	virtual void		FinishCreateSelf();
	virtual Boolean		HandleKeyPress(const EventRecord	&inKeyEvent);
};

bool HaveInputUp(void);

void DoCPlayerInput(CString inVarName, CString inStyle,
					CString	inMask, Rect inBounds, bool inRequired);

