/* =================================================================================
	CPlayerOval.h	
	
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
#include "CPlayerBox.h"

class CPlayerOval : public LPane {
public:
	enum { class_ID = 'PlOv' };	// Class ID - needs to be unique & not all lower-case

	// Standard constructor. Will call the SPaneInfo constructor
	CPlayerOval(	const Rect		&inBounds,
					const Boolean	inFill,
					const int8		inLineWidth,
					const int16		inColorIdx);
				
	// Destructor
	virtual 	~CPlayerOval();
	
protected:
	Boolean			mFill;
	Rect			mBounds;
	Int8			mLineWidth;			// Width of frame, in pixels
	Int16			mColorIdx;			// Index into the CLUT

	virtual void	DrawSelf();	
};
