/* =================================================================================
	CPlayerText.h	
	
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

#include "THeader.h"

#include "CText.h"

BEGIN_NAMESPACE_FIVEL

class CPlayerText :public CText 
{
	public:
		enum { class_ID = 'PlTx' };	// Class ID - needs to be unique & not all lower-case
	
		// Standard constructor.
		CPlayerText(const char	*inHeader,	// Name of header format to use.
				const TRect	&inBounds,		// Bouding rect (relative to PlayerView's rect)
				const char 	*inText,		// The 'raw' string (including format chars)
				const int8	ignore = 0,
				const int8	ignore2 = 0);	// Ignore these (for compatibility)
					
		// Destructor
		virtual 	~CPlayerText();
		
	protected:
		virtual void			DrawSelf();	
};

END_NAMESPACE_FIVEL