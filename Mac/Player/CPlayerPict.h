/* =================================================================================
	CPlayerPict.h	
	
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
#include "CPicture.h"
#include "TPoint.h"

BEGIN_NAMESPACE_FIVEL

class CPlayerPict 
{
	public:
		enum { class_ID = 'PlBx' };	// Class ID - needs to be unique & not all lower-case

		// Standard constructor. Will call the SPaneInfo constructor
		CPlayerPict(CPicture		*inPict,
					TPoint			&inPt,
					bool			inMatte = false);
					
		// Destructor
		virtual 	~CPlayerPict();
};

END_NAMESPACE_FIVEL
