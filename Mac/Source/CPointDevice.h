/********************************************

	CPointDevice

	Object that manages a pointing
	device, like a mouse or a touchscreen.

	This is a pure virtual class.

*********************************************/

#ifndef _H_CPOINTDEVICE
#define _H_CPOINTDEVICE

#include "CObject.h"

class CPointDevice : CObject 
{

  	public:

					CPointDevice() {}

		virtual 	~CPointDevice() {}

		//
		//	Show and Hide are sent before and after user
		//	interaction occurs. This allows some pointing
		//	devices to show or hide cursors as appropriate.
		//
		virtual void	Show() {}
		virtual void	Hide() {}

		//
		//	Return TRUE if the user used the pointing device.
		//	Return the location of that use in (x, y).
		//
		virtual int16	GetPoint(int16 * /* x */, int16 * /* y */) { return FALSE; }
};

#endif
