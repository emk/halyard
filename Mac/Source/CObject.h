/***********************************************

	Base object class for 5L.

	Does nothing but serve as a common root.

************************************************/

#ifndef _H_COBJECT
#define _H_COBJECT

//	To make is easy to see what methods are overridden,
//	I'm defining override to be the same as virtual.
//	Use override in any method definition that is an override.
//
#define	OVERRIDE	virtual


class CObject {

	//
	//	Dummy constructors/destructors.
	//

	public:

	 	CObject(void);
		virtual ~CObject(void);
};

typedef CObject *CObjectPtr;

#endif
