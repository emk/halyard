//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999, Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//
// KPoint.h : 
//

#if !defined (_KPoint_h_)
#define _KPoint_h_

#include "KCommon.h"
#include "KObject.h"

class KPoint : public KObject
{ 
    public:
		KPoint(int32 inX = 0, int32 inY = 0);
		KPoint(const KPoint &inPt);

		void			Set(int32 inX, int32 inY);
		void			Set(const KPoint &inPt);

		void			Offset(KPoint &inPt);

		inline void		SetX(int32 inX) { m_X = inX; }
		inline void		SetY(int32 inY) { m_Y = inY; }

		inline void		OffsetX(int32 inXOffset) { m_X += inXOffset; }
		inline void		OffsetY(int32 inYOffset) { m_Y += inYOffset; }
		
		inline int32	X(void) const { return (m_X); }
		inline int32	Y(void) const { return (m_Y); }

		
		KPoint			&operator=(const KPoint &inPt);
		bool			operator==(KPoint &inPt);

#if defined (MACOS)
		Point			GetPoint(void) const;
		void			Set(Point &inPt);
#endif
#if defined (WIN32)
		POINT			GetPOINT(void) const;
		void			Set(POINT &inPt);
#endif

	protected:
        int32		m_X;
		int32		m_Y;
};
#endif // _KPoint_h_

/*
 $Log$
 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
