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
// KRect.h : 
//

#if !defined (_KRect_h_)
#define _KRect_h_

#include "KCommon.h"
#include "KObject.h"
#include "KPoint.h"

class KRect : public KObject 
{  
	public:
		KRect(int32 inTop = 0, int32 inLeft = 0, int32 inBottom = 0, int32 inRight = 0);
		KRect(const KRect &rect);

		void		Set(int32 inTop, int32 inLeft, int32 inBottom, int32 inRight);
		void		Set(const KRect &inRect);
		void		Offset(KPoint &inPt);

		inline void SetTop(int32 inTop) { m_Top = inTop; }
		inline void	SetLeft(int32 inLeft) { m_Left = inLeft; }
		inline void	SetBottom(int32 inBottom) { m_Bottom = inBottom; }
		inline void	SetRight(int32 inRight) { m_Right = inRight; }

		inline void	OffsetTop(int32 inTopOffset) { m_Top += inTopOffset; }
		inline void	OffsetLeft(int32 inLeftOffset) { m_Left += inLeftOffset; }
		inline void	OffsetBottom(int32 inBottomOffset) { m_Bottom += inBottomOffset; }
		inline void	OffsetRight(int32 inRightOffset) { m_Right += inRightOffset; }

		inline int32	Top(void) const { return (m_Top); }
		inline int32	Left(void) const { return (m_Left); }
		inline int32	Bottom(void) const { return (m_Bottom); }
		inline int32	Right(void) const { return (m_Right); }

		int32		Width(void) { return (m_Right - m_Left); }
		int32		Height(void) { return (m_Bottom - m_Top); }

		KRect		&operator=(const KRect &inRect);
		bool		operator==(KRect &inRect);

		bool		Contains(KPoint &inPt);
		KPoint		TopLeft(void);

#if defined (MACOS)
		Rect		GetRect(void) const;
		void		Set(Rect &inRect);
#endif
#if defined (WIN32)
		RECT		GetRECT(void) const;
		void		Set(RECT &inRect);
#endif
		
	protected:
        int32	m_Top;
		int32	m_Left;
		int32	m_Bottom;
		int32	m_Right;
};

#endif // _KRect_h_

/*
 $Log$
 Revision 1.1  2000/05/11 12:59:44  chuck
 v 2.01 b1

*/
