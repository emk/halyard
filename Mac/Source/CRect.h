//
//	CRect.h
//

#ifndef _H_CRECT
#define _H_CRECT

#include "Mac5L.h"

//  CRect & CPoint classes.
//
class CPoint 
{
    
    public:

        int32     x, y;

        CPoint(int32 x0 = 0, int32 y0 = 0) { x = x0; y = y0; }
        CPoint(CPoint &pt) { x = pt.x; y = pt.y; }

		void MakeMacPt(Point *macPt);
		
        void Set(int32 x0, int32 y0) { x = x0; y = y0; }
};

class CRect 
{
    
    public:

        int32     left, top, right, bottom;

        CRect(int32 L = 0, int32 T = 0, int32 R = 0, int32 B = 0);
        CRect(CRect &r);
        
        void	MakeMacRect(Rect *macRect);

        void    Set(int32 L, int32 T, int32 R, int32 B);
        int     Contains(CPoint &pt);
};

#endif