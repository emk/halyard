//
//	CRect.cp
//

#include "CRect.h"

/*****************************

    GRAPHICS CLASS METHODS

*****************************/
//  Rectangle stuff.
//
CRect::CRect(int32 L, int32 T, int32 R, int32 B)
{
    left = L;
    top = T;
    right = R;
    bottom = B;
}

CRect::CRect(CRect &r)
{
    left = r.left;
    top = r.top;
    right = r.right;
    bottom = r.bottom;
}

//
// MakeMacRect
//
void CRect::MakeMacRect(Rect *macRect)
{
	macRect->top = top;
	macRect->bottom = bottom;
	macRect->right = right;
	macRect->left = left;
}

//
//	MakeMacPt
//
void CPoint::MakeMacPt(Point *macPt)
{
	macPt->v = y;
	macPt->h = x;
}

/***********************************************************************
 * Function: CRect::Set
 *
 *  Parameter L     (left, top, right,bottom coords)
 *  Parameter T
 *  Parameter R
 *  Parameter B
 * Return:
 *
 * Comments:
 *  Set rect. components
 ***********************************************************************/
void CRect::Set(int32 L, int32 T, int32 R, int32 B)
{
    left = L;
    top = T;
    right = R;
    bottom = B;
}

/***********************************************************************
 * Function: CRect::Contains
 *
 *  Parameter pt
 * Return:
 *   TRUE if pt is in "this" rectangle, FALSE othw.
 * Comments:
 *
 ***********************************************************************/
int CRect::Contains(CPoint &pt)
{
    return ((pt.x >= left) && (pt.x <= right) &&
            (pt.y >= top) && (pt.y <= bottom));
}
