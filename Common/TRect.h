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
// TRect.h : 
//

#if !defined (_TRect_h_)
#define _TRect_h_

#ifdef _5L_QUICKTIME_
#include <MacTypes.h>
#endif // _5L_QUICKTIME_

#include "TCommon.h"
#include "TObject.h"
#include "TPoint.h"

BEGIN_NAMESPACE_FIVEL

/*-----------------------------------------------------------------

CLASS
    TRect

	A class to represent a rectangular region.    

AUTHOR
    Chuck Officer<br>

------------------------------------------------------------------*/
class TRect : public TObject 
{  
	public:
		//////////
		// Constructor.
		//
		// [in_optional] inTop - top of rect (default 0)
		// [in_optional] inLeft - left side of rect (default 0)
		// [in_optional] inBottom - bottom of rect (default 0)
		// [in_optional] inRight - right side of rect (default 0)
		//
		TRect(int32 inTop = 0, int32 inLeft = 0, int32 inBottom = 0, int32 inRight = 0);
		
		//////////
		// Copy Constructor.
		//
		// [in] rect - copy values from another TRect
		//
		TRect(TRect &rect);

		//////////
		// Set the coordinates for the rectangle.
		//
		// [in] inTop - top of rect 
		// [in] inLeft - left side of rect 
		// [in] inBottom - bottom of rect 
		// [in] inRight - right side of rect 
		//
		void		Set(int32 inTop, int32 inLeft, int32 inBottom, int32 inRight);
		
		//////////
		// Copy the coordinates for this rectangle from another TRect.
		//
		// [in] inRect - TRect to copy coordinates from
		//
		void		Set(const TRect &inRect);
		
		//////////
		// Offset the rectangle using the given TPoint.
		//
		// [in] inPt - TPoint used to offset the rectangle
		//
		void		Offset(TPoint &inPt);

		//////////
		// Set top coordinate.
		//
		// [in] inTop - the top coordinate 
		//
		inline void SetTop(int32 inTop) { m_Top = inTop; }
		
		//////////
		// Set left coordinate.
		//
		// [in] inLeft - the left coordinate
		//
		inline void	SetLeft(int32 inLeft) { m_Left = inLeft; }
		
		//////////
		// Set bottom coordinate.
		//
		// [in] inBottom - the bottom coordinate
		//
		inline void	SetBottom(int32 inBottom) { m_Bottom = inBottom; }
		
		//////////
		// Set right coordinate.
		//
		// [in] inRight - the right coordinate
		//
		inline void	SetRight(int32 inRight) { m_Right = inRight; }

		//////////
		// Offset the top coordinate.
		//
		// [in] inTopOffset - offset for the top coordinate 
		//
		inline void	OffsetTop(int32 inTopOffset) { m_Top += inTopOffset; }
		
		//////////
		// Offset the left coordinate.
		//
		// [in] inLeftOffset - offset for the left coordinate
		//
		inline void	OffsetLeft(int32 inLeftOffset) { m_Left += inLeftOffset; }
		
		//////////
		// Offset the bottom coordinate.
		//
		// [in] inBottomOffset - offset for the bottom coordinate
		//
		inline void	OffsetBottom(int32 inBottomOffset) { m_Bottom += inBottomOffset; }
		
		//////////
		// Offset the right coordinate.
		//
		// [in] inRightOffset - offset for the right coordinate
		//
		inline void	OffsetRight(int32 inRightOffset) { m_Right += inRightOffset; }

		//////////
		// Get the top coordinate.
		//
		// [out] return - the top coordinate
		//
		inline int32	Top(void) const { return (m_Top); }
		
		//////////
		// Get the left coordinate.
		//
		// [out] return - the left coordinate
		//
		inline int32	Left(void) const { return (m_Left); }
		
		//////////
		// Get the bottom coordinate.
		//
		// [out] return - the bottom coordinate
		//
		inline int32	Bottom(void) const { return (m_Bottom); }
		
		//////////
		// Get the right coordinate.
		//
		// [out] return - the right coordinate
		//
		inline int32	Right(void) const { return (m_Right); }

		//////////
		// Get the top-left corner of the rectangle.
		//
		// [out] return - the top-left corner
		//
		TPoint          TopLeft(void) const;

		//////////
		// Get the width of the rectangle.
		//
		// [out] return - the width
		//
		int32		Width(void) { return (m_Right - m_Left); }
		
		//////////
		// Get the height of the rectangle.
		//
		// [out] return - the height
		//
		int32		Height(void) { return (m_Bottom - m_Top); }

		//////////
		// Set the value of this rect using another TRect.  Same as Set().
		//
		// [in] inPt - TRect to copy values from (r-value)
		// [out] return - l-value with coordinates set to r-value
		//
		TRect		&operator=(const TRect &inRect);
		
		//////////
		// Equality check.
		//
		// [in] inRect - another rectangle to compare against
		// [out] return - true if they are equal, false otherwise
		//
		bool		operator==(TRect &inRect);

		//////////
		// Check whether a TPoint is contained within the rectangle.
		//
		// [in] inPt - a TPoint to check for containment
		// [out] return - true if the point is contained within the rectangle, 
		//			false otherwise
		//
		bool		Contains(TPoint &inPt);

#if defined (_5L_QUICKTIME_)
		//////////
		// Get a Quicktime Rect structure for the rectangle.<br>
		// COMPILE FLAG - if defined _5L_QUICKTIME
		//
		// [out] return - a Quicktime Rect structure for the rectangle
		//
		Rect		GetRect(void) const;
		
		//////////
		// Set the rectangle using the values from a Quicktime Rect structure.<br>
		// COMPILE FLAG - if defined _5L_QUICKTIME
		//
		// [in] inPt - a Quicktime Rect structure
		//
		void		Set(Rect &inRect);
#endif
#if defined (_5L_WIN32_)
		//////////
		// Get a Win32 RECT structure for the rectangle.<br>
		// COMPILE FLAG - if defined _5L_WIN32_
		//
		// [out] return - a Win32 RECT structure for the rectangle
		//
		RECT		GetRECT(void);
		
		//////////
		// Set the point using the values from a Win32 RECT structure.<br>
		// COMPILE FLAG - if defined _5L_WIN32_
		//
		// [in] inPt - a Win32 RECT structure
		//
		void		Set(RECT &inRect);
#endif
		
	protected:
        //////////
		// Top Coordinate
		//
		int32	m_Top;

		//////////
		// Left Coordinate
		//
		int32	m_Left;
		
		//////////
		// Bottom Coordinate
		//
		int32	m_Bottom;
		
		//////////
		// Right Coordinate
		//
		int32	m_Right;
};

END_NAMESPACE_FIVEL

#endif // _TRect_h_

/*
 $Log$
 Revision 1.3.4.1  2002/04/22 08:17:58  emk
 Updated Common code to build on Macintosh and pass all unit tests.

 Revision 1.3  2002/03/04 15:16:13  hamon
 Added support for compiler's namespaces. Namespaces are only enabled on macintosh.
Moved OS specific configuration to TPlatform.h
Changes by Elizabeth and Eric, okayed by Eric.

 Revision 1.2  2002/02/27 16:38:21  emk
 Cross-platform code merge!

 * Merged back in support for the Macintosh platform.  This is an ongoing
   process, and we'll need to do more work.

 * Separated out platform specific configuration with big block comments.

 * Merged in a few changes from KBTree which appeared to fix bugs.

 * Merged in IntToString, UIntToString, DoubleToString changes from the
   Macintosh engine, and improved the error handling.  NOTE: doubles now
   print using "%f" (the way the Mac engine always did it).  This means
   that "tstr = 0.0" will set 'tstr' to "0.000000", not "0" (as it
   did in the Win32 engine).

 This code might not build on Windows.  As soon as I hear from ssharp
 that he's updated the project files, I'll test it myself.

 Revision 1.1  2001/09/24 15:11:00  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.1  2000/04/06 17:06:10  chuck
 Initial check-in

*/
