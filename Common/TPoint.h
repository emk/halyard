// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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
// TPoint.h : 
//

#if !defined (_TPoint_h_)
#define _TPoint_h_

BEGIN_NAMESPACE_FIVEL

/*-----------------------------------------------------------------

CLASS
    TPoint

	A class to represent a point in 2D space.    

AUTHOR
    Chuck Officer<br>

------------------------------------------------------------------*/
class TPoint 
{ 
    public:
		//////////
		// Constructor.
		//
		// [in_optional] inX - set the X-coordinate (default 0)
		// [in_optional] inY - set the y-coordinate (default 0)
		//
		TPoint(int32 inX = 0, int32 inY = 0);

		//////////
		// Copy Constructor.
		//
		// [in] inPt - a TPoint to copy from
		//
		TPoint(const TPoint &inPt);

		//////////
		// Set values for the point.
		//
		// [in] inX - set the X-coordinate
		// [in] inY - set the y-coordinate
		//
		void			Set(int32 inX, int32 inY);

		//////////
		// Set values for the point using another TPoint.
		//
		// [in] inPt - TPoint to copy values from
		//
		void			Set(const TPoint &inPt);

		//////////
		// Offset the point using another TPoint.
		//
		// [in] inPt - TPoint used to offset values for this point
		//
		void			Offset(TPoint &inPt);

		//////////
		// Set the X-coordinate.
		//
		// [in] inX - the X-coordinate
		//
		inline void		SetX(int32 inX) { m_X = inX; }
		
		//////////
		// Set the Y-coordinate.
		//
		// [in] inY - the Y-coordinate
		//
		inline void		SetY(int32 inY) { m_Y = inY; }

		//////////
		// Offset the X-coordinate.
		//
		// [in] inXOffset - offset for the X-coordinate
		//
		inline void		OffsetX(int32 inXOffset) { m_X += inXOffset; }
		
		//////////
		// Offset the Y-coordinate.
		//
		// [in] inYOffset - offset for the Y-coordinate
		//
		inline void		OffsetY(int32 inYOffset) { m_Y += inYOffset; }
		
		//////////
		// Get the X-coordinate
		//
		// [out] return - the X-coordinate
		//
		inline int32	X(void) const { return (m_X); }
		
		//////////
		// Get the Y-coordinate
		//
		// [out] return - the Y-coordinate
		//
		inline int32	Y(void) const { return (m_Y); }

		//////////
		// Set values for this point using another TPoint.  Same as Set().
		//
		// [in] inPt - TPoint to copy values from (r-value)
		// [out] return - l-value with coordinates set to r-value
		//
		TPoint			&operator=(const TPoint &inPt);
		
		//////////
		// Equality check.
		//
		// [in] inPt - a TPoint to check against for equality
		// [out] return - true if the two points are equal, false otherwise
		//
		bool			operator==(const TPoint &inPt) const;

	protected:
        //////////
		// X-Coordinate
		//
		int32		m_X;
		
		//////////
		// Y-Coordinate
		//
		int32		m_Y;
};

inline std::ostream &operator<<(std::ostream &out, const TPoint &p) {
	out << "(point " << p.X() << " " << p.Y() << ")";
	return out;
}

END_NAMESPACE_FIVEL

#endif // _TPoint_h_

/*
 $Log$
 Revision 1.8  2004/02/02 22:07:55  djin
   * Cleaned up a whole bunch of legacy code that can be much simpler now
     that we have TValue.
   * Removed TObject, TArray and TBTree because they have overstayed their
     welcome by about a decade.

 Revision 1.7  2004/01/30 22:31:18  kwasi
   * Removed SET primitive.
   * Replaced TVariable/TVariableManager with TValue/new-TVariableManager.
   * Implemented operator<< for many basic types used by TValue.

 Revision 1.6  2003/06/13 10:57:30  emk
 Further use of precompiled headers; pruning of various inappropriate
 includes.

 Revision 1.5  2003/06/04 15:24:11  brian
 Added support for polygons and polygonal zones.

 Revision 1.4  2002/06/20 16:32:53  emk
 Merged the 'FiveL_3_3_4_refactor_lang_1' branch back into the trunk.  This
 branch contained the following enhancements:

   * Most of the communication between the interpreter and the
     engine now goes through the interfaces defined in
     TInterpreter.h and TPrimitive.h.  Among other things, this
     refactoring makes will make it easier to (1) change the interpreter
     from 5L to Scheme and (2) add portable primitives that work
     the same on both platforms.
   * A new system for handling callbacks.

 I also slipped in the following, unrelated enhancements:

   * MacOS X fixes.  Classic Mac5L once again runs under OS X, and
     there is a new, not-yet-ready-for-prime-time Carbonized build.
   * Bug fixes from the "Fix for 3.4" list.

 Revision 1.3.6.1  2002/06/19 22:50:55  emk
 3.3.4.11 - Refactored Mac code to move primitives from CCard.{h,cpp} to
 TMacPrimitives.{h,cpp}, and break most of the remaining dependencies on
 the 5L interpreter.

 Language changes: LOADPICK, RVAR and RNODE are gone.  I've also disabled
 the Mac PAUSE command until Douglas tells me how it should work.

 Testing: Please beat *very* hard on this build, and pay special attention
 to WAIT, NAP, TIMEOUT, and similar commands.

 Next up: I plan to merge this branch into HEAD tomorrow.

 Revision 1.3  2002/05/15 11:05:17  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.2.4.2  2002/04/30 07:57:24  emk
 3.3.2.5 - Port Win32 code to use the 20Kloc of Common code that now
 exists.  The (defstyle ...) command should work, but (textaa ...) isn't
 available yet.

 Next up: Implement the (textaa ...) command and the low-level
 GraphicsTools::Image::DrawBitMap.

 Revision 1.2.4.1  2002/04/22 08:17:58  emk
 Updated Common code to build on Macintosh and pass all unit tests.

 Revision 1.2  2002/03/04 15:16:10  hamon
 Added support for compiler's namespaces. Namespaces are only enabled on macintosh.
Moved OS specific configuration to TPlatform.h
Changes by Elizabeth and Eric, okayed by Eric.

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
