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
// TRect.h : 
//

#if !defined (_TRect_h_)
#define _TRect_h_

BEGIN_NAMESPACE_FIVEL

/*-----------------------------------------------------------------

CLASS
    TRect

	A class to represent a rectangular region.    

AUTHOR
    Chuck Officer<br>

------------------------------------------------------------------*/
class TRect 
{  
    public:
	    TRect(int32 inLeft = 0, int32 inTop = 0, 
			  int32 inRight = 0, int32 inBottom = 0);

		TRect(const TRect &rect);
	    
	    void Set(int32 inLeft, int32 inTop, 
				 int32 inRight, int32 inBottom);
		
	    void Set(const TRect &inRect);

	    void Offset(const TPoint &inPt);

		inline void SetTop(int32 inTop) { m_Top = inTop; }
		
		inline void	SetLeft(int32 inLeft) { m_Left = inLeft; }
		
		inline void	SetBottom(int32 inBottom) { m_Bottom = inBottom; }
		
		inline void	SetRight(int32 inRight) { m_Right = inRight; }

	    inline void	OffsetLeft(int32 inLeftOffset) { m_Left += inLeftOffset; }
		
		inline void	OffsetBottom(int32 inBottomOffset) { 
			m_Bottom += inBottomOffset; 
		}
		
		inline void	OffsetRight(int32 inRightOffset) { 
			m_Right += inRightOffset; 
		}

		inline int32 Top() const { return (m_Top); }
		
		inline int32 Left() const { return (m_Left); }
		
		inline int32 Bottom() const { return (m_Bottom); }
		
		inline int32 Right() const { return (m_Right); }

		TPoint TopLeft() const;

		int32 Width() { return (m_Right - m_Left); }
		
		int32 Height() { return (m_Bottom - m_Top); }

		TRect &operator=(const TRect &inRect);
		
		bool operator==(const TRect &inRect) const;

        // Does TRect contain TPoint?
		bool Contains(TPoint &inPt);
		
	protected:
        int32 m_Top;
		int32 m_Left;
		int32 m_Bottom;
		int32	m_Right;
};

inline std::ostream &operator<<(std::ostream &out, const TRect &r) {
	out << "(rect " << r.Left() << " " << r.Top() << " " << r.Right() << " "
		<< r.Bottom() << ")";
	return out;
}

END_NAMESPACE_FIVEL

#endif // _TRect_h_

/*
 $Log$
 Revision 1.10  2004/02/05 22:11:15  kwasi
   * Fixed TRect argument order (finally!).
   * Deleted TString and TURL.

 Revision 1.9  2004/02/02 22:07:55  djin
   * Cleaned up a whole bunch of legacy code that can be much simpler now
     that we have TValue.
   * Removed TObject, TArray and TBTree because they have overstayed their
     welcome by about a decade.

 Revision 1.8  2004/01/30 22:31:18  kwasi
   * Removed SET primitive.
   * Replaced TVariable/TVariableManager with TValue/new-TVariableManager.
   * Implemented operator<< for many basic types used by TValue.

 Revision 1.7  2003/06/13 10:57:30  emk
 Further use of precompiled headers; pruning of various inappropriate
 includes.

 Revision 1.6  2003/06/04 15:24:12  brian
 Added support for polygons and polygonal zones.

 Revision 1.5  2002/06/20 16:32:53  emk
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

 Revision 1.4.6.1  2002/06/19 22:50:55  emk
 3.3.4.11 - Refactored Mac code to move primitives from CCard.{h,cpp} to
 TMacPrimitives.{h,cpp}, and break most of the remaining dependencies on
 the 5L interpreter.

 Language changes: LOADPICK, RVAR and RNODE are gone.  I've also disabled
 the Mac PAUSE command until Douglas tells me how it should work.

 Testing: Please beat *very* hard on this build, and pay special attention
 to WAIT, NAP, TIMEOUT, and similar commands.

 Next up: I plan to merge this branch into HEAD tomorrow.

 Revision 1.4  2002/05/15 11:05:17  emk
 3.3.3 - Merged in changes from FiveL_3_3_2_emk_typography_merge branch.
 Synopsis: The Common code is now up to 20Kloc, anti-aliased typography
 is available, and several subsystems have been refactored.  For more
 detailed descriptions, see the CVS branch.

 The merged Mac code hasn't been built yet; I'll take care of that next.

 Revision 1.3.4.2  2002/05/15 08:13:15  emk
 3.3.2.8 - Overhauled assertion handling to call FatalError and log problems in 5L.log.  Also added hooks for unfading the screen before displaying errors (this is needed to play nicely with the Mac gamma fader).

 Made tweaks to support the migration of Mac (buttpcx ...) to the new anti-aliased typography library.

 The TBTree destructor is still a broken nightmare, especially on FatalError's forced shutdowns.  Expect *both* FiveL's to do something childish immediately after fatal errors and assertion failures.

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
