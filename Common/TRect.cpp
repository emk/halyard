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
// TRect.cpp : 
//

#include "THeader.h"
#include "TRect.h"

USING_NAMESPACE_FIVEL

TRect::TRect(int32 inTop /* = 0 */, int32 inLeft /* = 0 */, 
			 int32 inBottom /* = 0 */, int32 inRight /* = 0 */)
{ 
	m_Top = inTop;
	m_Left = inLeft;
	m_Bottom = inBottom; 
	m_Right = inRight;
}

TRect::TRect(const TRect &inRect)
{
	m_Top = inRect.m_Top;
	m_Left = inRect.m_Left;
	m_Bottom = inRect.m_Bottom;
	m_Right = inRect.m_Right;
}

void TRect::Set(int32 inTop, int32 inLeft, int32 inBottom, int32 inRight)
{  
	m_Top = inTop; 
	m_Left = inLeft; 
	m_Bottom = inBottom; 
	m_Right = inRight; 
}

void TRect::Set(const TRect &inRect)
{
	m_Top = inRect.Top();
	m_Left = inRect.Left();
	m_Bottom = inRect.Bottom();
	m_Right = inRect.Right();
}

TPoint TRect::TopLeft(void) const
{
	TPoint retPt(m_Left, m_Top);
	return retPt;
}

TRect &TRect::operator=(const TRect &inRect)
{
	Set(inRect);

	return (*this);
}

// 
// equality operator
//
bool TRect::operator==(TRect &inRect)
{
	if ((inRect.Top() == m_Top) and
		(inRect.Left() == m_Left) and
		(inRect.Bottom() == m_Bottom) and
		(inRect.Right() == m_Right))
	{
		return (true);
	}
	return (false);
}

bool TRect::Contains(TPoint &inPt)
{
	if ((inPt.X() >= m_Left) and 
		(inPt.X() <= m_Right) and
		(inPt.Y() >= m_Top) and 
		(inPt.Y() <= m_Bottom))
	{
		return (true);
	}
	return (false);
}

void TRect::Offset(const TPoint &inPt)
{
	m_Top += inPt.Y();
	m_Bottom += inPt.Y();
	m_Left += inPt.X();
	m_Right += inPt.X();
}

#if defined (_5L_QUICKTIME_)
Rect TRect::GetRect(void) const
{
	Rect	retRect;
	
	retRect.top = (short) m_Top;
	retRect.left = (short) m_Left;
	retRect.bottom = (short) m_Bottom;
	retRect.right = (short) m_Right;

	return (retRect);
}

void TRect::Set(Rect &inRect)
{
	m_Top = inRect.top;
	m_Left = inRect.left;
	m_Bottom = inRect.bottom;
	m_Right = inRect.right;
}
#endif

#if defined (_5L_WIN32_)
RECT TRect::GetRECT(void)
{
	RECT	retRect;

	retRect.top = m_Top;
	retRect.left = m_Left;
	retRect.bottom = m_Bottom;
	retRect.right = m_Right;

	return (retRect);
}
void TRect::Set(RECT &inRect)
{
	m_Top = inRect.top;
	m_Left = inRect.left;
	m_Bottom = inRect.bottom;
	m_Right = inRect.right;
}
#endif
/*
 $Log$
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

 Revision 1.3.4.1  2002/05/15 08:13:15  emk
 3.3.2.8 - Overhauled assertion handling to call FatalError and log problems in 5L.log.  Also added hooks for unfading the screen before displaying errors (this is needed to play nicely with the Mac gamma fader).

 Made tweaks to support the migration of Mac (buttpcx ...) to the new anti-aliased typography library.

 The TBTree destructor is still a broken nightmare, especially on FatalError's forced shutdowns.  Expect *both* FiveL's to do something childish immediately after fatal errors and assertion failures.

 Revision 1.3  2002/03/04 15:16:11  hamon
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
