// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

// No longer compiles, and doesn't offer much performance advantage,
// but still cool.
#if 0

#include <wx/wx.h>
#include <wx/rawbtmp.h>

#include "TCommon.h"
#include "fade_asm.h"

void OptimizedDrawingStart()
{
	// Nothing to do.
}

void OptimizedDrawingEnd()
{
    __asm {
		emms                    // Make it safe to use floating point.
    }
}

void OptimizedFadePixels(wxRawBitmapPixelRef24 inPixelsBegin, 
						 wxRawBitmapPixelRef24 inPixelsEnd,
						 wxRawBitmapPixelRef24 outPixelsBegin,
						 uint16 inValue)
{
    __asm {
		// Load four copies of inValue into mm2.
		mov ax, [inValue]       // eax = 0000vvvv
        shl eax, 16             // eax = vvvv0000
		mov ax, [inValue]       // eax = vvvvvvvv
		movd mm2, eax           // mm2 = 00000000|vvvvvvvv
		movd mm0, eax           // mm0 = 00000000|vvvvvvvv
		psllq mm0, 32           // mm0 = vvvvvvvv|00000000
		por mm2, mm0            // mm2 = vvvv|vvvv|vvvv|vvvv

		// We'll need some zeros for use with the unpack instructions.
		pxor mm7, mm7           // mm7 = 00|00|00|00|00|00|00|00

		// Prepare our various pixel pointers.
		mov ebx, [inPixelsBegin]
		mov ecx, [inPixelsEnd]
		mov edx, [outPixelsBegin]

		// Do our loop.
		jmp check
body:   movq mm0, [ebx]
		movq mm1, mm0
		punpcklbw mm0, mm7
		punpckhbw mm1, mm7
		pmullw mm0, mm2
		pmullw mm1, mm2
		psrlw mm0, 8
		psrlw mm1, 8
		packuswb mm0, mm1
		movq [edx], mm0
		add ebx, 8
		add edx, 8
check:  cmp ebx, ecx
        jc body
    }
}

#endif