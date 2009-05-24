// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

#include "AppHeaders.h"

#ifdef __WXMAC_OSX__
// Needed for Cairo.  This must be included before cairo.h, or else some of
// the integer typedefs will conflict.
#include <Carbon/Carbon.h>
#endif

#include "CairoContext.h"

using namespace Halyard;


//=========================================================================
//  CairoContext methods
//=========================================================================

CairoContext::CairoContext(wxBitmap &inPixmap)
    : mSurface(NULL), mCairo(NULL)
{
    mDC.SelectObject(inPixmap);
    mSurface = CreateSurface(mDC, inPixmap.GetWidth(), inPixmap.GetHeight());
    if (cairo_surface_status(mSurface) != CAIRO_STATUS_SUCCESS)
        gLog.Fatal("halyard.cairo",
                   "Error creating cairo_surface_t for bitmap");
    mCairo = cairo_create(mSurface);
    if (cairo_status(mCairo) != CAIRO_STATUS_SUCCESS)
        gLog.Fatal("halyard.cairo", "Error creating cairo_t for bitmap");
}

#if defined(__WXMAC_OSX__)

#include <cairo-quartz.h>

cairo_surface_t *
CairoContext::CreateSurface(wxDC &inDC, int inWidth, int inHeight) {
    wxGraphicsContext *wx_context(inDC.GetGraphicsContext());
    CGContextRef context((CGContextRef) wx_context->GetNativeContext());
    if (!context)
        THROW("Cannot retrieve native graphics context for DC");
    return cairo_quartz_surface_create_for_cg_context(context,
                                                      inWidth, inHeight);
}

#elif defined(__WXMSW__)

#include <windows.h>
#include <cairo-win32.h>

cairo_surface_t *
CairoContext::CreateSurface(wxDC &inDC, int inWidth, int inHeight) {
    return cairo_win32_surface_create((HDC) inDC.GetHDC());
}

#else
#error "No implementation of CairoContext::CreateSurface for this platform"
#endif

CairoContext::~CairoContext() {
    if (mCairo) {
        cairo_destroy(mCairo);
        mCairo = NULL;
    }
    if (mSurface) {
        cairo_surface_destroy(mSurface);
        mSurface = NULL;
    }
}

void CairoContext::SetSourceColor(const GraphicsTools::Color &inColor) {
    cairo_set_source_rgba(mCairo,
                          inColor.red / 255.0, inColor.green / 255.0,
                          inColor.blue / 255.0, inColor.alpha / 255.0);
}

void CairoContext::TransformRectToUnitSquare(const wxRect &inRect,
                                             int inStrokeWidth) {
    cairo_translate(mCairo, inRect.GetX() + inStrokeWidth / 2.0,
                    inRect.GetY() + inStrokeWidth / 2.0);
    cairo_scale(mCairo, inRect.GetWidth() - inStrokeWidth,
                inRect.GetHeight() - inStrokeWidth);
}
