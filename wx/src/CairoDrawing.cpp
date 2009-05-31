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

#include "CairoDrawing.h"

using namespace Halyard;


//=========================================================================
//  CairoSurfacePtr methods
//=========================================================================

/// Create a CairoSurfacePtr without an attached surface.  This can be
/// initialized at a latter time as follows:
///
///   CairoSurfacePtr surface;
///   surface = CairoSurfacePtr(cairo_win32_surface_create(my_dc));
CairoSurfacePtr::CairoSurfacePtr()
    : mSurface(NULL)
{
}

/// Take ownership of a Cairo surface, without increasing the reference
/// count.  Note that this constructor must be invoked explicitly, because
/// we don't want the ownership transfer to be surprising.  Note that we
/// also check cairo_surface_status to make sure this surface is valid.
CairoSurfacePtr::CairoSurfacePtr(cairo_surface_t *inSurface)
    : mSurface(inSurface)
{
    ASSERT(inSurface != NULL);
    if (cairo_surface_status(mSurface) != CAIRO_STATUS_SUCCESS)
        gLog.Fatal("halyard.cairo",
                   "Invalid surface passed to CairoSurfacePtr: %s",
                   cairo_status_to_string(cairo_surface_status(mSurface)));
}

/// Copy constructor.
CairoSurfacePtr::CairoSurfacePtr(const CairoSurfacePtr &inSurface)
    : mSurface(inSurface.mSurface)
{
    if (mSurface != NULL)
        cairo_surface_reference(mSurface);
}

/// Destructor.  Calls cairo_surface_destroy.
CairoSurfacePtr::~CairoSurfacePtr() {
    if (mSurface != NULL)
        cairo_surface_destroy(mSurface);
}

/// Set this surface ref to point to inSurface.  Note that we delibrately
/// neglect to provide operator=(cairo_surface_t *), because we want people
/// to use the explicit CairoSurfacePtr constructor and avoid nasty
/// surprises about surface ownership.
CairoSurfacePtr &CairoSurfacePtr::operator=(const CairoSurfacePtr &inSurface) {
    // Be careful: *this and inSurface might be the same object.
    if (mSurface != inSurface.mSurface) {
        if (mSurface != NULL)
            cairo_surface_destroy(mSurface);
        if (inSurface.mSurface != NULL)
            cairo_surface_reference(inSurface.mSurface);
        mSurface = inSurface.mSurface;
    }
    return *this;
}


//=========================================================================
//  Creating CairoSurfacePtr objects
//=========================================================================

#if defined(__WXMAC_OSX__)

#include <cairo-quartz.h>

CairoSurfacePtr
CairoSurfacePtr::FromDC(wxDC &inDC) {
    wxGraphicsContext *wx_context(inDC.GetGraphicsContext());
    CGContextRef context((CGContextRef) wx_context->GetNativeContext());
    if (!context)
        THROW("Cannot retrieve native graphics context for DC");
    wxSize size(inDC.GetSize());
    cairo_surface_t *surface =
        cairo_quartz_surface_create_for_cg_context(context,
                                                   size.GetWidth(),
                                                   size.GetHeight());
    return CairoSurfacePtr(surface);
}

#elif defined(__WXMSW__)

#include <windows.h>
#include <cairo-win32.h>

CairoSurfacePtr
CairoSurfacePtr::FromDC(wxDC &inDC) {
    return CairoSurfacePtr(cairo_win32_surface_create((HDC) inDC.GetHDC()));
}

#else
#error "No implementation of CairoContext::CreateSurface for this platform"
#endif


//=========================================================================
//  CairoContext methods
//=========================================================================

CairoContext::CairoContext()
    : mCairo(NULL)
{
}

void CairoContext::Initialize(CairoSurfacePtr inSurface) {
    ASSERT(!inSurface.is_null());
    ASSERT(mSurface.is_null());
    ASSERT(!mCairo);

    cairo_status_t surface_status(cairo_surface_status(inSurface.get()));
    if (surface_status != CAIRO_STATUS_SUCCESS)
        gLog.Fatal("halyard.cairo",
                   "Cannot create cairo_t for broken surface: %s",
                   cairo_status_to_string(surface_status));
    
    mSurface = inSurface;
    mCairo = cairo_create(mSurface.get());
    if (cairo_status(mCairo) != CAIRO_STATUS_SUCCESS)
        gLog.Fatal("halyard.cairo",
                   "Error creating cairo_t for surface: %s",
                   cairo_status_to_string(cairo_status(mCairo)));
}

CairoContext::CairoContext(CairoSurfacePtr inSurface)
    : mCairo(NULL)
{
    Initialize(inSurface);
}

CairoContext::~CairoContext() {
    if (mCairo) {
        if (cairo_status(mCairo) != CAIRO_STATUS_SUCCESS)
            gLog.Warn("halyard.cairo",
                      "Error during Cairo drawing operations: %s",
                      cairo_status_to_string(cairo_status(mCairo)));

        cairo_destroy(mCairo);
        mCairo = NULL;
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


//=========================================================================
//  CairoBitmapContext methods
//=========================================================================

CairoBitmapContext::CairoBitmapContext(wxBitmap &inBitmap) {
    mDC.SelectObject(inBitmap);
    Initialize(CairoSurfacePtr::FromDC(mDC));
}
