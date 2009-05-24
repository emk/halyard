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

#ifndef CairoContext_H
#define CairoContext_H

#include <cairo.h>

/// A Cairo drawing context for a wxBitmap.  This class is a thin
/// wrapper around an ordinary cairo_t drawing context, and it can be
/// passed to functions expecting a cairo_t value.  It also provides
/// some convenience functions.
class CairoContext {
    wxMemoryDC mDC;
    cairo_surface_t *mSurface;
    cairo_t *mCairo;

    /// Platform-specific surface allocation.
    cairo_surface_t *CreateSurface(wxDC &inDC, int inWidth, int inHeight);

public:
    /// Construct a Cairo context for inPixmap.
    CairoContext(wxBitmap &inPixmap);
    ~CairoContext();

    /// Implicity convert a CairoContext to a cairo_t *.
    operator cairo_t *() { return mCairo; }

    /// Retrieve the surface associated with this context.
    cairo_surface_t *GetSurface() { return mSurface; }

    /// Call cairo_set_source_rgba using inColor.
    void SetSourceColor(const GraphicsTools::Color &inColor);

    /// Transform the drawing context so that the square 0,0,1,1 will be
    /// mapped to inRect.  If you pass in inStrokeWidth, value of inRect
    /// will be inset by half of inStrokeWidth, allowing you to draw a
    /// Halyard-style stroke entirely within inRect.
    void TransformRectToUnitSquare(const wxRect &inRect, int inStrokeWidth = 0);
};

#endif // CairoContext_H
