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

#ifndef CairoDrawing_H
#define CairoDrawing_H

#include <cairo.h>

/// A cairo_surface_t "smart pointer".  A CairoSurfacePtr maintains a
/// reference to a Cairo surface, and releases the reference when the
/// CairoSurfacePtr is destroyed.  Conceptually, this class should work as
/// much like boost::shared_ptr as possible.
class CairoSurfacePtr {
    cairo_surface_t *mSurface;

public:
    CairoSurfacePtr();
    explicit CairoSurfacePtr(cairo_surface_t *inSurface);
    CairoSurfacePtr(const CairoSurfacePtr &inSurface);
    ~CairoSurfacePtr();
    CairoSurfacePtr &operator=(const CairoSurfacePtr &inPtr);

    /// Is this pointer NULL?  We provide this instead of 'operator bool',
    /// becaus the latter is actually quite hard to implement correctly.
    bool is_null() const { return mSurface == NULL; }

    /// Return the underlying surface without incrementing the reference
    /// count.  This function is analogous to boost::shared_ptr::get.
    cairo_surface_t *get() { return mSurface; }

    /// Create a surface from the specified wxDC.  Does not support alpha
    /// channels in any sort of portable fashion.
    static CairoSurfacePtr FromDC(wxDC &inDC);

    int GetWidth();
    int GetHeight();
};

/// A Cairo drawing context for a wxBitmap.  This class is a thin
/// wrapper around an ordinary cairo_t drawing context, and it can be
/// passed to functions expecting a cairo_t value.  It also provides
/// some convenience functions.
class CairoContext {
    CairoSurfacePtr mSurface;
    cairo_t *mCairo;

protected:
    /// Create a CairoContext without initializing it.  You must call
    /// Initialize before finishing the constructor.
    CairoContext();

    /// Initialize the context.
    void Initialize(CairoSurfacePtr inSurface);

public:
    /// Construct a Cairo context for inSurface.
    CairoContext(CairoSurfacePtr inSurface);
    virtual ~CairoContext();

    /// Implicity convert a CairoContext to a cairo_t *.
    operator cairo_t *() { return mCairo; }

    /// Retrieve the surface associated with this context.
    CairoSurfacePtr GetSurface() { return mSurface; }

    /// Call cairo_set_source_rgba using inColor.
    void SetSourceColor(const GraphicsTools::Color &inColor);

    /// Transform the drawing context so that the square 0,0,1,1 will be
    /// mapped to inRect.  If you pass in inStrokeWidth, value of inRect
    /// will be inset by half of inStrokeWidth, allowing you to draw a
    /// Halyard-style stroke entirely within inRect.  Returns true if the
    /// transform succeeded, and false if the resulting matrix would be
    /// invalid.
    bool TransformRectToUnitSquare(const wxRect &inRect, int inStrokeWidth = 0);
};

/// A Cairo drawing context for a wxBitmap.  Note that this class does not
/// correctly support wxBitmap objects with alpha channels on Windows.
class CairoBitmapContext : public CairoContext {
    wxMemoryDC mDC;

public:
    /// Construct a Cairo context for a bitmap.
    CairoBitmapContext(wxBitmap &inBitmap);
};

#endif // CairoDrawing_H
