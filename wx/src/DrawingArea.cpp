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

#include <wx/rawbmp.h>

#include "DrawingArea.h"
#include "CairoContext.h"
#include "Stage.h"
#include "CommonWxConv.h"

#if CONFIG_HAVE_QUAKE2
#include "Quake2Engine.h"
#endif // CONFIG_HAVE_QUAKE2

using namespace Halyard;


//=========================================================================
//  DrawingArea methods
//=========================================================================

DrawingArea::DrawingArea(Stage *inStage, int inWidth, int inHeight,
						 bool inHasAlpha)
	: mStage(inStage), mBounds(wxPoint(0, 0), wxSize(inWidth, inHeight)),
      mIsShown(true), mHasAlpha(inHasAlpha)
{
	InitializePixmap();
}

DrawingArea::DrawingArea(Stage *inStage, const wxRect &inBounds,
						 bool inHasAlpha)
	: mStage(inStage), mBounds(inBounds), mIsShown(true), mHasAlpha(inHasAlpha)
{
	InitializePixmap();
}

DrawingArea::~DrawingArea() {
    // We're going away, so force recompositing later.
    InvalidateDrawingArea(false);
}

bool DrawingArea::HasAreaOfZero() const {
    // wxRawBitmap will return invalid iterators when used with pixmaps of
    // area 0.  So before calling any wxRawBitmap functions (or any routine
    // named *Opt), call this function and immediately give up if it
    // returns false.
	return (mBounds.GetWidth() == 0 || mBounds.GetHeight() == 0);
}

void DrawingArea::InitializePixmap() {
    if (HasAreaOfZero())
        return;

	mPixmap.Create(mBounds.GetWidth(), mBounds.GetHeight(),
				   mHasAlpha ? 32 : 24);
	if (mHasAlpha)
		mPixmap.UseAlpha();
	Clear();

    MaybeInitializeGameOverlay();
}

#if CONFIG_HAVE_QUAKE2

void DrawingArea::MaybeInitializeGameOverlay() {
    // If we don't have a running Quake 2 instance, return immediately.
    if (!Quake2Engine::HaveInstance())
        return;

    // If the pixmap has area zero, then we don't have any valid data to
    // pass to Quake anyway.
    if (HasAreaOfZero())
        return;

    int format;
    unsigned char *data;
    int stride;
    if (mHasAlpha) {
        // PORTING - This assumes a BGR offscreen buffer.
        wxAlphaPixelData pdata(mPixmap);
        wxAlphaPixelData::Iterator iter(pdata);
        format = Q2_FORMAT_BGRA_PREMUL;
        ASSERT(wxAlphaPixelData::Iterator::PixelFormat::BLUE == 0);
        data = &(iter.Blue());
        iter.OffsetY(pdata, 1);
        stride = &(iter.Blue()) - data;
    } else {
        // PORTING - This assumes a BGR offscreen buffer.
        wxNativePixelData pdata(mPixmap);
        wxNativePixelData::Iterator iter(pdata);
        format = Q2_FORMAT_BGR;
        ASSERT(wxNativePixelData::Iterator::PixelFormat::BLUE == 0);
        data = &(iter.Blue());
        iter.OffsetY(pdata, 1);
        stride = &(iter.Blue()) - data;
    }
    shared_ptr<wxQuake2Overlay>
        ptr(new wxQuake2Overlay(format, data, mBounds, stride));
    mQuake2Overlay = ptr;
}

void DrawingArea::GameOverlayDelete() {
    if (mQuake2Overlay)
        mQuake2Overlay = shared_ptr<wxQuake2Overlay>();
}

void DrawingArea::GameOverlayDirtyRect(const wxRect &inRect) {
    if (mQuake2Overlay)
        mQuake2Overlay->DirtyRect(inRect);
}

void DrawingArea::GameOverlayShow(bool inShouldShow) {
    if (mQuake2Overlay)
        mQuake2Overlay->Show(inShouldShow);
}

void DrawingArea::GameOverlayMoveTo(const wxPoint &inPoint) {
    if (mQuake2Overlay)
        mQuake2Overlay->MoveTo(inPoint);
}

#else // !CONFIG_HAVE_QUAKE2

void DrawingArea::MaybeInitializeGameOverlay() {}

void DrawingArea::GameOverlayDelete() {}

void DrawingArea::GameOverlayDirtyRect(const wxRect &inRect) {}

void DrawingArea::GameOverlayShow(bool inShouldShow) {}

void DrawingArea::GameOverlayMoveTo(const wxPoint &inPoint) {}

#endif // !CONFIG_HAVE_QUAKE2

void DrawingArea::InvalidateRect(const wxRect &inRect, int inInflate,
                                 bool inHasPixmapChanged)
{
    // TODO - We can probably get away with less invalidation if this
    // DrawingArea isn't shown, but keep a careful eye on the Quake
    // overlays.
	wxRect r(inRect);
	r.Inflate(inInflate);
    r.Intersect(wxRect(wxPoint(0, 0),
                       mBounds.GetSize()));
	if (!r.IsEmpty()) {
        if (inHasPixmapChanged)
            GameOverlayDirtyRect(r);
	    r.Offset(mBounds.GetPosition());
	    mStage->InvalidateRect(r);
	}
}

void DrawingArea::InvalidateDrawingArea(bool inHasPixmapChanged) {
    InvalidateRect(wxRect(0, 0, mBounds.GetWidth(), mBounds.GetHeight()),
                   0, inHasPixmapChanged);
}

void DrawingArea::SetSize(const wxSize &inSize) {
    // If we have a Quake 2 overlay, get rid of it.
    GameOverlayDelete();

    // Invalidate the rectangle covered by our original size.
    InvalidateDrawingArea(false);
    
    // Update our size.
    mBounds = wxRect(mBounds.GetPosition(), inSize);

    // Allocate a new, empty pixmap.  This will invalidate the rectangle
    // covered by the new size, and reallocate our Quake 2 overlay if we're
    // supposed to have one.
    mPixmap = wxBitmap();
    InitializePixmap();
}

void DrawingArea::Show(bool inShow) {
    if (inShow != mIsShown) {
        mIsShown = inShow;
        GameOverlayShow(inShow);
        InvalidateDrawingArea(false);
    }
}

void DrawingArea::MoveTo(const wxPoint &inPoint) {
    InvalidateDrawingArea(false);
    mBounds = wxRect(inPoint, mBounds.GetSize());
    GameOverlayMoveTo(inPoint);
    InvalidateDrawingArea(false);
}

void DrawingArea::Clear() {
	if (mHasAlpha) {
		Clear(GraphicsTools::Color(0x00, 0x00, 0x00, 0x00));
	} else {
		Clear(GraphicsTools::Color(0x00, 0x00, 0x00));
	}
}

void DrawingArea::Clear(const GraphicsTools::Color &inColor) {
    if (HasAreaOfZero())
        return;

    // Actually, I have no idea what Cairo would do in this case, but this
    // rule was enforced by the old code.
    if (!mHasAlpha && !inColor.IsCompletelyOpaque())
		THROW("Cannot clear opaque overlay with transparent color.");
    
    CairoContext cr(GetPixmap());
    cr.SetSourceColor(inColor);
    cairo_set_operator(cr, CAIRO_OPERATOR_SOURCE);
    cairo_paint(cr);

    InvalidateDrawingArea();
}

void DrawingArea::DrawLine(const wxPoint &inFrom, const wxPoint &inTo,
						   const GraphicsTools::Color &inColor, int inWidth)
{
    if (HasAreaOfZero())
        return;
    
    CairoContext cr(GetPixmap());

    // Unfortunately, our legacy line-drawing semantics are fairly
    // broken--horizontal and vertical lines were special-cased in the
    // old code, and diagonal lines were just barely supported in a few
    // special cases.  If you want saner semantics, you'll need a new
    // line-drawing primitive.  But this should match old-style
    // horizontal and vertical lines exactly.
    if (inFrom.x == inTo.x) {
        cairo_move_to(cr, inFrom.x + inWidth / 2.0, inFrom.y);
        cairo_line_to(cr, inTo.x   + inWidth / 2.0, inTo.y);
    } else if (inFrom.y == inTo.y) {
        cairo_move_to(cr, inFrom.x, inFrom.y + inWidth / 2.0);
        cairo_line_to(cr, inTo.x,   inTo.y   + inWidth / 2.0);
    } else {
        // Yes, our legacy diagonal lines really did have round endcaps--I
        // checked under wxWidgets 2.9 on Windows, and wxWidgets 2.8 on the
        // Mac.  Pretty unfortunate, really.
        cairo_set_line_cap(cr, CAIRO_LINE_CAP_ROUND);
        cairo_move_to(cr, inFrom.x, inFrom.y);
        cairo_line_to(cr, inTo.x,   inTo.y);
    }

    cr.SetSourceColor(inColor);
    cairo_set_line_width(cr, inWidth);
    cairo_stroke(cr);

    // This is slightly annoying, but should handle the maximum bounds of
    // our various types of line.
    InvalidateRect(wxRect(wxPoint(std::min(inFrom.x, inTo.x),
                                  std::min(inFrom.y, inTo.y)),
                          wxPoint(std::max(inFrom.x, inTo.x),
                                  std::max(inFrom.y, inTo.y))),
                   inWidth);
}

void DrawingArea::FillBox(const wxRect &inBounds,
						  const GraphicsTools::Color &inColor)
{
    if (HasAreaOfZero())
        return;
    
    CairoContext cr(GetPixmap());
    cr.TransformRectToUnitSquare(inBounds);
    cairo_rectangle(cr, 0, 0, 1, 1);
    cr.SetSourceColor(inColor);
    cairo_fill(cr);

    InvalidateRect(inBounds);
}

void DrawingArea::OutlineBox(const wxRect &inBounds,
                             const GraphicsTools::Color &inColor,
							 int inWidth)
{
    if (HasAreaOfZero())
        return;

    CairoContext cr(GetPixmap());

    cairo_save(cr);
    cr.TransformRectToUnitSquare(inBounds, inWidth);
    cairo_rectangle(cr, 0, 0, 1, 1);
    cairo_restore(cr); // Undo transformation before drawing stroke.

    cr.SetSourceColor(inColor);
    cairo_set_line_width(cr, inWidth);
    cairo_stroke(cr);

    InvalidateRect(inBounds);
}

void DrawingArea::FillOval(const wxRect &inBounds, 
                           const GraphicsTools::Color &inColor)
{
    if (HasAreaOfZero())
        return;
    
    CairoContext cr(GetPixmap());
    cr.TransformRectToUnitSquare(inBounds);
    cairo_arc(cr, 0.5, 0.5, 0.5, 0, 2 * M_PI);
    cr.SetSourceColor(inColor);
    cairo_fill(cr);

    InvalidateRect(inBounds);
}

void DrawingArea::OutlineOval(const wxRect &inBounds,
                              const GraphicsTools::Color &inColor,
                              int inWidth)
{
    if (HasAreaOfZero())
        return;

    CairoContext cr(GetPixmap());

    cairo_save(cr);
    cr.TransformRectToUnitSquare(inBounds, inWidth);
    cairo_arc(cr, 0.5, 0.5, 0.5, 0, 2 * M_PI);
    cairo_restore(cr); // Undo transformation before drawing stroke.

    cr.SetSourceColor(inColor);
    cairo_set_line_width(cr, inWidth);
    cairo_stroke(cr);

    InvalidateRect(inBounds);
}

void DrawingArea::DrawGreyMap(GraphicsTools::Point inPoint,
                              const GraphicsTools::GreyMap *inGreyMap,
                              GraphicsTools::Color inColor)
{
    if (HasAreaOfZero())
        return;

    CairoContext cr(GetPixmap());

    // Wrap a Cairo surface around our greymap.
    unsigned char *data = const_cast<unsigned char *>(inGreyMap->pixels);
    cairo_surface_t *greymap_surface = 
        cairo_image_surface_create_for_data(data, CAIRO_FORMAT_A8,
                                            inGreyMap->width, inGreyMap->height,
                                            inGreyMap->stride);
    if (cairo_surface_status(greymap_surface) != CAIRO_STATUS_SUCCESS)
        gLog.Fatal("halyard.cairo",
                   "Error creating cairo_surface_t in DrawGreyMap");

    cr.SetSourceColor(inColor);
    cairo_translate(cr, inPoint.x, inPoint.y);
    cairo_mask_surface(cr, greymap_surface, 0, 0);

    cairo_surface_destroy(greymap_surface);

	InvalidateRect(wxRect(inPoint.x, inPoint.y,
						  inGreyMap->width, inGreyMap->height));
}

wxSize DrawingArea::MeasureBitmap(const wxBitmap &inBitmap,
                                  double inScaleX, double inScaleY)
{
    return wxSize(ceil(inBitmap.GetWidth() * inScaleX),
                  ceil(inBitmap.GetHeight() * inScaleY));
}

void DrawingArea::DrawBitmap(const wxBitmap &inBitmap,
							 wxCoord inX, wxCoord inY,
                             double inScaleX, double inScaleY)
{
    if (HasAreaOfZero())
        return;

    CairoContext cr(GetPixmap());
    CairoContext src_cr(const_cast<wxBitmap &>(inBitmap));

    cairo_translate(cr, inX, inY);
    cairo_scale(cr, inScaleX, inScaleY);
    cairo_set_source_surface(cr, src_cr.GetSurface(), 0, 0);
    cairo_paint(cr);

    InvalidateRect(wxRect(wxPoint(inX, inY),
                          MeasureBitmap(inBitmap, inScaleX, inScaleY)));
}

void DrawingArea::Mask(const wxBitmap &inMask, wxCoord inX, wxCoord inY)
{
    // If either bitmap has an area of 0, give up immediately.
    if (HasAreaOfZero() || inMask.GetWidth() == 0 || inMask.GetHeight() == 0)
        return;

    CairoContext cr(GetPixmap());
    CairoContext mask_cr(const_cast<wxBitmap &>(inMask));

    // Our transfer semantics are a bit weird: We want to erase our
    // destination surface everywhere that inMask is not opaque.  Only the
    // alpha channel on inMask matters.  This drawing code was arrived at
    // by trial and error.
    cairo_set_source_surface(cr, mask_cr.GetSurface(), inX, inY);
    cairo_set_operator(cr, CAIRO_OPERATOR_DEST_IN);
    cairo_paint(cr);

    InvalidateRect(wxRect(inX, inY, inMask.GetWidth(), inMask.GetHeight()));
}

void DrawingArea::DrawDCContents(wxDC &inDC)
{
    if (HasAreaOfZero())
        return;
    
	wxMemoryDC dc;
	dc.SelectObject(GetPixmap());
	if (!dc.Blit(0, 0, GetPixmap().GetWidth(), GetPixmap().GetHeight(),
				 &inDC, 0, 0))
		Clear(GraphicsTools::Color(0, 0, 0));
	InvalidateRect(wxRect(0, 0,
						  GetPixmap().GetWidth(), GetPixmap().GetHeight()));
}

GraphicsTools::Color DrawingArea::GetPixel(wxCoord inX, wxCoord inY) {
	wxRect local_bounds(0, 0, GetBounds().width, GetBounds().height);
	if (!local_bounds.Contains(wxPoint(inX, inY)) || HasAreaOfZero())
		THROW("Can't get color of point outside of current drawing area");

	GraphicsTools::Color result;
	if (!HasAlpha()) {
		wxMemoryDC dc;
		dc.SelectObjectAsSource(GetPixmap());
		wxColour c;
		dc.GetPixel(inX, inY, &c);
		result.red   = c.Red();
		result.green = c.Green();
		result.blue  = c.Blue();
		result.alpha = 255;
	} else {
		wxAlphaPixelData data(GetPixmap());
		wxAlphaPixelData::Iterator i(data);
		i.Offset(data, inX, inY);
		// Attempt to reverse pre-multiplications.
		result.red   = i.Alpha() ? (i.Red()   * 255) / i.Alpha() : 0;
		result.green = i.Alpha() ? (i.Green() * 255) / i.Alpha() : 0;
		result.blue  = i.Alpha() ? (i.Blue()  * 255) / i.Alpha() : 0;
		result.alpha = i.Alpha();
	}
	return result;
}

void DrawingArea::CompositeInto(wxDC &inDC, const wxRect &inClipRect) {
    if (HasAreaOfZero()) 
        return;

	if (mIsShown && inClipRect.Intersects(mBounds)) {
		// Figure out how much of inClipRect actually applies to us.
		wxRect clip(inClipRect);
		clip.Intersect(mBounds);
		
		// Log this operation so we have some hope of actually being
		// able to debug off-screen compositing.
		wxLogTrace(TRACE_STAGE_DRAWING,
				   wxT("Dirty: %d %d %d %d Bounds: %d %d %d %d Clip: %d %d %d %d"),
				   inClipRect.GetLeft(), inClipRect.GetTop(),
				   inClipRect.GetRight(), inClipRect.GetBottom(),
				   mBounds.GetLeft(), mBounds.GetTop(),
				   mBounds.GetRight(), mBounds.GetBottom(),
				   clip.GetLeft(), clip.GetTop(),
				   clip.GetRight(), clip.GetBottom());

		// Do the compositing.
		wxMemoryDC dc;
		dc.SelectObjectAsSource(GetPixmap());
		wxPoint src_loc(clip.x - mBounds.x, clip.y - mBounds.y);
		if (!inDC.Blit(clip.x, clip.y, clip.width, clip.height,
					   &dc, src_loc.x, src_loc.y))
			gLog.Error("halyard", "Cannot composite offscreen layers.");
	}
}
