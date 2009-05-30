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

#include "Typography.h"
#include "DrawingArea.h"
#include "CairoDrawing.h"
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
	InitializeSurface();
}

DrawingArea::DrawingArea(Stage *inStage, const wxRect &inBounds,
						 bool inHasAlpha)
	: mStage(inStage), mBounds(inBounds), mIsShown(true), mHasAlpha(inHasAlpha)
{
	InitializeSurface();
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
    // TODO - Do we still need this for cairo_image_surface?
	return (mBounds.GetWidth() == 0 || mBounds.GetHeight() == 0);
}

void DrawingArea::InitializeSurface() {
    if (HasAreaOfZero())
        return;

    // Create a new Cairo surface with the appropriate size and bit depth.
    // Note that we need to use cairo_image_surface (and not one of the
    // various platform-specific accelerated surfaces) because we need to
    // be able to get at the raw data in MaybeInitializeGameOverlay and in
    // GetPixel.
    cairo_format_t format =
        HasAlpha() ? CAIRO_FORMAT_ARGB32 : CAIRO_FORMAT_RGB24;
    mSurface = CairoSurfacePtr(cairo_image_surface_create(format, 
                                                          mBounds.GetWidth(),
                                                          mBounds.GetHeight()));

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

    // PORTABILITY - Determine what format our surface is in.  Note that
    // our cairo_image_surface is stored using native endianness.
    ASSERT(wxBYTE_ORDER == wxLITTLE_ENDIAN);
    int format = mHasAlpha ? Q2_FORMAT_BGRA_PREMUL : Q2_FORMAT_BGRX;

    unsigned char *data = cairo_image_surface_get_data(mSurface.get());
    int stride = cairo_image_surface_get_stride(mSurface.get());
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
                                 bool inHasSurfaceChanged)
{
    // TODO - We can probably get away with less invalidation if this
    // DrawingArea isn't shown, but keep a careful eye on the Quake
    // overlays.
	wxRect r(inRect);
	r.Inflate(inInflate);
    r.Intersect(wxRect(wxPoint(0, 0),
                       mBounds.GetSize()));
	if (!r.IsEmpty()) {
        if (inHasSurfaceChanged)
            GameOverlayDirtyRect(r);
	    r.Offset(mBounds.GetPosition());
	    mStage->InvalidateRect(r);
	}
}

void DrawingArea::InvalidateDrawingArea(bool inHasSurfaceChanged) {
    InvalidateRect(wxRect(0, 0, mBounds.GetWidth(), mBounds.GetHeight()),
                   0, inHasSurfaceChanged);
}

void DrawingArea::SetSize(const wxSize &inSize) {
    // If we have a Quake 2 overlay, get rid of it.
    GameOverlayDelete();

    // Invalidate the rectangle covered by our original size.
    InvalidateDrawingArea(false);
    
    // Update our size.
    mBounds = wxRect(mBounds.GetPosition(), inSize);

    // Allocate a new, empty surface.  This will invalidate the rectangle
    // covered by the new size, and reallocate our Quake 2 overlay if we're
    // supposed to have one.
    mSurface = CairoSurfacePtr();
    InitializeSurface();
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
    
    CairoContext cr(mSurface);
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
    
    CairoContext cr(mSurface);

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
    
    CairoContext cr(mSurface);
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

    CairoContext cr(mSurface);

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
    
    CairoContext cr(mSurface);
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

    CairoContext cr(mSurface);

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

    CairoContext cr(mSurface);

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

    CairoContext cr(mSurface);
    CairoBitmapContext src_cr(const_cast<wxBitmap &>(inBitmap));

    cairo_translate(cr, inX, inY);
    cairo_scale(cr, inScaleX, inScaleY);
    cairo_set_source_surface(cr, src_cr.GetSurface().get(), 0, 0);
    cairo_paint(cr);

    InvalidateRect(wxRect(wxPoint(inX, inY),
                          MeasureBitmap(inBitmap, inScaleX, inScaleY)));
}

void DrawingArea::Mask(const wxBitmap &inMask, wxCoord inX, wxCoord inY)
{
    // If either bitmap has an area of 0, give up immediately.
    if (HasAreaOfZero() || inMask.GetWidth() == 0 || inMask.GetHeight() == 0)
        return;

    CairoContext cr(mSurface);
    CairoBitmapContext mask_cr(const_cast<wxBitmap &>(inMask));

    // Our transfer semantics are a bit weird: We want to erase our
    // destination surface everywhere that inMask is not opaque.  Only the
    // alpha channel on inMask matters.  This drawing code was arrived at
    // by trial and error.
    cairo_set_source_surface(cr, mask_cr.GetSurface().get(), inX, inY);
    cairo_set_operator(cr, CAIRO_OPERATOR_DEST_IN);
    cairo_paint(cr);

    InvalidateRect(wxRect(inX, inY, inMask.GetWidth(), inMask.GetHeight()));
}

void DrawingArea::DrawDCContents(wxDC &inDC)
{
    if (HasAreaOfZero())
        return;
    
    CairoContext cr(mSurface);
    CairoSurfacePtr src(CairoSurfacePtr::FromDC(inDC));
    cairo_set_source_surface(cr, src.get(), 0, 0);
    cairo_paint(cr);
    // TODO - If the blit fails for some reason, call:
    //Clear(GraphicsTools::Color(0, 0, 0));

	InvalidateDrawingArea();
}

void DrawingArea::DrawSimpleText(GraphicsTools::Point inAt, wxString inText,
                                 GraphicsTools::Color inColor)
{
    // Choose a text style to use.
    Typography::Style style("Times", 12);
    style.SetShadowOffset(0);
    style.SetColor(inColor);

    // Build a styled string.
	Typography::StyledText text(style);
    text.AppendText(inText.wc_str());
    text.EndConstruction();

    // Render the text.
    Typography::TextRenderingEngine engine(text, inAt,
                                           mBounds.GetWidth() - inAt.x,
                                           Typography::kLeftJustification,
                                           this);
	engine.RenderText();
}

GraphicsTools::Color DrawingArea::GetPixel(wxCoord inX, wxCoord inY) {
	wxRect local_bounds(0, 0, GetBounds().width, GetBounds().height);
	if (!local_bounds.Contains(wxPoint(inX, inY)) || HasAreaOfZero())
		THROW("Can't get color of point outside of current drawing area");

    // Recover the actual pixel data.  Note that the pixel is stored using
    // native endianness, so we need to treat it as a uint32 and extract
    // the channels using arithmetic operators.
    unsigned char *data = cairo_image_surface_get_data(mSurface.get());
    int stride = cairo_image_surface_get_stride(mSurface.get());
    uint32 pixel = *reinterpret_cast<uint32 *>(data + stride*inY +
                                               4*sizeof(unsigned char)*inX);
    unsigned char alpha = (pixel >> 24) & 0xFF;
    unsigned char red   = (pixel >> 16) & 0xFF;
    unsigned char green = (pixel >>  8) & 0xFF;
    unsigned char blue  = (pixel >>  0) & 0xFF;
    
	if (!HasAlpha()) {
        return GraphicsTools::Color(red, green, blue);
	} else {
		// Attempt to reverse pre-multiplications, avoiding division by 0.
        if (alpha)
            return GraphicsTools::Color((red   * 255) / alpha,
                                        (green * 255) / alpha,
                                        (blue  * 255) / alpha,
                                        alpha);
        else
            return GraphicsTools::Color(0, 0, 0, 0);
	}
}

void DrawingArea::CompositeInto(CairoContext &inCr) {
    if (HasAreaOfZero() || !mIsShown)
        return;

    // Draw the contents of our offscreen pixmap.
    cairo_set_source_surface(inCr, mSurface.get(), mBounds.x, mBounds.y);
    cairo_paint(inCr);
}
