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

#include "TamaleHeaders.h"
#include "DrawingArea.h"
#include "DrawingAreaOpt.h"
#include "Stage.h"
#include "CommonWxConv.h"
#include "Quake2Engine.h"

USING_NAMESPACE_FIVEL

DrawingArea::DrawingArea(Stage *inStage, int inWidth, int inHeight,
						 bool inHasAlpha)
	: mStage(inStage), mBounds(wxPoint(0, 0), wxSize(inWidth, inHeight)),
      mIsShown(true)
{
	InitializePixmap(inHasAlpha);
}

DrawingArea::DrawingArea(Stage *inStage, const wxRect &inBounds,
						 bool inHasAlpha)
	: mStage(inStage), mBounds(inBounds), mIsShown(true)
{
	InitializePixmap(inHasAlpha);
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

void DrawingArea::InitializePixmap(bool inHasAlpha) {
	mPixmap.Create(mBounds.GetWidth(), mBounds.GetHeight(),
				   inHasAlpha ? 32 : 24);
	if (inHasAlpha)
		mPixmap.UseAlpha();
	Clear();

    if (Quake2Engine::HaveInstance())
        InitializeQuake2Overlay();
}

void DrawingArea::InitializeQuake2Overlay()
{
    // If the pixmap has area zero, then we don't have any valid data to
    // pass to Quake anyway.
    if (HasAreaOfZero())
        return;

    int format;
    unsigned char *data;
    int stride;
    if (mPixmap.HasAlpha()) {
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
        if (inHasPixmapChanged && mQuake2Overlay)
            mQuake2Overlay->DirtyRect(r);
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
    if (mQuake2Overlay)
        mQuake2Overlay = shared_ptr<wxQuake2Overlay>();

    // Invalidate the rectangle covered by our original size.
    InvalidateDrawingArea(false);
    
    // Update our size.
    mBounds = wxRect(mBounds.GetPosition(), inSize);

    // Allocate a new, empty pixmap.  This will invalidate the rectangle
    // covered by the new size, and reallocate our Quake 2 overlay if we're
    // supposed to have one.
    bool alpha = mPixmap.HasAlpha();
    mPixmap = wxBitmap();
    InitializePixmap(alpha);
}

void DrawingArea::Show(bool inShow) {
    if (inShow != mIsShown) {
        mIsShown = inShow;
        if (mQuake2Overlay)
            mQuake2Overlay->Show(inShow);
        InvalidateDrawingArea(false);
    }
}

void DrawingArea::MoveTo(const wxPoint &inPoint) {
    InvalidateDrawingArea(false);
    mBounds = wxRect(inPoint, mBounds.GetSize());
    if (mQuake2Overlay)
        mQuake2Overlay->MoveTo(inPoint);
    InvalidateDrawingArea(false);
}

void DrawingArea::Clear() {
	if (mPixmap.HasAlpha()) {
		Clear(GraphicsTools::Color(0x00, 0x00, 0x00, 0x00));
	} else {
		Clear(GraphicsTools::Color(0x00, 0x00, 0x00));
	}
}

void DrawingArea::Clear(const GraphicsTools::Color &inColor) {
	if (HasAreaOfZero()) {
        // Do nothing.
    } else if (mPixmap.HasAlpha()) {
		wxAlphaPixelData data(mPixmap);
		ClearOpt(data, inColor);
	} else if (inColor.IsCompletelyOpaque()) {
		wxMemoryDC dc;
		dc.SelectObject(GetPixmap());
		wxBrush brush(wxColor(inColor.red, inColor.green, inColor.blue),
					  wxSOLID);
		dc.SetBackground(brush);
		dc.Clear();
	} else {
		THROW("Cannot clear opaque overlay with transparent color.");
	}
    InvalidateDrawingArea();
}

void DrawingArea::DrawLine(const wxPoint &inFrom, const wxPoint &inTo,
						   const GraphicsTools::Color &inColor, int inWidth)
{
    // We special-case straight line drawing, and always use hand-rolled
    // primitives, because nobody's drawing API is really consistent about
    // where straight lines should go, and we'd like to make some reasonable
    // guarantees.
    bool is_straight = false;
    wxRect bounds;
    if (inFrom.x == inTo.x) {
        is_straight = true;
        bounds = wxRect(wxPoint(inFrom.x, inFrom.y),
                        wxPoint(inTo.x + inWidth - 1, inTo.y - 1));
    } else if (inFrom.y == inTo.y) {
        is_straight = true;
        bounds = wxRect(wxPoint(inFrom.x, inFrom.y),
                        wxPoint(inTo.x - 1, inTo.y + inWidth - 1));
    }

    // Do the actual drawing.
	if (HasAreaOfZero()) {
        // Do nothing.
    } else if (mPixmap.HasAlpha()) {
        if (is_straight) {       
            wxAlphaPixelData data(mPixmap);
            FillBoxOpt(data, bounds, inColor);
        } else {
            gLog.Error("Can't draw diagonal lines on transparent overlay");
        }
    } else {
        if (is_straight) {
            wxNativePixelData data(mPixmap);
            FillBoxOpt(data, bounds, inColor);
        } else if (inColor.IsCompletelyOpaque()) {
            wxColor color = GraphicsToolsToWxColor(inColor);
            wxMemoryDC dc;
            dc.SelectObject(GetPixmap());
            wxPen pen(color, inWidth, wxSOLID);
            dc.SetPen(pen);
            dc.DrawLine(inFrom.x, inFrom.y, inTo.x, inTo.y);
        } else {
            gLog.Error("Can't draw diagonal transparent lines");
        }
    }

    // This is slightly annoying, but should handle the maximum bounds of
    // Windows GDI lines.
    InvalidateRect(wxRect(wxPoint(std::min(inFrom.x, inTo.x),
                                  std::min(inFrom.y, inTo.y)),
                          wxPoint(std::max(inFrom.x, inTo.x),
                                  std::max(inFrom.y, inTo.y))),
                   inWidth);
}

void DrawingArea::FillBox(const wxRect &inBounds,
						  const GraphicsTools::Color &inColor)
{
    if (HasAreaOfZero()) {
        // Do nothing.
    } else if (mPixmap.HasAlpha()) {
		wxAlphaPixelData data(mPixmap);
		FillBoxOpt(data, inBounds, inColor);
	} else if (inColor.IsCompletelyOpaque()) {
		wxColor color = GraphicsToolsToWxColor(inColor);
		wxMemoryDC dc;
		dc.SelectObject(GetPixmap());
		wxBrush brush(color, wxSOLID);
		dc.SetBrush(brush);
		dc.SetPen(*wxTRANSPARENT_PEN);
		dc.DrawRectangle(inBounds.x, inBounds.y,
						 inBounds.width, inBounds.height);
	} else {
		wxNativePixelData data(mPixmap);
		FillBoxOpt(data, inBounds, inColor);
	}
	InvalidateRect(inBounds);
}

void DrawingArea::OutlineBox(const wxRect &inBounds,
                             const GraphicsTools::Color &inColor,
							 int inWidth)
{
    // Do this using box drawing primitives for reliable placement and
    // alpha channel support.
    FillBox(wxRect(inBounds.x, inBounds.y, inBounds.width, inWidth), inColor);
    FillBox(wxRect(inBounds.x, inBounds.y + inWidth,
                   inWidth, inBounds.height - 2*inWidth), inColor);
    FillBox(wxRect(inBounds.x, inBounds.y + inBounds.height - inWidth,
                   inBounds.width, inWidth), inColor);
    FillBox(wxRect(inBounds.x + inBounds.width - inWidth, inBounds.y + inWidth,
                   inWidth, inBounds.height - 2*inWidth), inColor);
}

void DrawingArea::DrawPixMap(GraphicsTools::Point inPoint,
							 GraphicsTools::PixMap &inPixMap)
{
    if (HasAreaOfZero()) {
        // Do nothing.
    } else if (mPixmap.HasAlpha()) {
		wxAlphaPixelData data(mPixmap);
		DrawPixMapOpt(data, inPoint, inPixMap);
	} else {
		wxNativePixelData data(mPixmap);
		DrawPixMapOpt(data, inPoint, inPixMap);		
	}
	InvalidateRect(wxRect(inPoint.x, inPoint.y,
						  inPixMap.width, inPixMap.height));
}

void DrawingArea::DrawBitmap(const wxBitmap &inBitmap,
							 wxCoord inX, wxCoord inY,
							 bool inTransparent)
{
	wxMemoryDC dc;
	dc.SelectObject(GetPixmap());
	dc.DrawBitmap(inBitmap, inX, inY, inTransparent);
    InvalidateRect(wxRect(inX, inY,
                          inBitmap.GetWidth(),
                          inBitmap.GetHeight()));
}

void DrawingArea::Mask(const wxBitmap &inMask, wxCoord inX, wxCoord inY)
{
    if (HasAreaOfZero() ||
        inMask.GetWidth() == 0 || inMask.GetHeight() == 0)
    {
        // One or both bitmaps has an area of 0, so do nothing.
    } else if (mPixmap.HasAlpha() && inMask.HasAlpha()) {
        // Both bitmaps have alpha channels, so call our helper routine.
        // It's safe to cast away const, because the mask won't be
        // modified.
		wxAlphaPixelData data(mPixmap);
		wxAlphaPixelData mask(const_cast<wxBitmap &>(inMask));
		MaskOpt(data, mask, inX, inY);
    } else {
        THROW("Mask graphic and drawing area must both have alpha channels");
    }
    InvalidateRect(wxRect(inX, inY, inMask.GetWidth(), inMask.GetHeight()));
}

void DrawingArea::DrawDCContents(wxDC &inDC)
{
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
	if (!local_bounds.Inside(wxPoint(inX, inY)))
		THROW("Can't get color of point outside of current drawing area");

	GraphicsTools::Color result;
	if (!HasAlpha()) {
		wxMemoryDC dc;
		dc.SelectObject(GetPixmap());
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
	if (mIsShown && inClipRect.Intersects(mBounds)) {
		// Figure out how much of inClipRect actually applies to us.
		wxRect clip(inClipRect);
		clip.Intersect(mBounds);
		
		// Log this operation so we have some hope of actually being
		// able to debug off-screen compositing.
		wxLogTrace(TRACE_STAGE_DRAWING,
				   "Dirty: %d %d %d %d Bounds: %d %d %d %d Clip: %d %d %d %d",
				   inClipRect.GetLeft(), inClipRect.GetTop(),
				   inClipRect.GetRight(), inClipRect.GetBottom(),
				   mBounds.GetLeft(), mBounds.GetTop(),
				   mBounds.GetRight(), mBounds.GetBottom(),
				   clip.GetLeft(), clip.GetTop(),
				   clip.GetRight(), clip.GetBottom());

		// Do the compositing.
		wxMemoryDC dc;
		dc.SelectObject(GetPixmap());
		wxPoint src_loc(clip.x - mBounds.x, clip.y - mBounds.y);
		if (!inDC.Blit(clip.x, clip.y, clip.width, clip.height,
					   &dc, src_loc.x, src_loc.y))
			gLog.Error("Cannot composite offscreen layers.");
	}
}
