// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"
#include "DrawingArea.h"
#include "Stage.h"
#include "CommonWxConv.h"

void DrawingArea::InvalidateStage() {
    mStage->InvalidateStage();
}

void DrawingArea::InvalidateRect(const wxRect &inRect) {
	mStage->InvalidateRect(inRect);
}

void DrawingArea::Clear(const wxColor &inColor) {
    wxMemoryDC dc;
    dc.SelectObject(GetPixmap());
    wxBrush brush(inColor, wxSOLID);
    dc.SetBackground(brush);
    dc.Clear();
    InvalidateStage();
}

void DrawingArea::DrawLine(const wxPoint &inFrom, const wxPoint &inTo,
						   const wxColour &inColor, int inWidth)
{
    wxMemoryDC dc;
    dc.SelectObject(GetPixmap());
	wxPen pen(inColor, inWidth, wxSOLID);
	dc.SetPen(pen);
	dc.DrawLine(inFrom.x, inFrom.y, inTo.x, inTo.y);
	InvalidateRect(wxRect(inFrom, inTo));
}

void DrawingArea::FillBox(const wxRect &inBounds, 
						  const GraphicsTools::Color &inColor)
{
	if (inColor.alpha == 0x00)
	{
		wxColor color = GraphicsToolsToWxColor(inColor);
		wxMemoryDC dc;
		dc.SelectObject(GetPixmap());
		wxBrush brush(color, wxSOLID);
		dc.SetBrush(brush);
		dc.SetPen(*wxTRANSPARENT_PEN);
		dc.DrawRectangle(inBounds.x, inBounds.y,
						 inBounds.width, inBounds.height);
		InvalidateRect(inBounds);
	} 
	else
	{
		FillBoxAlpha(inBounds, inColor);
	}
}

void DrawingArea::OutlineBox(const wxRect &inBounds, const wxColour &inColor,
							 int inWidth)
{
    wxMemoryDC dc;
    dc.SelectObject(GetPixmap());
	wxPen pen(inColor, inWidth, wxSOLID);
	dc.SetPen(pen);
	dc.SetBrush(*wxTRANSPARENT_BRUSH);
	dc.DrawRectangle(inBounds.x, inBounds.y, inBounds.width, inBounds.height);
	InvalidateRect(inBounds);
}

void DrawingArea::DrawBitmap(const wxBitmap &inBitmap,
							 wxCoord inX, wxCoord inY,
							 bool inTransparent)
{
    wxMemoryDC dc;
    dc.SelectObject(GetPixmap());
    dc.DrawBitmap(inBitmap, inX, inY, inTransparent);
    InvalidateRect(wxRect(inX, inY,
                          inX + inBitmap.GetWidth(),
                          inY + inBitmap.GetHeight()));
}

void DrawingArea::DrawDCContents(wxDC &inDC)
{
    wxMemoryDC dc;
    dc.SelectObject(GetPixmap());
	if (!dc.Blit(0, 0, GetPixmap().GetWidth(), GetPixmap().GetHeight(),
				 &inDC, 0, 0))
	{
		Clear(*wxBLACK);
	}
}
