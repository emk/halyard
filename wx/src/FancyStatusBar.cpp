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
#include "FancyStatusBar.h"

//=========================================================================
//  ProgressMeter
//=========================================================================

///  A simple thermometer-style class with a setable color.
class ProgressMeter : public wxWindow
{
	wxColour mColor;
	float mValue;

	void Draw(wxDC &inDC);
	void Refresh();
    void OnPaint(wxPaintEvent &inEvent);

public:
	ProgressMeter(wxWindow *inParent);

	void SetColor(const wxColor &inColor) { mColor = inColor; Refresh(); }
	void SetValue(float inValue) { mValue = inValue; Refresh(); }

    DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ProgressMeter, wxWindow)
    EVT_PAINT(ProgressMeter::OnPaint)
END_EVENT_TABLE()

ProgressMeter::ProgressMeter(wxWindow *inParent)
	: wxWindow(inParent, -1), mValue(0.0)
{
	mColor = FancyStatusBar::DEFAULT_PROGRESS_COLOR;
}

void ProgressMeter::Draw(wxDC &inDC)
{
	wxBrush brush(mColor, wxSOLID);
	inDC.SetBrush(brush);
	inDC.SetPen(*wxTRANSPARENT_PEN);
	wxSize sz = GetSize();
	inDC.DrawRectangle(0, 0, mValue * sz.GetWidth(), sz.GetHeight());
}

void ProgressMeter::Refresh()
{
	wxClientDC dc(this);
	Draw(dc);
}

void ProgressMeter::OnPaint(wxPaintEvent &inEvent)
{
    wxPaintDC dc(this);
	Draw(dc);
}


//=========================================================================
//  FancyStatusBar
//=========================================================================
//  A status bar which contains a progress indicator.

wxColour FancyStatusBar::DEFAULT_PROGRESS_COLOR(0x00, 0x00, 0x80);

BEGIN_EVENT_TABLE(FancyStatusBar, wxStatusBar)
    EVT_SIZE(FancyStatusBar::OnSize)
END_EVENT_TABLE()

enum {
	TEXT_FIELD,
	PROGRESS_FIELD,
	THUMB_FIELD,
	FIELD_COUNT,

	TEXT_FIELD_WIDTH     = -1, // resizable
	PROGRESS_FIELD_WIDTH = 150,
	THUMB_FIELD_WIDTH    = 20
};

FancyStatusBar::FancyStatusBar(wxWindow *inParent)
	: wxStatusBar(inParent, -1)
{
	int field_widths[FIELD_COUNT] =
		{TEXT_FIELD_WIDTH, PROGRESS_FIELD_WIDTH, THUMB_FIELD_WIDTH};
	SetFieldsCount(FIELD_COUNT, field_widths);
	mProgressMeter = new ProgressMeter(this);
}

void FancyStatusBar::OnSize(wxSizeEvent &event)
{
	wxRect rect;
	GetFieldRect(PROGRESS_FIELD, rect);
	rect.Deflate(1, 1);
	mProgressMeter->SetSize(rect);
}

void FancyStatusBar::SetProgressColor(const wxColour &inColor)
{
	mProgressMeter->SetColor(inColor);
}

void FancyStatusBar::SetProgress(float inValue)
{
	mProgressMeter->SetValue(inValue);
}

