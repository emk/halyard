// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
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
#include "StageFrame.h"
#include "MediaInfoPane.h"

using namespace Halyard;

enum { TEXT_INSET = 4 };

BEGIN_EVENT_TABLE(MediaInfoPane, wxPanel)
    EVT_SIZE(MediaInfoPane::OnSize)
END_EVENT_TABLE()

MediaInfoPane::MediaInfoPane(StageFrame *inStageFrame)
    : wxPanel(inStageFrame, wxID_ANY, wxDefaultPosition, wxSize(0, 0))
{
    mStaticText =
        new wxStaticText(this, wxID_ANY, wxT(""),
                         wxPoint(TEXT_INSET, TEXT_INSET),
                         wxDefaultSize, wxST_NO_AUTORESIZE);

    // Force an update to our displayed text.
    mHasText = true;
    ClearText();
}

void MediaInfoPane::ClearText() {
    if (mHasText) {
        mHasText = false;
        mStaticText->SetLabel(wxT("Not waiting on media."));
    }
}

void MediaInfoPane::SetText(const wxString &inText) {
    mHasText = true;
    mStaticText->SetLabel(inText);
}

void MediaInfoPane::OnSize(wxSizeEvent &inEvent) {
    mStaticText->SetSize(GetSize() - wxSize(2*TEXT_INSET, 2*TEXT_INSET));
}
