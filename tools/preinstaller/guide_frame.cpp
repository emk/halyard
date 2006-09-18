// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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

// This code was originally based on the example code at
// <http://wxwidgets.org/docs/tutorials/hello.htm>.

#include "stdafx.h"
#include "guide_frame.h"
#include "application.h"

#define SHOW_DEBUGGING 1

enum {
    ID_Timer = 1,

    ID_InstallQuickTime = 1,
    ID_InstallApplication
};

BEGIN_EVENT_TABLE(GuideFrame, wxFrame)
    EVT_BUTTON(ID_InstallQuickTime, GuideFrame::OnInstallQuickTime)
    EVT_BUTTON(ID_InstallApplication, GuideFrame::OnInstallApplication)
    EVT_TIMER(ID_Timer, GuideFrame::OnTimer)
END_EVENT_TABLE()

GuideFrame::GuideFrame()
    : wxFrame((wxFrame *)NULL, -1, "Install Program",
              wxDefaultPosition, wxDefaultSize,
              wxCLOSE_BOX|wxSYSTEM_MENU|wxCAPTION|wxRESIZE_BORDER)
{   
    // Set the owner of our mTimer object.
    mTimer.SetOwner(this, ID_Timer);

    // A status bar for debugging purposes.
    if (SHOW_DEBUGGING)
        CreateStatusBar();

    // Create a white window background.  This looks more "installer-like".
    // Make the foreground text black for a clean contrast.
    wxWindow *background = new wxPanel(this, -1, wxDefaultPosition,
                                       wxDefaultSize);
    background->SetBackgroundColour(*wxWHITE);
    background->SetForegroundColour(*wxBLACK);

    // Create a one-column grid for layout.
    wxSizer *sizer = new wxFlexGridSizer(1, 20, 20);

    // Create our QuickTime installer button.
    sizer->Add(new wxStaticText(background, -1, "1. Install QuickTime"));
    mQTButton = new wxButton(background, ID_InstallQuickTime,
                             "Install QuickTime");
    sizer->Add(mQTButton);

    // Create our application installer button.
    wxString app_name(wxGetApp().GetApplicationName());
    sizer->Add(new wxStaticText(background, -1, "2. Install " + app_name));
    mAppButton = new wxButton(background, ID_InstallApplication,
                              "Install " + app_name);
    sizer->Add(mAppButton);
    mAppButton->Disable();

    // Attach the sizer.
    background->SetSizer(sizer);
    sizer->SetSizeHints(background);
    Layout();

    // Position this frame in the center of the screen.
    CenterOnScreen();
}

void GuideFrame::OnTimer(wxTimerEvent& event) {
    if (wxGetApp().HaveAppropriateQuickTimeVersion()) {
        mAppButton->Enable();
        mTimer.Stop();
    }

    if (SHOW_DEBUGGING) {
        wxString str;
        str.Printf("QuickTime version: %08x",
                   wxGetApp().QuickTimeVersion());
        SetStatusText(str);
    }
}

void GuideFrame::OnInstallQuickTime(wxCommandEvent& event) {
    mQTButton->Disable();
    wxGetApp().LaunchQuickTimeInstaller();

    // Send a timer event periodically.  We don't want to do this too
    // often, because checking the QuickTime version is fairly expensive.
    mTimer.Start(3000, wxTIMER_CONTINUOUS);
}

void GuideFrame::OnInstallApplication(wxCommandEvent& event) {
    wxGetApp().LaunchApplicationInstaller();

    // TODO - Disable button and exit program.
}
