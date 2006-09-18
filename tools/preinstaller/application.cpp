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
#include "application.h"
#include "guide_frame.h"
#include "CommandLine.h"

IMPLEMENT_APP(Application)

// TODO - We should read this from a configuration from a file, but we need
// to get this program working ASAP.  We can generalize it later.
#define APPLICATION_NAME "Virtual Terrorism Response Academy"
#define QUICKTIME_INSTALLER_NAME "QuickTimeInstaller.exe"
#define APPLICATION_INSTALLER_NAME "VTRA Setup.exe"
#define MINIMUM_QUICKTIME_VERSION 0x06508000

bool Application::OnInit() {
    wxApp::OnInit();

    // If we have a new enough version of QuickTime, bypass the
    // preinstaller entirely.
    if (HaveAppropriateQuickTimeVersion()) {
        LaunchApplicationInstaller();
        return FALSE;
    } else {
        // Otherwise, display our window.
        wxFrame *frame = new GuideFrame();
        frame->Show(TRUE);
        SetTopWindow(frame);
        return TRUE;
    }
} 

/// Do we have a new enough version of QuickTime?  Don't call this function
/// too often; it's fairly slow.
bool Application::HaveAppropriateQuickTimeVersion() {
    return QuickTimeVersion() >= MINIMUM_QUICKTIME_VERSION;
}

/// Get the installed version of QuickTime.  This function may take
/// a third of a second or so to run, so don't call it too often.
long Application::QuickTimeVersion() {
    long version;
    OSErr result;

    // Set up our Macintosh Toolbox glue code.  This is the function
    // that actually checks whether QuickTime is installed, so we can't
    // just call this once at application startup.
    ::InitializeQTML(0);

    // Call the Gestalt manager and ask for QuickTime.  If we're lucky,
    // this should work even in the absence of the QuickTime DLL.
    result = ::Gestalt(gestaltQuickTime, &version);

    // Shut down our Macintosh Toolbox glue code.
    ::TerminateQTML();

    return (result == noErr) ? version : 0;
}

void Application::LaunchQuickTimeInstaller() {
    LaunchAsync(GetQuickTimeInstallerName());
}

void Application::LaunchApplicationInstaller() {
    LaunchAsync(GetApplicationInstallerName());
}

wxString Application::GetApplicationName() {
    return APPLICATION_NAME;
}

wxString Application::GetQuickTimeInstallerName() {
    return QUICKTIME_INSTALLER_NAME;
}

wxString Application::GetApplicationInstallerName() {
    return APPLICATION_INSTALLER_NAME;
}

/// Launch an executable with no arguments in the current directory.
void Application::LaunchAsync(const wxString &path) {
    std::vector<std::string> args;
    args.push_back(path.mb_str());
    CommandLine cl(args);
    wxExecute(cl.WindowsQuotedString().c_str(), wxEXEC_ASYNC);    
}
