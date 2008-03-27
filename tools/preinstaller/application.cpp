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

// This code was originally based on the example code at
// <http://wxwidgets.org/docs/tutorials/hello.htm>.

#include "stdafx.h"
#include "application.h"
#include "guide_frame.h"
#include "CommandLine.h"

IMPLEMENT_APP(Application)

bool Application::OnInit() {
    wxApp::OnInit();

    // If we have a new enough version of QuickTime, bypass the
    // preinstaller entirely.
    QuickTimeInstallStatus qtstatus = GetQuickTimeInstallStatus();
    if (qtstatus == QUICKTIME_OK) {
        LaunchApplicationInstaller();
        return FALSE;
    } else {
        // Otherwise, display our window.  If QuickTime is too old, ask
        // GuideFrame to display a warning dialog about upgrading QuickTime
        // Pro.
        wxFrame *frame = new GuideFrame(qtstatus == QUICKTIME_TOO_OLD);
        frame->Show(TRUE);
        SetTopWindow(frame);
        return TRUE;
    }
} 

/// Do we have a new enough version of QuickTime?  Don't call this function
/// too often; it's fairly slow.
Application::QuickTimeInstallStatus Application::GetQuickTimeInstallStatus() {
    long version = QuickTimeVersion();
    if (version == 0)
        return QUICKTIME_NOT_INSTALLED;
    else if (version < MINIMUM_QUICKTIME_VERSION)
        return QUICKTIME_TOO_OLD;
    else
        return QUICKTIME_OK;
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
    // We spoke to Apple licensing (see bug #3581 for name, e-mail and
    // contact details), and they clarified that it's OK to run the
    // QuickTime installer in passive mode.
    LaunchAsync(GetQuickTimeInstallerName(), "/passive");
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

/// Stupid hack to work around lack of wxStaticText::Wrap in versions of
/// wxWidgets prior to 2.6.2: Hard-code a pre-wrapped weclome message.
wxString Application::GetWelcomeMessage() {
    return WELCOME_MESSAGE;
}

/// Launch an executable with no arguments in the current directory.
void Application::LaunchAsync(const wxString &path, const wxString &flag) {
    std::vector<std::string> args;
    args.push_back(path.mb_str());
    CommandLine cl(args);
    std::string cmd_line(cl.WindowsQuotedString());
    if (flag != "")
        cmd_line += " " + flag; // We don't want to quote this.
    wxExecute(cmd_line.c_str(), wxEXEC_ASYNC);    
}
