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

#ifndef APPLICATION_H
#define APPLICATION_H 1

/// Our main application object.
class Application : public wxApp {
protected:
    virtual bool OnInit();

public:
    enum QuickTimeInstallStatus {
        QUICKTIME_NOT_INSTALLED,
        QUICKTIME_TOO_OLD,
        QUICKTIME_OK
    };

    QuickTimeInstallStatus GetQuickTimeInstallStatus();
    long QuickTimeVersion();

    void LaunchQuickTimeInstaller();
    void LaunchApplicationInstaller();

    wxString GetApplicationName();
    wxString GetQuickTimeInstallerName();
    wxString GetApplicationInstallerName();

    wxString GetWelcomeMessage();

private:
    void LaunchAsync(const wxString &path, const wxString &flag = "");
};

/// Declare a 'wxGetApp()' method.
DECLARE_APP(Application)

// TODO - We should read this from a configuration from a file, but we need
// to get this program working ASAP.  We can generalize it later.
#define APPLICATION_NAME "Virtual Terrorism Response Academy"
#define QUICKTIME_INSTALLER_NAME "QuickTimeInstaller.exe"
#define APPLICATION_INSTALLER_NAME "VTRA Setup.exe"
#define MINIMUM_QUICKTIME_VERSION 0x06508000
#define WELCOME_MESSAGE \
    "Welcome to the Virtual Terrorism\n" \
    "Response Academy Setup Wizard"
#define QUICKTIME_MAJOR_VERSION "7"

#endif // !defined(APPLICATION_H)
