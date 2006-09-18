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

#ifndef APPLICATION_H
#define APPLICATION_H 1

/// Our main application object.
class Application : public wxApp {
protected:
    virtual bool OnInit();

public:
    bool HaveAppropriateQuickTimeVersion();
    long QuickTimeVersion();

    void LaunchQuickTimeInstaller();
    void LaunchApplicationInstaller();

    wxString GetApplicationName();
    wxString GetQuickTimeInstallerName();
    wxString GetApplicationInstallerName();

    wxString GetWelcomeMessage();

private:
    void LaunchAsync(const wxString &path);
};

/// Declare a 'wxGetApp()' method.
DECLARE_APP(Application)

#endif // !defined(APPLICATION_H)
