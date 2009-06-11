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

#ifndef AppLog_H
#define AppLog_H


//////////
/// This is a wxWindows log target which knows how to write messages to
/// the Halyard logging subsystem.
///
class AppLog : public wxLog
{
    bool mShouldSilentlyLogNonFatalErrors;

public:
    AppLog();

    //////////
    /// Call this function to prevent the display of any further wxWidgets
    /// errors to the user.  This is needed when processing crash reports,
    /// because if the wxWidgets crash reporter encounters any non-fatal
    /// errors during the submission process, it will send them to us for
    /// logging, and we don't want them to loop back into the crash
    /// reporter.
    ///
    /// Note that calling this function changes error-handling policy:
    /// Errors which were displayed to the user (and which were potentially
    /// fatal if we were in runtime mode), are just silently logged after
    /// this function is called.
    ///
    void SilentlyLogNonFatalErrors();
    
    virtual void DoLog(wxLogLevel inLevel, const wxChar *inMsg,
                       time_t inTimeStamp);
};

#endif // AppLog_H
