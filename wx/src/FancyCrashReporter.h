// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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

#ifndef FancyCrashReporter_H
#define FancyCrashReporter_H

#include "ModelView.h"
#include "CrashReporter.h"

/// Subclass our standard crash reporting class and make it use
/// wxDebugReport.
class FancyCrashReporter : public FIVEL_NS CrashReporter, public model::View {
    struct FileInfo {
        wxString path;
        wxString description;
        FileInfo(const wxString &_path, const wxString &_description)
            : path(_path), description(_description) {}
    };

    typedef std::vector<FileInfo> FileInfoVector;
    FileInfoVector mFileInfo;
    std::string mScriptName;
    std::string mScriptVersion;
    std::string mScriptReportUrl;
    std::string mCurrentCard;
    std::string mRecentCard;
    bool mIsProcessingCrash;

    const char *GetReportUrl(FIVEL_NS CrashType inType);
    void ReportCrashInCrashRepoter(const char *inReason);

public:
    FancyCrashReporter() : mIsProcessingCrash(false) {}
    void BeginInterceptingCrashes();
	void RegisterDocument(FIVEL_NS Document *inDocument);
    void ObjectChanged();
    void ObjectDeleted();
    void AddDiagnosticFile(const std::string &inFileName,
                           const std::string &inDescription);
    void SetCurrentCard(const std::string &inCardName);
    void CrashNow(const char *inReason, FIVEL_NS CrashType inType);

    const char *GetScriptName() const { return mScriptName.c_str(); }
    const char *GetScriptVersion() const  { return mScriptVersion.c_str(); }
    const char *GetScriptReportUrl() const { return mScriptReportUrl.c_str(); }
    const char *GetCurrentCard() const { return mCurrentCard.c_str(); }
    const char *GetRecentCard() const { return mRecentCard.c_str(); }
};

#endif // FancyCrashReporter_H
