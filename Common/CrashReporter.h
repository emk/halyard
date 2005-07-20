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

#ifndef CrashReporter_H
#define CrashReporter_H

BEGIN_NAMESPACE_FIVEL

class Document;

/// Handles automatic crash reporting, if available on a given platform.
/// Applications will typically subclass this class to provide more
/// sophisticated crash reporting--by default, we just call abort().
class CrashReporter {
    static CrashReporter *sInstance;

protected:
    static void InternalAssertionFailure();

public:
    CrashReporter();
    virtual ~CrashReporter() {}

    virtual void BeginInterceptingCrashes();
	virtual void RegisterDocument(Document *inDocument);
    virtual void AddDiagnosticFile(const std::string &inFileName,
                                   const std::string &inDescription);
    virtual void SetCurrentCard(const std::string &inCardName);
    virtual void CrashNow(const char *inReason = NULL,
                          CrashType inType = APPLICATION_CRASH);

    static void InitializeCrashReporting(CrashReporter *inReporter);
    static CrashReporter *GetInstance();
    static bool HaveInstance();
};

END_NAMESPACE_FIVEL

#endif // CrashReporter_H
