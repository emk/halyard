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

#include "CommonHeaders.h"
#include "CrashReporter.h"

USING_NAMESPACE_FIVEL


//=========================================================================
//  CrashReporter Methods
//=========================================================================

CrashReporter *CrashReporter::sInstance = NULL;

CrashReporter::CrashReporter() {
}

/// Turn on crash reporting.  This method won't be called until until
/// it's safe to call CrashReporter::GetInstance().
void CrashReporter::BeginInterceptingCrashes() {
}

/// Register a script document with the CrashReporter.  This is
/// used to get the script debug report URL and the script name.
void CrashReporter::RegisterDocument(Document *inDocument) {
}

/// Register the specified file for inclusion in any crash reports.
/// Typically used for log files, which can be really helpful.
void CrashReporter::AddDiagnosticFile(const std::string &inFileName,
                                      const std::string &inDescription)
{
}

/// Called every time the current card changes.
/// @param inCardName The card name, or an empty string if no card.
void CrashReporter::SetCurrentCard(const std::string &inCardName) {
}

/// Crash immediately, with an optional reason.
///
/// @param inReason NULL for segfaults, an informative string for asserts.
/// @param inType Was this crash caused by the application or a script?
void CrashReporter::CrashNow(const char *inReason, CrashType inType) {
    // This will generate *some* sort of crash under most operating
    // systems.
    // PORTABILITY - Should we print an error message if we have one?
    abort();
}

/// Report an error in the CrashReporter internals.  Unfortunately, we
/// can't ASSERT here (or log), because we're part of the
/// assertion-handling machinery.  D'oh!
void CrashReporter::InternalAssertionFailure() {
    // This will generate *some* sort of crash under most operating
    // systems.
    // PORTABILITY - Should we print an error message?
    abort();
}

/// Initialize the CrashReporter subsystem.
void CrashReporter::InitializeCrashReporting(CrashReporter *inReporter) {
    if (sInstance || !inReporter)
        InternalAssertionFailure();
    sInstance = inReporter;

    // Now that our instance is installed, it's safe to intercept segfaults
    // and other crashes.
    sInstance->BeginInterceptingCrashes();
}

/// Get the current CrashReporter instance.  This will initialize
/// the CrashReporter subsystem if necessary.
CrashReporter *CrashReporter::GetInstance() {
    if (!sInstance)
        InternalAssertionFailure();
    return sInstance;
}

/// Return true if and only if we have a crash reporter.
bool CrashReporter::HaveInstance() {
    return (sInstance != NULL);
}
