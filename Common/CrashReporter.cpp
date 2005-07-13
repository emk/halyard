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

/// Register the specified file for inclusion in any crash reports.
/// Typically used for log files, which can be really helpful.
void CrashReporter::AddDiagnosticFile(const std::string &inFileName,
                                      const std::string &inDescription)
{
}

/// Crash immediately, with the specified reason.  Typically used
/// to implement assertion failures.
///
/// @param inReason NULL for segfaults, an informative string for asserts.
void CrashReporter::CrashNow(const char *inReason) {
    // This will generate *some* sort of crash under most operating
    // systems.
    abort();
}

/// Report an error in the CrashReporter internals.  Unfortunately, we
/// can't ASSERT here (or log), because we're part of the
/// assertion-handling machinery.  D'oh!
void CrashReporter::InternalAssertionFailure() {
    // This will generate *some* sort of crash under most operating
    // systems.
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
