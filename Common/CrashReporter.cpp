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
//  Portable CrashReporterImpl
//=========================================================================

#if !FIVEL_PLATFORM_WIN32

/// This is a useless, portable stub implementation.  It knows how to call
/// abort(), and that's it.
class CrashReporterImpl : public CrashReporter {
public:
    CrashReporterImpl() {}
};

#endif // !FIVEL_PLATFORM_WIN32

//=========================================================================
//  Win32 CrashReporterImpl
//=========================================================================

#if FIVEL_PLATFORM_WIN32

#include <CrashRpt.h>

/// A pretty good Win32 CrashReporter implementation based on an
/// open source crash reporting library.
class CrashReporterImpl : public CrashReporter {
public:
    CrashReporterImpl();
    virtual ~CrashReporterImpl();

    virtual void AddDiagnosticFile(const std::string &inFileName,
                                   const std::string &inDescription);
    virtual void CrashNow(const char *inReason);

private:
    _se_translator_function mOldTranslator;
    static void structured_exception_handler(unsigned int code,
                                             struct _EXCEPTION_POINTERS* info);

};

CrashReporterImpl::CrashReporterImpl() {
    ::Install(NULL, "tamale-bug@iml.dartmouth.edu", "Tamale: Crash Report");
    ::AddRegistryHive("HKEY_CURRENT_USER\\Software\\Tamale",
                      "Registry data for Tamale");

    // Let's see if we can keep segfaults from getting converted into
    // C++ exceptions, which completely prevents us from doing anything
    // useful.  This is a tricky technique described by the boost project:
    //
    //   http://www.boost.org/more/error_handling.html
    //
    // Basically, our compiler runtime has already installed an
    // _se_translator_function that maps "structured exceptions" (e.g.,
    // segfaults) into C++ exceptions, which means that any 'catch (...)'
    // eats segfaults silently, which is *really* bad, because the crash
    // reporter will never see them.  (We're probably OK disabling this
    // warning, which wants us to use /EHa, because we never allow the
    // exception to unwind the stack.)
    //
    // If you're running into problems with 'catch (...)' on other
    // platforms, we probably need to replace those bits of code with
    // 'catch (std::exception &)' as described in the article.
#pragma warning (disable: 4535)
    mOldTranslator = ::_set_se_translator(&structured_exception_handler);
}

CrashReporterImpl::~CrashReporterImpl() {
    ::_set_se_translator(mOldTranslator);
    ::Uninstall();
}

void CrashReporterImpl::AddDiagnosticFile(const std::string &inFileName,
                                          const std::string &inDescription)
{
    ::AddFile(inFileName.c_str(), inDescription.c_str());
}

void CrashReporterImpl::CrashNow(const char *inReason) {
    // We need to convert inReason to Unicode, for reasons unknown.
    // We really should use ::MultiByteToWideString, but I'm not sure
    // that's available on all of our target systems.  So we'll just
    // assume that our code page is roughly equivalent to the first 128
    // positions of Unicode.
    size_t len = strlen(inReason);
    BSTR str = ::SysAllocStringLen(NULL, len);
    for (size_t i = 0; i < len; i++)
        // Promote directly to wchar_t.
        str[i] = inReason[i];
	std::cerr << "Calling GenerateErrorReportEx\n" << std::flush;
    ::GenerateErrorReportEx(::GetInstance(), NULL, str);
    ::SysFreeString(str);

    // We've already sent the crash report.  There's no real reason
    // to call abort()--it would just show Yet Another Dialog to
    // the user--so just exit.
    exit(1);
}

void CrashReporterImpl::structured_exception_handler(
    unsigned int code,
    struct _EXCEPTION_POINTERS* info)
{
    // We could just rethrow the current exception, if we simply want to
    // make structured exception handling work normally (instead of the
    // broken MSVC++ runtime way).  Unfortunately, although this approach
    // will pop up our error report, it also causes the exception to occur
    // in the normal fashion *after* the crash (probably this is a
    // delibrate design decision of the CrashRpt library, but I'm too lazy
    // to investigate).
    //
    //throw;

    // This approach seems to work very nicely. 
    ::GenerateErrorReportEx(::GetInstance(), info, NULL);
    exit(1);
}

#endif // FIVEL_PLATFORM_WIN32


//=========================================================================
//  CrashReporter Methods
//=========================================================================

CrashReporter *CrashReporter::sInstance = NULL;

CrashReporter::CrashReporter() {
}

/// Register the specified file for inclusion in any crash reports.
/// Typically used for log files, which can be really helpful.
void CrashReporter::AddDiagnosticFile(const std::string &inFileName,
                                      const std::string &inDescription)
{
}

/// Crash immediately, with the specified reason.  Typically used
/// to implement assertion failures.
void CrashReporter::CrashNow(const char *inReason) {
    abort();
}

/// Initialize the CrashReporter subsystem.
void CrashReporter::InitializeCrashReporting() {
    if (!sInstance)
        sInstance = new CrashReporterImpl();
}

/// Get the current CrashReporter instance.  This will initialize
/// the CrashReporter subsystem if necessary.
CrashReporter *CrashReporter::GetInstance() {
    if (!sInstance)
        InitializeCrashReporting();
    if (!sInstance)
        // We can't ASSERT here, because we're part of the
        // assertion-handling machinery.  D'oh!
        abort();
    return sInstance;
}
