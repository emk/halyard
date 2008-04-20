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

#include "AppHeaders.h"

// We need snprintf.
#include <stdio.h>
#ifdef APP_PLATFORM_WIN32
#define snprintf _snprintf
#endif // APP_PLATFORM_WIN32

#include <wx/debugrpt.h>
#include <wx/sstream.h>

#ifdef APP_PLATFORM_WIN32
#include <windows.h>
#include <wx/msw/registry.h>
#endif // APP_PLATFORM_WIN32

#include "TVersion.h"
#include "doc/Document.h"
#include "doc/HalyardProgram.h"
#include "FileSystem.h"
#include "HalyardApp.h"
#include "Stage.h"
#include "FancyCrashReporter.h"

using namespace Halyard;
namespace FS = FileSystem;


//=========================================================================
//  FancyDebugReport
//=========================================================================

/// Subclass the wxDebugReportUpload so we can add extra information.
class FancyDebugReport : public wxDebugReportUpload {
    FancyCrashReporter *mReporter;
    const char *mReason;
    
    void AddAppInfo(wxXmlNode *nodeRoot);
    void GetXmlNodeText(wxString &outText, wxXmlNode *node);

public:
    FancyDebugReport(FancyCrashReporter *inReporter,
                     const char *inReportUrl,
                     const char *inReason);

    Context GetContext() const;
    void AddScreenshot();
    void AddRegistryData();

protected:
    virtual bool DoAddSystemInfo(wxXmlNode *nodeSystemInfo);
    virtual void DoAddCustomContext(wxXmlNode *nodeRoot);
    virtual bool OnServerReply(const wxArrayString& reply);
};

FancyDebugReport::FancyDebugReport(FancyCrashReporter *inReporter,
                                   const char *inReportUrl,
                                   const char *inReason)
    : wxDebugReportUpload(wxString(inReportUrl, wxConvLocal),
                          wxT("report:file"), wxT("action")),
      mReporter(inReporter), mReason(inReason)
{
}

/// If we have an explicit reason for this crash, we won't have an
/// exception context, so start stack traces from inside wxDebugReport.
wxDebugReport::Context FancyDebugReport::GetContext() const {
    return mReason ? Context_Current : Context_Exception;
}

/// Ask the Stage to take a screenshot of whatever would be displayed by
/// the next update.  This probably isn't safe to do after a real
/// crash--the Stage might be broken.
void FancyDebugReport::AddScreenshot() {
    wxFileName path(GetDirectory(), wxT("stage.png"));
    wxGetApp().GetStage()->Screenshot(path.GetFullPath());
    AddFile(path.GetFullName(), wxT("script graphics"));
}

#ifndef APP_PLATFORM_WIN32

/// This function does nothing except on Windows.
/// PORTABILITY - Should we implement a generic version of these functions?
void FancyDebugReport::AddRegistryData() {}
bool DoAddSystemInfo(wxXmlNode *nodeSystemInfo) {
    return wxDebugReportUpload::DoAddSystemInfo(nodeSystemInfo);
}

#else // defined(APP_PLATFORM_WIN32)

/// Dump our registry data and add it to the debug report.
void FancyDebugReport::AddRegistryData() {
    // Build a file name to dump our registry data to.
    wxFileName path(GetDirectory(), "preferences.reg");

    // Dump our registry data.
    wxRegKey reg("HKEY_CURRENT_USER\\Software\\Halyard");
    reg.Export(path.GetFullPath());

    // Add the file to our debug report.
    AddFile(path.GetFullName(), "application preferences");
}

/// Add our processor information, which wxWidgets doesn't provide in
/// a portable fashion.
bool FancyDebugReport::DoAddSystemInfo(wxXmlNode *nodeSystemInfo) {
    wxDebugReportUpload::DoAddSystemInfo(nodeSystemInfo);

    SYSTEM_INFO info;
    ::GetSystemInfo(&info);

    wxString processors, processorLevel, processorRevision;
    processors << info.dwNumberOfProcessors;
    processorLevel << info.wProcessorLevel;
    processorRevision << info.wProcessorRevision;

    wxString architecture;
    switch (info.wProcessorArchitecture) {
        case PROCESSOR_ARCHITECTURE_INTEL: architecture = "Intel"; break;
        case PROCESSOR_ARCHITECTURE_IA64:  architecture = "IA64";  break;
        case PROCESSOR_ARCHITECTURE_AMD64: architecture = "AMD64"; break;
        default: architecture = "unknown";
    }

    nodeSystemInfo->AddProperty("processors", processors);
    nodeSystemInfo->AddProperty("architecture", architecture);
    nodeSystemInfo->AddProperty("processorLevel", processorLevel);
    nodeSystemInfo->AddProperty("processorRevision", processorRevision);
    return true;
}

#endif // defined(APP_PLATFORM_WIN32)

/// Add application-specific information to the debug report.
void FancyDebugReport::AddAppInfo(wxXmlNode *nodeRoot) {
    wxXmlNode *app = new wxXmlNode(wxXML_ELEMENT_NODE, wxT("application"));
    nodeRoot->AddChild(app);
    app->AddProperty(wxT("version"),
                     wxString(VERSION_STRING, wxConvLocal));
    app->AddProperty(wxT("buildDate"),
                     wxString(BUILD_TIMESTAMP, wxConvLocal));
    app->AddProperty(wxT("scriptName"),
                     wxString(mReporter->GetScriptName(), wxConvLocal));
    app->AddProperty(wxT("scriptVersion"),
                     wxString(mReporter->GetScriptVersion(), wxConvLocal));
    app->AddProperty(wxT("currentCard"),
                     wxString(mReporter->GetCurrentCard(), wxConvLocal));
    app->AddProperty(wxT("recentCard"),
                     wxString(mReporter->GetRecentCard(), wxConvLocal));
    
    wxDateTime now(wxDateTime::Now());
    wxString formatted(now.FormatISODate() + wxT(" ") + now.FormatISOTime());
    app->AddProperty(wxT("crashDate"), formatted);
}

/// Add extra information to our XML crash report.
void FancyDebugReport::DoAddCustomContext(wxXmlNode *nodeRoot) {
    wxDebugReportUpload::DoAddCustomContext(nodeRoot);

    // If we have an explicit reason for this crash, add it to the report.
    if (mReason) {
        wxXmlNode *assertion =
            new wxXmlNode(wxXML_ELEMENT_NODE, wxT("assertion"));
        nodeRoot->AddChild(assertion);
        wxString reason(mReason, wxConvLocal);
        assertion->AddProperty(wxT("reason"), reason);
    }

    // Add any other information we have lying around.
    AddAppInfo(nodeRoot);
}

/// Get all the text recursively contained within this node.
void FancyDebugReport::GetXmlNodeText(wxString &outText, wxXmlNode *node) {
    if (node->GetType() == wxXML_TEXT_NODE) {
        outText << node->GetContent();
    } else if (node->GetType() == wxXML_ELEMENT_NODE) {
        wxXmlNode *child = node->GetChildren();
        for (; child != NULL; child = child->GetNext())
            GetXmlNodeText(outText, child);
    }
    // Ignore other types of nodes.
    // XXX - We probably need to parse entity nodes here.
}

/// Display the server's response to the user.
bool FancyDebugReport::OnServerReply(const wxArrayString& reply) {
    // If we didn't get a reply--which is weird--assume the upload failed.
    if (reply.IsEmpty())
        return false;
 
    // Build a string from the server response.
    wxString response;
    for (size_t n = 0; n < reply.GetCount(); n++)
        response << reply[n] << '\n';

    // Parse the response.  If this fails, assume our server is broken.
    wxStringInputStream responseStream(response);
    wxXmlDocument doc(responseStream);
    if (!doc.IsOk())
        return false;
    wxXmlNode *root = doc.GetRoot();
    if (root->GetName() != wxT("crashResponse"))
        return false;

    // Extract the info we need from the response.
    wxString title = wxT("Thank you!");
    wxString description =
        wxT("Your debug report has been submitted. ")
        wxT("Thank you for helping us improve this program!");
    wxString link;
    wxXmlNode *node = root->GetChildren();
    for (; node != NULL; node = node->GetNext()) {
        // Extract the node's text.
        wxString content;
        GetXmlNodeText(content, node);
 
        // Store the node's text in the appropriate variable.
        if (node->GetName() == wxT("title"))
            title = content;
        else if (node->GetName() == wxT("description"))
            description = content;
        else if (node->GetName() == wxT("link"))
            link = content;
        // Ignore unknown nodes.
    }

    // Pop up a simple dialog.
    wxMessageDialog dlg(NULL, description, title, wxOK);
    dlg.ShowModal();

    return true;
}


//=========================================================================
//  FancyCrashReporter Methods
//=========================================================================

void FancyCrashReporter::BeginInterceptingCrashes() {
    // Start passing segaults, etc., to wxApp::OnFatalException().
    ::wxHandleFatalExceptions();
}

void FancyCrashReporter::RegisterDocument(Halyard::Document *inDocument) {
    // Register ourselves as a ModelView of the program object.
    SetObject(inDocument->GetHalyardProgram());

    // Try to register various document-related files with the crash
    // reporter.  We start with our spec file, which (if it exists) contains
    // version information.
    FS::Path spec(FS::GetBaseDirectory().AddComponent("release.spec"));
    if (spec.DoesExist())
        AddDiagnosticFile(spec.ToNativePathString(), "Version information");

    // We then try to find our UpdaterInstaller log, which is helpful for
    // debugging update-related crashes.
    // TODO - This path logic is duplicated in FiveL.cpp and UpdateInstaller.
    FS::Path data_dir(FS::GetScriptLocalDataDirectory());
    FS::Path temp_dir(data_dir.AddComponent("Updates").AddComponent("temp"));
    FS::Path update_log(temp_dir.AddComponent("log"));
    if (update_log.DoesExist())
        AddDiagnosticFile(update_log.ToNativePathString(), "Update log");
}

void FancyCrashReporter::ObjectChanged() {
    // Cache some useful information locally.
    mScriptName = GetObject()->GetString("name");
    mScriptVersion = GetObject()->GetString("version");
    mScriptReportUrl = GetObject()->GetString("dbgreporturl");
}

void FancyCrashReporter::ObjectDeleted() {
    // We don't actually need to clear our cached values; they're still more
    // informative than the empty string.
}

void FancyCrashReporter::AddDiagnosticFile(const std::string &inFileName,
                                           const std::string &inDescription)
{
    // We need to pass an absolute path to wxDebugReport::AddFile, or it
    // will assume that we're referring to a file in the crash report
    // temporary directory.  So make sure our path is absolute.
    wxString filename(inFileName.c_str(), wxConvLocal);
    wxFileName path(filename);
    if (!path.IsAbsolute())
        path.MakeAbsolute();
    wxString description(inDescription.c_str(), wxConvLocal);
    mFileInfo.push_back(FileInfo(path.GetFullPath(), description));
}

void FancyCrashReporter::SetCurrentCard(const std::string &inCardName) {
    if (mCurrentCard != "" && mCurrentCard != mRecentCard)
        mRecentCard = mCurrentCard;    
    mCurrentCard = inCardName;
}

const char *FancyCrashReporter::GetReportUrl(Halyard::CrashType inType) {
    if (inType == SCRIPT_CRASH)
        return GetScriptReportUrl();
    else
        return CRASH_REPORT_URL;
}

/// OK, we only get called if the *crash reporter* has itself crashed.
/// Report this error as safely as we can, without relying on wxWidgets
/// or any of our regular library functions. Yuck.
void FancyCrashReporter::ReportCrashInCrashRepoter(const char *inReason) {
    // Figure out what error message to display.
    const char *message = inReason ? inReason : "The application crashed.";

    // Format a nice, human-readble error message, and make sure it's
    // NULL-terminated.
    char buffer[1024];
    snprintf(buffer, sizeof(buffer), "An error occured while trying "
             "to report a previous error:\n\n  %s\n\n"
             "Please report this if you know how.",
             message);
    buffer[sizeof(buffer)-1] = '\0';

    // Display the error *very carefully* and abort.
    TLogger::SafeAlert(true, buffer);
    ::abort();
}

void FancyCrashReporter::CrashNow(const char *inReason, CrashType inType) {
    // If we're already processing a crash, then something we called died
    // on its own.  So presumably it isn't safe to run our usual code here.
    if (mIsProcessingCrash)
        ReportCrashInCrashRepoter(inReason);
    mIsProcessingCrash = true;

    // If we're handling a script crash, and the script didn't specify
    // a report URL, exit immediately.
    if (inType == SCRIPT_CRASH && mScriptReportUrl == "")
        ::exit(1);

    // Generate our debug report.
    FancyDebugReport report(this, GetReportUrl(inType), inReason);
    report.AddAll(report.GetContext());

    // Open up any logs we haven't opened yet, and as a side effect, write
    // the most recent entries in those logs to disk.  (TLogger maintains
    // an internal history of recent writes to unopened logs.)  We need to
    // do this before checking mFileInfo, below, because it may add new
    // files to our report.
    TLogger::OpenRemainingLogsForCrash();

    // Add any useful files to the report.
    FileInfoVector::iterator i = mFileInfo.begin();
    for (; i != mFileInfo.end(); ++i)
        report.AddFile(i->path, i->description);

    // Add our registry keys.
    report.AddRegistryData();

    // Add a screenshot, but only if we're processing a script crash.
    // (After real crashes, this probably would fail anyway.)
    //
    // Disabled because it adds substantially to the size of the crash
    // report.
    //if (inType == SCRIPT_CRASH)
    //    report.AddScreenshot();

    // Ask the user whether they want to submit this bug report, and if
    // so, process it.
    wxDebugReportPreviewStd preview;
    if (preview.Show(report)) {
        if (!report.Process() && report.GetCompressedFileName() != wxT("")) {
            // OK, processing failed, which generally means we can't talk
            // to our server--but we did at least create a debug report.
            // So allow the user to save the report.
            wxFileDialog dlg(NULL, wxT("Save debug report"), wxT(""),
                             wxT("debugrpt"), wxT("ZIP archive (*.zip)|*.zip"),
                             wxSAVE|wxOVERWRITE_PROMPT);
            if (dlg.ShowModal() == wxID_OK) {
                // wxCopyFile automatically overwrites the destination.
                ::wxCopyFile(report.GetCompressedFileName(), dlg.GetPath());
            }
        }
    }

    // Exit the application with an error result.  We don't want to call
    // abort() here, because that might try to submit a bug to Microsoft!
    ::exit(1);
}
