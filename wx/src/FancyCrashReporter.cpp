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

#include "TamaleHeaders.h"

#include <wx/debugrpt.h>
#include <wx/sstream.h>

#ifdef __WXMSW__
#include <wx/msw/registry.h>
#endif // __WXMSW__

#include "FancyCrashReporter.h"

USING_NAMESPACE_FIVEL

// The URL of our crash report submission form.
#define URL "http://iml2.hitchcock.org/intranet/crashes/wxtest/action"


//=========================================================================
//  FancyDebugReport
//=========================================================================

/// Subclass the wxDebugReportUpload so we can add extra information.
class FancyDebugReport : public wxDebugReportUpload {
    const char *mReason;

    void GetXmlNodeText(wxString &outText, wxXmlNode *node);

public:
    FancyDebugReport(const char *inReason);

    Context GetContext() const;
    void AddRegistryData();

protected:
    virtual void DoAddCustomContext(wxXmlNode *nodeRoot);
    virtual bool OnServerReply(const wxArrayString& reply);
};

FancyDebugReport::FancyDebugReport(const char *inReason)
    : wxDebugReportUpload(URL, "report:file", "action"),
      mReason(inReason)
{
}

/// If we have an explicit reason for this crash, we won't have an
/// exception context, so start stack traces from inside wxDebugReport.
wxDebugReport::Context FancyDebugReport::GetContext() const {
    return mReason ? Context_Current : Context_Exception;
}

#ifndef __WXMSW__

/// This function does nothing except on Windows.
/// PORTABILITY - Should we implement a generic version of this function?
void FancyDebugReport::AddRegistryData() {}

#else // defined(__WXMSW__)

/// Dump our registry data and add it to the debug report.
void FancyDebugReport::AddRegistryData() {
    // Build a file name to dump our registry data to.
    wxFileName path(GetDirectory(), "preferences.reg");

    // Dump our registry data.
    wxRegKey reg("HKEY_CURRENT_USER\\Software\\Tamale");
    reg.Export(path.GetFullPath());

    // Add the file to our debug report.
    AddFile(path.GetFullName(), "application preferences");
}

#endif // defined(__WXMSW__)

/// Add extra information to our XML crash report.
void FancyDebugReport::DoAddCustomContext(wxXmlNode *nodeRoot) {
    wxDebugReportUpload::DoAddCustomContext(nodeRoot);

    // If we have an explicit reason for this crash, add it to the report.
    if (mReason) {
        wxXmlNode *assertion = new wxXmlNode(wxXML_ELEMENT_NODE, "assertion");
        nodeRoot->AddChild(assertion);
        assertion->AddProperty("reason", mReason);
    }
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
    if (root->GetName() != "crashResponse")
        return false;

    // Extract the info we need from the response.
    wxString title = "Thank you!";
    wxString description = ("Your debug report has been submitted. "
                            "Thank you for helping us improve this program!");
    wxString link;
    wxXmlNode *node = root->GetChildren();
    for (; node != NULL; node = node->GetNext()) {
        // Extract the node's text.
        wxString content;
        GetXmlNodeText(content, node);
 
        // Store the node's text in the appropriate variable.
        if (node->GetName() == "title")
            title = content;
        else if (node->GetName() == "description")
            description = content;
        else if (node->GetName() == "link")
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

void FancyCrashReporter::AddDiagnosticFile(const std::string &inFileName,
                                           const std::string &inDescription)
{
    // We need to pass an absolute path to wxDebugReport::AddFile, or it
    // will assume that we're referring to a file in the crash report
    // temporary directory.  So make sure our path is absolute.
    wxFileName path(inFileName.c_str());
    if (!path.IsAbsolute())
        path.MakeAbsolute();
    mFileInfo.push_back(FileInfo(path.GetFullPath(), inDescription.c_str()));
}

void FancyCrashReporter::CrashNow(const char *inReason) {
    // Generate our debug report.
    FancyDebugReport report(inReason);
    report.AddAll(report.GetContext());

    // Add any useful files to the report.
    FileInfoVector::iterator i = mFileInfo.begin();
    for (; i != mFileInfo.end(); ++i)
        report.AddFile(i->path, i->description);

    // Add our registry keys.
    report.AddRegistryData();

    // Ask the user whether they want to submit this bug report, and if
    // so, process it.
    wxDebugReportPreviewStd preview;
    if (preview.Show(report)) {
        if (!report.Process() && report.GetCompressedFileName() != "") {
            // OK, processing failed, which generally means we can't talk
            // to our server--but we did at least create a debug report.
            // So allow the user to save the report.
            wxFileDialog dlg(NULL, "Save debug report", "",
                             "debugrpt", "ZIP archive (*.zip)|*.zip",
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
