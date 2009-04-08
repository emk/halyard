// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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

#ifndef UrlRequest_H
#define UrlRequest_H

#include "InvisibleElement.h"

// These typedefs should match those in curl/curl.h.  We re-declare them
// here so that we don't need to include curl/curl.h in this header file.
typedef void CURL;
struct CURLMsg;

/// An asynchronous HTTP (FTP, etc.) request, implemented using libcurl.
/// We implement this as an element, because doing so gives us easy access
/// to lifecycle management and an event dispatcher.
class UrlRequest : public InvisibleElement {
    typedef std::map<CURL*,UrlRequest*> RequestMap;

    /// Maps all existing easy handles to the corresponding UrlRequest.
    static RequestMap sRequests;

    enum State {
        INITIALZING,     //< We're still setting up mHandle.
        STARTED,         //< mHandle is attached to gCurlMultiHandle.
        STOPPED          //< mHandle is detached from gCurlMultiHandle.
    };

    /// What is the current state of this request?
    State mState;

    /// A libcurl "easy handle" handle representing this request.
    CURL *mHandle;

    static int ProgressCallback(void *data, double dltotal, double dlnow, 
                                double ultotal, double ulnow);
    static size_t WriteCallback(char* ptr, size_t size, size_t nmemb,
                                void *data);

    /// Called when data is transferred.
    int DoProgress(double dltotal, double dlnow,
                   double ultotal, double ulnow);

    /// Called when data needs to be written.
    size_t DoWrite(char* ptr, size_t size, size_t nmemb);

    /// Called when we receive a message about this request via the multi
    /// handle.
    void HandleMessage(struct CURLMsg *inMsg);

public:
    /// Initialize libcurl.
    static void Initialize();

    /// Clean up libcurl.
    static void Cleanup();

    /// Run idle-time tasks for all outstanding UrlRequest objects.
    static void ProcessAllRequests();

    /// Create a new asynchronous URL request.
    UrlRequest(Stage *inStage, const wxString &inName,
               Halyard::TCallbackPtr inDispatcher,
               const wxString &inUrl);

    virtual ~UrlRequest();

    /// Start the URL request.
    void Start();

    /// Abort a running URL request.
    void Stop();
};

#endif // UrlRequest_H
