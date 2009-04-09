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

#include "AppHeaders.h"
#include "UrlRequest.h"
#include "EventDispatcher.h"

#include <curl/curl.h>

using namespace Halyard;


//========================================================================
//  Error checking
//========================================================================

namespace {
    /// Check the result code of a curl_easy_* function.  Do not use with
    /// curl_multi_* functions!
    void CHKE(CURLcode err) {
        if (err != CURLE_OK) {
            // TODO - We should just throw the exception here, but I'm too
            // lazy to format a reasonable error string for the exception,
            // so let's cheat and take advantage of gLog for now.
            gLog.Error("halyard.url",
                       "A CURL error occurred (%d): %s",
                       err, curl_easy_strerror(err));
            THROW("Cannot perform URL request");
        }
    }

    /// Check the result code of a curl_multi_* function.  Do not use with
    /// curl_easy_* functions!
    void CHKM(CURLMcode err) {
        if (err != CURLM_OK) {
            // TODO - We should just throw the exception here, but I'm too
            // lazy to format a reasonable error string for the exception,
            // so let's cheat and take advantage of gLog for now.
            gLog.Error("halyard.url",
                       "A CURL error occurred (%d): %s",
                       err, curl_multi_strerror(err));
            THROW("Cannot perform URL request");
        }
    }
}


//========================================================================
//  Static methods
//========================================================================

namespace {
    /// This handle is used to control all our different transfers.  It
    /// would normally be a static member of UrlRequest, but we don't want
    /// to use any curl.h data types in our header.
    CURLM *gCurlMultiHandle = NULL;
}

UrlRequest::RequestMap UrlRequest::sRequests;

void UrlRequest::Initialize() {
    ASSERT(gCurlMultiHandle == NULL);
    curl_global_init(CURL_GLOBAL_ALL);
    gCurlMultiHandle = curl_multi_init();
    if (!gCurlMultiHandle)
        gLog.Fatal("halyard.url",
                   "Cannot create cURL handle for multiple requests");
}

void UrlRequest::Cleanup() {
    ASSERT(gCurlMultiHandle != NULL);
    curl_multi_cleanup(gCurlMultiHandle);
    gCurlMultiHandle = NULL;
    curl_global_cleanup();
}

void UrlRequest::ProcessAllRequests() {
    if (gCurlMultiHandle) {
        // Let CURL take care of all outstanding data transfers.
        int running_count;
        CURLMcode result;
        do {
            result = curl_multi_perform(gCurlMultiHandle, &running_count);
        } while (result == CURLM_CALL_MULTI_PERFORM);
        CHKM(result);

        // Check to see if any transfers have completed, and if so, notify
        // the correct instance.
        CURLMsg *m;
        do {
            int remaining;
            m = curl_multi_info_read(gCurlMultiHandle, &remaining);
            if (m) {
                RequestMap::iterator found(sRequests.find(m->easy_handle));
                ASSERT(found != sRequests.end());
                if (found != sRequests.end())
                    found->second->HandleMessage(m);
            }
        } while (m != NULL);
    }
}

int UrlRequest::ProgressCallback(void *data, double dltotal, double dlnow, 
                                 double ultotal, double ulnow)
{
    return static_cast<UrlRequest*>(data)->DoProgress(dltotal, dlnow,
                                                      ultotal, ulnow);
}

size_t UrlRequest::WriteCallback(char* ptr, size_t size, size_t nmemb,
                                 void *data)
{
    return static_cast<UrlRequest*>(data)->DoWrite(ptr, size, nmemb);
}


//========================================================================
//  Instance methods
//========================================================================

UrlRequest::UrlRequest(Stage *inStage, const wxString &inName,
                       Halyard::TCallbackPtr inDispatcher,
                       const wxString &inUrl)
    : InvisibleElement(inStage, inName, inDispatcher),
      mState(INITIALZING), mHandle(NULL)
{
    gLog.Debug("halyard.url", "%s: Making URL request to %s",
               (const char *) GetName().mb_str(),
               (const char *) inUrl.mb_str());

    mHandle = curl_easy_init();
    if (!mHandle)
        gLog.Fatal("halyard.url", "Unable to initialize CURL handle");
    try {
        // Set up some standard options.
        CHKE(curl_easy_setopt(mHandle, CURLOPT_NOPROGRESS, 0));
        CHKE(curl_easy_setopt(mHandle, CURLOPT_URL,
                              (const char *) inUrl.mb_str()));
        CHKE(curl_easy_setopt(mHandle, CURLOPT_WRITEFUNCTION,
                              &WriteCallback));
        CHKE(curl_easy_setopt(mHandle, CURLOPT_WRITEDATA, this));
        CHKE(curl_easy_setopt(mHandle, CURLOPT_PROGRESSFUNCTION,
                              &ProgressCallback));
        CHKE(curl_easy_setopt(mHandle, CURLOPT_PROGRESSDATA, this));
        CHKE(curl_easy_setopt(mHandle, CURLOPT_FOLLOWLOCATION, 1));
        CHKE(curl_easy_setopt(mHandle, CURLOPT_MAXREDIRS, 10));

        Start();
    } catch (...) {
        // If an error occurs, clean up our handle before continuing.
        curl_easy_cleanup(mHandle);
        mHandle = NULL;
        throw;
    }
}

UrlRequest::~UrlRequest() {
    // If we're currently running, we need to shut everything down.
    if (mState == STARTED)
        Stop();

    // Once we've left the STARTED state, we can dispose of our handle
    // normally.
    ASSERT(mState == INITIALZING || mState == STOPPED);
    curl_easy_cleanup(mHandle);
}

void UrlRequest::Start() {
    // Register our handle.
    ASSERT(mState == INITIALZING);
    CHKM(curl_multi_add_handle(gCurlMultiHandle, mHandle));
    ASSERT(sRequests.find(mHandle) == sRequests.end());
    sRequests.insert(RequestMap::value_type(mHandle, this));
    mState = STARTED;
}

int UrlRequest::DoProgress(double dltotal, double dlnow,
                           double ultotal, double ulnow)
{
    ASSERT(mState == STARTED);
    return 0;
}

size_t UrlRequest::DoWrite(char* ptr, size_t size, size_t nmemb) {
    ASSERT(mState == STARTED);
    size_t byte_count(size * nmemb);
    std::string data(ptr, byte_count);
    GetEventDispatcher()->DoEventDataReceived(data);
    return byte_count;
}

void UrlRequest::HandleMessage(struct CURLMsg *inMsg) {
    ASSERT(mState == STARTED);

    // There's only one kind of message in current versions of CURL.
    if (inMsg->msg == CURLMSG_DONE) {
        // Get the result of the transfer and log it.
        CURLcode result(inMsg->data.result);
        std::string message(curl_easy_strerror(result));
        gLog.Debug("halyard.url", "%s: Transfer finished (%d): %s",
                   (const char *) GetName().mb_str(),
                   result, message.c_str());

        // Pass an event to the interpreter.
        bool success(result == CURLE_OK);
        GetEventDispatcher()->DoEventTransferFinished(success, message);
    }
}

void UrlRequest::Stop() {
    // Remove mHandle from gCurlMultiHandle.  If the transfer is
    // incomplete, this will abort any further data transfer (at least
    // according to the maintainers on #curl).  Note that for some
    // relatively recent versions of libcurl, this will break badly if
    // pipelining is enabled:
    //   http://curl.haxx.se/mail/lib-2008-01/0192.html
    // However, the maintainers on #curl say these bugs have been fixed.
    ASSERT(mState == STARTED);
    CHKM(curl_multi_remove_handle(gCurlMultiHandle, mHandle));
    ASSERT(sRequests.find(mHandle) != sRequests.end());
    sRequests.erase(mHandle);
    mState = STOPPED;
}
