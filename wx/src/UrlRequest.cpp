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
#include "TVersion.h"
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
            gLog.Error("halyard.url-request",
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
            gLog.Error("halyard.url-request",
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
        gLog.Fatal("halyard.url-request",
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
//  Proxy configuration
//========================================================================

#ifdef APP_PLATFORM_WIN32

#include <windows.h>
#include <Winhttp.h>

void UrlRequest::ConfigureProxyServer(CURL *inHandle, const wxString &inUrl) {
    // TODO - See the following URLs:
    //
    //   http://stackoverflow.com/questions/202547/how-do-i-find-out-the-browsers-proxy-settings
    //   http://msdn.microsoft.com/en-us/library/aa384096.aspx
    //   http://msdn.microsoft.com/en-us/library/aa384122(VS.85).aspx
    //   http://msdn.microsoft.com/en-us/library/aa383660(VS.85).aspx
    //
    // Our implementation is based on the answer at Stack Overflow.  Note
    // that there's several sample programs in Windows SDK 6.1, but that
    // they each take a different approach to configuring the proxy server.

    bool auto_detect = false;
    bool have_proxy = false, have_proxy_bypass = false,
         have_auto_config_url = false;
    wxString proxy, proxy_bypass, auto_config_url;

    // Look up the Internet Explorer proxy server preferences.
    WINHTTP_CURRENT_USER_IE_PROXY_CONFIG config;
    if (!WinHttpGetIEProxyConfigForCurrentUser(&config)) {
        DWORD error(GetLastError());
        if (error != ERROR_FILE_NOT_FOUND)
            gLog.Debug("halyard.url-request.proxy",
                       "Error getting IE proxy configuration: %d", (int) error);
        // The Stack Overflow post suggests we should fall back to
        // auto_detect here, but frankly, auto_detect looks like a
        // potential security headache, and I'd rather not turn it on
        // unless the user has actually configured their system to use it
        // (in which case, their network administrator has hopefully
        // weighed the risks).
    } else {
        auto_detect = config.fAutoDetect ? true : false;
        if (config.lpszProxy) {
            have_proxy = true;
            proxy = config.lpszProxy;
            GlobalFree(config.lpszProxy);
            gLog.Trace("halyard.url-request.proxy", "Proxy: %s",
                       (const char *) proxy.mb_str());
        }
        if (config.lpszProxyBypass) {
            have_proxy_bypass = true;
            proxy_bypass = config.lpszProxyBypass;
            GlobalFree(config.lpszProxyBypass);
            gLog.Trace("halyard.url-request.proxy", "Proxy bypass: %s",
                       (const char *) proxy_bypass.mb_str());
        }
        if (config.lpszAutoConfigUrl) {
            have_auto_config_url = true;
            auto_config_url = config.lpszAutoConfigUrl;
            GlobalFree(config.lpszAutoConfigUrl);
            gLog.Trace("halyard.url-request.proxy", "Proxy auto-config URL: %s",
                       (const char *) auto_config_url.mb_str());
        }
    }
    gLog.Trace("halyard.url-request.proxy", "Proxy auto-detect: %d",
               (int) auto_detect);

    // If we've been asked to use automatic proxy configuration and/or
    // detection, do our best to set it up.
    if (auto_detect) {
        // Open up a WinHttp handle.  By passing
        // WINHTTP_ACCESS_TYPE_DEFAULT_PROXY, we actually ask it to look up
        // the system wide proxycfg.exe settings, which are entirely
        // different than the IE settings.  I have no idea whether this has
        // any effect on WinHttpGetProxyForUrl, but it probably can't hurt.
        wxString user_agent(VERSION_STRING);
        HINTERNET hSession(WinHttpOpen(user_agent.wc_str(wxConvLibc),
                                       WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
                                       WINHTTP_NO_PROXY_NAME,
                                       WINHTTP_NO_PROXY_NAME, 0));
        if (hSession) {
            WINHTTP_AUTOPROXY_OPTIONS options;
            WINHTTP_PROXY_INFO proxy_info;
            std::wstring auto_config_url_w;

            // Set up our request.
            memset(&options, 0, sizeof(options));
            if (auto_detect) {
                options.dwFlags |= WINHTTP_AUTOPROXY_AUTO_DETECT;
                options.dwAutoDetectFlags = (WINHTTP_AUTO_DETECT_TYPE_DHCP |
                                             WINHTTP_AUTO_DETECT_TYPE_DNS_A);
            }
            if (have_auto_config_url) {
                // Make sure we have a local copy of auto_config_url in
                // Unicode format.  We don't rely directly on wc_str(),
                // because in ANSI mode, it may return a pointer to a
                // temporary buffer.
                auto_config_url_w =
                    std::wstring(auto_config_url.wc_str(wxConvLibc));
                options.dwFlags |= WINHTTP_AUTOPROXY_CONFIG_URL;
                options.lpszAutoConfigUrl = auto_config_url_w.c_str();
            }
            options.fAutoLogonIfChallenged = TRUE;

            // Try to look up the proxy server for this URL.
            BOOL success =
                WinHttpGetProxyForUrl(hSession, inUrl.wc_str(wxConvLibc),
                                      &options, &proxy_info);
            if (success &&
                proxy_info.dwAccessType == WINHTTP_ACCESS_TYPE_NAMED_PROXY)
            {
                // TODO - We really need to do a better job of parsing the
                // config.lpszProxy field, which can contain multiple
                // proxies, separated by either whitespace or semicolons,
                // and which may contain URL schemes in one of two separate
                // formats.  Yes, this is getting a bit silly (and since
                // this structure is used by many different functions, I
                // don't even know how much of this applies to us).
                // http://msdn.microsoft.com/en-us/library/aa383912(VS.85).aspx
                if (proxy_info.lpszProxy) {
                    have_proxy = true;
                    proxy = config.lpszProxy;
                    gLog.Trace("halyard.url-request.proxy", "Auto Proxy: %s",
                               (const char *) proxy.mb_str());
                }
                if (proxy_info.lpszProxyBypass) {
                    wxString ignored_bypass(config.lpszProxyBypass);
                    gLog.Trace("halyard.url-request.proxy",
                               "Ignoring unexpected Auto Proxy Bypass: %s",
                               (const char *) ignored_bypass.mb_str());
                }
            } else {
                gLog.Trace("halyard.url-request.proxy",
                           "WinHttpGetProxyForUrl failed: %u",
                           GetLastError());
            }

            // Shut down our WinHttp session.
            WinHttpCloseHandle(hSession);
        } else {
            gLog.Trace("halyard.url-request.proxy", "Error in WinHttpOpen: %u",
                       GetLastError());
        }
    }

    // According to a poster on StackOverflow, all the settings below
    // should be interpreted as "no proxy".
    // http://stackoverflow.com/questions/202547/how-do-i-find-out-the-browsers-proxy-settings
    if (proxy == ":" || proxy == "::" ||
        proxy_bypass == ":" || proxy_bypass == "::")
        have_proxy = have_proxy_bypass = false;

    // Try our regular HTTP proxy settings.
    if (have_proxy) {
        // TODO - There may be multiple proxy servers listed in proxy.
        //
        // TODO - Actually parse lpszProxyBypass.  For now, we're assuming
        // that any server we want to talk to is on the other side of a
        // proxy server, which should work well enough for our immediate
        // needs.
        //
        // lpszProxyBypass appears to be documented here:
        // http://msdn.microsoft.com/en-us/library/aa384098(VS.85).aspx
        // Basically: Hostnames or IP addresses or both, with optional
        // wildcard characters.  The special value "<local>" means any host
        // without a period.  On my machine, values are separated by ";".
        CHKE(curl_easy_setopt(inHandle, CURLOPT_PROXY,
                              (const char *) proxy.mb_str()));
    }
}

#else // !defined(APP_PLATFORM_WIN32)

void UrlRequest::ConfigureProxyServer(CURL *inHandle, const wxString &inUrl) {
    // We don't attempt to auto-configure proxies on non-Windows platforms.
}

#endif // !defined(APP_PLATFORM_WIN32)

//========================================================================
//  Instance methods
//========================================================================

UrlRequest::UrlRequest(Stage *inStage, const wxString &inName,
                       Halyard::TCallbackPtr inDispatcher,
                       const wxString &inUrl)
    : InvisibleElement(inStage, inName, inDispatcher),
      mState(INITIALZING), mHandle(NULL), mHeaders(NULL)
{
    gLog.Debug("halyard.url-request", "%s: Making URL request to %s",
               GetLogName(), (const char *) inUrl.mb_str());

    mHandle = curl_easy_init();
    if (!mHandle)
        gLog.Fatal("halyard.url-request", "Unable to initialize CURL handle");
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

        // Attempt to turn HTTP response codes >= 400 into outright
        // failures.  According to the manual, this doesn't always
        // work--some authentication failures will not result in a failed
        // transfer.
        CHKE(curl_easy_setopt(mHandle, CURLOPT_FAILONERROR, 1));

        // If necessary, attempt to configure a proxy server.
        ConfigureProxyServer(mHandle, inUrl);
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
    curl_slist_free_all(mHeaders);
    curl_easy_cleanup(mHandle);
}

void UrlRequest::ConfigurePost(const std::string &inContentType,
                               const std::string &inBody)
{
    ASSERT(mState == INITIALZING);
    gLog.Trace("halyard.url-request", "%s: Posting %d bytes as %s",
               GetLogName(), inBody.length(), inContentType.c_str());

    // Don't convert POST to GET when following redirects.
    CHKE(curl_easy_setopt(mHandle, CURLOPT_POSTREDIR, CURL_REDIR_POST_ALL));

    // Specify our POST data.  Note that we must set the size first!
    CHKE(curl_easy_setopt(mHandle, CURLOPT_POSTFIELDSIZE, inBody.length()));
    CHKE(curl_easy_setopt(mHandle, CURLOPT_COPYPOSTFIELDS, inBody.c_str()));
}

void UrlRequest::ConfigureSetHeader(const std::string &inHeader,
                                    const std::string &inValue)
{
    ASSERT(mState == INITIALZING);
    gLog.Trace("halyard.url-request", "%s: Send header %s: %s",
               GetLogName(), inHeader.c_str(), inValue.c_str());
    std::string header(inHeader);
    if (inValue == "")
        header += ":";
    else
        header += ": " + inValue;
    mHeaders = curl_slist_append(mHeaders, header.c_str());
    if (!mHeaders)
        gLog.Fatal("halyard.url-request", "Unable to allocate header");
}

void UrlRequest::Start() {
    ASSERT(mState == INITIALZING);

    // Finish our remaining setup.
    CHKE(curl_easy_setopt(mHandle, CURLOPT_HTTPHEADER, mHeaders));

    // Register our handle.
    CHKM(curl_multi_add_handle(gCurlMultiHandle, mHandle));
    ASSERT(sRequests.find(mHandle) == sRequests.end());
    sRequests.insert(RequestMap::value_type(mHandle, this));
    mState = STARTED;
    gLog.Trace("halyard.url-request", "%s: Started", GetLogName());
}

int UrlRequest::DoProgress(double dltotal, double dlnow,
                           double ultotal, double ulnow)
{
    ASSERT(mState == STARTED);
    gLog.Trace("halyard.url-request",
               "%s: Progress up %.0f/%.0f down %.0f/%.0f",
               GetLogName(), ulnow, ultotal, dlnow, dltotal);

    // Attempt to turn our progress into a value between 0.0 and 1.0.  This
    // isn't terribly accurate, and it may actually decrease as a download
    // progresses, just like a normal web browser.
    double now(ulnow + dlnow);
    double total(ultotal + dltotal);
    double fraction(total > 0 ? now / total : 0);
    GetEventDispatcher()->DoEventProgressChanged(false, fraction);
    return 0;
}

size_t UrlRequest::DoWrite(char* ptr, size_t size, size_t nmemb) {
    ASSERT(mState == STARTED);
    size_t byte_count(size * nmemb);
    std::string data(ptr, byte_count);
    gLog.Trace("halyard.url-request", "%s: Write %d bytes",
               GetLogName(), byte_count);
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
        gLog.Debug("halyard.url-request", "%s: Transfer finished (%d): %s",
                   GetLogName(), result, message.c_str());

        // Pass events to the interpreter.
        GetEventDispatcher()->DoEventProgressChanged(true, 1.0);
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
    gLog.Trace("halyard.url-request", "%s: Stopped", GetLogName());
}

std::string UrlRequest::GetResponseContentType() {
    char *outContentType;
    CHKE(curl_easy_getinfo(mHandle, CURLINFO_CONTENT_TYPE, &outContentType));
    if (outContentType)
        return outContentType;
    else
        return "";
}
