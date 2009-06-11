// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
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
#include "Downloader.h"
#include "HalyardApp.h"
#include "UrlRequest.h"
#include "TInterpreter.h"
#include "CommonWxConv.h"

using namespace Halyard;

Download::Download(const std::string &url, const std::string &file, 
                   CURL *request) 
    : m_out(wxString(file.c_str(), wxConvLocal)), m_url(url),
      m_request(request), m_shouldCancel(false)
{
}

// TODO - add exception handler? 
size_t WriteToFile(char* ptr, size_t size, size_t nmemb, void *data) {
    Download *dl = (Download *) data;
    dl->m_out.Write(ptr, size*nmemb);
    return dl->m_out.LastWrite();
}

int ProgressCallback(void *data, double dltotal, double dlnow, 
                     double ultotal, double ulnow) {
    Download *dl = (Download *) data; 

    if (dl->m_shouldCancel) 
        return 1;
    // Make sure any exceptions get caught here, because they can mess with 
    // cURL. Just die if an exception happens. 
    // TODO - should we change this to just cancelling the download on 
    // an exception? Or cancel the download, store the exception, and 
    // throw it again once we're outside of a cURL callback? 
    try {
        TInterpreterManager::GetInstance()->DoIdle(false);
    } catch (std::exception &e) {
        gLog.Fatal("halyard", "Unexpected internal error: %s", e.what()); 
    } catch (...) {
        gLog.Fatal("halyard", "Unexpected, unknown internal error.");
    }
    return 0;
}

#define RETURN_ON_ERROR(EXPR)     \
{                                 \
    CURLcode error_code = (EXPR); \
    if (error_code != CURLE_OK) { \
        gLog.Debug("halyard", "cURL error (%d): %s", error_code, error_buffer); \
        return false;             \
    }                             \
}

bool Download::Perform() {
    if (!m_out.Ok()) {
        return false;
    }
    char error_buffer[CURL_ERROR_SIZE];
    
    RETURN_ON_ERROR(curl_easy_setopt(m_request, CURLOPT_URL, m_url.c_str()));

    RETURN_ON_ERROR(curl_easy_setopt(m_request, CURLOPT_WRITEFUNCTION, 
                                     &WriteToFile));
    RETURN_ON_ERROR(curl_easy_setopt(m_request, CURLOPT_WRITEDATA, this));
    RETURN_ON_ERROR(curl_easy_setopt(m_request, CURLOPT_PROGRESSFUNCTION, 
                                     &ProgressCallback));
    RETURN_ON_ERROR(curl_easy_setopt(m_request, CURLOPT_PROGRESSDATA, this));
    RETURN_ON_ERROR(curl_easy_setopt(m_request, CURLOPT_FOLLOWLOCATION, 1));
    RETURN_ON_ERROR(curl_easy_setopt(m_request, CURLOPT_MAXREDIRS, 10));
    
    UrlRequest::ConfigureProxyServer(m_request, ToWxString(m_url));

    RETURN_ON_ERROR(curl_easy_perform(m_request));

    return true;
}

void Download::Cancel() {
    m_shouldCancel = true;
}

Downloader *Downloader::s_instance = NULL;
bool Downloader::s_haveAlreadyCreatedSingleton = false;

Downloader::Downloader() {
    ASSERT(s_haveAlreadyCreatedSingleton == false);
    s_haveAlreadyCreatedSingleton = true;
    ASSERT(s_instance == NULL);
    s_instance = this;

    m_request = curl_easy_init();

    curl_easy_setopt(m_request, CURLOPT_NOPROGRESS, 0);
    m_currentDownload = NULL;
}

Downloader::~Downloader() { }

bool Downloader::Get(const std::string &URL, const std::string &file) {
    Download d(URL, file, m_request);
    m_currentDownload = &d;
    bool result = d.Perform();
    m_currentDownload = NULL;
    return result;
}

void Downloader::CancelDownload() {
    if (m_currentDownload != NULL)
        m_currentDownload->Cancel();
}
