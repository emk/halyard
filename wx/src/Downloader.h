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

#ifndef Downloader_H
#define Downloader_H
#include <curl/curl.h>
#include <wx/wfstream.h>

class Download {
public:
    Download(const std::string &url, const std::string &file, 
             CURL *request);
    bool Perform();
    void Cancel();
protected:
    wxFileOutputStream m_out;
    std::string m_url;
    CURL *m_request;
    bool m_shouldCancel;
    
    friend size_t WriteToFile(char* ptr, size_t size, size_t nmemb, 
                              void *data);
    friend int ProgressCallback(void *data, double dltotal, double dlnow, 
                                double ultotal, double ulnow);
};

class Downloader
{
public:
    Downloader();
    ~Downloader();

    /// Download the given URL to the given filename.
    /// 
    /// \param URL  The URL to download from.
    /// \param file  The file to download to. 
    /// \return  False if the download succeeds, true if there are errors. 
    bool Get(const std::string &URL, const std::string &file);

    /// Get the singleton downloader object.
    static Downloader *GetInstance() 
        { ASSERT(s_instance); return s_instance; };

    /// Cancel the download in progress. 
    void CancelDownload();

protected:
    static Downloader *s_instance;
    static bool s_haveAlreadyCreatedSingleton;

    CURL * m_request;
    Download * m_currentDownload;
};

#endif // Downloader_H
