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
#include "Downloader.h"
#include "FiveLApp.h"
#include "TInterpreter.h"

USING_NAMESPACE_FIVEL

Download::Download(const std::string &url, const std::string &file, 
				   cURLpp::Easy *request) 
: m_out(file.c_str()), m_url(url), m_request(request), m_shouldCancel(false) { 
}

size_t Download::WriteToFile(char* ptr, size_t size, size_t nmemb) {
	m_out.Write(ptr, size*nmemb);
	return m_out.LastWrite();
}

int Download::ProgressCallback(double dltotal, double dlnow, 
							   double ultotal, double ulnow) {
    if (m_shouldCancel) 
		return 1;
	TInterpreter::GetInstance()->DoIdle(false);
	return 0;
}

bool Download::Perform() {
	if (!m_out.Ok()) {
		return false;
	}
	
	try {
		m_request->setOpt(cURLpp::Options::Url(m_url));
		
		cURLpp::Types::WriteFunctionFunctor 
			functor(this, &Download::WriteToFile);
		m_request->setOpt(cURLpp::Options::WriteFunction(functor));
		
		cURLpp::Types::ProgressFunctionFunctor 
			progress(this, &Download::ProgressCallback);
		m_request->setOpt(cURLpp::Options::ProgressFunction(progress));

		m_request->perform();
	} catch (cURLpp::RuntimeError &e) {
		gDebugLog.Log("cURL runtime error: %s\n", e.what());
		return false;
	} catch (cURLpp::LogicError &e) {
		gDebugLog.Error("cURL logic error: %s\n", e.what());
		return false;
	}

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

	m_request.setOpt(cURLpp::Options::NoProgress(false));
	m_currentDownload = NULL;
}

Downloader::~Downloader() { }

bool Downloader::Get(const std::string &URL, const std::string &file) {
	Download d(URL, file, &m_request);
	m_currentDownload = &d;
	bool result = d.Perform();
	m_currentDownload = NULL;
	return result;
}

void Downloader::CancelDownload() {
	if (m_currentDownload != NULL)
		m_currentDownload->Cancel();
}