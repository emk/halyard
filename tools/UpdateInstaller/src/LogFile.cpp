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

#include <stdio.h>
#include <exception>

#define BOOST_FILESYSTEM_SOURCE
#include "boost/filesystem/path.hpp"
#include "boost/filesystem/convenience.hpp"

#include "LogFile.h"
#include "Interface.h"

using namespace boost::filesystem;

LogFile::LogFile(const path &file) : mLogFile(NULL) {
  create_directories(file.branch_path());
  mLogFile = fopen(file.native_file_string().c_str(), "a+");
  if (mLogFile == NULL) {
    throw new std::runtime_error("Couldn't open log file: " 
				 + file.native_file_string());
  }
}

LogFile::~LogFile() {
  fclose(mLogFile);
}

void LogFile::Log(const boost::format &message, Severity severity) {
  Log(message.str().c_str(), severity);
}

void LogFile::Log(const std::string &message, Severity severity) {
  Log(message.c_str(), severity);
}

void LogFile::Log(const char *message, Severity severity) {
  fprintf(mLogFile, "%s\n", message);
  fflush(mLogFile);
  if (severity == FATAL)
      ReportError(message);
}
