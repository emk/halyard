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

#include <stdexcept>
#include "CommandLine.h"

CommandLine::CommandLine(int argc, char **argv) {
	for (int i = 0; i < argc; i++) {
		mArgs.push_back(std::string(argv[i]));
	}
}

std::string CommandLine::WindowsQuotedString() const {
	std::string result("");
	
	std::vector<std::string>::const_iterator iter = mArgs.begin();
	for(; iter != mArgs.end(); ++iter) {
		if (iter != mArgs.begin()) 
			result += " ";
		result += WindowsQuoteArgument(*iter);
	}

	return result;
}

std::string CommandLine::WindowsQuoteArgument(std::string arg) const {
	std::string result("");
	
	result += "\"";

	size_t backslash_count = 0;
	std::string::const_iterator iter = arg.begin();
	for (; iter != arg.end(); ++iter) {
		switch (*iter) {
		case '"':
			result += std::string(2*backslash_count+1, '\\');
			backslash_count = 0;
			result += '"';
			break;
		case '\\':
			++backslash_count;
			break;
		default:
			if (backslash_count != 0) {
				result += std::string(backslash_count, '\\');
				backslash_count = 0;
			}
			result += *iter;
		}
	}

	result += "\"";
	return result;
}

#ifdef WIN32

#include <cassert>
#include <windows.h>

bool CommandLine::ExecAsync(const std::string &inProgram,
                            const CommandLine &inArgs)
{
    /// Ideally, the first HWND parameter would point to the application's
    /// front-most window, so that we can attempt privilege escalation.
    /// But it doesn't, so the escalation dialog may be created in a
    /// minimized state, and many users may fail to find it.
    ///
    /// If we used 'runas' instead of 'open', we would supposedly get a
    /// privilege escalation dialog even without a proper manifest.  But we
    /// don't want that.
    HINSTANCE result =
        ::ShellExecuteA(NULL, "open", inProgram.c_str(),
                        inArgs.WindowsQuotedString().c_str(), NULL, 1);

    // Result codes of less than 32 are errors.  See
    // http://blogs.msdn.com/oldnewthing/archive/2006/11/08/1035971.aspx
    // for the gory details of this return value.
    return (result >= HINSTANCE(32));
}


#else

bool CommandLine::ExecAsync(const std::string &inProgram,
                            const CommandLine &inArgs)
{
    // We used to issue a compiler error for platforms without ExecAsync,
    // but we don't need it unless we're trying to make the updater work.
    // And that's a very long-term goal anywhere but Windows.
    throw std::runtime_error("Not implemented: CommandLine::ExecAsync");
}

#endif
