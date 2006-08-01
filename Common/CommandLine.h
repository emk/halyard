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

#ifndef CommandLine_H
#define CommandLine_H

#include <string>
#include <vector>

/* PORTABILITY - needs to be factored to work on platforms other than Windows. 
   Should add a NullTerminatedArray method to return an argv array in the 
   format wxWidgets uses for wxExecute, which can be used on Unix-like 
   platforms. */
class CommandLine {
public:
	CommandLine(int argc, char **argv);
	CommandLine(std::vector<std::string> &args) : mArgs(args) {}

	std::string WindowsQuotedString() const;
private:
	std::string WindowsQuoteArgument(std::string arg) const;

	std::vector<std::string> mArgs;
};

#endif // CommandLine_H
