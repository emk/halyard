// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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

// This is the interface between the GUI layer and portable
// command-line-style updater internals.

#ifndef Interface_H
#define Interface_H

// These functions are defined by our platform-specific GUI layer.
extern void UpdateProgressRange(size_t step_count);
extern void UpdateProgress(size_t steps_completed);
extern void ReportError(const char *message);
extern bool IsSafeToRelaunchAutomatically();
extern void AskUserToRelaunch();

// This function is defined in main.cpp.
extern void UpdaterMain(size_t argc, const char **argv);

#endif // UpdateInstaler_H

