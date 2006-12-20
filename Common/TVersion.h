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

#define VERSION_MAJOR_NUM          0
#define VERSION_MINOR_NUM          3
#define VERSION_REV_BIG            4
#define VERSION_REV_SMALL          0

// For various unfortunate preprocessor reasons, we need a duplicate copy
// in string format.
#define VERSION_MAJOR_NUM_STRING   "0"
#define VERSION_MINOR_NUM_STRING   "3"
#define VERSION_REV_BIG_STRING     "4"
#define VERSION_REV_SMALL_STRING   "0"

#define SHORT_NAME          "Tamale"
#define VERSION_STRING \
    SHORT_NAME " " VERSION_MAJOR_NUM_STRING "." \
    VERSION_MINOR_NUM_STRING "." VERSION_REV_BIG_STRING "." \
    VERSION_REV_SMALL_STRING " (Development)"

// Needed for Windows resource files.
#define VERSION_COMMAS \
    VERSION_MAJOR_NUM,VERSION_MINOR_NUM,VERSION_REV_BIG,VERSION_REV_SMALL
#define VERSION_COMMAS_STRING \
    VERSION_MAJOR_NUM_STRING ", " VERSION_MINOR_NUM_STRING ", " \
    VERSION_REV_BIG_STRING ", " VERSION_REV_SMALL_STRING

#define TAMALE_COPYRIGHT_NAME \
    SHORT_NAME " multimedia engine"
#define TAMALE_COPYRIGHT_NOTICE \
    "Copyright 1993-2006 Trustees of Dartmouth College. All rights reserved."

/// The date at which the files including TVersion.h were built.  This will
/// work right as long as you change the version number for official builds.
#define BUILD_TIMESTAMP (__DATE__ " " __TIME__)

/// The URL of our crash report submission form.
#define CRASH_REPORT_URL \
    "http://iml.dartmouth.edu/intranet/crashes/upload/action/27"

// There used to be a CVS $Log$ comment here, but it got mostly merged
// into Release-Notes.txt.  For fine-grained logging, please use the
// version control system.
