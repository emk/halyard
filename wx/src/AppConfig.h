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

//
// Platform-specific configuration of the wxWindows-based front end.
// wxWindows doesn't aim to make applications *completely* portable,
// and we occasionally need to adjust to the oddities of the local
// platform.  All the configuration options should be kept in this
// file--and we should definitely refrain from using __WXMSW__,
// __WXMAC__, etc., anywhere else in the code (unless we start
// writing our own wxWindows widgets).

#ifndef AppConfig_H
#define AppConfig_H

/// Are we building against our custom-patched version of wxWidgets
/// 2.6.1p1?
///
/// TODO - This is not a particularly accurate way to check for our patched
/// wxWidgets, but it will do, since most sites have long since upgraded to
/// the latest point release of either 2.6 or 2.8.
#define HAVE_CUSTOM_WXWIDGETS \
    wxCHECK_VERSION(2,6,1) && !wxCHECK_VERSION(2,6,2)

//////////
/// Set this option to 1 to enable Quake 2 support.  You'll
/// also need to place a specially-patched version of Quake 2
/// in the libs/ directory, and add wxq2 to the application's
/// dependencies.
///
/// To disable Quake 2 support, set this option to 0, and remove
/// wxq2 from this applications's dependencies.
///
#define CONFIG_OPTION_QUAKE2             1

#ifdef __WXMSW__
#	define CONFIG_HAVE_QUICKTIME         1
#	define CONFIG_HAVE_ACTIVEX           1
#	define CONFIG_HAVE_AUDIOSTREAMS      1
#	define CONFIG_HAVE_SCRIPTEDITOR      1
#	define CONFIG_LOCATION_BOX_IS_COMBO  1
#	define CONFIG_USE_XPMS               0
#	define CONFIG_ENABLE_FULL_SCREEN     1
#	define CONFIG_HAVE_FANCYCRASHREPORT  1
#else // !defined __WXMSW__
#	define CONFIG_HAVE_QUICKTIME         0
#	define CONFIG_HAVE_ACTIVEX           0
#	define CONFIG_HAVE_AUDIOSTREAMS      0
#	define CONFIG_HAVE_SCRIPTEDITOR      0
#	define CONFIG_LOCATION_BOX_IS_COMBO  1
#	define CONFIG_USE_XPMS               1
#	define CONFIG_ENABLE_FULL_SCREEN     0
#	define CONFIG_HAVE_FANCYCRASHREPORT  0
#endif // !defined __WXMSW__

#ifdef __WXMSW__
#	define CONFIG_HAVE_QUAKE2            CONFIG_OPTION_QUAKE2
#else // !defined __WXMSW__
#	define CONFIG_HAVE_QUAKE2	         0
#endif // !defined __WXMSW__

#endif // AppConfig_H
