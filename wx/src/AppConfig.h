// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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

//////////
// Set this option to 1 to enable Quake 2 support.  You'll
// also need to place a specially-patched version of Quake 2
// in the libs/ directory, and add wxq2 to the application's
// dependencies.
//
// To disable Quake 2 support, set this option to 0, and remove
// wxq2 from this applications's dependencies.
//
#define CONFIG_OPTION_QUAKE2             1

#ifdef __WXMSW__
#	define CONFIG_HAVE_QUICKTIME         1
#	define CONFIG_LOCATION_BOX_IS_COMBO  1
#	define CONFIG_USE_XPMS               0
#	define CONFIG_ENABLE_FULL_SCREEN     1
#else // !defined __WXMSW__
#	define CONFIG_HAVE_QUICKTIME         0
#	define CONFIG_LOCATION_BOX_IS_COMBO  0
#	define CONFIG_USE_XPMS               1
#	define CONFIG_ENABLE_FULL_SCREEN     0
#endif // !defined __WXMSW__

#ifdef __WXMSW__
#	define CONFIG_HAVE_QUAKE2            CONFIG_OPTION_QUAKE2
#else // !defined __WXMSW__
#	define CONFIG_HAVE_QUAKE2	         0
#endif // !defined __WXMSW__

#endif // AppConfig_H
