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

#endif // AppConfig_H
