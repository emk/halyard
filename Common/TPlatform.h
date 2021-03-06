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

#if !defined (_TPlatform_h_)
#define _TPlatform_h_


/// \defgroup platform Platform-Specific Configuration
///
/// Platform-specific configuration code.
///
/// \author Chuck Officer
/// \author ...and others
///@{


/*=========================================================================
**  Autoconf-Detected Features
**=========================================================================
**  With non-MSVC++ compilers, the presence of these features is detected
**  by autoconf, which arranges for us to be passed various preprocessor
**  definitions.  Under MSVC++, we don't have autoconf.  So make sure that
**  this section assumes MSVC++ defaults in the absense of autoconf.
*/

// If the compiler doesn't support __attribute__, define a macro which
// makes all uses of __attribute__ disappear.  MSVC++: Does not have
// __attribute__.
#ifndef HAVE___ATTRIBUTE__
#define __attribute__(args)
#endif


/*=========================================================================
**  Win32 Configuration
**=======================================================================*/

#if defined (WIN32)

#define APP_PLATFORM_WIN32 (1)

#if defined (_DEBUG)
#define DEBUG
#endif

/* For now, the Windows engine uses some non-standard string functions */
/* instead of using our own, equivalent code. */
#define HAVE__STRLWR 1
#define HAVE__STRUPR 1
#define HAVE__STRICMP 1
#define HAVE__VSNPRINTF 1


/*=========================================================================
**  Macintosh Configuration
**=======================================================================*/

#elif defined __APPLE__

#define APP_PLATFORM_MACINTOSH (1)

#ifndef NDEBUG
#define DEBUG (1)
#endif /* NDEBUG */


/*=========================================================================
**  Other Platform Configuration
**=======================================================================*/

#else

#define APP_PLATFORM_OTHER (1)

#ifndef NDEBUG
#define DEBUG (1)
#endif /* NDEBUG */

#endif


/*=========================================================================
**  Platform-dependent configuration
**=======================================================================*/

#ifndef APP_PLATFORM_WIN32
#define __declspec(ARG)
#endif


/*=========================================================================
**  Namespace Configuration
**=========================================================================
**  It's possible to define most of our common classes in a
**  'Halyard' namespace.
*/

#define BEGIN_NAMESPACE_HALYARD  namespace Halyard {
#define END_NAMESPACE_HALYARD    }

///@}

#endif /* _TPlatform_h_ */
