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

#include "AppHeaders.h"

#if APP_PLATFORM_WIN32

#include <windows.h>

void HideSystemWindows() {
    // I found this code at
    // http://www.thecodeproject.com/win32/AntonioWinLock.asp , but there's
    // a version of it on dozens of web sites.
    //
    // BUG - This doesn't actually make sense if the taskbar isn't on our
    // monitor, but that's a pretty rare configuration.
    ::ShowWindow(::FindWindow("Shell_TrayWnd", NULL), SW_HIDE);

    // Disable the screen saver while we're in full-screen mode.  There's
    // also related code in FiveLApp.cpp that tried to do the same thing,
    // but it doesn't work for password-protected screen savers.  This
    // does.  In theory, we should only need this fix, and not the one if
    // FiveLApp.cpp, but we're in low-disruption mode before a major
    // release.
    ::SystemParametersInfo(SPI_SETSCREENSAVEACTIVE, FALSE, 0,
                           SPIF_SENDWININICHANGE);
}

void ShowSystemWindows() {
    ::ShowWindow(::FindWindow("Shell_TrayWnd", NULL), SW_SHOW);
    
    // Re-enable the screen saver.  Fortunately, this doesn't appear to
    // turn the screen saver on if the user has turned it off in the
    // display properties.
    ::SystemParametersInfo(SPI_SETSCREENSAVEACTIVE, TRUE, 0,
                           SPIF_SENDWININICHANGE);
}

#else // !APP_PLATFORM_WIN32

void HideSystemWindows() {
}

void ShowSystemWindows() {
}

#endif

