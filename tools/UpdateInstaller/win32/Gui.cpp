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

// Win32 GUI for update installer.  This file is originally based on the
// Win32 wizard code for a GUI application.  I'm assume that Microsoft does
// not assert copyright on their wizard skeleton code, and in any case,
// I've changed this code heavily before using it.
//
// Note that this file currently needs to be compiled in Unicode mode, so
// that we have easy access to CommandLineToArgvW.  There are several
// places in this file where we take advantage of the fact that TCHAR is
// really wchar_t--but we try to do that only where necessary.

#include "stdafx.h"
#include <shellapi.h>
#include "Gui.h"

// The interface between our GUI and the rest of the program.
#include "Interface.h"

#define MAX_LOADSTRING 100

/// Our progress dialog.
static HWND gDialog;
static HWND gProgressBar;

// Forward declarations.
static INT_PTR CALLBACK ProgressDialogCallback(HWND, UINT, WPARAM, LPARAM);
static void ReportWindowsError();
static void ReportErrorInternal(LPTSTR message);
static std::wstring ToApproximateUnicode(LPCSTR input);
static std::string FromUnicode(LPCWSTR input);

/// The main entry point for our application.
int APIENTRY _tWinMain(HINSTANCE hInstance,
                       HINSTANCE hPrevInstance,
                       LPTSTR    lpCmdLine,
                       int       nCmdShow)
{
	UNREFERENCED_PARAMETER(hPrevInstance);
	UNREFERENCED_PARAMETER(lpCmdLine);

    // Make sure our progress bar control is properly loaded, or dialog
    // box creation will fail.
    INITCOMMONCONTROLSEX init;
    init.dwSize = sizeof(init);
    init.dwICC = ICC_PROGRESS_CLASS;
    ::InitCommonControlsEx(&init);

    // Create our dialog, and show it.
    gDialog = ::CreateDialog(hInstance, MAKEINTRESOURCE(IDD_INSTALLING), NULL,
                             ProgressDialogCallback);
    if (!gDialog)
        ReportWindowsError();
    ::ShowWindow(gDialog, nCmdShow);
    ::UpdateWindow(gDialog);

    // Look up our progress bar control.
    gProgressBar = ::GetDlgItem(gDialog, IDC_PROGRESS);
    if (!gProgressBar)
        ReportWindowsError();

    // Break our command-line into an ordinary argv array.  Note that
    // CommandLineToArgvW is only available for Unicode characters, not for
    // Windows ANSI or MBCS strings, so we need to compile this program in
    // Unicode mode.  Of course, we also need to convert all our strings
    int argc;
    LPTSTR *wargv = CommandLineToArgvW(lpCmdLine, &argc);
    
    // Convert our arguments from Unicode to a fake argv array.
    std::vector<std::string> args;
    std::vector<const char*> argv;
    args.push_back("UpdateInstaller.exe");
    for (int i = 0; i < argc; ++i)
        args.push_back(FromUnicode(wargv[i]));
	for (size_t j = 0; j < args.size(); ++j)
		argv.push_back(args[j].c_str());

    // See the CommandLineToArgv documentation for a discussion of how to
    // dispose of wargv--we supposedly only need a single LocalFree call.
    ::LocalFree(wargv);

    // Call our POSIX-style 'main' function.
    UpdaterMain(argv.size(), &argv[0]);

    // Free up our resources, close our window, and exit.  
    ::DestroyWindow(gDialog);
    return 0;
}

/// Set the number of steps required to complete the progress bar.
void UpdateProgressRange(size_t step_count) {
    ::SendMessage(gProgressBar, PBM_SETRANGE, 0, MAKELPARAM(0, step_count));
}

/// Update the progress bar, and process any pending Windows events.
void UpdateProgress(size_t steps_completed) {
    // Update the progress bar.
    ::SendMessage(gProgressBar, PBM_SETPOS, steps_completed, 0);

    // Process any messages in our queue, but don't block--we want to
    // return to our caller as soon as our Windows event processing is
    // done.  Note that modeless dialog messages need special handling (in
    // a fashion which is nearly identical to MacOS 9 and earlier!).
	MSG msg;
	while (::PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
		if (!::IsWindow(gDialog) || !::IsDialogMessage(gDialog, &msg)) {
			::TranslateMessage(&msg);
			::DispatchMessage(&msg);
		}
	}
}

/// Dialog callback handler for our progress dialog.
INT_PTR CALLBACK ProgressDialogCallback(HWND hDlg, UINT message,
                                        WPARAM wParam, LPARAM lParam)
{
	UNREFERENCED_PARAMETER(lParam);
	switch (message) {
        case WM_INITDIALOG:
            return (INT_PTR) TRUE;

        case WM_COMMAND:
            // Ignore OK and Cancel commands, which may be generated by
            // various key presses.  The user can't dismiss this dialog,
            // because doing so will likely result in an incomplete update
            // and broken program.
            if (LOWORD(wParam) == IDOK || LOWORD(wParam) == IDCANCEL)
                return (INT_PTR) TRUE;
            break;
	}
	return (INT_PTR)FALSE;
}

/// Convert the error message to Unicode, and display a simple error dialog
/// on the screen.
void ReportError(const char *message) {
    std::wstring wmessage(ToApproximateUnicode(message));
    ReportErrorInternal(const_cast<LPTSTR>(wmessage.c_str()));
}

/// Report the last Windows error which occurred.
void ReportWindowsError() {
    DWORD err = ::GetLastError();
    LPTSTR buffer;
    if (::FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                        FORMAT_MESSAGE_FROM_SYSTEM,
                        NULL, err, 0, (LPTSTR) &buffer, 0, NULL) > 0)
    {
        ReportErrorInternal(buffer);
        ::LocalFree(buffer);
    } else {
        ReportErrorInternal(_T("An unknown internal error occurred."));
    }
}

/// Display a simple error dialog on the screen.
void ReportErrorInternal(LPTSTR message) {
    std::wstring full_message =
        L"An error occurred while installing the update.  You will\n"
        L"probably need to reinstall the program.  We apologize for\n"
        L"this inconvenience.  If you want to notify us of this problem,\n"
        L"please write down the following message:\n\n";
    full_message += message;
    ::MessageBox(NULL, full_message.c_str(),
                 NULL, MB_TASKMODAL | MB_OK | MB_ICONSTOP);

    // Fail hard.  If we're lucky, this will send a short-form crash report
    // to Microsoft's centralized crash reporter.  And if we're signed our
    // application using a level 3 Authenticode key, we should be able to get
    // a Windows Logo program account and download the crash reports.
    ::abort();
}

// Convert an LPCSTR to an LPCWSTR (stored in a std::wstring).  This
// function can be slightly lossy under certain circumstances, so it's
// really only appropriate for output displayed to the user.  Note that we
// leave an extra null byte at the end of the output string.
std::wstring ToApproximateUnicode(LPCSTR input) {
    // Figure out how many characters to allocate for our output buffer.
    int size_needed =
        ::MultiByteToWideChar(CP_ACP, 0, input, -1, NULL, 0);
    // Overloaded return value of zero indicates failure.  Remember, the
    // empty string needs a one-character buffer to hold the null
    // terminator byte.
    if (size_needed == 0)
        ReportWindowsError();
    
    // Actually do the conversion.
    std::wstring result(size_needed, L'\0');
    int bytes_converted =
        ::MultiByteToWideChar(CP_ACP, 0, input, -1,
                              const_cast<LPWSTR>(result.c_str()),
							  size_needed);
    if (bytes_converted == 0)
        ReportWindowsError();
    return result;
}

// Convert an LPCWSTR to an LPCSTR (stored in a std::wstring).  Since we'll
// be calling this on file names, we work very hard to make the translation
// as accurate as we can, and we fail if we notice that anything goes
// wrong.  Note that we leave an extra null byte at the end of the output
// string.
std::string FromUnicode(LPCWSTR input) {
    // Figure out how many characters to allocate for our output buffer.
    // This is the best I can do given the somewhat vague MSDN
    // documentation. What a horrible API!
    BOOL used_default_char = FALSE;
    int size_needed =
        ::WideCharToMultiByte(CP_ACP, WC_NO_BEST_FIT_CHARS,
                              input, -1, NULL, 0, NULL, &used_default_char);
    // Overloaded return value of zero indicates failure.  Remember, the
    // empty string needs a one-character buffer to hold the null
    // terminator byte.
    if (size_needed == 0)
        ReportWindowsError();
    if (used_default_char)
        ReportErrorInternal(_T("Unable to convert string from Unicode"));
    
    // Actually do the conversion.
    std::string result(size_needed, '\0');
    int bytes_converted =
        ::WideCharToMultiByte(CP_ACP, WC_NO_BEST_FIT_CHARS,
                              input, -1, const_cast<LPSTR>(result.c_str()),
							  size_needed, NULL, &used_default_char);
    if (bytes_converted == 0)
        ReportWindowsError();
    if (used_default_char)
        ReportErrorInternal(_T("Unable to convert string from Unicode"));
    return result;
}
