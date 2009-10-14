#include "stdafx.h"
#include <shellapi.h>

#define ERR_CASE(var,err) case (err): var=_T(#err); break;

int APIENTRY _tWinMain(HINSTANCE hInstance,
                       HINSTANCE hPrevInstance,
                       LPTSTR    lpCmdLine,
                       int       nCmdShow)
{
  // These apparently must be backslashes on Vista and newer; on XP, forward
  // slashes work, but on Vista and newer you get a file not found error.
  LPTSTR engine_path = _T("engine\\win32\\Halyard.exe");

  // According to http://msdn.microsoft.com/en-us/library/bb762153(VS.85).aspx
  // the return value of ShellExecute should be cast to an int and compared
  // to 32 or one of the constants below; if it is less than 32, it is
  // an error.
  int result = (int) ShellExecute(NULL, _T("open"), engine_path, 
                                  lpCmdLine, NULL, 1);
  if (result < 32) {
    TCHAR message[200];
    LPTSTR error_name;
    switch (result) {
      // This is the list of error codes returned by ShellExecute,
      // according to the MSDN link above, except for SE_ERR_FNF and
      // SE_ERR_PNF which are duplicates of ERROR_FILE_NOT_FOUND and
      // ERROR_PATH_NOT_FOUND.
      ERR_CASE(error_name, ERROR_FILE_NOT_FOUND);
      ERR_CASE(error_name, ERROR_PATH_NOT_FOUND);
      ERR_CASE(error_name, ERROR_BAD_FORMAT);
      ERR_CASE(error_name, SE_ERR_ACCESSDENIED);
      ERR_CASE(error_name, SE_ERR_ASSOCINCOMPLETE);
      ERR_CASE(error_name, SE_ERR_DDEBUSY);
      ERR_CASE(error_name, SE_ERR_DDEFAIL);
      ERR_CASE(error_name, SE_ERR_DDETIMEOUT);
      ERR_CASE(error_name, SE_ERR_DLLNOTFOUND);
      ERR_CASE(error_name, SE_ERR_NOASSOC);
      ERR_CASE(error_name, SE_ERR_OOM);
      ERR_CASE(error_name, SE_ERR_SHARE);
    default: 
      error_name = _T("Unknown error");
    }

    // _sntprintf_s? Really, Microsoft? Do you really need that many arbitrary
    // extra characters to be happy?
    _sntprintf_s(message, 200, _T("Error: Could not launch '%s' with arguments ")
                               _T("'%s': %s (%d)"),
                 engine_path, lpCmdLine, error_name, result);
    message[199] = 0;
    MessageBox(NULL, message, NULL, MB_OK);
  }

  return (result < 32);
}
