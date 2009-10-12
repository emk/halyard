#include "stdafx.h"
#include <shellapi.h>

int APIENTRY _tWinMain(HINSTANCE hInstance,
                       HINSTANCE hPrevInstance,
                       LPTSTR    lpCmdLine,
                       int       nCmdShow)
{
  HINSTANCE result = ShellExecute(NULL, _T("open"), _T("engine/win32/Halyard.exe"), 
                                  lpCmdLine, NULL, 1);

  return 0;
}
