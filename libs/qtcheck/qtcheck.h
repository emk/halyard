// These macros automatically manage DLL symbol exporting and importing.
// The macro QTCHECK_EXPORTS should only be declared when compiling
#ifdef QTCHECK_EXPORTS
#define QTCHECK_API __declspec(dllexport)
#else
#define QTCHECK_API __declspec(dllimport)
#endif

// Call this before calling any other functions.
QTCHECK_API void __stdcall QuickTimeCheckSetup();

// Get the version of QuickTime.  Returns 0 if not installed,
// and a weirdly-encoded version number when present.  See
// http://developer.apple.com/technotes/tn/tn1197.html for a discussion
// of how to interpret this value.
QTCHECK_API int __stdcall QuickTimeVersion();

// Return the version of the specified QuickTime component.  The arguments
// are four-character codes identifying the component.  The return value is
// 0 if no such component is installed, and a weirdly-encoded version
// number if the component is present.
QTCHECK_API int __stdcall QuickTimeComponentVersion(char *type, char *subtype);

// Call this after calling any other functions.
QTCHECK_API void __stdcall QuickTimeCheckCleanup();
