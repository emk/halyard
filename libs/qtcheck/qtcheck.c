// Windows includes.
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

// Fake Macintosh includes.  This is the QuickTime glue layer.
#include <QTML.h>
#include <Gestalt.h>

// C library includes.
#include <string.h>

// Our own local header.
#include "qtcheck.h"

void __stdcall QuickTimeCheckSetup() {
    // Set up our Macintosh Toolbox glue code.
    InitializeQTML(0);
}

int __stdcall QuickTimeVersion() {
    long version;
    OSErr result;

    // Call the Gestalt manager and ask for QuickTime.  If we're lucky,
    // this should work even in the absence of the QuickTime DLL.
    result = Gestalt(gestaltQuickTime, &version);
    return (result == noErr) ? version : 0;
}

int __stdcall QuickTimeComponentVersion(char *type, char *subtype) {
    OSType real_type, real_subtype;
    ComponentInstance ci;
    long version;

    // If we don't have QuickTime, we can't have any components, either.
    if (QuickTimeVersion() == 0)
        return 0;

    // If our type or subtype is invalid, return 0.
    if (strlen(type) != 4 || strlen(subtype) != 4)
        return 0;

    // Convert our type strings to Macintosh OSType codes.
    real_type = (type[0] << 24 | type[1] << 16 | type[2] << 8 | type[3]);
    real_subtype =
        (subtype[0] << 24 | subtype[1] << 16 | subtype[2] << 8 | subtype[3]);

    // Open up an instance of our component.
    ci = OpenDefaultComponent(real_type, real_subtype);
    if (!ci)
        return 0;

    // Get the version of our component.
    version = GetComponentVersion(ci);
    if (GetComponentInstanceError(ci) != noErr)
        return 0;

    // Close our component instance.
    if (CloseComponent(ci) != noErr)
        return 0;
    
    return version;
}

void __stdcall QuickTimeCheckCleanup() {
    // Shut down our Macintosh Toolbox glue code.
    TerminateQTML();
}
