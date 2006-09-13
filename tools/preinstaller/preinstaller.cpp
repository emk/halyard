#include "stdafx.h"

class MyApp : public wxApp {
    virtual bool OnInit();
};

class MyFrame : public wxFrame {
public: 
    MyFrame();
    
private:
    wxTimer mTimer;
    wxButton *mQTButton;
    wxButton *mAppButton;

    void OnTimer(wxTimerEvent& event);
    void OnInstallQuickTime(wxCommandEvent& event);
    void OnInstallApplication(wxCommandEvent& event);

    int QuickTimeVersion();

    DECLARE_EVENT_TABLE()
};

enum {
    ID_Timer = 1,

    ID_InstallQuickTime = 1,
    ID_InstallApplication
};

BEGIN_EVENT_TABLE(MyFrame, wxFrame)
    EVT_BUTTON(ID_InstallQuickTime, MyFrame::OnInstallQuickTime)
    EVT_BUTTON(ID_InstallApplication, MyFrame::OnInstallApplication)
    EVT_TIMER(ID_Timer, MyFrame::OnTimer)
END_EVENT_TABLE()

IMPLEMENT_APP(MyApp)

bool MyApp::OnInit() {
    wxApp::OnInit();

    // Display our window.
    MyFrame *frame = new MyFrame();
    frame->Show(TRUE);
    SetTopWindow(frame);
    return TRUE;
} 

MyFrame::MyFrame()
    : wxFrame((wxFrame *)NULL, -1, "Install Program",
              wxDefaultPosition, wxDefaultSize,
              wxCLOSE_BOX|wxSYSTEM_MENU|wxCAPTION|wxRESIZE_BORDER)
{   
    CreateStatusBar();

    // Create a white window background.  This looks more "installer-like".
    // Make the foreground text black for a clean contrast.
    wxWindow *background = new wxPanel(this, -1, wxDefaultPosition,
                                       wxDefaultSize);
    background->SetBackgroundColour(*wxWHITE);
    background->SetForegroundColour(*wxBLACK);

    // Create a one-column grid for layout.
    wxSizer *sizer = new wxFlexGridSizer(1, 20, 20);

    sizer->Add(new wxStaticText(background, -1, "1. Install QuickTime"));
    mQTButton = new wxButton(background, ID_InstallQuickTime,
                             "Install QuickTime");
    sizer->Add(mQTButton);
    sizer->Add(new wxStaticText(background, -1, "2. Install Application"));
    mAppButton = new wxButton(background, ID_InstallApplication,
                              "Install Application");
    sizer->Add(mAppButton);

    // Attach the sizer.
    background->SetSizer(sizer);
    sizer->SetSizeHints(background);
    Layout();

    // Position this frame in the center of the screen.
    CenterOnScreen();

    // Send a timer event periodically.  We don't want to do this too
    // often, because checking the QuickTime version is fairly expensive.
    mTimer.SetOwner(this, ID_Timer);
    mTimer.Start(3000, wxTIMER_CONTINUOUS);
}

void MyFrame::OnTimer(wxTimerEvent& event) {
    wxString str;
    if (QuickTimeVersion() == 0) { // XXX - Need configurable version.
        mQTButton->Enable();
        mAppButton->Disable();
    } else {
        mQTButton->Disable();
        mAppButton->Enable();
    }
    str.Printf("QuickTime version: %08x", QuickTimeVersion());
    SetStatusText(str);
}

void MyFrame::OnInstallQuickTime(wxCommandEvent& event) {
    wxMessageBox("QuickTime!", "Blah", wxOK | wxICON_INFORMATION, this);
}

void MyFrame::OnInstallApplication(wxCommandEvent& event) {
    wxMessageBox("Application!", "Blah", wxOK | wxICON_INFORMATION, this);
}


int MyFrame::QuickTimeVersion() {
    long version;
    OSErr result;

    // Set up our Macintosh Toolbox glue code.
    ::InitializeQTML(0);

    // Call the Gestalt manager and ask for QuickTime.  If we're lucky,
    // this should work even in the absence of the QuickTime DLL.
    result = ::Gestalt(gestaltQuickTime, &version);

    // Shut down our Macintosh Toolbox glue code.
    ::TerminateQTML();

    return (result == noErr) ? version : 0;
}
