#include <wx/wx.h>

#include "TInterpreter.h"
#include "TLogger.h"

#include "FiveLApp.h"
#include "Stage.h"


//=========================================================================
//  StageFrame Methods
//=========================================================================

BEGIN_EVENT_TABLE(StageFrame, wxFrame)
    EVT_MENU(FIVEL_EXIT, StageFrame::OnExit)
    EVT_MENU(FIVEL_RELOAD_SCRIPT, StageFrame::OnReloadScript)
    EVT_MENU(FIVEL_ABOUT, StageFrame::OnAbout)
    EVT_MENU(FIVEL_SHOW_LOG, StageFrame::OnShowLog)
    EVT_MENU(FIVEL_FULL_SCREEN, StageFrame::OnFullScreen)
    EVT_UPDATE_UI(FIVEL_FULL_SCREEN, StageFrame::UpdateUiFullScreen)
    EVT_MENU(FIVEL_DISPLAY_XY, StageFrame::OnDisplayXy)
    EVT_UPDATE_UI(FIVEL_DISPLAY_XY, StageFrame::UpdateUiDisplayXy)
    EVT_CLOSE(StageFrame::OnClose)
END_EVENT_TABLE()

StageFrame::StageFrame(const wxChar *inTitle, wxSize inSize)
    : wxFrame((wxFrame*) NULL, -1, inTitle,
              wxDefaultPosition, wxDefaultSize,
              wxMINIMIZE_BOX|wxSYSTEM_MENU|wxCAPTION)
{
    // Set up useful logging.
    mLogWindow = new wxLogWindow(this, "Application Log", FALSE);

    // Make our background black.  This should theoretically be handled
    // by 'background->SetBackgroundColour' below, but Windows takes a
    // fraction of a second to show that object.
    SetBackgroundColour(*wxBLACK);

    // Create a background panel to surround our stage with.  This keeps
    // life simple.
    wxPanel *background = new wxPanel(this);
    background->SetBackgroundColour(*wxBLACK);

    // Create a stage object to scribble on.  Use a sizer to center it
    // on the background.
    mStage = new Stage(background, this, inSize);

    // Align our stage within the background panel using a pair of
    // sizers.  I arrived at these parameters using trial and error.
    wxBoxSizer *v_sizer = new wxBoxSizer(wxVERTICAL);
    wxBoxSizer *h_sizer = new wxBoxSizer(wxHORIZONTAL);
    v_sizer->Add(h_sizer, 1 /* stretch */, wxALIGN_CENTER, 0);
    h_sizer->Add(mStage, 0 /* no stretch */, wxALIGN_CENTER, 0);
    background->SetSizer(v_sizer);

    // Set up our File menu.
    mFileMenu = new wxMenu();
    mFileMenu->Append(FIVEL_RELOAD_SCRIPT, "&Reload Script\tCtrl+R",
                      "Reload the currently executing 5L script.");
    mFileMenu->AppendSeparator();
    mFileMenu->Append(FIVEL_EXIT, "E&xit\tCtrl+Q", "Exit the application.");

    // Set up our View menu.
    mViewMenu = new wxMenu();
    mViewMenu->AppendCheckItem(FIVEL_FULL_SCREEN,
                               "&Full Screen\tCtrl+F",
                               "Use a full screen window.");
    mViewMenu->AppendSeparator();
    mViewMenu->AppendCheckItem(FIVEL_DISPLAY_XY, "Display Cursor &XY",
                               "Display the cursor's XY position.");

    // Set up our Window menu.
    mWindowMenu = new wxMenu();
    mWindowMenu->Append(FIVEL_SHOW_LOG, "Show &Log\tCtrl+L",
    			"Show application log window.");
    

    // Set up our Help menu.
    mHelpMenu = new wxMenu();
    mHelpMenu->Append(FIVEL_ABOUT, "&About",
                      "About the 5L multimedia system.");

    // Set up our menu bar.
    mMenuBar = new wxMenuBar();
    mMenuBar->Append(mFileMenu, "&File");
    mMenuBar->Append(mViewMenu, "&View");
    mMenuBar->Append(mWindowMenu, "&Window");
    mMenuBar->Append(mHelpMenu, "&Help");
    SetMenuBar(mMenuBar);

    // Add a tool bar.
    CreateToolBar();
    wxToolBar *tb = GetToolBar();
    tb->AddTool(FIVEL_RELOAD_SCRIPT, "Reload", wxBITMAP(tb_reload),
                "Reload Script");
    tb->AddSeparator();
    tb->AddCheckTool(FIVEL_DISPLAY_XY, "Display XY", wxBITMAP(tb_xy),
                     wxNullBitmap, "Display Cursor XY");
    tb->Realize();
	
    // Add a status bar with 1 field.  The 0 disables the resize thumb
    // at the lower right.
    CreateStatusBar(1, 0);

    // Resize the "client area" of the window (the part that's left over
    // after menus, status bars, etc.) to hold the stage.
    SetClientSize(inSize);
}

void StageFrame::OnExit()
{
    Close(TRUE);
}

void StageFrame::OnReloadScript()
{
    if (TInterpreterManager::HaveInstance())
    {
	TInterpreterManager *manager = TInterpreterManager::GetInstance();
	if (manager->FailedToLoad())
	{
	    // The previous attempt to load a script failed, so we need
	    // to try loading it again.
	    manager->RequestRetryLoadScript();
	}
	else
	{
	    // We have a running script.  Go ahead and reload it.
	    ASSERT(TInterpreter::HaveInstance());
	    TInterpreter *interp = TInterpreter::GetInstance();
	    manager->RequestReloadScript(interp->CurCardName().c_str());
	}
    }
	    
    SetStatusText("Script reloaded.");
}

void StageFrame::OnAbout()
{
    wxMessageDialog about(this,
                          "wx5L Prototype\n"
                          "Copyright 2002 The Trustees of Dartmouth College",
                          "About wx5L", wxOK);
    about.ShowModal();
}

void StageFrame::OnShowLog()
{
    mLogWindow->Show(TRUE);
}

void StageFrame::UpdateUiFullScreen(wxUpdateUIEvent &inEvent)
{
    inEvent.Check(IsFullScreen());
}

void StageFrame::OnFullScreen()
{
    if (IsFullScreen())
        ShowFullScreen(FALSE);
    else
    {
        ShowFullScreen(TRUE);
        // TODO - WXBUG - We need to raise above the Windows dock more
        // quickly.
    }
}

void StageFrame::UpdateUiDisplayXy(wxUpdateUIEvent &inEvent)
{
    inEvent.Check(mStage->IsDisplayingXy());
}

void StageFrame::OnDisplayXy()
{
    mStage->ToggleDisplayXy();
}

void StageFrame::OnClose(wxCloseEvent &inEvent)
{
    // If we've got an interpreter manager, we'll need to ask it to
    // shut down the application.
    if (TInterpreterManager::HaveInstance())
        TInterpreterManager::GetInstance()->RequestQuitApplication();

    // Detach this object from the application.  This will make various
    // assertions far more useful, and keep people from reaching us
    // through the application object.
    wxGetApp().DetachStageFrame();

    // Mark this object for destruction (as soon as the event queue
    // is empty).
    Destroy();
}


//=========================================================================
//  Stage Methods
//=========================================================================

BEGIN_EVENT_TABLE(Stage, wxWindow)
    EVT_MOTION(Stage::OnMouseMove)
    EVT_PAINT(Stage::OnPaint)
END_EVENT_TABLE()

Stage::Stage(wxWindow *inParent, StageFrame *inFrame, wxSize inStageSize)
    : wxWindow(inParent, -1, wxDefaultPosition, inStageSize),
      mFrame(inFrame), mStageSize(inStageSize),
      mOffscreenPixmap(inStageSize.GetWidth(), inStageSize.GetHeight(), -1),
      mIsDisplayingXy(false)
{
    SetBackgroundColour(*wxBLACK);
    ClearStage(*wxBLACK);
}

void Stage::OnMouseMove(wxMouseEvent &inEvent)
{
    if (mIsDisplayingXy)
    {
        wxClientDC dc(this);

        // Adapted from the drawing sample included with wxWindows.
        wxPoint pos = inEvent.GetPosition();
        long x = dc.DeviceToLogicalX(pos.x);
        long y = dc.DeviceToLogicalY(pos.y);
        wxString str;
        str.Printf("X: %d, Y: %d", (int) x, (int) y);
        mFrame->SetStatusText(str);
    }
}

void Stage::OnPaint(wxPaintEvent &inEvent)
{
    // Set up our drawing contexts.
    wxPaintDC screen_dc(this);
    wxMemoryDC offscreen_dc;
    offscreen_dc.SelectObject(mOffscreenPixmap);
    
    // Blit our offscreen pixmap to the screen.
    screen_dc.Blit(0, 0, mStageSize.GetWidth(), mStageSize.GetHeight(),
		   &offscreen_dc, 0, 0);
}

void Stage::InvalidateRect(const wxRect &inRect)
{
    Refresh(FALSE, &inRect);
}

wxColour Stage::GetColor(const GraphicsTools::Color &inColor)
{
    if (inColor.alpha)
	gDebugLog.Caution("Removing alpha channel from color");
    return wxColour(inColor.red, inColor.green, inColor.blue);
}

void Stage::ClearStage(const wxColor &inColor)
{
    wxMemoryDC dc;
    dc.SelectObject(mOffscreenPixmap);
    wxBrush brush(inColor, wxSOLID);
    dc.SetBackground(brush);
    dc.Clear();
    InvalidateRect(wxRect(0, 0,
			  mStageSize.GetWidth(), mStageSize.GetHeight()));
}

void Stage::DrawBitmap(const wxBitmap &inBitmap, wxCoord inX, wxCoord inY,
		       bool inTransparent)
{
    wxMemoryDC dc;
    dc.SelectObject(mOffscreenPixmap);
    dc.DrawBitmap(inBitmap, inX, inY, inTransparent);
    InvalidateRect(wxRect(inX, inY,
			  inX + inBitmap.GetWidth(),
			  inY + inBitmap.GetHeight()));
}
