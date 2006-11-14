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

#include "TamaleHeaders.h"

#include <wx/laywin.h>
#include <wx/config.h>

#include "TVersion.h"
#include "TInterpreter.h"
#include "doc/Document.h"
#include "doc/TamaleProgram.h"
#include "CrashReporter.h"
#include "TDeveloperPrefs.h"

#include "AppConfig.h"
#include "AppGlobals.h"
#include "AppGraphics.h"
#include "FiveLApp.h"
#include "StageFrame.h"
#include "Stage.h"
#include "ProgramTree.h"
#include "LocationBox.h"
#include "FancyStatusBar.h"
#include "TestHarness.h"
#include "Listener.h"
#include "Timecoder.h"
#include "ScriptEditor.h"
#include "GuiUtil.h"
#include "dlg/ProgramPropDlg.h"
#include "dlg/AdjustScreenDlg.h"
#include "dlg/AdjustScreenConfirmDlg.h"


USING_NAMESPACE_FIVEL

//=========================================================================
//  StageBackground
//=========================================================================

///  This class implements the wxWindow *behind* the stage.  It has few
///  duties other than (1) being black and (2) keeping the stage centered.
class StageBackground : public wxSashLayoutWindow
{
	DECLARE_EVENT_TABLE();

	StageFrame *mStageFrame;
	Stage *mStage;

	void OnSize(wxSizeEvent& event);

public:
	StageBackground(StageFrame *inStageFrame);
	void UpdateColor();
	void CenterStage(Stage *inStage);

	void UpdateStagePosition();
};

BEGIN_EVENT_TABLE(StageBackground, wxSashLayoutWindow)
    EVT_SIZE(StageBackground::OnSize)
END_EVENT_TABLE()

StageBackground::StageBackground(StageFrame *inStageFrame)
	: wxSashLayoutWindow(inStageFrame, -1, wxDefaultPosition, wxDefaultSize,
						 wxNO_BORDER),
	  mStageFrame(inStageFrame), mStage(NULL)
{
	if (mStageFrame->IsFullScreen())
		SetBackgroundColour(STAGE_BACKGROUND_COLOR);
	else
		SetBackgroundColour(STAGE_BACKGROUND_COLOR_NEUTRAL);
}

void StageBackground::UpdateStagePosition()
{
	if (mStage)
	{
		// Calling CentreOnParent produces weird results when the size
		// of stage is exactly equal to the size of background (and I've
		// tracked down enough wxWindows bugs this afternoon).  So let's
		// just do this by hand.
		wxSize bg_size = GetClientSize();
		wxSize stage_size = mStage->GetClientSize();
		int xpos = (bg_size.GetWidth() - stage_size.GetWidth()) / 2;
		int ypos = (bg_size.GetHeight() - stage_size.GetHeight()) / 2;
		mStage->Move(xpos, ypos);
	}
}

void StageBackground::OnSize(wxSizeEvent &WXUNUSED(event))
{
	UpdateStagePosition();
}

void StageBackground::UpdateColor()
{
	// We're called immediately *before* toggling ShowFullScreen,
	// so we need to set our background color to what we want it
	// to be *after* the call.  So this test is backwards...
	if (!mStageFrame->IsFullScreen())
		SetBackgroundColour(STAGE_BACKGROUND_COLOR);
	else
		SetBackgroundColour(STAGE_BACKGROUND_COLOR_NEUTRAL);
}

void StageBackground::CenterStage(Stage *inStage)
{
	ASSERT(!mStage);
	mStage = inStage;
	UpdateStagePosition();
}


//=========================================================================
//  StageFrame Methods
//=========================================================================

BEGIN_EVENT_TABLE(StageFrame, SashFrame)
    EVT_MENU(FIVEL_EXIT, StageFrame::OnExit)

    EVT_UPDATE_UI(FIVEL_NEW_PROGRAM, StageFrame::UpdateUiNewProgram)
    EVT_MENU(FIVEL_NEW_PROGRAM, StageFrame::OnNewProgram)
    EVT_UPDATE_UI(FIVEL_OPEN_PROGRAM, StageFrame::UpdateUiOpenProgram)
    EVT_MENU(FIVEL_OPEN_PROGRAM, StageFrame::OnOpenProgram)
    EVT_UPDATE_UI(FIVEL_SAVE_PROGRAM, StageFrame::UpdateUiSaveProgram)
    EVT_MENU(FIVEL_SAVE_PROGRAM, StageFrame::OnSaveProgram)
    EVT_UPDATE_UI(FIVEL_EDIT_SCRIPTS, StageFrame::UpdateUiDevTool)
    EVT_MENU(FIVEL_EDIT_SCRIPTS, StageFrame::OnEditScripts)
    EVT_UPDATE_UI(FIVEL_RELOAD_SCRIPTS, StageFrame::OnUpdateUiReloadScripts)
    EVT_MENU(FIVEL_RELOAD_SCRIPTS, StageFrame::OnReloadScripts)
    EVT_UPDATE_UI(FIVEL_RUN_TESTS, StageFrame::UpdateUiDevTool)
    EVT_MENU(FIVEL_RUN_TESTS, StageFrame::OnRunTests)

    EVT_UPDATE_UI(FIVEL_ABOUT, StageFrame::UpdateUiDevTool)
    EVT_MENU(FIVEL_ABOUT, StageFrame::OnAbout)
    EVT_UPDATE_UI(FIVEL_SHOW_LOG, StageFrame::UpdateUiDevTool)
    EVT_MENU(FIVEL_SHOW_LOG, StageFrame::OnShowLog)
    EVT_UPDATE_UI(FIVEL_SHOW_LISTENER, StageFrame::UpdateUiDevTool)
    EVT_MENU(FIVEL_SHOW_LISTENER, StageFrame::OnShowListener)
    EVT_UPDATE_UI(FIVEL_SHOW_TIMECODER, StageFrame::UpdateUiDevTool)
    EVT_MENU(FIVEL_SHOW_TIMECODER, StageFrame::OnShowTimecoder)
    EVT_UPDATE_UI(FIVEL_FULL_SCREEN, StageFrame::UpdateUiFullScreen)
    EVT_MENU(FIVEL_FULL_SCREEN, StageFrame::OnFullScreen)
    EVT_UPDATE_UI(FIVEL_DISPLAY_XY, StageFrame::UpdateUiDisplayXy)
    EVT_MENU(FIVEL_DISPLAY_XY, StageFrame::OnDisplayXy)
    EVT_UPDATE_UI(FIVEL_DISPLAY_GRID, StageFrame::UpdateUiDisplayGrid)
    EVT_MENU(FIVEL_DISPLAY_GRID, StageFrame::OnDisplayGrid)
    EVT_UPDATE_UI(FIVEL_DISPLAY_BORDERS, StageFrame::UpdateUiDisplayBorders)
    EVT_MENU(FIVEL_DISPLAY_BORDERS, StageFrame::OnDisplayBorders)
    EVT_UPDATE_UI(FIVEL_PROPERTIES, StageFrame::UpdateUiProperties)
    EVT_MENU(FIVEL_PROPERTIES, StageFrame::OnProperties)
    EVT_UPDATE_UI(FIVEL_EDIT_MODE, StageFrame::UpdateUiEditMode)
    EVT_MENU(FIVEL_EDIT_MODE, StageFrame::OnEditMode)
    EVT_UPDATE_UI(FIVEL_EDIT_CARD_SCRIPT,
                  StageFrame::UpdateUiEditCardScript)
    EVT_MENU(FIVEL_EDIT_CARD_SCRIPT, StageFrame::OnEditCardScript)
    EVT_UPDATE_UI(FIVEL_JUMP_CARD, StageFrame::UpdateUiJumpCard)
    EVT_MENU(FIVEL_JUMP_CARD, StageFrame::OnJumpCard)
    EVT_UPDATE_UI(FIVEL_STOP_MOVIES, StageFrame::UpdateUiStopMovies)
    EVT_MENU(FIVEL_STOP_MOVIES, StageFrame::OnStopMovies)

    EVT_ACTIVATE(StageFrame::OnActivate)
	EVT_SIZE(StageFrame::OnSize)
    EVT_CLOSE(StageFrame::OnClose)
END_EVENT_TABLE()

StageFrame::StageFrame(wxSize inSize)
    : SashFrame((wxFrame*) NULL, -1, wxGetApp().GetAppName(),
                "StageFrame", wxDefaultSize,
                wxDEFAULT_FRAME_STYLE),
	  mDocument(NULL),
      mAreFullScreenOptionsActive(false),
      mCurrentFullScreenDisplayId(wxNOT_FOUND),
	  mIsUpdatingVideoMode(false)
{
    // Set up useful logging.
    mLogWindow = new wxLogWindow(this, "Application Log", FALSE);

	// We create our tool windows on demand.
	for (int i = 0; i < TOOL_COUNT; i++)
		mToolWindows[i] = NULL;

	// Get an appropriate icon for this window.
    SetIcon(wxICON(ic_5L));

    // Make our background black.  This should theoretically be handled
    // by 'background->SetBackgroundColour' below, but Windows takes a
    // fraction of a second to show that object.
    SetBackgroundColour(STAGE_FRAME_COLOR);

	// Create a sash window holding a tree widget.
	mProgramTree = new ProgramTree(this, FIVEL_PROGRAM_TREE);
	mProgramTree->SetOrientation(wxLAYOUT_VERTICAL);
	mProgramTree->SetAlignment(wxLAYOUT_LEFT);
	mProgramTree->SetSashVisible(wxSASH_RIGHT, TRUE);

    // Create a background panel to surround our stage with.  This keeps
    // life simple.
    mBackground = new StageBackground(this);
    SetMainWindow(mBackground);

    // Create a stage object to scribble on, and center it.
    mStage = new Stage(mBackground, this, inSize);
	mBackground->CenterStage(mStage);
	mStage->Hide();

    // Set up our File menu.
    mFileMenu = new wxMenu();
    mFileMenu->Append(FIVEL_NEW_PROGRAM, "&New Program...\tCtrl+N",
                      "Create a new Tamale program.");
    mFileMenu->Append(FIVEL_OPEN_PROGRAM, "&Open Program...\tCtrl+O",
                      "Open an existing Tamale program.");
    mFileMenu->Append(FIVEL_SAVE_PROGRAM, "&Save Program\tCtrl+S",
                      "Save the current Tamale program.");
    mFileMenu->AppendSeparator();
    mFileMenu->Append(FIVEL_EDIT_SCRIPTS, "&Edit Scripts\tCtrl+E",
                      "Edit the Tamale script files for this program.");
    mFileMenu->Append(FIVEL_RELOAD_SCRIPTS, "&Reload Scripts\tCtrl+R",
                      "Reload the currently executing Tamale scripts.");
    mFileMenu->AppendSeparator();
    mFileMenu->Append(FIVEL_RUN_TESTS, "Run &Tests\tCtrl+T",
                      "Run test cases for Tamale and/or current script.");
    mFileMenu->AppendSeparator();
    mFileMenu->Append(FIVEL_EXIT, "E&xit", "Exit the application.");

    // Set up our Card menu.
    mCardMenu = new wxMenu();
    mCardMenu->Append(FIVEL_EDIT_MODE, "&Edit Card\tCtrl+Space",
                      "Enter or exit card-editing mode.");
    mCardMenu->Append(FIVEL_EDIT_CARD_SCRIPT, "Edit Card Sc&ript\tAlt+.",
                      "Edit this card's script.");
    mCardMenu->Append(FIVEL_JUMP_CARD, "&Jump to Card...\tCtrl+J",
                      "Jump to a specified card by name.");
    mCardMenu->Append(FIVEL_STOP_MOVIES, "&Stop Movies\tEsc",
                      "Stop any playing movies.");

    // Set up our View menu.  Only include the "Full Screen" item on
	// platforms where it's likely to work.
    mViewMenu = new wxMenu();
#if CONFIG_ENABLE_FULL_SCREEN
    mViewMenu->AppendCheckItem(FIVEL_FULL_SCREEN,
                               "&Full Screen\tCtrl+F",
                               "Use a full screen window.");
    mViewMenu->AppendSeparator();
#endif // CONFIG_ENABLE_FULL_SCREEN
    mViewMenu->AppendCheckItem(FIVEL_DISPLAY_XY, "Display Cursor &XY",
                               "Display the cursor's XY position.");
    mViewMenu->AppendCheckItem(FIVEL_DISPLAY_GRID, "Display &Grid\tCtrl+G",
                               "Display a grid over the card.");
    mViewMenu->AppendCheckItem(FIVEL_DISPLAY_BORDERS,
                               "Display &Borders\tCtrl+B",
                               "Display the borders of interactive elements.");
    mViewMenu->AppendSeparator();
	mViewMenu->Append(FIVEL_PROPERTIES,
					  "&Properties...\tAlt+Enter",
					  "Edit the properties of the selected object.");

    // Set up our Window menu.
    mWindowMenu = new wxMenu();
    mWindowMenu->Append(FIVEL_SHOW_LISTENER, "Show &Listener\tCtrl+L",
                        "Show interactive script listener.");
    mWindowMenu->Append(FIVEL_SHOW_TIMECODER, "Show &Timecoder\tCtrl+T",
                        "Show the movie timecoding utility.");
    mWindowMenu->Append(FIVEL_SHOW_LOG, "Show &Debug Log",
                        "Show application debug log window.");

    // Set up our Help menu.
    mHelpMenu = new wxMenu();
    mHelpMenu->Append(FIVEL_ABOUT, "&About",
                      "About the 5L multimedia system.");

    // Set up our menu bar.
    mMenuBar = new wxMenuBar();
    mMenuBar->Append(mFileMenu, "&File");
    mMenuBar->Append(mCardMenu, "&Card");
    mMenuBar->Append(mViewMenu, "&View");
    mMenuBar->Append(mWindowMenu, "&Window");
    mMenuBar->Append(mHelpMenu, "&Help");
    SetMenuBar(mMenuBar);

    // Add a tool bar.
    CreateToolBar();
    wxToolBar *tb = GetToolBar();
    tb->AddTool(FIVEL_RELOAD_SCRIPTS, "Reload", wxBITMAP(tb_reload),
                "Reload Scripts");
	mLocationBox = new LocationBox(tb);
	tb->AddControl(mLocationBox);
    tb->AddSeparator();
    tb->AddCheckTool(FIVEL_DISPLAY_XY, "Display XY", wxBITMAP(tb_xy),
                     wxNullBitmap, "Display Cursor XY");
    tb->AddCheckTool(FIVEL_DISPLAY_GRID, "Display Grid", wxBITMAP(tb_grid),
                     wxNullBitmap, "Display Grid");
    tb->AddCheckTool(FIVEL_DISPLAY_BORDERS,
                     "Display Borders", wxBITMAP(tb_borders),
                     wxNullBitmap, "Display Borders");
    tb->Realize();
        
    // Add a status bar.
	SetStatusBar(new FancyStatusBar(this));

    // Resize the "client area" of the window (the part that's left over
    // after menus, status bars, etc.) to hold the stage and the program
	// tree.
    SetClientSize(wxSize(inSize.GetWidth() + mProgramTree->GetMinimumSizeX(),
						 inSize.GetHeight()));

	// Don't allow the window to get any smaller.
	// XXX - This probably isn't a reliable way to do this.
	mMinimumFrameSize = GetSize();
	SetSizeHints(mMinimumFrameSize.GetWidth(), mMinimumFrameSize.GetHeight());

	// Re-load our saved frame layout.  We can't do this until after
	// our setup is completed.
	LoadFrameLayout();
}

void StageFrame::LoadSashLayout(wxConfigBase *inConfig) {
	long program_tree_width = mProgramTree->GetMinimumSizeX();
	inConfig->Read("ProgramTreeWidth", &program_tree_width);
	mProgramTree->SetDefaultWidth(program_tree_width);
}

void StageFrame::SaveSashLayout(wxConfigBase *inConfig) {
	inConfig->Write("ProgramTreeWidth", mProgramTree->GetSize().GetWidth());
}

#if !wxUSE_DISPLAY
void StageFrame::FindBestFullScreenVideoMode() {}
void StageFrame::SetFullScreenVideoMode() {}
void StageFrame::ResetVideoMode() {}
#else // wxUSE_DISPLAY

void StageFrame::FindBestFullScreenVideoMode()
{
    // Get the number of the current display. 
    //
    // XXX - I'm not sure that wxWidgets' underlying toplevel.cpp resize
    // code (in ShowFullScreen) does something reasonable when the display
    // is wxNOT_FOUND.
    int current = wxDisplay::GetFromWindow(this);
    if (current == wxNOT_FOUND) {
        gLog.Caution("Can't find display for stage window, assuming primary");
        current = 0;
    }

    // If we've already analyzed this display, we don't need to do it again.
    if (mCurrentFullScreenDisplayId == current)
        return;
    mCurrentFullScreenDisplayId = current;

	// TODO - DirectX support is broken in wxWindows.  A fair bit of work
	// is required to fix it.  For now, leave it alone.
	//wxDisplay::UseDirectX(true);
	wxDisplay display(mCurrentFullScreenDisplayId);

    // Calculate the aspect ratio.  We don't use this yet, but we may
    // in the future, so we'll just log it for now.
    wxRect current_geom(display.GetGeometry());
    float aspect = 1.0*current_geom.GetWidth()/current_geom.GetHeight();
    gDebugLog.Log("Current screen aspect ratio: %.2f", aspect);

    // Check for old-style multihead support, where two monitors appear as
    // one double-wide monitor with an 8:3 aspect ratio.  We always want to
    // pick a 4:3 mode on such monitors, which will deactivate the second
    // screen instead of splitting us between both.  Of course, if there's
    // more than one display, we have to assume any funny-shaped monitors
    // really are funny-shaped.  And we don't try to be clever about
    // multiple joined 3:4 monitors or other oddities; it's too much work.
    if ((aspect > (8.0/3.0)*0.95) && wxDisplay::GetCount() == 1)
        gDebugLog.Log("Looks like two monitors combined into one display");

	// Search for the most promising mode.
	wxArrayVideoModes modes = display.GetModes();
	mFullScreenVideoMode = wxDefaultVideoMode;
	wxSize min_size = mStage->GetStageSize();
	for (size_t i = 0; i < modes.GetCount(); i++)
	{
		wxVideoMode &mode = modes[i];
		gDebugLog.Log("Found mode: %dx%d, %d bit, %d Hz (aspect %.2f)",
					  mode.w, mode.h, mode.bpp, mode.refresh,
                      1.0*mode.w/mode.h);

		// See if the video mode is good enough to display our content.
		if (mode.bpp >= 24 &&
			mode.w >= min_size.GetWidth() && mode.h >= min_size.GetHeight())
		{
			// If the video mode is better than what we have, keep.
			if (mFullScreenVideoMode == wxDefaultVideoMode ||
				(mode.w <= mFullScreenVideoMode.w &&
				 mode.h <= mFullScreenVideoMode.h))
			{
				mFullScreenVideoMode = mode;
			}
		}
	}

	// DANGER - We must clear the refresh rate here.  This causes the OS
	// to pick a refresh rate for us, depending on what the OS and the
	// video card drivers think is safe.  We can't simply choose the highest
	// advertised refresh rate, because it's *still* (in 2003) possible to
	// damage monitors by setting high refresh rates.  Unfortunately, this
	// may cause Windows to choose ridiculously low refresh rates on some
	// systems.  That's the price of safety.
	mFullScreenVideoMode.refresh = 0;

	// Log the video mode we chose.
	if (mFullScreenVideoMode == wxDefaultVideoMode)
		gLog.Log("Screen resizing not available.");
	else
		gLog.Log("Best full screen mode: %dx%d, %d bit",
				 mFullScreenVideoMode.w, mFullScreenVideoMode.h,
				 mFullScreenVideoMode.bpp);
}

void StageFrame::SetFullScreenVideoMode() {
    FindBestFullScreenVideoMode();
    ComputeResizePrefName();
	if (mFullScreenVideoMode != wxDefaultVideoMode) {
        bool should_confirm;
        if (ShouldResizeScreen(should_confirm)) {
            // Resize the screen.
            wxDisplay display(mCurrentFullScreenDisplayId);
            display.ChangeMode(mFullScreenVideoMode);

            // Make sure the user actually likes the new mode.
            if (should_confirm && !ConfirmScreenSize())
                display.ResetMode();                
        }
	}
	mBackground->UpdateStagePosition();
}

void StageFrame::ResetVideoMode()
{
	if (mFullScreenVideoMode != wxDefaultVideoMode)
	{
		wxDisplay display(mCurrentFullScreenDisplayId);
		display.ResetMode();
	}		
}

void StageFrame::ComputeResizePrefName() {
    mResizePrefName = "";
    size_t count = wxDisplay::GetCount();
    for (size_t i = 0; i < count; i++) {
        // Separate monitors with a colon.
        if (mResizePrefName != "")
            mResizePrefName += ":";

        // Store width and height for each monitor.
        wxRect r(wxDisplay(i).GetGeometry());
        wxString geometry;
        geometry.sprintf("%dx%d", r.GetWidth(), r.GetHeight());
        mResizePrefName += geometry;

        // Mark the current display in our list.
        if (i == mCurrentFullScreenDisplayId)
            mResizePrefName += "*";
    }
    mResizePrefName = "/Resize/" + mResizePrefName;
}

bool StageFrame::ShouldResizeScreen(bool &outShouldConfirm) {
	wxConfigBase *config = wxConfigBase::Get();

    // Unless the user has pressed the shift key, we first check the
    // registry to see if we know the answer.
    if (::wxGetKeyState(WXK_SHIFT) == false) {
        bool should_resize;
        if (config->Read(mResizePrefName, &should_resize)) {
            outShouldConfirm = false;
            return should_resize;
        }
    }

    // We're going to need to ask the user.  But just in case the engine
    // crashes when it resizes the screen (or does something else
    // childish), we'll default our registry key to false.  We'll change it
    // later if the user decides to resize.
    config->Write(mResizePrefName, false);

    // We'll want to confirm this change.
    outShouldConfirm = true;
    
    // OK, now we can actually ask the user.
    AdjustScreenDlg dlg(this);
    return (dlg.ShowModal() == wxID_YES);
}

bool StageFrame::ConfirmScreenSize() {
	wxConfigBase *config = wxConfigBase::Get();
    AdjustScreenConfirmDlg dlg(this);
    bool is_confirmed = (dlg.ShowModal() == wxID_YES);
    if (is_confirmed)
        config->Write(mResizePrefName, true);
    return is_confirmed;
}

#endif // wxUSE_DISPLAY

bool StageFrame::ShowFullScreen(bool show, long style)
{
	mProgramTree->Show(!show);
	mBackground->UpdateColor();
	if (!show)
	{
        UpdateVideoMode(show, IsIconized());
		SetSizeHints(mMinimumFrameSize.GetWidth(),
					 mMinimumFrameSize.GetHeight());
	}
	bool result = SashFrame::ShowFullScreen(show, style);
	if (show)
	{
		// Set our size hints to exactly the stage size, so we can
		// avoid inappropriate padding in full screen mode.
		SetSizeHints(mStage->GetSize().GetWidth(),
					 mStage->GetSize().GetHeight());
        UpdateVideoMode(show, IsIconized());
	}
	return result;
}

void StageFrame::Iconize(bool iconize) {
    if (iconize)
        UpdateVideoMode(IsFullScreen(), iconize);
    SashFrame::Iconize(iconize);
    if (!iconize)
        UpdateVideoMode(IsFullScreen(), iconize);
}

void StageFrame::UpdateVideoMode(bool inIsFullScreen, bool inIsIconized) {
	// If we've been called recursively, bail now.
	if (mIsUpdatingVideoMode)
		return;

	// Set up a flag so we can detect recursive calls.
	StValueRestorer<bool> saved_value(mIsUpdatingVideoMode);
	mIsUpdatingVideoMode = true;

    bool want_full_screen_options = (inIsFullScreen && !inIsIconized);   
    if (want_full_screen_options != mAreFullScreenOptionsActive) {
        if (want_full_screen_options) {
            SetFullScreenVideoMode();
            HideSystemWindows();
            mAreFullScreenOptionsActive = true;
        } else {
            ShowSystemWindows();
            ResetVideoMode();
            mAreFullScreenOptionsActive = false;
        }
    }
}

void StageFrame::NewDocument()
{
	wxASSERT(mDocument == NULL);
	wxDirDialog dlg(this, "Add Tamale data to an existing program folder:");

	wxConfigBase *config = wxConfigBase::Get();
	wxString recent;
	if (config->Read("/Recent/DocPath", &recent))
		dlg.SetPath(recent);

	if (dlg.ShowModal() == wxID_OK)
	{
		wxString file = dlg.GetPath();
		mDocument = new Document(file.mb_str());
		config->Write("/Recent/DocPath", file);

		ProgramPropDlg prop_dlg(this, mDocument->GetTamaleProgram());
		prop_dlg.ShowModal();

		SetObject(mDocument->GetTamaleProgram());
		mProgramTree->RegisterDocument(mDocument);
        CrashReporter::GetInstance()->RegisterDocument(mDocument);
		mStage->Show();
	}
}

void StageFrame::OpenDocument()
{
	wxASSERT(mDocument == NULL);
	wxFileDialog dlg(this, "Open a Tamale program folder:",
                     "", "", "Tamale program (data.tam)|data.tam",
                     wxOPEN|wxHIDE_READONLY);

    // Set the dialog's default path to last file opened, if any.
	wxConfigBase *config = wxConfigBase::Get();
	wxString recent;
	if (config->Read("/Recent/DocPath", &recent))
		dlg.SetPath(recent);

	if (dlg.ShowModal() == wxID_OK)
	{
        // We call GetDirectory instead of GetPath because we don't actually
        // care about the file itself--it's just a known file within a valid
        // Tamale program directory.
		wxString dir = dlg.GetDirectory();
        OpenDocument(dir);

        // We want the full path here--not just the directory--to save
        // typing the next time the user opens this document.
		config->Write("/Recent/DocPath", dlg.GetPath());
	}
}

void StageFrame::OpenDocument(const wxString &inDirPath) {
    mDocument = new Document(inDirPath.mb_str(), Document::OPEN);
    SetObject(mDocument->GetTamaleProgram());
    mProgramTree->RegisterDocument(mDocument);
    CrashReporter::GetInstance()->RegisterDocument(mDocument);
    mStage->MaybeShowSplashScreen();
    mStage->Show();
}

void StageFrame::LoadIcon(const std::string &inName, wxIconBundle &ioIcons,
                          bool &ioHaveIcon)
{
    wxBitmap bitmap = mStage->GetScriptGraphic(inName);
    if (bitmap.Ok()) {
        wxIcon icon;
        icon.CopyFromBitmap(bitmap);
        ioIcons.AddIcon(icon);
        ioHaveIcon = true;
    }
}

void StageFrame::ObjectChanged()
{
    // Load our icons, if we have any.
    wxIconBundle icons;
    bool have_icon = false;
    LoadIcon("icon32.png", icons, have_icon);
    LoadIcon("icon16.png", icons, have_icon);
    if (have_icon)
        SetIcons(icons);

    // Get the name of the script we're running.
    std::string script_name = GetObject()->GetString("name").c_str();

    // Tell our FileSystem module the script name, and make sure that
    // we actually have a script data directory.
    // TODO - Creating the directory should be handled by FileSystem,
    // but it doesn't know how, and can't see wxMkdir.
    FileSystem::SetScriptName(script_name);
    wxString script_data_dir =
        FileSystem::GetScriptDataDirectory().ToNativePathString().c_str();
    if (!::wxDirExists(script_data_dir) && !::wxMkdir(script_data_dir))
        gLog.FatalError(("Can't create " + script_data_dir).mb_str());

    // Update our application name.
    if (TInterpreterManager::IsInRuntimeMode()) {
        // If we're in runtime mode, only show the name of the script in
        // the task bar, and change our application name appropriately.
        wxGetApp().SetAppName(script_name.c_str());
        SetTitle(wxGetApp().GetAppName());
    } else {
        // If we're in edit mode, show the name of the application and
        // the path of the document, too.
        SetTitle(wxString(script_name.c_str()) +
			     " - " + wxGetApp().GetAppName() + " - [" +
                 wxString(mDocument->GetSavePath().c_str()) + "]");
    }
}

void StageFrame::ObjectDeleted()
{
    FileSystem::SetScriptName("");

    // Only reset our name and icon if we're not in runtime mode--there's
    // no point revealing our real application name or icon to the user.
    if (!TInterpreterManager::IsInRuntimeMode()) {
        SetTitle(wxGetApp().GetAppName());
        SetIcon(wxICON(ic_5L));
    }
}

bool StageFrame::ShouldDisableScreenSaver() {
    // Feel free to change this to any reasonable behavior.
    return IsFullScreen() || mStage->IsMediaPlaying();
}

#ifdef FIVEL_PLATFORM_WIN32

bool StageFrame::MSWTranslateMessage(WXMSG* pMsg) {
    // HACK - We need to forcibly update our UI here, because Windows won't
    // update our accelerators properly if the menu bar has never been
    // shown.
    //
    // It might be more efficient for us to do this at idle time, actually,
    // but I'm not sure if the UI data might get slightly stale and cause
    // assertion failures.
    if (TInterpreterManager::IsInRuntimeMode())
        UpdateWindowUI();
    return SashFrame::MSWTranslateMessage(pMsg);
}

WXLRESULT StageFrame::MSWWindowProc(WXUINT message, WXWPARAM wParam,
                                    WXLPARAM lParam)
{
    // Intercept attempts to turn on the screen-saver, and disable them
    // if that's what we want.  This isn't terribly well-documented
    // (except for Delphi programmers, for some reason), but it appears
    // to work fine as is.
    // 
    // TODO - Note that this only works if the user hasn't turned on
    // password protected screen savers, which require a heavier-weight
    // fix.  See GuiUtil.cpp.  The only purpose served by this code at the
    // moment is to disable some screensavers in development mode while
    // video is playing.
    if (message == WM_SYSCOMMAND && wParam == SC_SCREENSAVE) {
        if (ShouldDisableScreenSaver())
            return -1;
    }
    return SashFrame::MSWWindowProc(message, wParam, lParam);
}

#endif // FIVEL_PLATFORM_WIN32

bool StageFrame::AreDevToolsAvailable() {
    return (!TInterpreterManager::IsInRuntimeMode()
            || gDeveloperPrefs.GetPref(DEVTOOLS) == DEVTOOLS_ENABLED);
}

void StageFrame::UpdateUiDevTool(wxUpdateUIEvent &inEvent) {
    inEvent.Enable(AreDevToolsAvailable());
}

void StageFrame::OnExit(wxCommandEvent &inEvent)
{
    Close(FALSE);
}

void StageFrame::UpdateUiNewProgram(wxUpdateUIEvent &inEvent)
{
	//inEvent.Enable(mDocument == NULL);
    /// \todo Turn "new program" back on when it works better.
    inEvent.Enable(false);
}

void StageFrame::OnNewProgram(wxCommandEvent &inEvent)
{
	BEGIN_EXCEPTION_TRAPPER()
		NewDocument();
	END_EXCEPTION_TRAPPER(TException::ReportException)
}

void StageFrame::UpdateUiOpenProgram(wxUpdateUIEvent &inEvent)
{
	inEvent.Enable(AreDevToolsAvailable() && mDocument == NULL);
}

void StageFrame::OnOpenProgram(wxCommandEvent &inEvent)
{
	BEGIN_EXCEPTION_TRAPPER()
		OpenDocument();
	END_EXCEPTION_TRAPPER(TException::ReportException)
}

void StageFrame::UpdateUiSaveProgram(wxUpdateUIEvent &inEvent)
{
	inEvent.Enable(AreDevToolsAvailable() &&
                   mDocument != NULL && mDocument->IsDirty());
}

void StageFrame::OnSaveProgram(wxCommandEvent &inEvent)
{
	mDocument->Save();
}

void StageFrame::OnEditScripts(wxCommandEvent &inEvent)
{
    ScriptEditor::EditScripts();
}

void StageFrame::OnUpdateUiReloadScripts(wxUpdateUIEvent &inEvent)
{
    if (TInterpreterManager::HaveInstance()) {
        TInterpreterManager *manager = TInterpreterManager::GetInstance();
        inEvent.Enable(AreDevToolsAvailable() &&
                       manager->InterpreterHasBegun());
    } else {
        inEvent.Enable(false);
    }
}

void StageFrame::OnReloadScripts(wxCommandEvent &inEvent)
{
    if (TInterpreterManager::HaveInstance())
    {
        // Save our documents and focus our stage.
        if (!ScriptEditor::SaveAllForReloadScript())
            return;
        mStage->SetFocus();

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

void StageFrame::OnRunTests(wxCommandEvent &inEvent)
{
	TestHarness::GetInstance()->RunTests();
}

void StageFrame::OnAbout(wxCommandEvent &inEvent)
{
    wxMessageDialog about(this,
                          VERSION_STRING "\n"
                          TAMALE_COPYRIGHT_NOTICE,
                          "About Tamale", wxOK);
    about.ShowModal();
}

void StageFrame::OnShowLog(wxCommandEvent &inEvent)
{
    mLogWindow->Show(TRUE);
}

void StageFrame::OnShowListener(wxCommandEvent &inEvent)
{
	if (!mToolWindows[TOOL_LISTENER])
		mToolWindows[TOOL_LISTENER] = new Listener(this);
	if (!mToolWindows[TOOL_LISTENER]->IsShown())
		mToolWindows[TOOL_LISTENER]->Show();
	mToolWindows[TOOL_LISTENER]->Raise();
}

void StageFrame::OnShowTimecoder(wxCommandEvent &inEvent)
{
	if (!mToolWindows[TOOL_TIMECODER])
		mToolWindows[TOOL_TIMECODER] = new Timecoder(this);
	if (!mToolWindows[TOOL_TIMECODER]->IsShown())
		mToolWindows[TOOL_TIMECODER]->Show();
	mToolWindows[TOOL_TIMECODER]->Raise();	
}

void StageFrame::UpdateUiFullScreen(wxUpdateUIEvent &inEvent)
{
    inEvent.Check(IsFullScreen());
    inEvent.Enable(AreDevToolsAvailable());
}

void StageFrame::OnFullScreen(wxCommandEvent &inEvent)
{
    if (IsFullScreen()) {
        // If we leave full-screen mode, make sure runtime mode is turned
        // off, too.  This can happen if we boot into runtime mode but
        // have devtools enabled, and a developer wants to turn the
        // runtime back into an editor for debugging.
        if (TInterpreterManager::IsInRuntimeMode())
            TInterpreterManager::SetRuntimeMode(false);
        ShowFullScreen(FALSE);
    } else {
        ShowFullScreen(TRUE);
        // TODO - WXBUG - We need to raise above the Windows dock more
        // quickly.
    }
}

void StageFrame::UpdateUiDisplayXy(wxUpdateUIEvent &inEvent)
{
    inEvent.Check(mStage->IsDisplayingXy());
    inEvent.Enable(AreDevToolsAvailable());
}

void StageFrame::OnDisplayXy(wxCommandEvent &inEvent)
{
    mStage->ToggleDisplayXy();
}

void StageFrame::UpdateUiDisplayGrid(wxUpdateUIEvent &inEvent)
{
    inEvent.Check(mStage->IsDisplayingGrid());
    inEvent.Enable(AreDevToolsAvailable());
}

void StageFrame::OnDisplayGrid(wxCommandEvent &inEvent)
{
    mStage->ToggleDisplayGrid();
}

void StageFrame::UpdateUiDisplayBorders(wxUpdateUIEvent &inEvent)
{
    inEvent.Check(mStage->IsDisplayingBorders());
    inEvent.Enable(AreDevToolsAvailable());
}

void StageFrame::OnDisplayBorders(wxCommandEvent &inEvent)
{
    mStage->ToggleDisplayBorders();
}

void StageFrame::UpdateUiProperties(wxUpdateUIEvent &inEvent)
{
	inEvent.Enable(AreDevToolsAvailable() && mDocument != NULL);
}

void StageFrame::OnProperties(wxCommandEvent &inEvent)
{
	ProgramPropDlg prop_dlg(this, mDocument->GetTamaleProgram());
	prop_dlg.ShowModal();
}

void StageFrame::UpdateUiEditMode(wxUpdateUIEvent &inEvent)
{
	if (AreDevToolsAvailable() &&
        mDocument != NULL &&
        mStage->IsScriptInitialized())
	{
		inEvent.Enable(TRUE);
		if (mStage->IsInEditMode())
			inEvent.SetText("&Run Card\tCtrl+Space");
		else
			inEvent.SetText("&Edit Card\tCtrl+Space");
	}
	else
	{
		inEvent.Enable(FALSE);
	}
}

void StageFrame::OnEditMode(wxCommandEvent &inEvent)
{
	mStage->SetEditMode(!mStage->IsInEditMode());
}

void StageFrame::UpdateUiEditCardScript(wxUpdateUIEvent &inEvent) {
    inEvent.Enable(AreDevToolsAvailable() &&
                   mStage->IsScriptInitialized() &&
                   TInterpreter::HaveInstance() &&
                   TInterpreter::GetInstance()->CurCardName() != "");
}

void StageFrame::OnEditCardScript(wxCommandEvent &inEvent) {
    ASSERT(TInterpreter::HaveInstance());
    TInterpreter *interp = TInterpreter::GetInstance();
    std::string name = interp->CurCardName();
    ASSERT(name != "");
    ScriptEditor::ShowDefinition(name.c_str());
}

void StageFrame::UpdateUiJumpCard(wxUpdateUIEvent &inEvent)
{
    inEvent.Enable(AreDevToolsAvailable() && mStage->CanJump());
}

void StageFrame::OnJumpCard(wxCommandEvent &inEvent)
{
	if (!IsFullScreen())
		mLocationBox->Prompt();
	else
	{
		wxTextEntryDialog dialog(this, "Jump to Card", "Card:");
		if (dialog.ShowModal() == wxID_OK)
			mLocationBox->TryJump(dialog.GetValue());
	}
}

void StageFrame::UpdateUiStopMovies(wxUpdateUIEvent &inEvent)
{
	inEvent.Enable(mStage->IsMediaPlaying());
}

void StageFrame::OnStopMovies(wxCommandEvent &inEvent)
{
	mStage->EndMediaElements();
}

void StageFrame::OnActivate(wxActivateEvent &inEvent) {
    // We need to call UpdateVideoMode when the window activates, because
    // StageFrame::Iconize won't always get called when we're directly
    // de-iconized by Windows.
	UpdateVideoMode(IsFullScreen(), IsIconized());
}

void StageFrame::OnSize(wxSizeEvent &inEvent)
{
	// Make sure no sash window can be expanded to obscure parts of our
	// stage.  We need to do this whenever the window geometry changes.
	wxSize client_size = GetClientSize();
	wxSize stage_size = GetStage()->GetStageSize();
	wxCoord available_space = client_size.GetWidth() - stage_size.GetWidth();
	if (available_space > 0)
	{
		// XXX - If available_space <= 0, then our window still hasn't
		// been resized to hold the stage, and we shouldn't call
		// SetDefaultSize unless we want weird things to happen.
		if (mProgramTree->GetSize().GetWidth() > available_space)
			mProgramTree->SetDefaultWidth(available_space);
		mProgramTree->SetMaximumSizeX(available_space);
	}

    // Let our parent class call the layout algorithm for us.
    inEvent.Skip();
}

void StageFrame::OnClose(wxCloseEvent &inEvent)
{
	// If we're in full screen mode, leave it, so we don't exit Tamale
	// with the screen in an awkward resized mode.
	if (IsFullScreen())
		ShowFullScreen(false);

    // Ask the script editor whether it wants to close.
    if (ScriptEditor::ProcessEventIfExists(inEvent) && inEvent.GetVeto())
        return;

	// Ask the user to save any unsaved documents.
	if (mDocument && mDocument->IsDirty())
	{
		if (!inEvent.CanVeto())
		{
			mDocument->Save();
			wxLogError("Tamale forced to exit; saved document.");
		}
		else
		{
			wxMessageDialog dlg(this, "Save current Tamale program?",
								"Tamale", wxYES_NO|wxCANCEL|wxCENTRE|
								wxICON_EXCLAMATION);
			int button = dlg.ShowModal();
			if (button == wxID_YES)
				mDocument->Save();
			else if (button == wxID_CANCEL)
			{
				inEvent.Veto();
				return;
			}
		}
	}

	// Save our layout one last time.
	MaybeSaveFrameLayout();

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
