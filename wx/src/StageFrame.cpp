// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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

#include "TInterpreter.h"
#include "doc/Document.h"
#include "doc/TamaleProgram.h"

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
#include "dlg/ProgramPropDlg.h"


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

BEGIN_EVENT_TABLE(StageFrame, wxFrame)
    EVT_MENU(FIVEL_EXIT, StageFrame::OnExit)

    EVT_UPDATE_UI(FIVEL_NEW_PROGRAM, StageFrame::UpdateUiNewProgram)
    EVT_MENU(FIVEL_NEW_PROGRAM, StageFrame::OnNewProgram)
    EVT_UPDATE_UI(FIVEL_OPEN_PROGRAM, StageFrame::UpdateUiOpenProgram)
    EVT_MENU(FIVEL_OPEN_PROGRAM, StageFrame::OnOpenProgram)
    EVT_UPDATE_UI(FIVEL_SAVE_PROGRAM, StageFrame::UpdateUiSaveProgram)
    EVT_MENU(FIVEL_SAVE_PROGRAM, StageFrame::OnSaveProgram)
    EVT_MENU(FIVEL_EDIT_SCRIPTS, StageFrame::OnEditScripts)
    EVT_UPDATE_UI(FIVEL_RELOAD_SCRIPTS, StageFrame::OnUpdateUiReloadScripts)
    EVT_MENU(FIVEL_RELOAD_SCRIPTS, StageFrame::OnReloadScripts)
    EVT_MENU(FIVEL_RUN_TESTS, StageFrame::OnRunTests)

    EVT_MENU(FIVEL_ABOUT, StageFrame::OnAbout)
    EVT_MENU(FIVEL_SHOW_LOG, StageFrame::OnShowLog)
    EVT_MENU(FIVEL_SHOW_LISTENER, StageFrame::OnShowListener)
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
    EVT_UPDATE_UI(FIVEL_INSERT_BACKGROUND, StageFrame::UpdateUiInsertBackground)
    EVT_MENU(FIVEL_INSERT_BACKGROUND, StageFrame::OnInsertBackground)
    EVT_UPDATE_UI(FIVEL_EDIT_MODE, StageFrame::UpdateUiEditMode)
    EVT_MENU(FIVEL_EDIT_MODE, StageFrame::OnEditMode)
    EVT_UPDATE_UI(FIVEL_JUMP_CARD, StageFrame::UpdateUiJumpCard)
    EVT_MENU(FIVEL_JUMP_CARD, StageFrame::OnJumpCard)
    EVT_UPDATE_UI(FIVEL_STOP_MOVIES, StageFrame::UpdateUiStopMovies)
    EVT_MENU(FIVEL_STOP_MOVIES, StageFrame::OnStopMovies)
    EVT_SASH_DRAGGED(FIVEL_PROGRAM_TREE, StageFrame::OnSashDrag)
	EVT_SIZE(StageFrame::OnSize)
    EVT_CLOSE(StageFrame::OnClose)
END_EVENT_TABLE()

StageFrame::StageFrame(wxSize inSize)
    : wxFrame((wxFrame*) NULL, -1, wxGetApp().GetAppName(),
              LoadFramePosition(), wxDefaultSize,
			  wxDEFAULT_FRAME_STYLE),
	  mDocument(NULL),
	  mHaveLoadedFrameLayout(false)
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
    mFileMenu->Append(FIVEL_EXIT, "E&xit\tCtrl+Q", "Exit the application.");

    // Set up our Card menu.
    mCardMenu = new wxMenu();
    mCardMenu->Append(FIVEL_EDIT_MODE, "&Edit Card\tCtrl+Space",
                      "Enter or exit card-editing mode.");
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

	// Set up our Insert menu.
    mInsertMenu = new wxMenu();
	mInsertMenu->Append(FIVEL_INSERT_BACKGROUND,
						"&Background",
						"Insert a new card background to use as a template.");

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
    mMenuBar->Append(mInsertMenu, "&Insert");
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

	// Calculate this once at startup.
	FindBestFullScreenVideoMode();
}

wxPoint StageFrame::LoadFramePosition()
{
	// TODO - Sanity-check position.
	long pos_x, pos_y;
	wxConfigBase *config = wxConfigBase::Get();
	if (config->Read("/Layout/Default/StageFrame/Left", &pos_x) &&
		config->Read("/Layout/Default/StageFrame/Top", &pos_y))
		return wxPoint(pos_x, pos_y);
	else
		return wxDefaultPosition;
}

void StageFrame::LoadFrameLayout()
{
	// Get our default values.
	wxSize sz = GetClientSize();
	long is_maximized = IsMaximized();
	long sz_client_width = sz.GetWidth();
	long sz_client_height = sz.GetHeight();
	long program_tree_width = mProgramTree->GetMinimumSizeX();

	// Load values from our config file.
	wxConfigBase *config = wxConfigBase::Get();
	config->Read("/Layout/Default/StageFrame/IsMaximized", &is_maximized);
	config->Read("/Layout/Default/StageFrame/ClientWidth", &sz_client_width);
	config->Read("/Layout/Default/StageFrame/ClientHeight", &sz_client_height);
	config->Read("/Layout/Default/StageFrame/ProgramTreeWidth",
				 &program_tree_width);

	// Restore our non-maximized layout first.  We restore the
	// ProgramTree width before anything else, so it will get appropriately
	// adjusted by the frame resize events.
	// NOTE - We'll only make the window larger, never smaller, because
	// we assume that GetClientSize is currently the minimum allowable.
	mProgramTree->SetDefaultWidth(program_tree_width);
	wxSize new_size = GetClientSize();
	if (sz_client_width >= new_size.GetWidth() &&
		sz_client_height >= new_size.GetHeight())
		new_size = wxSize(sz_client_width, sz_client_height);
	SetClientSize(new_size);

	// If necessary, maximize our window.
	if (is_maximized)
		Maximize(TRUE);

	// It's now safe to resave these values.
	mHaveLoadedFrameLayout = true;
}

void StageFrame::MaybeSaveFrameLayout()
{
	// Don't save the frame layout if we haven't loaded it yet, or if we're
	// in full-screen mode (which has an automatically-chosen layout).
	if (!mHaveLoadedFrameLayout || IsFullScreen())
		return;

	wxConfigBase *config = wxConfigBase::Get();
	config->Write("/Layout/Default/StageFrame/IsMaximized",
				  IsMaximized() ? 1 : 0);
	if (!IsMaximized())
	{
		// Only save the window position if we aren't maximized.
		wxPoint pos = GetPosition();
		wxSize sz = GetClientSize();
		config->Write("/Layout/Default/StageFrame/Left", pos.x);
		config->Write("/Layout/Default/StageFrame/Top", pos.y);
		config->Write("/Layout/Default/StageFrame/ClientWidth", sz.GetWidth());
		config->Write("/Layout/Default/StageFrame/ClientHeight",
					  sz.GetHeight());
	}
	config->Write("/Layout/Default/StageFrame/ProgramTreeWidth",
				  mProgramTree->GetSize().GetWidth());
}

#if !wxUSE_DISPLAY
void StageFrame::FindBestFullScreenVideoMode() {}
void StageFrame::SetFullScreenVideoMode() {}
void StageFrame::ResetVideoMode() {}
#else // wxUSE_DISPLAY

void StageFrame::FindBestFullScreenVideoMode()
{
	// TODO - DirectX support is broken in wxWindows.  A fair bit of work
	// is required to fix it.  For now, leave it alone.
	//wxDisplay::UseDirectX(true);
	wxDisplay display;

	// Search for the most promising mode.
	wxArrayVideoModes modes = display.GetModes();
	mFullScreenVideoMode = wxDefaultVideoMode;
	wxSize min_size = mStage->GetStageSize();
	for (int i = 0; i < modes.GetCount(); i++)
	{
		wxVideoMode &mode = modes[i];
		gDebugLog.Log("Found mode: %dx%d, %d bit, %d Hz",
					  mode.w, mode.h, mode.bpp, mode.refresh);

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

void StageFrame::SetFullScreenVideoMode()
{
	if (mFullScreenVideoMode != wxDefaultVideoMode)
	{
		wxDisplay display;
		display.ChangeMode(mFullScreenVideoMode);
	}
	mBackground->UpdateStagePosition();
}

void StageFrame::ResetVideoMode()
{
	if (mFullScreenVideoMode != wxDefaultVideoMode)
	{
		wxDisplay display;
		display.ResetMode();
	}		
}

#endif // wxUSE_DISPLAY

bool StageFrame::ShowFullScreen(bool show, long style)
{
	mProgramTree->Show(!show);
	mBackground->UpdateColor();
	if (!show)
	{
		ResetVideoMode();
		SetSizeHints(mMinimumFrameSize.GetWidth(),
					 mMinimumFrameSize.GetHeight());
	}
	bool result = wxFrame::ShowFullScreen(show, style);
	if (show)
	{
		// Set our size hints to exactly the stage size, so we can
		// avoid inappropriate padding in full screen mode.
		SetSizeHints(mStage->GetSize().GetWidth(),
					 mStage->GetSize().GetHeight());
		SetFullScreenVideoMode();
	}
	return result;
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
		mStage->Show();
	}
}

void StageFrame::OpenDocument()
{
	wxASSERT(mDocument == NULL);
	wxDirDialog dlg(this, "Open a Tamale program folder:");

	wxConfigBase *config = wxConfigBase::Get();
	wxString recent;
	if (config->Read("/Recent/DocPath", &recent))
		dlg.SetPath(recent);

	if (dlg.ShowModal() == wxID_OK)
	{
		wxString file = dlg.GetPath();
		mDocument = new Document(file.mb_str(), Document::OPEN);
		config->Write("/Recent/DocPath", file);

		SetObject(mDocument->GetTamaleProgram());
		mProgramTree->RegisterDocument(mDocument);
		mStage->Show();
	}
}

void StageFrame::ObjectChanged()
{
	SetTitle(wxString(GetObject()->GetString("name").c_str()) +
			 " - " + wxGetApp().GetAppName() + " - [" +
			 wxString(mDocument->GetSavePath().c_str()) + "]");
}

void StageFrame::ObjectDeleted()
{
	SetTitle(wxGetApp().GetAppName());
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
	END_EXCEPTION_TRAPPER()
}

void StageFrame::UpdateUiOpenProgram(wxUpdateUIEvent &inEvent)
{
	inEvent.Enable(mDocument == NULL);
}

void StageFrame::OnOpenProgram(wxCommandEvent &inEvent)
{
	BEGIN_EXCEPTION_TRAPPER()
		OpenDocument();
	END_EXCEPTION_TRAPPER()
}

void StageFrame::UpdateUiSaveProgram(wxUpdateUIEvent &inEvent)
{
	inEvent.Enable(mDocument != NULL && mDocument->IsDirty());
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
        inEvent.Enable(manager->InterpreterHasBegun());
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
            // to try loading it again.  We call NotifyScriptReload to
			// purge any remaining data from a partially completed load.
            mStage->NotifyScriptReload();
            manager->RequestRetryLoadScript();
        }
        else
        {
            // We have a running script.  Go ahead and reload it.
            ASSERT(TInterpreter::HaveInstance());
            TInterpreter *interp = TInterpreter::GetInstance();
            manager->RequestReloadScript(interp->CurCardName().c_str());

            // Tell our stage that the script is going away.
            mStage->NotifyScriptReload();
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
                          "Tamale\n"
                          "Copyright 1993-2004 The Trustees of Dartmouth College.  All rights reserved.",
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
}

void StageFrame::OnFullScreen(wxCommandEvent &inEvent)
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

void StageFrame::OnDisplayXy(wxCommandEvent &inEvent)
{
    mStage->ToggleDisplayXy();
}

void StageFrame::UpdateUiDisplayGrid(wxUpdateUIEvent &inEvent)
{
    inEvent.Check(mStage->IsDisplayingGrid());
}

void StageFrame::OnDisplayGrid(wxCommandEvent &inEvent)
{
    mStage->ToggleDisplayGrid();
}

void StageFrame::UpdateUiDisplayBorders(wxUpdateUIEvent &inEvent)
{
    inEvent.Check(mStage->IsDisplayingBorders());
}

void StageFrame::OnDisplayBorders(wxCommandEvent &inEvent)
{
    mStage->ToggleDisplayBorders();
}

void StageFrame::UpdateUiProperties(wxUpdateUIEvent &inEvent)
{
	inEvent.Enable(mDocument != NULL);
}

void StageFrame::OnProperties(wxCommandEvent &inEvent)
{
	ProgramPropDlg prop_dlg(this, mDocument->GetTamaleProgram());
	prop_dlg.ShowModal();
}

void StageFrame::UpdateUiInsertBackground(wxUpdateUIEvent &inEvent)
{
	inEvent.Enable(mDocument != NULL);
}

void StageFrame::OnInsertBackground(wxCommandEvent &inEvent)
{
	mDocument->GetTamaleProgram()->InsertBackground();
}

void StageFrame::UpdateUiEditMode(wxUpdateUIEvent &inEvent)
{
	if (mDocument != NULL && mStage->IsScriptInitialized())
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

void StageFrame::UpdateUiJumpCard(wxUpdateUIEvent &inEvent)
{
    inEvent.Enable(mStage->CanJump());
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
	inEvent.Enable(mStage->IsMediaPlaying() ||
				   (TInterpreter::HaveInstance() &&
					TInterpreter::GetInstance()->Napping()));
}

void StageFrame::OnStopMovies(wxCommandEvent &inEvent)
{
	if (TInterpreter::HaveInstance() && TInterpreter::GetInstance()->Napping())
		TInterpreter::GetInstance()->KillNap();
	mStage->EndMediaElements();
}

void StageFrame::OnSashDrag(wxSashEvent &inEvent)
{
    if (inEvent.GetDragStatus() == wxSASH_STATUS_OUT_OF_RANGE)
        return;
    if (inEvent.GetId() != FIVEL_PROGRAM_TREE)
		return;

	mProgramTree->SetDefaultWidth(inEvent.GetDragRect().width);

    wxLayoutAlgorithm layout;
    layout.LayoutFrame(this, mBackground);
	MaybeSaveFrameLayout();
}

void StageFrame::OnSize(wxSizeEvent &WXUNUSED(inEvent))
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

	// Ask wxLayoutAlgorithm to do smart resizing, taking our various
	// subwindows and constraints into account.
    wxLayoutAlgorithm layout;
    layout.LayoutFrame(this, mBackground);
	MaybeSaveFrameLayout();
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
