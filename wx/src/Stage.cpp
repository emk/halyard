// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TamaleHeaders.h"

#include <wx/treectrl.h>
#include <wx/laywin.h>
#include <wx/config.h>
#include <wx/filename.h>
#include <wx/clipbrd.h>
#include <wx/image.h>

#include "TInterpreter.h"
#include "TVariable.h"
#include "TStyleSheet.h"
#include "doc/Document.h"
#include "doc/TamaleProgram.h"

#include "AppConfig.h"
#include "AppGlobals.h"
#include "AppGraphics.h"
#include "FiveLApp.h"
#include "Stage.h"
#include "ProgramTree.h"
#include "Element.h"
#include "MovieElement.h"
#include "Listener.h"
#include "Timecoder.h"
#include "TestHarness.h"
#include "FancyStatusBar.h"
#include "LocationBox.h"
#include "EventDispatcher.h"
#include "ImageCache.h"
#include "CursorManager.h"
#include "Transition.h"
#include "dlg/ProgramPropDlg.h"

#if CONFIG_HAVE_QUAKE2
#	include "Quake2Engine.h"
#endif // CONFIG_HAVE_QUAKE2

#define IDLE_INTERVAL (33) // milliseconds

USING_NAMESPACE_FIVEL


//=========================================================================
//  StageBackground
//=========================================================================
//  This class implements the wxWindow *behind* the stage.  It has few
//  duties other than (1) being black and (2) keeping the stage centered.

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
    EVT_MENU(FIVEL_RELOAD_SCRIPT, StageFrame::OnReloadScript)
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
    mFileMenu->Append(FIVEL_RELOAD_SCRIPT, "&Reload Script\tCtrl+R",
                      "Reload the currently executing Tamale script.");
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
    tb->AddTool(FIVEL_RELOAD_SCRIPT, "Reload", wxBITMAP(tb_reload),
                "Reload Script");
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
	inEvent.Enable(mDocument == NULL);
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

void StageFrame::OnReloadScript(wxCommandEvent &inEvent)
{
    if (TInterpreterManager::HaveInstance())
    {
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
	TestHarness::StartTests();
}

void StageFrame::OnAbout(wxCommandEvent &inEvent)
{
    wxMessageDialog about(this,
                          "Tamale Prototype\n"
                          "Copyright 2002 The Trustees of Dartmouth College",
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
	inEvent.Enable(mStage->IsMoviePlaying() ||
				   (TInterpreter::HaveInstance() &&
					TInterpreter::GetInstance()->Napping()));
}

void StageFrame::OnStopMovies(wxCommandEvent &inEvent)
{
	if (TInterpreter::HaveInstance() && TInterpreter::GetInstance()->Napping())
		TInterpreter::GetInstance()->KillNap();
	mStage->DeleteMovieElements();
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


//=========================================================================
//  Stage Methods
//=========================================================================

BEGIN_EVENT_TABLE(Stage, wxWindow)
	EVT_IDLE(Stage::OnIdle)
    EVT_MOTION(Stage::OnMouseMove)
    EVT_ERASE_BACKGROUND(Stage::OnEraseBackground)
    EVT_PAINT(Stage::OnPaint)
    EVT_CHAR(Stage::OnChar)
    EVT_TEXT_ENTER(FIVEL_TEXT_ENTRY, Stage::OnTextEnter)
	EVT_LEFT_DOWN(Stage::OnLeftDown)
	EVT_LEFT_DCLICK(Stage::OnLeftDClick)
	EVT_LEFT_UP(Stage::OnLeftUp)
	EVT_RIGHT_DOWN(Stage::OnRightDown)
	EVT_RIGHT_DCLICK(Stage::OnRightDown)
END_EVENT_TABLE()

Stage::Stage(wxWindow *inParent, StageFrame *inFrame, wxSize inStageSize)
    : wxWindow(inParent, -1, wxDefaultPosition, inStageSize),
      mFrame(inFrame), mStageSize(inStageSize), mLastCard(""),
      mOffscreenPixmap(inStageSize.GetWidth(), inStageSize.GetHeight(), 24),
      mOffscreenFadePixmap(inStageSize.GetWidth(),
						   inStageSize.GetHeight(), 24),
	  mSavePixmap(inStageSize.GetWidth(), inStageSize.GetHeight(), 24),
	  mTextCtrl(NULL), mCurrentElement(NULL), mGrabbedElement(NULL),
	  mWaitElement(NULL),
      mIsDisplayingXy(false), mIsDisplayingGrid(false),
      mIsDisplayingBorders(false)
{
    SetBackgroundColour(STAGE_COLOR);
    ClearStage(*wxBLACK);
    
	mLastIdleEvent = ::wxGetLocalTimeMillis();
	mEventDispatcher = new EventDispatcher();
	mImageCache = new ImageCache();
	mCursorManager = new CursorManager();
	mTransitionManager = new TransitionManager();
	
    mTextCtrl =
        new wxTextCtrl(this, FIVEL_TEXT_ENTRY, "", wxDefaultPosition,
                       wxDefaultSize, wxNO_BORDER | wxTE_PROCESS_ENTER);
    mTextCtrl->Hide();

	wxLogTrace(TRACE_STAGE_DRAWING, "Stage created.");
}

Stage::~Stage()
{
	DeleteElements();
	delete mImageCache;
	delete mCursorManager;
	delete mEventDispatcher;
	delete mTransitionManager;
	wxLogTrace(TRACE_STAGE_DRAWING, "Stage deleted.");
}

bool Stage::IsScriptInitialized()
{
	// Assume that the script is properly initialized as soon as it has
	// entered a card.  This is good enough for now, but we'll need to
	// change it later.
	return (mLastCard != "");
}

void Stage::SetEditMode(bool inWantEditMode)
{
	if (IsInEditMode() == inWantEditMode)
		return;
	else if (inWantEditMode)
	{
		TInterpreter::GetInstance()->Stop();
		// TODO - NotifyExitCard() should be triggered from kernel.ss, but
		// this will mean auditing the engine code to only call
		// CurCardName, etc., only when there is a current card.
		NotifyExitCard();
		ClearStage(*wxBLACK);
	}
	else
	{
		wxASSERT(mLastCard != "");
		TInterpreter::GetInstance()->Go(mLastCard.c_str());
	}
}

bool Stage::IsInEditMode()
{
	wxASSERT(TInterpreter::HaveInstance());
	return TInterpreter::GetInstance()->IsStopped();
}

bool Stage::ShouldSendEvents()
{
	return IsScriptInitialized() && !IsInEditMode();
}

bool Stage::CanJump()
{
	// This should match the list of sanity-checks in the function below.
	return (TInterpreter::HaveInstance() &&
			IsScriptInitialized() &&
			!IsInEditMode());
}

void Stage::TryJumpTo(const wxString &inName)
{
	// We go to quite a lot of trouble to verify this request.
	if (!IsScriptInitialized())
		::wxLogError("Cannot jump until program has finished initializing.");
	else if (IsInEditMode())
		::wxLogError("Unimplemented: Cannot jump while in edit mode (yet).");
	else
	{
		wxASSERT(TInterpreter::HaveInstance());
		TInterpreter *interp = TInterpreter::GetInstance();
		if (!interp->IsValidCard(inName))
			::wxLogError("The card \'" + inName + "\' does not exist.");
		else
			interp->JumpToCardByName(inName);
	}
}

void Stage::RegisterCard(const wxString &inName)
{
	mFrame->GetProgramTree()->RegisterCard(inName);
}

void Stage::NotifyEnterCard()
{
	mLastCard = TInterpreter::GetInstance()->CurCardName();
	mFrame->GetLocationBox()->NotifyEnterCard();
	mFrame->GetProgramTree()->NotifyEnterCard();
}

void Stage::NotifyExitCard()
{
    if (mTextCtrl->IsShown())
        mTextCtrl->Hide();
	DeleteElements();
}

void Stage::NotifyScriptReload()
{
	mLastCard = "";
	mEventDispatcher->NotifyScriptReload();
	mImageCache->NotifyScriptReload();
	mFrame->GetProgramTree()->NotifyScriptReload();
    NotifyExitCard();
	gStyleSheetManager.RemoveAll();

	// For now, treat Quake 2 as a special case.
#if CONFIG_HAVE_QUAKE2
	if (Quake2Engine::IsInitialized())
		Quake2Engine::GetInstance()->NotifyScriptReload();
#endif // CONFIG_HAVE_QUAKE2
}

void Stage::NotifyElementsChanged()
{
	wxLogTrace(TRACE_STAGE_DRAWING, "Elements on stage have changed.");

	// Don't do anything unless there's a good chance this window still
	// exists in some sort of valid state.
	// TODO - Is IsShown a good way to tell whether a window is still good?
	if (IsShown())
	{
		// Update our element borders (if necessary) and fix our cursor.
		if (mIsDisplayingBorders)
			InvalidateStage();
		UpdateCurrentElementAndCursor();
	}
}

void Stage::EnterElement(Element *inElement, wxPoint &inPosition)
{
	ASSERT(inElement->GetEventDispatcher());
	inElement->GetEventDispatcher()->DoEventMouseEnter(inPosition);
}

void Stage::LeaveElement(Element *inElement, wxPoint &inPosition)
{
	ASSERT(inElement->GetEventDispatcher());
	inElement->GetEventDispatcher()->DoEventMouseLeave(inPosition);
}

void Stage::UpdateCurrentElementAndCursor(wxPoint &inPosition)
{
	// Find which element we're in.
	wxPoint pos = ScreenToClient(::wxGetMousePosition());
	Element *obj = FindLightWeightElement(pos);

	// Change the cursor, if necessary.  I haven't refactored this
	// into EnterElement/LeaveElement yet because of how we handle
	// mIsDisplayingXy.  Feel free to improve.
	if (!mGrabbedElement && (obj == NULL || obj != mCurrentElement))
	{
		if (mIsDisplayingXy)
			SetCursor(*wxCROSS_CURSOR);
		else
		{
			if (obj)
				SetCursor(obj->GetCursor());
			else
				SetCursor(wxNullCursor);
		}
	}

	// Update the current element.
	if (obj != mCurrentElement)
	{
		if (mCurrentElement && ShouldSendMouseEventsToElement(mCurrentElement))
			LeaveElement(mCurrentElement, inPosition);
		mCurrentElement = obj;
		if (obj && ShouldSendMouseEventsToElement(obj))
			EnterElement(obj, inPosition);
	}
}

void Stage::UpdateCurrentElementAndCursor()
{
	UpdateCurrentElementAndCursor(ScreenToClient(::wxGetMousePosition()));
}

void Stage::InterpreterSleep()
{
    // Put our interpreter to sleep.
	// TODO - Keep track of who we're sleeping for.
    ASSERT(TInterpreter::HaveInstance() &&
           !TInterpreter::GetInstance()->Paused());
    TInterpreter::GetInstance()->Pause();
}

void Stage::InterpreterWakeUp()
{
	// Wake up our Scheme interpreter.
	// We can't check TInterpreter::GetInstance()->Paused() because
	// the engine might have already woken the script up on its own.
	// TODO - Keep track of who we're sleeping for.
    ASSERT(TInterpreter::HaveInstance());
    TInterpreter::GetInstance()->WakeUp();
}

Stage::ElementCollection::iterator
Stage::FindElementByName(ElementCollection &inCollection,
						 const wxString &inName)
{
	ElementCollection::iterator i = inCollection.begin();
	for (; i != inCollection.end(); i++)
		if ((*i)->GetName() == inName)
			return i;
	return inCollection.end();
}

void Stage::OnIdle(wxIdleEvent &inEvent)
{
	if (mWaitElement && mWaitElement->HasReachedFrame(mWaitFrame))
		EndWait();

	// Send an idle event to the Scheme engine occasionally.
	if (ShouldSendEvents() &&
		::wxGetLocalTimeMillis() > mLastIdleEvent + IDLE_INTERVAL)
	{
		mLastIdleEvent = ::wxGetLocalTimeMillis();

		// We only pass the idle event to just the card, and not any
		// of the elements.  Idle event processing is handled differently
		// from most other events; we let the scripting language work
		// out the details.
		GetEventDispatcher()->DoEventIdle(inEvent);
	}
}

void Stage::OnMouseMove(wxMouseEvent &inEvent)
{
	// Do any mouse-moved processing for our Elements.
	UpdateCurrentElementAndCursor();
    if (mIsDisplayingXy)
    {
        wxClientDC dc(this);

        // Get our current screen location.
        wxPoint pos = inEvent.GetPosition();
        long x = dc.DeviceToLogicalX(pos.x);
        long y = dc.DeviceToLogicalY(pos.y);

        // Get the color at that screen location.
        // PORTING - May not work on non-Windows platforms, according to
        // the wxWindows documentation.
        wxMemoryDC offscreen_dc;
        offscreen_dc.SelectObject(mOffscreenPixmap);
        wxColour color;
        offscreen_dc.GetPixel(x, y, &color);

        // Update the status bar.
        wxString str;
        str.Printf("X: %d, Y: %d, C: %02X%02X%02X",
                   (int) x, (int) y, (int) color.Red(),
                   (int) color.Green(), color.Blue());
        mFrame->SetStatusText(str);
    }

	// XXX - We should send a mouse-moved event here, not an idle event,
	// but we're just testing potential performance.
	//if (ShouldSendEvents())
	//	GetEventDispatcher()->DoEventIdle();
}

void Stage::OnEraseBackground(wxEraseEvent &inEvent)
{
	wxLogTrace(TRACE_STAGE_DRAWING, "Ignoring request to erase stage.");

    // Ignore this event to prevent flicker--we don't need to erase,
    // because we redraw everything from the offscreen buffer.  We may need
    // to override more of these events elsewhere.

	// TODO - Sometimes parts of the frame don't get repainted.  Could we
	// somehow indirectly be responsible?
}

void Stage::OnPaint(wxPaintEvent &inEvent)
{
	wxLogTrace(TRACE_STAGE_DRAWING, "Painting stage.");

    // Set up our drawing context, and paint the screen.
    wxPaintDC screen_dc(this);
    PaintStage(screen_dc);
}

void Stage::PaintStage(wxDC &inDC)
{
    // Blit our offscreen pixmap to the screen.
    // TODO - Could we optimize drawing by only blitting dirty regions?
	inDC.DrawBitmap(mOffscreenPixmap, 0, 0, false);

    // If necessary, draw the grid.
    if (mIsDisplayingGrid)
    {
        int width = mStageSize.GetWidth();
        int height = mStageSize.GetHeight();
        int small_spacing = 10;
        int large_spacing = small_spacing * 10;

        // Draw the minor divisions of the grid.
        inDC.SetPen(*wxLIGHT_GREY_PEN);
        for (int x = 0; x < width; x += small_spacing)
            if (x % large_spacing)
                inDC.DrawLine(x, 0, x, height);
        for (int y = 0; y < width; y += small_spacing)
            if (y % large_spacing)
                inDC.DrawLine(0, y, width, y);

        // Draw the major divisions of the grid.
        inDC.SetPen(*wxGREEN_PEN);
        for (int x2 = 0; x2 < width; x2 += large_spacing)
            inDC.DrawLine(x2, 0, x2, height);
        for (int y2 = 0; y2 < width; y2 += large_spacing)
            inDC.DrawLine(0, y2, width, y2);
    }

	// If necessary, draw the borders.
	if (mIsDisplayingBorders)
	{
		DrawTextBorder(inDC);

		ElementCollection::iterator i = mElements.begin();
		for (; i != mElements.end(); i++)
			if ((*i)->IsShown())
				DrawElementBorder(inDC, *i);
	}
}

// XXX - these should be refactored, but it's just two lines they have 
// in common. I'm not sure if it's worth it. Feel free to do so if you
// want.
void Stage::DrawElementBorder(wxDC &inDC, Element *inElement)
{
	inDC.SetPen(*wxRED_PEN);
	inDC.SetBrush(*wxTRANSPARENT_BRUSH);

	inElement->DrawElementBorder(inDC);
}

void Stage::DrawTextBorder(wxDC &inDC)
{
	if (mTextCtrl->IsShown())
	{
		inDC.SetPen(*wxRED_PEN);
		inDC.SetBrush(*wxTRANSPARENT_BRUSH);
		
		// Draw the border *outside* our rectangle.
		wxRect r = mTextCtrl->GetRect();
		r.Inflate(1);
		inDC.DrawRectangle(r.x, r.y, r.width, r.height);
	}
}


void Stage::OnChar(wxKeyEvent &inEvent)
{
	// NOTE - We handle this event here, but the stage isn't always
	// focused.  Is this really a good idea?  Douglas tells me that
	// Director works like this, so at least there's precedent.
	if (!ShouldSendEvents())
		inEvent.Skip();
	else if (inEvent.GetKeyCode() == WXK_SPACE &&
			 inEvent.ControlDown() && !inEvent.AltDown())
		inEvent.Skip(); // Always allow toggling into edit mode.
	else
	{
		EventDispatcher *dispatcher = GetEventDispatcher();
		if (!dispatcher->DoEventChar(inEvent))
			inEvent.Skip();
	}
}

void Stage::OnTextEnter(wxCommandEvent &inEvent)
{
    // Get the text.
    wxString text = FinishModalTextInput();
    
    // Set up a drawing context.
    wxMemoryDC dc;
    dc.SelectObject(mOffscreenPixmap);
    
    // Prepare to draw the text.
    dc.SetTextForeground(mTextCtrl->GetForegroundColour());
    dc.SetTextBackground(mTextCtrl->GetBackgroundColour());
    dc.SetFont(mTextCtrl->GetFont());

    // Draw the text.
    // PORTING - These offsets are unreliable and platform-specific.
    wxPoint pos = mTextCtrl->GetPosition();
    dc.DrawText(text, pos.x + 2, pos.y);
}

void Stage::OnLeftDown(wxMouseEvent &inEvent)
{
	// Restore focus to the stage.
	SetFocus();

	// Dispatch the event.
	EventDispatcher *disp = FindEventDispatcher(inEvent.GetPosition());
	disp->DoEventLeftDown(inEvent, false);
}

void Stage::OnLeftDClick(wxMouseEvent &inEvent)
{
	EventDispatcher *disp = FindEventDispatcher(inEvent.GetPosition());
	disp->DoEventLeftDown(inEvent, true);	
}

void Stage::OnLeftUp(wxMouseEvent &inEvent)
{
	EventDispatcher *disp = FindEventDispatcher(inEvent.GetPosition());
	disp->DoEventLeftUp(inEvent);
}

void Stage::OnRightDown(wxMouseEvent &inEvent)
{
    if (!mIsDisplayingXy)
		inEvent.Skip();
	else
	{
		// Get the position of the click, build a string, and save the
		// position for next time.
		wxPoint pos = inEvent.GetPosition();
		wxString str;
		if (inEvent.ShiftDown() && mCopiedPoints.size() == 1)
			str.Printf("(rect %d %d %d %d)", 
					   (mCopiedPoints.end()-1)->x, 
					   (mCopiedPoints.end()-1)->y,
					   pos.x, pos.y);
		else if (inEvent.ShiftDown() && mCopiedPoints.size() > 1)
		{
			str.Printf("(polygon ");
			std::vector<wxPoint>::iterator i;
			for (i = mCopiedPoints.begin(); i != mCopiedPoints.end(); ++i)
				str += wxString::Format("(point %d %d) ", i->x, i->y);
			str += wxString::Format("(point %d %d))", pos.x, pos.y);
		}
		else
		{
			str.Printf("(point %d %d)", pos.x, pos.y);
			mCopiedPoints.clear();
		}
		mCopiedPoints.push_back(pos);

		// Copy our string to the clipboard.  This code snippet comes from
		// the wxWindows manual.
		if (wxTheClipboard->Open())
		{
			wxTheClipboard->SetData(new wxTextDataObject(str));
			wxTheClipboard->Close();
			mFrame->SetStatusText(wxString("Copied: ") + str);
		}
	}
}

void Stage::ValidateStage()
{
	// XXX - We can't actually *do* this using wxWindows, so we're
	// repainting the screen too often.  To fix this, we'll need to manage
	// dirty regions in mOffscreenPixmap manually, or do something else
	// to complicate things.
}

void Stage::InvalidateStage()
{
    InvalidateRect(wxRect(0, 0,
                          mStageSize.GetWidth(), mStageSize.GetHeight()));
}

void Stage::InvalidateRect(const wxRect &inRect)
{
	wxLogTrace(TRACE_STAGE_DRAWING, "Invalidating: %d %d %d %d",
			   inRect.x, inRect.y,
			   inRect.x + inRect.width, inRect.y + inRect.height);
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
    InvalidateStage();
}

void Stage::DrawLine(const wxPoint &inFrom, const wxPoint &inTo,
					 const wxColour &inColor, int inWidth)
{
    wxMemoryDC dc;
    dc.SelectObject(mOffscreenPixmap);
	wxPen pen(inColor, inWidth, wxSOLID);
	dc.SetPen(pen);
	dc.DrawLine(inFrom.x, inFrom.y, inTo.x, inTo.y);
	InvalidateRect(wxRect(inFrom, inTo));
}

void Stage::FillBox(const wxRect &inBounds, 
					const GraphicsTools::Color &inColor)
{
	if (inColor.alpha == 0x00)
	{
		wxColor color = GetColor(inColor);
		wxMemoryDC dc;
		dc.SelectObject(mOffscreenPixmap);
		wxBrush brush(color, wxSOLID);
		dc.SetBrush(brush);
		dc.SetPen(*wxTRANSPARENT_PEN);
		dc.DrawRectangle(inBounds.x, inBounds.y, inBounds.width, inBounds.height);
		InvalidateRect(inBounds);
	} 
	else
	{
		FillBoxAlpha(inBounds, inColor);
	}
}

void Stage::OutlineBox(const wxRect &inBounds, const wxColour &inColor,
					   int inWidth)
{
    wxMemoryDC dc;
    dc.SelectObject(mOffscreenPixmap);
	wxPen pen(inColor, inWidth, wxSOLID);
	dc.SetPen(pen);
	dc.SetBrush(*wxTRANSPARENT_BRUSH);
	dc.DrawRectangle(inBounds.x, inBounds.y, inBounds.width, inBounds.height);
	InvalidateRect(inBounds);
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

void Stage::DrawDCContents(wxDC &inDC)
{
    wxMemoryDC dc;
    dc.SelectObject(mOffscreenPixmap);
	if (!dc.Blit(0, 0, mStageSize.GetWidth(), mStageSize.GetHeight(),
				 &inDC, 0, 0))
	{
		ClearStage(*wxBLACK);
	}
}

void Stage::SaveGraphics(const wxRect &inBounds)
{
	wxMemoryDC srcDC, dstDC;
	srcDC.SelectObject(mOffscreenPixmap);
	dstDC.SelectObject(mSavePixmap);
	dstDC.Blit(inBounds.x, inBounds.y, inBounds.width, inBounds.height,
			   &srcDC, inBounds.x, inBounds.y);
}

void Stage::RestoreGraphics(const wxRect &inBounds)
{
	wxMemoryDC srcDC, dstDC;
	srcDC.SelectObject(mSavePixmap);
	dstDC.SelectObject(mOffscreenPixmap);
	dstDC.Blit(inBounds.x, inBounds.y, inBounds.width, inBounds.height,
			   &srcDC, inBounds.x, inBounds.y);
	InvalidateRect(inBounds);
}

void Stage::Screenshot(const wxString &inFilename)
{
	wxImage image = mOffscreenPixmap.ConvertToImage();
	image.SaveFile(inFilename, wxBITMAP_TYPE_PNG);
}

void Stage::ModalTextInput(const wxRect &inBounds,
                           const int inTextSize,
                           const wxColour &inForeColor,
                           const wxColour &inBackColor)
{
    ASSERT(!mTextCtrl->IsShown());

    // Update our control to have the right properties and show it.
    mTextCtrl->SetValue("");
    wxFont font(inTextSize, wxROMAN, wxNORMAL, wxNORMAL);
    mTextCtrl->SetFont(font);
    mTextCtrl->SetForegroundColour(inForeColor);
    mTextCtrl->SetBackgroundColour(inBackColor);
    mTextCtrl->SetSize(inBounds);
    mTextCtrl->Show();
    mTextCtrl->SetFocus();
	NotifyElementsChanged();

	InterpreterSleep();
}

wxString Stage::FinishModalTextInput()
{
    ASSERT(mTextCtrl->IsShown());

	InterpreterWakeUp();

	// Store our result somewhere useful.
	gVariableManager.SetString("_modal_input_text", mTextCtrl->GetValue());

    // Hide our text control and get the text.
    mTextCtrl->Hide();
	NotifyElementsChanged();
    return mTextCtrl->GetValue();
}

bool Stage::Wait(const wxString &inElementName, MovieFrame inUntilFrame)
{
	ASSERT(mWaitElement == NULL);

	// Look for our element.
	ElementCollection::iterator i =
		FindElementByName(mElements, inElementName);

	// Make sure we can wait on this element.
	// TODO - Refactor this error-handling code to a standalone
	// routine so we don't have to keep on typing it.
	const char *name = (const char *) inElementName;
	if (i == mElements.end())
	{
		gDebugLog.Caution("wait: Element %s does not exist", name);
		return false;
	}
	MovieElement *movie = dynamic_cast<MovieElement*>(*i);
	if (movie == NULL)
	{
		gDebugLog.Caution("wait: Element %s is not a movie", name);
		return false;		
	}

	// Return immediately (if we're already past the wait point) or
	// go to sleep for a while.
	if (movie->HasReachedFrame(inUntilFrame))
		gDebugLog.Log("wait: Movie %s has already past frame %d",
					  name, inUntilFrame);
	else
	{
		mWaitElement = movie;
		mWaitFrame = inUntilFrame;
		InterpreterSleep();
	}
	return true;
}

void Stage::EndWait()
{
	gDebugLog.Log("wait: Waking up.");
	ASSERT(mWaitElement != NULL);
	mWaitElement = NULL;
	mWaitFrame = 0;
	InterpreterWakeUp();
}

void Stage::RefreshStage(const std::string &inTransition, int inMilliseconds)
{
	// If we're supposed to run a transiton, do so now.
	if (inTransition != "none" && inMilliseconds > 0)
	{
		// Attempt to get a copy of whatever is on the screen.
		wxClientDC client_dc(this);
		wxBitmap before(mStageSize.GetWidth(), mStageSize.GetHeight(), 24);
		bool have_before;
		{
			wxMemoryDC before_dc;
			before_dc.SelectObject(before);
			have_before =
				before_dc.Blit(0, 0,
							   mStageSize.GetWidth(), mStageSize.GetHeight(),
							   &client_dc, 0, 0);
		}

		// Run transiton, if we can.
		if (have_before)
		{
			TransitionResources r(client_dc, before, mOffscreenPixmap,
								  mOffscreenFadePixmap);
			mTransitionManager->RunTransition(inTransition, inMilliseconds, r);
		}
	}

	// Draw our offscreen buffer to the screen, and mark that portion of
	// the screen as updated.
	{
		wxClientDC client_dc(this);
		PaintStage(client_dc);
	}
	ValidateStage();
}

void Stage::AddElement(Element *inElement)
{
	// Delete any existing Element with the same name.
	(void) DeleteElementByName(inElement->GetName());

	// Add the new Element to our list.
	mElements.push_back(inElement);
	NotifyElementsChanged();
}

Element *Stage::FindElement(const wxString &inElementName)
{
	ElementCollection::iterator i =
		FindElementByName(mElements, inElementName);
	if (i == mElements.end())
		return NULL;
	else
		return *i;
}

Element *Stage::FindLightWeightElement(const wxPoint &inPoint)
{
	// Look for the most-recently-added Element containing inPoint.
	Element *result = NULL;
	ElementCollection::iterator i = mElements.begin();
	for (; i != mElements.end(); i++)
		if ((*i)->IsLightWeight() && (*i)->IsPointInElement(inPoint))
			result = *i;
	return result;
}

EventDispatcher *Stage::FindEventDispatcher(const wxPoint &inPoint)
{
	// If a grab is in effect, return the element immediately.
	if (mGrabbedElement)
		return mGrabbedElement->GetEventDispatcher();

	// Otherwise, look things up normally.
	Element *elem = FindLightWeightElement(inPoint);
	if (elem && elem->GetEventDispatcher())
		return elem->GetEventDispatcher();
	else
		return GetEventDispatcher();
}

void Stage::DestroyElement(Element *inElement)
{
	wxString name = inElement->GetName();

	// Clean up any dangling references to this object.
	if (inElement == mGrabbedElement)
		MouseUngrab(mGrabbedElement);
	if (inElement == mCurrentElement)
		mCurrentElement = NULL;
	if (inElement == mWaitElement)
		EndWait();

	// Destroy the object.
	// TODO - Implemented delayed destruction so element callbacks can
	// destroy the element they're attached to.
	delete inElement;

	// Notify Scheme that the element is dead.
	if (TInterpreter::HaveInstance())
		TInterpreter::GetInstance()->ElementDeleted(name.mb_str());
}

bool Stage::DeleteElementByName(const wxString &inName)
{
	bool found = false;
	ElementCollection::iterator i = FindElementByName(mElements, inName);
	if (i != mElements.end())
	{
		// Completely remove from the collection first, then destroy.
		Element *elem = *i;
		mElements.erase(i);
		DestroyElement(elem);
		found = true;
	}
	NotifyElementsChanged();
	return found;
}

void Stage::DeleteElements()
{
	ElementCollection::iterator i = mElements.begin();
	for (; i != mElements.end(); ++i)
		DestroyElement(*i);
	mElements.clear();
	NotifyElementsChanged();
}

bool Stage::IsMoviePlaying()
{
	ElementCollection::iterator i = mElements.begin();
	for (; i != mElements.end(); ++i)
		if (dynamic_cast<MovieElement*>(*i))
			return true;
	return false;
}

static bool is_not_movie_element(Element *inElem)
{
	return dynamic_cast<MovieElement*>(inElem) == NULL;
}

void Stage::DeleteMovieElements()
{
	// Selectively deleting pointers from an STL sequence is a bit of a
	// black art--it's hard to call erase(...) while iterating, and
	// remove_if(...)  won't free the pointers correctly.  One solution
	// is to call std::partition or std::stable_partition to sort the
	// elements into those we wish to keep, and those we wish to delete,
	// then to handle all the deletions in a bunch.
	ElementCollection::iterator first =
		std::stable_partition(mElements.begin(), mElements.end(),
							  &is_not_movie_element);
	for (ElementCollection::iterator i = first; i != mElements.end(); ++i)
	{
		gDebugLog.Log("Stopping movie: %s", (*i)->GetName().mb_str());
		DestroyElement(*i);
	}
	mElements.erase(first, mElements.end());
}

void Stage::MouseGrab(Element *inElement)
{
	ASSERT(inElement->IsLightWeight());
	ASSERT(inElement->GetEventDispatcher());
	if (mGrabbedElement)
	{
		gLog.Error("Grabbing %s while %s is already grabbed",
				   inElement->GetName().mb_str(),
				   mGrabbedElement->GetName().mb_str());
		MouseUngrab(mGrabbedElement);
	}
	mGrabbedElement = inElement;
	CaptureMouse();
}

void Stage::MouseUngrab(Element *inElement)
{
	ASSERT(inElement->IsLightWeight());
	if (!mGrabbedElement)
	{
		gLog.Error("Ungrabbing %s when it isn't grabbed",
				   inElement->GetName().mb_str());
		return;
	}
	if (inElement != mGrabbedElement)
	{
		gLog.Error("Ungrabbing %s when %s is grabbed",
				   inElement->GetName().mb_str(),
				   mGrabbedElement->GetName().mb_str());
	}

	// Force updating of the current element, cursor, etc.
	if (mCurrentElement != mGrabbedElement)
		mCurrentElement = NULL;

	// Release our grab.
	mGrabbedElement = NULL;
	ReleaseMouse();
	UpdateCurrentElementAndCursor();
}

bool Stage::ShouldSendMouseEventsToElement(Element *inElement)
{
	return !mGrabbedElement || (inElement == mGrabbedElement);
}
