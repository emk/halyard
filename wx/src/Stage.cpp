// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TInterpreter.h"
#include "TLogger.h"
#include "TVariable.h"
#include "TStyleSheet.h"

// XXX - Huh?  Who included by TStyleSheet.h is defining DrawText?
#undef DrawText

#include "FiveLApp.h"
#include "Stage.h"
#include "Listener.h"


//=========================================================================
//  LocationBox
//=========================================================================
//  A specialized combobox that works more-or-less like the "Location"
//  box in a web browser's tool bar.

class LocationBox : public wxComboBox
{
	void TryJump(const wxString &inCardName);

public:
	LocationBox(wxToolBar *inParent);
	
	void NotifyEnterCard();

	void UpdateUiLocationBox(wxUpdateUIEvent &inEvent);
	void OnChar(wxKeyEvent &inEvent);
	void OnComboBoxSelected(wxCommandEvent &inEvent);

	//////////
	// Call this function to focus the location box and prepare for the
	// user to enter a card name.
	//
	void Prompt();

	DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(LocationBox, wxComboBox)
    EVT_UPDATE_UI(FIVEL_LOCATION_BOX, LocationBox::UpdateUiLocationBox)
	EVT_CHAR(LocationBox::OnChar)
	EVT_COMBOBOX(FIVEL_LOCATION_BOX, LocationBox::OnComboBoxSelected)
END_EVENT_TABLE()

LocationBox::LocationBox(wxToolBar *inParent)
	: wxComboBox(inParent, FIVEL_LOCATION_BOX, "",
				 wxDefaultPosition, wxSize(200, -1),
				 0, NULL, wxWANTS_CHARS|wxCB_DROPDOWN|wxCB_SORT)
{

}

void LocationBox::TryJump(const wxString &inCardName)
{
    if (TInterpreter::HaveInstance())
	{
		TInterpreter *interp = TInterpreter::GetInstance();
		if (interp->IsValidCard(inCardName))
		{
			// Update our drop-down list of cards.
			if (FindString(inCardName) == -1)
				Append(inCardName);

			// Jump to the specified card.
			interp->JumpToCardByName(inCardName);
		}
		else
		{
			wxLogError("The card \"" + inCardName + "\" does not exist.");
		}
	}
}

void LocationBox::NotifyEnterCard()
{
	ASSERT(TInterpreter::HaveInstance());
	SetValue(TInterpreter::GetInstance()->CurCardName().c_str());
}

void LocationBox::UpdateUiLocationBox(wxUpdateUIEvent &inEvent)
{
	inEvent.Enable(TInterpreter::HaveInstance());
}

void LocationBox::OnChar(wxKeyEvent &inEvent)
{
	if (inEvent.GetKeyCode() == WXK_RETURN)
		TryJump(GetValue());
	else
		inEvent.Skip();
}

void LocationBox::OnComboBoxSelected(wxCommandEvent &inEvent)
{
	TryJump(inEvent.GetString());
}

void LocationBox::Prompt()
{
	SetFocus();
}


//=========================================================================
//  StageFrame Methods
//=========================================================================

BEGIN_EVENT_TABLE(StageFrame, wxFrame)
    EVT_MENU(FIVEL_EXIT, StageFrame::OnExit)
    EVT_MENU(FIVEL_RELOAD_SCRIPT, StageFrame::OnReloadScript)
    EVT_MENU(FIVEL_ABOUT, StageFrame::OnAbout)
    EVT_MENU(FIVEL_SHOW_LOG, StageFrame::OnShowLog)
    EVT_MENU(FIVEL_SHOW_LISTENER, StageFrame::OnShowListener)
    EVT_UPDATE_UI(FIVEL_FULL_SCREEN, StageFrame::UpdateUiFullScreen)
    EVT_MENU(FIVEL_FULL_SCREEN, StageFrame::OnFullScreen)
    EVT_UPDATE_UI(FIVEL_DISPLAY_XY, StageFrame::UpdateUiDisplayXy)
    EVT_MENU(FIVEL_DISPLAY_XY, StageFrame::OnDisplayXy)
    EVT_UPDATE_UI(FIVEL_DISPLAY_GRID, StageFrame::UpdateUiDisplayGrid)
    EVT_MENU(FIVEL_DISPLAY_GRID, StageFrame::OnDisplayGrid)
    EVT_UPDATE_UI(FIVEL_DISPLAY_BORDERS, StageFrame::UpdateUiDisplayBorders)
    EVT_MENU(FIVEL_DISPLAY_BORDERS, StageFrame::OnDisplayBorders)
    EVT_UPDATE_UI(FIVEL_JUMP_CARD, StageFrame::UpdateUiJumpCard)
    EVT_MENU(FIVEL_JUMP_CARD, StageFrame::OnJumpCard)
    EVT_CLOSE(StageFrame::OnClose)
END_EVENT_TABLE()

StageFrame::StageFrame(const wxChar *inTitle, wxSize inSize)
    : wxFrame((wxFrame*) NULL, -1, inTitle,
              wxDefaultPosition, wxDefaultSize,
              wxMINIMIZE_BOX|wxSYSTEM_MENU|wxCAPTION)
{
    // Set up useful logging.
    mLogWindow = new wxLogWindow(this, "Application Log", FALSE);

	// We create our listener window on demand.
	mListenerWindow = NULL;

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

    // Set up our Card menu.
    mCardMenu = new wxMenu();
    mCardMenu->Append(FIVEL_JUMP_CARD, "&Jump to Card...\tCtrl+J",
                      "Jump to a specified card by name.");

    // Set up our View menu.
    mViewMenu = new wxMenu();
    mViewMenu->AppendCheckItem(FIVEL_FULL_SCREEN,
                               "&Full Screen\tCtrl+F",
                               "Use a full screen window.");
    mViewMenu->AppendSeparator();
    mViewMenu->AppendCheckItem(FIVEL_DISPLAY_XY, "Display Cursor &XY",
                               "Display the cursor's XY position.");
    mViewMenu->AppendCheckItem(FIVEL_DISPLAY_GRID, "Display &Grid\tCtrl+G",
                               "Display a grid over the card.");
    mViewMenu->AppendCheckItem(FIVEL_DISPLAY_BORDERS,
                               "Display &Borders\tCtrl+B",
                               "Display the borders of interactive elements.");

    // Set up our Window menu.
    mWindowMenu = new wxMenu();
    mWindowMenu->Append(FIVEL_SHOW_LISTENER, "Show &Listener\tCtrl+L",
                        "Show interactive script listener.");
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

void StageFrame::OnShowListener()
{
	if (!mListenerWindow)
		mListenerWindow = new Listener(this);
	if (!mListenerWindow->IsShown())
		mListenerWindow->Show();
	mListenerWindow->Raise();
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

void StageFrame::UpdateUiDisplayGrid(wxUpdateUIEvent &inEvent)
{
    inEvent.Check(mStage->IsDisplayingGrid());
}

void StageFrame::OnDisplayGrid()
{
    mStage->ToggleDisplayGrid();
}

void StageFrame::UpdateUiDisplayBorders(wxUpdateUIEvent &inEvent)
{
    inEvent.Check(mStage->IsDisplayingBorders());
}

void StageFrame::OnDisplayBorders()
{
    mStage->ToggleDisplayBorders();
}

void StageFrame::UpdateUiJumpCard(wxUpdateUIEvent &inEvent)
{
    inEvent.Enable(TInterpreter::HaveInstance());
}

void StageFrame::OnJumpCard()
{
	mLocationBox->Prompt();
	/*
    if (TInterpreter::HaveInstance())
    {
        wxTextEntryDialog dialog(this, "Jump to Card", "Card:");
        if (dialog.ShowModal() == wxID_OK)
        {
            wxString card_name = dialog.GetValue();
            TInterpreter::GetInstance()->JumpToCardByName(card_name);
        }
    }
	*/
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
    EVT_ERASE_BACKGROUND(Stage::OnEraseBackground)
    EVT_PAINT(Stage::OnPaint)
    EVT_TEXT_ENTER(FIVEL_TEXT_ENTRY, Stage::OnTextEnter)
	EVT_LEFT_DOWN(Stage::OnLeftDown)
END_EVENT_TABLE()

Stage::Stage(wxWindow *inParent, StageFrame *inFrame, wxSize inStageSize)
    : wxWindow(inParent, -1, wxDefaultPosition, inStageSize),
      mFrame(inFrame), mStageSize(inStageSize),
      mOffscreenPixmap(inStageSize.GetWidth(), inStageSize.GetHeight(), -1),
	  mTextCtrl(NULL), mLastStageObject(NULL),
      mIsDisplayingXy(false), mIsDisplayingGrid(false),
      mIsDisplayingBorders(false)

{
    SetBackgroundColour(*wxBLACK);
    ClearStage(*wxBLACK);
    
    mTextCtrl =
        new wxTextCtrl(this, FIVEL_TEXT_ENTRY, "", wxDefaultPosition,
                       wxDefaultSize, wxNO_BORDER | wxTE_PROCESS_ENTER);
    mTextCtrl->Hide();
}

Stage::~Stage()
{
	DeleteStageObjects();
}

void Stage::NotifyEnterCard()
{
	mFrame->GetLocationBox()->NotifyEnterCard();
}

void Stage::NotifyExitCard()
{
    if (mTextCtrl->IsShown())
        mTextCtrl->Hide();
	DeleteStageObjects();
}

void Stage::NotifyScriptReload()
{
    NotifyExitCard();
	gStyleSheetManager.RemoveAll();
}

void Stage::NotifyObjectsChanged()
{
	// Don't do anything unless there's a good chance this object still
	// exists in some sort of valid state.
	// TODO - Is IsShown a good way to tell whether a window is still good?
	if (IsShown())
	{
		// Update our object borders (if necessary).
		if (mIsDisplayingBorders)
			InvalidateStage();
	}
}

void Stage::OnMouseMove(wxMouseEvent &inEvent)
{
	// Do any mouse-moved processing for our StageObjects.
	StageObject *obj = FindLightWeightStageObject(inEvent.GetPosition());
	if (obj == NULL || obj != mLastStageObject)
	{
		if (!mIsDisplayingXy)
		{
			if (obj)
				SetCursor(wxCursor(wxCURSOR_HAND));
			else
				SetCursor(wxNullCursor);
		}
		mLastStageObject = obj;
	}

    if (mIsDisplayingXy)
    {
		SetCursor(*wxCROSS_CURSOR);

        wxClientDC dc(this);

        // Get our current screen location.
        wxPoint pos = inEvent.GetPosition();
        long x = dc.DeviceToLogicalX(pos.x);
        long y = dc.DeviceToLogicalY(pos.y);

        // Get the color at that screen location.
        // XXX - May not work on non-Windows platforms, according to
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
}

void Stage::OnEraseBackground(wxEraseEvent &inEvent)
{
    // Ignore this event to prevent flicker--we don't need to erase,
    // because we redraw everything from the offscreen buffer.  We may need
    // to override more of these events elsewhere.
}

void Stage::OnPaint(wxPaintEvent &inEvent)
{
    // Set up our drawing contexts.
    wxPaintDC screen_dc(this);
    wxMemoryDC offscreen_dc;
    offscreen_dc.SelectObject(mOffscreenPixmap);
    
    // Blit our offscreen pixmap to the screen.
    // TODO - Could we optimize drawing by only blitting dirty regions?
    screen_dc.Blit(0, 0, mStageSize.GetWidth(), mStageSize.GetHeight(),
                   &offscreen_dc, 0, 0);

    // If necessary, draw the grid.
    if (mIsDisplayingGrid)
    {
        int width = mStageSize.GetWidth();
        int height = mStageSize.GetHeight();
        int small_spacing = 10;
        int large_spacing = small_spacing * 10;

        // Draw the minor divisions of the grid.
        screen_dc.SetPen(*wxLIGHT_GREY_PEN);
        for (int x = 0; x < width; x += small_spacing)
            if (x % large_spacing)
                screen_dc.DrawLine(x, 0, x, height);
        for (int y = 0; y < width; y += small_spacing)
            if (x % large_spacing)
                screen_dc.DrawLine(0, y, width, y);

        // Draw the major divisions of the grid.
        screen_dc.SetPen(*wxGREEN_PEN);
        for (int x2 = 0; x2 < width; x2 += large_spacing)
            screen_dc.DrawLine(x2, 0, x2, height);
        for (int y2 = 0; y2 < width; y2 += large_spacing)
            screen_dc.DrawLine(0, y2, width, y2);
    }

	// If necessary, draw the borders.
	if (mIsDisplayingBorders)
	{
		if (mTextCtrl->IsShown())
			DrawObjectBorder(screen_dc, mTextCtrl->GetRect());

		StageObjectCollection::iterator i = mStageObjects.begin();
		for (; i != mStageObjects.end(); i++)
			DrawObjectBorder(screen_dc, (*i)->GetRect());
	}
}

void Stage::DrawObjectBorder(wxDC &inDC, const wxRect &inObjectRect)
{
	inDC.SetPen(*wxRED_PEN);
	inDC.SetBrush(*wxTRANSPARENT_BRUSH);

	// Draw the border *outside* our rectangle.
	wxRect r = inObjectRect;
	r.Inflate(1);
	inDC.DrawRectangle(r.x, r.y, r.width, r.height);
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
	StageObject *obj = FindLightWeightStageObject(inEvent.GetPosition());
	if (obj)
		obj->Click();
}

void Stage::InvalidateStage()
{
    InvalidateRect(wxRect(0, 0,
                          mStageSize.GetWidth(), mStageSize.GetHeight()));
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
    InvalidateStage();
}

void Stage::DrawPixMap(GraphicsTools::Point inPoint,
					   GraphicsTools::PixMap &inPixMap)
{
	// Mark the rectangle as dirty.
	InvalidateRect(wxRect(inPoint.x, inPoint.y,
						  inPixMap.width, inPixMap.height));

	// XXX - We draw our data in a slow, kludgy fashion.
	// This code is stolen from the Mac engine and
	// quickly hacked into a drawing routine.  It needs
	// to be replaced with something faster and cleaner.

	using GraphicsTools::AlphaBlendChannel;
	using GraphicsTools::Color;
	using GraphicsTools::Distance;
	using GraphicsTools::Point;
	
	// Clip our pixmap boundaries to fit within our screen.
	int gworld_width = mStageSize.GetWidth();
	int gworld_height = mStageSize.GetHeight();
	Point begin = inPoint;
	begin.x = Max(0, Min(gworld_width, begin.x));
	begin.y = Max(0, Min(gworld_height, begin.y));
	begin = begin - inPoint;
	Point end = inPoint + Point(inPixMap.width, inPixMap.height);
	end.x = Max(0, Min(gworld_width, end.x));
	end.y = Max(0, Min(gworld_height, end.y));
	end = end - inPoint;
	
	// Do some sanity checks on our clipping boundaries.
	ASSERT(begin.x == end.x || // No drawing
		   (0 <= begin.x && begin.x < end.x && end.x <= inPixMap.width));
	ASSERT(begin.y == end.y || // No drawing
		   (0 <= begin.y && begin.y < end.y && end.y <= inPixMap.height));
		
	// Figure out where in memory to get the data for the first row.
	Color *portable_base_addr = inPixMap.pixels;
	Distance portable_row_size = inPixMap.pitch;
	Color *portable_row_start =
		portable_base_addr + begin.y * portable_row_size + begin.x;

	// Do the actual drawing.
	// TODO - PERFORMANCE - This blitter is mind-numbingly slow.  We need
	// to somehow get access the the wxBitmap offscreen bits, which
	// will probably require some gross hacking of the wxWindows source.
	wxMemoryDC dc;
    dc.SelectObject(mOffscreenPixmap);
	wxPen pen(*wxBLACK, 1, wxSOLID);
	for (int y = begin.y; y < end.y; y++)
	{
		Color *portable_cursor = portable_row_start;
		for (int x = begin.x; x < end.x; x++)
		{
			// Make sure we're in bounds.
			ASSERT(portable_cursor >= portable_base_addr);
			ASSERT(portable_cursor <
				   portable_base_addr + (inPixMap.height *
										 portable_row_size));
			
			// Draw a single pixel, very slowly.
			GraphicsTools::Color new_color = *portable_cursor;
			wxColour color;
			dc.GetPixel(inPoint.x + x, inPoint.y + y, &color);
			unsigned char red = color.Red();
			unsigned char green = color.Green();
			unsigned char blue = color.Blue();
			red = AlphaBlendChannel(red, new_color.red, new_color.alpha);
			green = AlphaBlendChannel(green, new_color.green,
									  new_color.alpha);
			blue = AlphaBlendChannel(blue, new_color.blue,
									 new_color.alpha);

			// XXX - Probably the world's worst blit routine.
			pen.SetColour(red, green, blue);
			dc.SetPen(pen);
			dc.DrawPoint(inPoint.x + x, inPoint.y + y);
			portable_cursor++;
		}
		portable_row_start += portable_row_size;
	}
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
	NotifyObjectsChanged();

    // Put our Scheme interpreter to sleep.
    ASSERT(TInterpreter::HaveInstance() &&
           !TInterpreter::GetInstance()->Paused());
    TInterpreter::GetInstance()->Pause();
}

wxString Stage::FinishModalTextInput()
{
    ASSERT(mTextCtrl->IsShown());

    // Wake up our Scheme interpreter.
    ASSERT(TInterpreter::HaveInstance() &&
           TInterpreter::GetInstance()->Paused());
    TInterpreter::GetInstance()->WakeUp();

	// Store our result somewhere useful.
	gVariableManager.SetString("_modal_input_text", mTextCtrl->GetValue());

    // Hide our text control and get the text.
    mTextCtrl->Hide();
	NotifyObjectsChanged();
    return mTextCtrl->GetValue();
}

void Stage::AddStageObject(StageObject *inStageObject)
{
	// Delete any existing StageObject with the same name.
	(void) DeleteStageObjectByName(inStageObject->GetName());

	// Add the new StageObject to our list.
	mStageObjects.push_back(inStageObject);
	NotifyObjectsChanged();
}

StageObject *Stage::FindLightWeightStageObject(const wxPoint &inPoint)
{
	// Look for the most-recently-added StageObject containing inPoint.
	StageObject *result = NULL;
	StageObjectCollection::iterator i = mStageObjects.begin();
	for (; i != mStageObjects.end(); i++)
		if ((*i)->IsLightWeight() &&
			(*i)->IsPointInStageObject(inPoint))
			result = *i;
	return result;
}

bool Stage::DeleteStageObjectByName(const wxString &inName)
{
	bool found = false;
	StageObjectCollection::iterator i = mStageObjects.begin();
	for (; i != mStageObjects.end(); i++)
	{
		if ((*i)->GetName() == inName)
		{
			delete *i;
			mStageObjects.erase(i);
			found = true;
			break;
		}
	}
	NotifyObjectsChanged();
	return found;
}

void Stage::DeleteStageObjects()
{
	mLastStageObject = NULL;
	StageObjectCollection::iterator i = mStageObjects.begin();
	for (; i != mStageObjects.end(); i++)
		delete *i;
	mStageObjects.clear();
	NotifyObjectsChanged();
}


//=========================================================================
//  StageObject Methods
//=========================================================================

StageObject::StageObject(Stage *inStage, const wxString &inName)
	: mStage(inStage), mName(inName)
{
    ASSERT(mStage);
    ASSERT(mName != "");

	mStage->AddStageObject(this);
}
