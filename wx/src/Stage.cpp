// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"
#include "TInterpreter.h"
#include "TLogger.h"
#include "TVariable.h"
#include "TStyleSheet.h"

// XXX - Huh?  Who included by TStyleSheet.h (on Win32) is defining DrawText?
#undef DrawText

#include "AppConfig.h"
#include "AppGlobals.h"
#include "AppGraphics.h"
#include "FiveLApp.h"
#include "Stage.h"
#include "Element.h"
#include "MovieElement.h"
#include "Listener.h"
#include "Timecoder.h"
#include "LocationBox.h"
#if CONFIG_HAVE_QUAKE2
#	include "Quake2Engine.h"
#endif // CONFIG_HAVE_QUAKE2

USING_NAMESPACE_FIVEL


//=========================================================================
//  StageBackground
//=========================================================================
//  This class implements the wxWindow *behind* the stage.  It has few
//  duties other than (1) being black and (2) keeping the stage centered.

class StageBackground : public wxWindow
{
	DECLARE_EVENT_TABLE();

	bool mHaveStage;

	void OnSize(wxSizeEvent& event);

public:
	StageBackground(StageFrame *inStageFrame);
	void CenterStage(Stage *inStage);
};

BEGIN_EVENT_TABLE(StageBackground, wxWindow)
    EVT_SIZE(StageBackground::OnSize)
END_EVENT_TABLE()

StageBackground::StageBackground(StageFrame *inStageFrame)
	: wxWindow(inStageFrame, -1, wxDefaultPosition, wxDefaultSize),
	  mHaveStage(false)
{
    SetBackgroundColour(STAGE_BACKGROUND_COLOR);
}

void StageBackground::OnSize(wxSizeEvent& event)
{
    if (GetAutoLayout())
        Layout();
}

void StageBackground::CenterStage(Stage *inStage)
{
	ASSERT(!mHaveStage);
	mHaveStage = true;

	// Align our stage within the StageBackground using a pair of
    // sizers.  I arrived at these parameters using trial and error.
    wxBoxSizer *v_sizer = new wxBoxSizer(wxVERTICAL);
    wxBoxSizer *h_sizer = new wxBoxSizer(wxHORIZONTAL);
    v_sizer->Add(h_sizer, 1 /* stretch */, wxALIGN_CENTER, 0);
    h_sizer->Add(inStage, 0 /* no stretch */, wxALIGN_CENTER, 0);
    SetSizer(v_sizer);
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
    EVT_MENU(FIVEL_SHOW_TIMECODER, StageFrame::OnShowTimecoder)
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

	// We create our tool windows on demand.
	for (int i = 0; i < TOOL_COUNT; i++)
		mToolWindows[i] = NULL;

	// Get an appropriate icon for this window.
    SetIcon(wxICON(ic_5L));

    // Make our background black.  This should theoretically be handled
    // by 'background->SetBackgroundColour' below, but Windows takes a
    // fraction of a second to show that object.
    SetBackgroundColour(STAGE_FRAME_COLOR);

    // Create a background panel to surround our stage with.  This keeps
    // life simple.
    StageBackground *background = new StageBackground(this);

    // Create a stage object to scribble on, and center it.
    mStage = new Stage(background, this, inSize);
	background->CenterStage(mStage);

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

void StageFrame::OnExit(wxCommandEvent &inEvent)
{
    Close(TRUE);
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

void StageFrame::OnAbout(wxCommandEvent &inEvent)
{
    wxMessageDialog about(this,
                          "wx5L Prototype\n"
                          "Copyright 2002 The Trustees of Dartmouth College",
                          "About wx5L", wxOK);
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

void StageFrame::UpdateUiJumpCard(wxUpdateUIEvent &inEvent)
{
    inEvent.Enable(TInterpreter::HaveInstance());
}

void StageFrame::OnJumpCard(wxCommandEvent &inEvent)
{
    if (TInterpreter::HaveInstance())
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
	EVT_IDLE(Stage::OnIdle)
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
	  mTextCtrl(NULL), mCurrentElement(NULL), mWaitElement(NULL),
      mIsDisplayingXy(false), mIsDisplayingGrid(false),
      mIsDisplayingBorders(false)

{
    SetBackgroundColour(STAGE_COLOR);
    ClearStage(*wxBLACK);
    
    mTextCtrl =
        new wxTextCtrl(this, FIVEL_TEXT_ENTRY, "", wxDefaultPosition,
                       wxDefaultSize, wxNO_BORDER | wxTE_PROCESS_ENTER);
    mTextCtrl->Hide();

	wxLogTrace(TRACE_STAGE_DRAWING, "Stage created.");
}

Stage::~Stage()
{
	DeleteElements();
	wxLogTrace(TRACE_STAGE_DRAWING, "Stage deleted.");
}

void Stage::NotifyEnterCard()
{
	mFrame->GetLocationBox()->NotifyEnterCard();
}

void Stage::NotifyExitCard()
{
    if (mTextCtrl->IsShown())
        mTextCtrl->Hide();
	DeleteElements();
}

void Stage::NotifyScriptReload()
{
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

void Stage::UpdateCurrentElementAndCursor()
{
	// Find which element we're in.
	wxPoint pos = ScreenToClient(::wxGetMousePosition());
	Element *obj = FindLightWeightElement(pos);
	if (obj == NULL || obj != mCurrentElement)
	{
		if (mIsDisplayingXy)
			SetCursor(*wxCROSS_CURSOR);
		else
		{
			if (obj)
				SetCursor(wxCursor(wxCURSOR_HAND));
			else
				SetCursor(wxNullCursor);
		}
		mCurrentElement = obj;
	}
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
	wxLogTrace(TRACE_STAGE_DRAWING, "Ignoring request to erase stage.");

    // Ignore this event to prevent flicker--we don't need to erase,
    // because we redraw everything from the offscreen buffer.  We may need
    // to override more of these events elsewhere.
}

void Stage::OnPaint(wxPaintEvent &inEvent)
{
	wxLogTrace(TRACE_STAGE_DRAWING, "Painting stage.");

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
            if (y % large_spacing)
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
			DrawElementBorder(screen_dc, mTextCtrl->GetRect());

		ElementCollection::iterator i = mElements.begin();
		for (; i != mElements.end(); i++)
			if ((*i)->IsShown())
				DrawElementBorder(screen_dc, (*i)->GetRect());
	}
}

void Stage::DrawElementBorder(wxDC &inDC, const wxRect &inElementRect)
{
	inDC.SetPen(*wxRED_PEN);
	inDC.SetBrush(*wxTRANSPARENT_BRUSH);

	// Draw the border *outside* our rectangle.
	wxRect r = inElementRect;
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
	Element *obj = FindLightWeightElement(inEvent.GetPosition());
	if (obj)
		obj->Click();
	//else
		// TODO - For debugging Quake 2 integration.
	    //SetFocus();
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

void Stage::FillBox(const wxRect &inBounds, const wxColour &inColor)
{
    wxMemoryDC dc;
    dc.SelectObject(mOffscreenPixmap);
    wxBrush brush(inColor, wxSOLID);
    dc.SetBrush(brush);
    dc.SetPen(*wxTRANSPARENT_PEN);
	dc.DrawRectangle(inBounds.x, inBounds.y, inBounds.width, inBounds.height);
	InvalidateRect(inBounds);
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

void Stage::DestroyElement(Element *inElement)
{
	// Clean up any dangling references to this object.
	if (inElement == mCurrentElement)
		mCurrentElement = NULL;
	if (inElement == mWaitElement)
		EndWait();

	// Destroy the object.
	// TODO - Implemented delayed destruction so element callbacks can
	// destroy the element they're attached to.
	delete inElement;
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
	for (; i != mElements.end(); i++)
		DestroyElement(*i);
	mElements.clear();
	NotifyElementsChanged();
}
