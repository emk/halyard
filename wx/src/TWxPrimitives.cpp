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

// Needed for RegisterWxPrimitives.
#include "TamaleHeaders.h"
#include <wx/image.h>

#include "TPrimitives.h"
#include "TWxPrimitives.h"

// Needed to implement the primitives.
#include "TCommonPrimitives.h"
#include "CrashReporter.h"
#include "AppConfig.h"
#include "TStyleSheet.h"
#include "FiveLApp.h"
#include "StageFrame.h"
#include "Stage.h"
#include "DrawingArea.h"
#include "Zone.h"
#include "Overlay.h"
#include "AnimatedOverlay.h"
#include "MediaElement.h"
#include "MovieElement.h"
#include "Widget.h"
#include "FileSystem.h"
#include "EventDispatcher.h"
#include "ImageCache.h"
#include "CursorManager.h"
#include "CursorElement.h"
#include "AudioStream.h"
#include "GeigerAudioStream.h"
#include "VorbisAudioStream.h"
#include "AudioStreamElement.h"
#include "GeigerSynthElement.h"
#include "CommonWxConv.h"
#include "BrowserElementWx.h"
#include "BrowserElementIE.h"
#include "ActiveXElement.h"
#include "EditBox.h"
#include "TStateDB.h"
#include "dlg/MultiButtonDlg.h"
#include "Downloader.h"

USING_NAMESPACE_FIVEL
using GraphicsTools::Color;
using FileSystem::Path;


//=========================================================================
//  RegisterWxPrimitives
//=========================================================================
//  Install our wxWindows-specific primitives.  A lot of these are very
//  kludgy and should be replaced later on as the editing GUI improves.

void FIVEL_NS RegisterWxPrimitives() {
	REGISTER_5L_PRIMITIVE(ActiveX);
	REGISTER_5L_PRIMITIVE(ActiveXPropGet);
	REGISTER_5L_PRIMITIVE(ActiveXPropSet);
	REGISTER_5L_PRIMITIVE(AudioStreamGeiger);
	REGISTER_5L_PRIMITIVE(AudioStreamGeigerSetCps);
	REGISTER_5L_PRIMITIVE(AudioStreamSine);
	REGISTER_5L_PRIMITIVE(AudioStreamVorbis);
	REGISTER_5L_PRIMITIVE(Browser);
	REGISTER_5L_PRIMITIVE(BrowserCanBack);
	REGISTER_5L_PRIMITIVE(BrowserCanForward);
	REGISTER_5L_PRIMITIVE(BrowserCanReload);
	REGISTER_5L_PRIMITIVE(BrowserCanStop);
	REGISTER_5L_PRIMITIVE(BrowserBack);
	REGISTER_5L_PRIMITIVE(BrowserForward);
    REGISTER_5L_PRIMITIVE(BrowserLoadPage);
	REGISTER_5L_PRIMITIVE(BrowserReload);
	REGISTER_5L_PRIMITIVE(BrowserStop);
	REGISTER_5L_PRIMITIVE(CancelDownload);
	REGISTER_5L_PRIMITIVE(ColorAt);
	REGISTER_5L_PRIMITIVE(CopyStringToClipboard);
    REGISTER_5L_PRIMITIVE(CursorElement);
	REGISTER_5L_PRIMITIVE(DataPath);
	REGISTER_5L_PRIMITIVE(DcPop);
	REGISTER_5L_PRIMITIVE(DcPush);
	REGISTER_5L_PRIMITIVE(DcRect);
    REGISTER_5L_PRIMITIVE(DebugReportAddFile);
    REGISTER_5L_PRIMITIVE(DeleteElements);
    REGISTER_5L_PRIMITIVE(Dialog);
	REGISTER_5L_PRIMITIVE(Download);
	REGISTER_5L_PRIMITIVE(DrawBoxFill);
	REGISTER_5L_PRIMITIVE(DrawBoxOutline);
	REGISTER_5L_PRIMITIVE(DrawLine);
    REGISTER_5L_PRIMITIVE(DrawLoadProgress);
	REGISTER_5L_PRIMITIVE(EditBox);
	REGISTER_5L_PRIMITIVE(EditBoxGetValue);
	REGISTER_5L_PRIMITIVE(ElementExists);
	REGISTER_5L_PRIMITIVE(ElementIsShown);
	REGISTER_5L_PRIMITIVE(ElementSetShown);
	REGISTER_5L_PRIMITIVE(ElementSetInDragLayer);
	REGISTER_5L_PRIMITIVE(EnableExpensiveEvents);
	REGISTER_5L_PRIMITIVE(GeigerSynth);
    REGISTER_5L_PRIMITIVE(HideCursorUntilMouseMoved);
    REGISTER_5L_PRIMITIVE(Heartbeat);
	REGISTER_5L_PRIMITIVE(LaunchUpdateInstallerBeforeExiting);
	REGISTER_5L_PRIMITIVE(LoadPic);
	REGISTER_5L_PRIMITIVE(LoadSubPic);
    REGISTER_5L_PRIMITIVE(MarkUnprocessedEventsAsStale);
	REGISTER_5L_PRIMITIVE(Mask);
    REGISTER_5L_PRIMITIVE(MaybeLoadSplash);
    REGISTER_5L_PRIMITIVE(MeasurePic);
	REGISTER_5L_PRIMITIVE(MediaSetVolume);
	REGISTER_5L_PRIMITIVE(MouseGrab);
	REGISTER_5L_PRIMITIVE(MouseIsGrabbed);
	REGISTER_5L_PRIMITIVE(MouseIsGrabbedBy);
	REGISTER_5L_PRIMITIVE(MousePosition);
	REGISTER_5L_PRIMITIVE(MouseUngrab);
	REGISTER_5L_PRIMITIVE(Movie);
	REGISTER_5L_PRIMITIVE(MovieEndPlayback);
	REGISTER_5L_PRIMITIVE(MoviePause);
	REGISTER_5L_PRIMITIVE(MovieResume);
	REGISTER_5L_PRIMITIVE(MovieSetTimeout);
    REGISTER_5L_PRIMITIVE(MoveElementTo);
	REGISTER_5L_PRIMITIVE(NotifyEnterCard);
	REGISTER_5L_PRIMITIVE(NotifyExitCard);
    REGISTER_5L_PRIMITIVE(OpenInBrowser);
	REGISTER_5L_PRIMITIVE(Overlay);
	REGISTER_5L_PRIMITIVE(OverlaySetShape);
	REGISTER_5L_PRIMITIVE(OverlayAnimated);
	REGISTER_5L_PRIMITIVE(Refresh);
	REGISTER_5L_PRIMITIVE(RefreshSplashScreen);
	REGISTER_5L_PRIMITIVE(Screenshot);
	REGISTER_5L_PRIMITIVE(RegisterCard);
	REGISTER_5L_PRIMITIVE(RegisterCursor);
	REGISTER_5L_PRIMITIVE(RegisterEventDispatcher);
    REGISTER_5L_PRIMITIVE(Screen);
    REGISTER_5L_PRIMITIVE(SetImageCacheSize);
    REGISTER_5L_PRIMITIVE(SetZoneCursor);
    REGISTER_5L_PRIMITIVE(TamaleExit);
	REGISTER_5L_PRIMITIVE(TextAA);
    REGISTER_5L_PRIMITIVE(Wait);
    REGISTER_5L_PRIMITIVE(WakeUpIfNecessary);
    REGISTER_5L_PRIMITIVE(WantsCursorGet);
    REGISTER_5L_PRIMITIVE(WantsCursorSet);
    REGISTER_5L_PRIMITIVE(Zone);
    REGISTER_5L_PRIMITIVE(ZoneSetShape);
}


//=========================================================================
//  Utility Functions
//=========================================================================

#define FIND_ELEMENT(TYPE, VAR, NAME) \
	ElementPtr VAR##_temp = wxGetApp().GetStage()->FindElement(NAME); \
	if (!VAR##_temp) { \
		THROW("The element does not exist."); \
	} \
	shared_ptr<TYPE> VAR = \
        shared_ptr<TYPE>(VAR##_temp, dynamic_cast_tag()); \
	if (!VAR) { \
		THROW("The element is not of type " #TYPE); \
	}

static DrawingArea *GetCurrentDrawingArea() {
	return wxGetApp().GetStage()->GetCurrentDrawingArea();
}

// This method registers an element with the stage. We can't do registration
// in Element::Element because it's dangerous to create smart-pointers to
// objects which are not yet fully created.
template <typename E>
static void register_elem(shared_ptr<E> elem_ptr) {
    Stage *stage = wxGetApp().GetStage();
    stage->AddElement(boost::static_pointer_cast<Element>(elem_ptr));
}

// A wrapper around register_elem which creates the necessary shared_ptr.
template <typename E>
static E *R(E *elem) {
    register_elem(ElementPtr(elem));
    return elem;
}

#define CHECK_SUSPEND_OK(PRIMNAME) \
    do { \
        if (!TInterpreter::GetInstance()->CanSuspend()) { \
            THROW("You cannot call " PRIMNAME " from inside a callback."); \
        } \
    } while (0)

static wxBitmap load_picture(const std::string &inName);


//=========================================================================
//  Implementation of wxWindows Primitives
//=========================================================================

DEFINE_5L_PRIMITIVE(ActiveX) {
	std::string name, control_name;
	TRect bounds;
    TCallbackPtr dispatcher;

	inArgs >> SymbolName(name) >> dispatcher >> bounds >> control_name;

    R(new ActiveXElement(wxGetApp().GetStage(), name.c_str(),
                         TToWxRect(bounds), dispatcher, control_name.c_str()));
}

DEFINE_5L_PRIMITIVE(ActiveXPropGet) {
    std::string name, prop;
    inArgs >> SymbolName(name) >> prop;
    FIND_ELEMENT(ActiveXElement, element, name.c_str());
    ::SetPrimitiveResult(WxToTValue(element->Prop(prop.c_str())));
}

DEFINE_5L_PRIMITIVE(ActiveXPropSet) {
    std::string name, prop;
    TValue value;
    inArgs >> SymbolName(name) >> prop >> value;
    FIND_ELEMENT(ActiveXElement, element, name.c_str());
    element->SetProp(prop.c_str(), TToWxValue(value));
}

DEFINE_5L_PRIMITIVE(AudioStreamGeiger) {
	std::string name, path;
    double volume;
	TCallbackPtr dispatcher;
	inArgs >> SymbolName(name) >> dispatcher >> path >> volume;
    R(new AudioStreamElement(wxGetApp().GetStage(), name.c_str(),
                             new GeigerAudioStream(path.c_str(), volume),
							 dispatcher));
}

DEFINE_5L_PRIMITIVE(AudioStreamGeigerSetCps) {
	std::string name;
	double cps;
	inArgs >> SymbolName(name) >> cps;
	FIND_ELEMENT(AudioStreamElement, element, name.c_str());
	GeigerAudioStream *stream =
		dynamic_cast<GeigerAudioStream*>(element->GetAudioStream());
	if (stream)
		stream->SetChirpsPerSecond(cps);
	else
		THROW("Audio stream was not a geiger stream.");
}

DEFINE_5L_PRIMITIVE(AudioStreamSine) {
	std::string name;
    double volume;
	uint32 frequency;
	TCallbackPtr dispatcher;	
	inArgs >> SymbolName(name) >> dispatcher >> volume >> frequency;
    R(new AudioStreamElement(wxGetApp().GetStage(), name.c_str(),
                             new SineAudioStream(frequency, volume),
							 dispatcher));
}

DEFINE_5L_PRIMITIVE(AudioStreamVorbis) {
	std::string name, path;
    double volume;
	uint32 buffer_size;
	bool should_loop;
	TCallbackPtr dispatcher;
	inArgs >> SymbolName(name) >> dispatcher >> path >> volume >> buffer_size
		   >> should_loop;
    R(new AudioStreamElement(wxGetApp().GetStage(), name.c_str(),
                             new VorbisAudioStream(path.c_str(),
                                                   buffer_size,
                                                   should_loop,
                                                   volume),
							 dispatcher));
}

DEFINE_5L_PRIMITIVE(Browser) {
	std::string name;
    bool want_builtin;
	TRect bounds;
    TCallbackPtr dispatcher;

	inArgs >> SymbolName(name) >> dispatcher >> bounds >> want_builtin;

    if (want_builtin)
        R(new BrowserElementWx(wxGetApp().GetStage(), name.c_str(),
                               TToWxRect(bounds), dispatcher));
    else
        R(new BrowserElementIE(wxGetApp().GetStage(), name.c_str(),
                               TToWxRect(bounds), dispatcher));
}

DEFINE_5L_PRIMITIVE(BrowserCanBack) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(BrowserElement, browser, name.c_str());
    ::SetPrimitiveResult(browser->CanGoBack());
}

DEFINE_5L_PRIMITIVE(BrowserCanForward) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(BrowserElement, browser, name.c_str());
    ::SetPrimitiveResult(browser->CanGoForward());
}

DEFINE_5L_PRIMITIVE(BrowserCanReload) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(BrowserElement, browser, name.c_str());
    ::SetPrimitiveResult(browser->CanRefresh());
}

DEFINE_5L_PRIMITIVE(BrowserCanStop) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(BrowserElement, browser, name.c_str());
    ::SetPrimitiveResult(browser->CanStop());
}

DEFINE_5L_PRIMITIVE(BrowserBack) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(BrowserElement, browser, name.c_str());
    ::SetPrimitiveResult(browser->GoBack());
}

DEFINE_5L_PRIMITIVE(BrowserForward) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(BrowserElement, browser, name.c_str());
    ::SetPrimitiveResult(browser->GoForward());
}

DEFINE_5L_PRIMITIVE(BrowserLoadPage) {
    std::string name, file_or_url;
    inArgs >> SymbolName(name) >> file_or_url;
    FIND_ELEMENT(BrowserElement, browser, name.c_str());
	browser->LoadPage(file_or_url.c_str());
}

DEFINE_5L_PRIMITIVE(BrowserReload) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(BrowserElement, browser, name.c_str());
    ::SetPrimitiveResult(browser->Refresh());
}

DEFINE_5L_PRIMITIVE(BrowserStop) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(BrowserElement, browser, name.c_str());
    ::SetPrimitiveResult(browser->Stop());
}

DEFINE_5L_PRIMITIVE(CancelDownload) {
	Downloader::GetInstance()->CancelDownload();
}

DEFINE_5L_PRIMITIVE(ColorAt) {
	TPoint at;
	inArgs >> at;
	::SetPrimitiveResult(GetCurrentDrawingArea()->GetPixel(at.X(), at.Y()));
}

DEFINE_5L_PRIMITIVE(CopyStringToClipboard) {
	// This primitive is to allow people to write developer tools in scheme.
	// It isn't actually a necessary part of the API. 
	std::string string;
	inArgs >> string;
	wxGetApp().GetStage()->CopyStringToClipboard(string.c_str());
}

DEFINE_5L_PRIMITIVE(CursorElement) {
	std::string name, cursor_reg_name;
	TRect bounds;
	TCallbackPtr dispatcher;
	bool is_trans;
	
	inArgs >> SymbolName(name) >> bounds >> dispatcher >> is_trans
           >> SymbolName(cursor_reg_name);
    Stage *stage = wxGetApp().GetStage();

    // Lots of pointer casting fun: We need an ElementPtr and a CursorPtr
    // which both point to this same object.  Watch the steps carefully...
    shared_ptr<CursorElement> elem(
        new CursorElement(stage, name.c_str(), TToWxRect(bounds), dispatcher,
                          is_trans, cursor_reg_name));
    register_elem(elem);
    
    // Tell the elem to register itself (passing it a copy of the smart
    // pointer that it will need).
    // TODO - Ick.
    elem->Register(wxGetApp().GetStage()->GetCursorManager(), elem);
}

DEFINE_5L_PRIMITIVE(DataPath) {
    // By the time we get here, our application name will actually be
    // our script name.
    FileSystem::Path dir = FileSystem::GetScriptDataDirectory();
    ::SetPrimitiveResult(dir.ToNativePathString());
}

DEFINE_5L_PRIMITIVE(DcPop) {
	std::string name;	
	inArgs >> SymbolName(name);
	FIND_ELEMENT(Element, elem, name.c_str());
	wxGetApp().GetStage()->PopDrawingContext(elem);
}

DEFINE_5L_PRIMITIVE(DcPush) {
	std::string name;	
	inArgs >> SymbolName(name);
	FIND_ELEMENT(Element, elem, name.c_str());
	wxGetApp().GetStage()->PushDrawingContext(elem);
}

DEFINE_5L_PRIMITIVE(DcRect) {
	wxRect bounds = GetCurrentDrawingArea()->GetBounds();
	::SetPrimitiveResult(WxToTRect(wxRect(0, 0, bounds.width, bounds.height)));
}

DEFINE_5L_PRIMITIVE(DebugReportAddFile) {
    std::string name;
    std::string description;
    inArgs >> name >> description;
    CrashReporter::GetInstance()->AddDiagnosticFile(name, description);
}

DEFINE_5L_PRIMITIVE(DeleteElements) {
	if (!inArgs.HasMoreArguments()) {
		wxGetApp().GetStage()->DeleteElements();
	} else {
		while (inArgs.HasMoreArguments()) {
			std::string name;
			inArgs >> SymbolName(name);
			bool found =
				wxGetApp().GetStage()->DeleteElementByName(name.c_str());
			if (!found)
				gDebugLog.Caution("Deleting non-existant element '%s'.",
								  name.c_str());
		}
	}
}

DEFINE_5L_PRIMITIVE(Dialog) {
    std::string title, message, button1, button2, button3;
    inArgs >> title >> message >> button1;
    if (inArgs.HasMoreArguments())
        inArgs >> button2;
    if (inArgs.HasMoreArguments())
        inArgs >> button3;
    MultiButtonDlg dlg(wxGetApp().GetStage(),
                       title.c_str(), message.c_str(), button1.c_str(),
                       button2.c_str(), button3.c_str());
    ::SetPrimitiveResult(dlg.ShowModal());
}

DEFINE_5L_PRIMITIVE(Download) {
	std::string URL, file;
	inArgs >> URL >> file;

	::SetPrimitiveResult(Downloader::GetInstance()->Get(URL, file));
}

DEFINE_5L_PRIMITIVE(DrawBoxFill) {
	TRect bounds;
	Color color;

	inArgs >> bounds >> color;
	GetCurrentDrawingArea()->FillBox(TToWxRect(bounds), color);
}

DEFINE_5L_PRIMITIVE(DrawBoxOutline) {
	TRect bounds;
	Color color;
	int32 width;

	inArgs >> bounds >> color >> width;
	GetCurrentDrawingArea()->OutlineBox(TToWxRect(bounds), color, width);

}

DEFINE_5L_PRIMITIVE(DrawLine) {
	TPoint from, to;
	Color color;
	int32 width;

	inArgs >> from >> to >> color >> width;
	GetCurrentDrawingArea()->DrawLine(TToWxPoint(from), TToWxPoint(to),
									  color, width);

}

DEFINE_5L_PRIMITIVE(DrawLoadProgress) {
    wxGetApp().GetStage()->DrawLoadProgress();
}

DEFINE_5L_PRIMITIVE(EditBox) {
	std::string name, text;
    TCallbackPtr dispatcher;
	TRect bounds;
    uint32 text_sz;
    bool multiline, send_enter_event;

	inArgs >> SymbolName(name) >> dispatcher >> bounds >> text >> text_sz
           >> multiline >> send_enter_event;

    R(new EditBox(wxGetApp().GetStage(), name.c_str(), dispatcher,
                  TToWxRect(bounds), text.c_str(), text_sz, multiline,
                  send_enter_event));
}

DEFINE_5L_PRIMITIVE(EditBoxGetValue) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(EditBox, elem, name.c_str());
    ::SetPrimitiveResult(elem->GetValue().mb_str());
}

DEFINE_5L_PRIMITIVE(ElementExists) {
	std::string name;
	inArgs >> SymbolName(name);
	if (wxGetApp().GetStage()->FindElement(name.c_str()))
		::SetPrimitiveResult(true);
	else
		::SetPrimitiveResult(false);
}

DEFINE_5L_PRIMITIVE(ElementIsShown) {
	std::string name;
	inArgs >> SymbolName(name);
	FIND_ELEMENT(Element, element, name.c_str());
	::SetPrimitiveResult(element->IsShown());
}

DEFINE_5L_PRIMITIVE(ElementSetShown) {
	std::string name;
	bool show;
	inArgs >> SymbolName(name) >> show;
	FIND_ELEMENT(Element, element, name.c_str());
	element->Show(show);
	// TODO - Override MovieElement::Show for unshowable movies.
}

DEFINE_5L_PRIMITIVE(ElementSetInDragLayer) {
	std::string name;
	bool in_drag_layer;
	inArgs >> SymbolName(name) >> in_drag_layer;
	FIND_ELEMENT(LightweightElement, element, name.c_str());
	element->SetInDragLayer(in_drag_layer);
}

DEFINE_5L_PRIMITIVE(EnableExpensiveEvents) {
	bool enable;
	inArgs >> enable;
	EventDispatcher::EnableExpensiveEvents(enable);
}

DEFINE_5L_PRIMITIVE(GeigerSynth) {
	std::string name, state_path, chirp_location, loop_location;
	double loop_cps, volume;
	uint32 buffer_size;

	inArgs >> name >> SymbolName(state_path) >> chirp_location
           >> volume >> buffer_size;

    GeigerSynthElement *element =
        R(new GeigerSynthElement(wxGetApp().GetStage(), name.c_str(),
                                 state_path, chirp_location.c_str(), 1000,
                                 volume));

	while (inArgs.HasMoreArguments()) {
		inArgs >> loop_cps >> loop_location;
        element->AddLoop(loop_cps, loop_location.c_str());
	}
    element->DoneAddingLoops();
}

DEFINE_5L_PRIMITIVE(HideCursorUntilMouseMoved) {
    wxGetApp().GetStage()->HideCursorUntilMouseMoved();
}

DEFINE_5L_PRIMITIVE(Heartbeat) {
	::SkipPrimitiveLogging();
    wxGetApp().Heartbeat();
}


//  

/*---------------------------------------------------------------------
    (LOADPIC PICTURE X Y <FLAGS...>)

	Display the given picture at the given location (x, y).

	XXX - Flags not implemented!
-----------------------------------------------------------------------*/

static wxBitmap load_picture(const std::string &inName) {
	// Load our image.
	return wxGetApp().GetStage()->GetImageCache()->GetBitmap(inName.c_str());
}

static void draw_picture(const std::string &inName, TPoint inLoc,
						 TRect *inRect = NULL)
{
	// Load our image.
	wxBitmap bitmap(load_picture(inName));
	if (!bitmap.Ok())
		THROW("Can't load the specified image");

	// If we were given a sub-rectangle, try to extract it.
	if (inRect) {
		wxRect rect(inRect->Left(), inRect->Top(),
					inRect->Right() - inRect->Left(),
					inRect->Bottom() - inRect->Top());
		if (rect.GetX() < 0 || rect.GetY() < 0 ||
			rect.GetWidth() > bitmap.GetWidth() ||
			rect.GetHeight() > bitmap.GetHeight())
		{
			THROW("Sub-rectangle does not fit inside image");
		}
		bitmap = bitmap.GetSubBitmap(rect);
		inLoc.SetX(inLoc.X() + rect.GetX());
		inLoc.SetY(inLoc.Y() + rect.GetY());
	}

	// Draw our bitmap.
	GetCurrentDrawingArea()->DrawBitmap(bitmap, inLoc.X(), inLoc.Y());

	// Update our special variables.
	// XXX - TRect constructor uses height/width order!  Ayiee!
	TRect bounds(TRect(inLoc.X(), inLoc.Y(),
					   inLoc.X() + bitmap.GetWidth(),
					   inLoc.Y() + bitmap.GetHeight()));
					   
	UpdateSpecialVariablesForGraphic(bounds);	
}

DEFINE_5L_PRIMITIVE(LaunchUpdateInstallerBeforeExiting) {
    wxGetApp().LaunchUpdateInstallerBeforeExiting();
}

DEFINE_5L_PRIMITIVE(LoadPic) {
	std::string	picname;
    TPoint		loc;

	inArgs >> picname >> loc;

	// Process flags.
	// XXX - We're just throwing them away for now.
	if (inArgs.HasMoreArguments())
		THROW("loadpic flags are not implemented");

	// Do the dirty work.
	draw_picture(picname, loc);
}

DEFINE_5L_PRIMITIVE(LoadSubPic) {
	std::string	picname;
    TPoint		loc;
	TRect		subrect;

	inArgs >> picname >> loc >> subrect;
	draw_picture(picname, loc, &subrect);
}

DEFINE_5L_PRIMITIVE(MarkUnprocessedEventsAsStale) {
    EventDispatcher::UpdateMaxStaleTime();
}

DEFINE_5L_PRIMITIVE(Mask) {
    std::string	path;
    TPoint		loc;

	inArgs >> path >> loc;
    wxBitmap mask = load_picture(path.c_str());
	GetCurrentDrawingArea()->Mask(mask, loc.X(), loc.Y());
}

DEFINE_5L_PRIMITIVE(MaybeLoadSplash) {
    std::string picname;
    inArgs >> picname;
	wxGetApp().GetStage()->MaybeDrawSplashGraphic(picname.c_str());
}

DEFINE_5L_PRIMITIVE(MeasurePic) {
	std::string	picname;
    inArgs >> picname;
    wxBitmap pic(load_picture(picname));
    if (!pic.Ok()) {
		THROW("Can't load the specified image");
    } else {
        wxRect r(wxRect(0, 0, pic.GetWidth(), pic.GetHeight()));
        ::SetPrimitiveResult(WxToTRect(r));
    }
}

DEFINE_5L_PRIMITIVE(NotifyEnterCard) {
	std::string name;
	inArgs >> name;
	wxGetApp().GetStage()->NotifyEnterCard(name.c_str());
	::SkipPrimitiveLogging();
}

DEFINE_5L_PRIMITIVE(NotifyExitCard) {
	std::string name;
	inArgs >> name;
	wxGetApp().GetStage()->NotifyExitCard();
	::SkipPrimitiveLogging();
}

DEFINE_5L_PRIMITIVE(MediaSetVolume) {
	std::string name, channel_name;
	double volume;
	inArgs >> SymbolName(name) >> SymbolName(channel_name) >> volume;
	FIND_ELEMENT(MediaElement, stream, name.c_str());
	stream->SetVolume(channel_name, volume);
}

DEFINE_5L_PRIMITIVE(MouseGrab) {
	std::string name;
	inArgs >> SymbolName(name);
	FIND_ELEMENT(LightweightElement, elem, name.c_str());
	wxGetApp().GetStage()->MouseGrab(elem);
}

DEFINE_5L_PRIMITIVE(MouseIsGrabbed) {
	::SetPrimitiveResult(wxGetApp().GetStage()->MouseIsGrabbed());
}

DEFINE_5L_PRIMITIVE(MouseIsGrabbedBy) {
	std::string name;
	inArgs >> SymbolName(name);
	FIND_ELEMENT(LightweightElement, elem, name.c_str());
	::SetPrimitiveResult(wxGetApp().GetStage()->MouseIsGrabbedBy(elem));
}

DEFINE_5L_PRIMITIVE(MousePosition) {
	// XXX - Stop returning out-of-bounds co-ordinates.
	wxPoint p = wxGetApp().GetStage()->ScreenToClient(::wxGetMousePosition());
	::SetPrimitiveResult(TPoint(p.x, p.y));
}

DEFINE_5L_PRIMITIVE(MouseUngrab) {
	std::string name;
	inArgs >> SymbolName(name);
	FIND_ELEMENT(LightweightElement, elem, name.c_str());
	wxGetApp().GetStage()->MouseUngrab(elem);
}

DEFINE_5L_PRIMITIVE(Movie) {
	std::string name, path;
    TCallbackPtr dispatcher;
	TRect bounds;
    double volume;
	bool controller, audio_only, loop, interaction, report_captions;

	inArgs >> SymbolName(name) >> dispatcher >> bounds >> path >> volume
           >> controller >> audio_only >> loop >> interaction
           >> report_captions;

	MovieWindowStyle style = 0;
	if (controller)
		style |= MOVIE_CONTROLLER;
	if (audio_only)
		style |= MOVIE_AUDIO_ONLY;
	if (loop)
		style |= MOVIE_LOOP;
	if (interaction)
		style |= MOVIE_INTERACTION;
	if (report_captions)
		style |= MOVIE_REPORT_CAPTIONS;

    R(new MovieElement(wxGetApp().GetStage(), name.c_str(), dispatcher,
                       TToWxRect(bounds), path.c_str(), 0, style, volume));
}

DEFINE_5L_PRIMITIVE(MovieEndPlayback) {
	std::string name;
	inArgs >> SymbolName(name);
	FIND_ELEMENT(MediaElement, movie, name.c_str());
    movie->EndPlayback();
}

// Note: these primitives may not be happy if the underlying movie code 
// does not like to be paused.
DEFINE_5L_PRIMITIVE(MoviePause) {
	std::string name;
	inArgs >> SymbolName(name);
	FIND_ELEMENT(MediaElement, movie, name.c_str());
	movie->Pause();
}

DEFINE_5L_PRIMITIVE(MovieResume) {
	std::string name;
	inArgs >> SymbolName(name);
	FIND_ELEMENT(MediaElement, movie, name.c_str());
	movie->Resume();
}

DEFINE_5L_PRIMITIVE(MovieSetTimeout) {
    std::string name;
    uint32 timeout;
	inArgs >> SymbolName(name) >> timeout;
	FIND_ELEMENT(MovieElement, movie, name.c_str());
    movie->SetTimeout(timeout);
}

DEFINE_5L_PRIMITIVE(MoveElementTo) {
	std::string name;
	TPoint p;
	inArgs >> SymbolName(name) >> p;
    FIND_ELEMENT(Element, elem, name.c_str());
    elem->MoveTo(TToWxPoint(p));
}

DEFINE_5L_PRIMITIVE(Refresh) {
	std::string transition;
	int32 milliseconds;
	inArgs >> SymbolName(transition) >> milliseconds;
	wxGetApp().GetStage()->RefreshStage(transition, milliseconds);
}

DEFINE_5L_PRIMITIVE(RefreshSplashScreen) {
    wxGetApp().GetStage()->RefreshSplashScreen();
}

DEFINE_5L_PRIMITIVE(OpenInBrowser) {
    std::string url;
    inArgs >> url;
    ::SetPrimitiveResult(::wxLaunchDefaultBrowser(url.c_str()));
}

DEFINE_5L_PRIMITIVE(Overlay) {
	std::string name, cursor;
	TRect bounds;
	TCallbackPtr dispatcher;
	bool is_trans, are_trans_areas_clickable;
	
	inArgs >> SymbolName(name) >> bounds >> dispatcher >> cursor >> is_trans
           >> are_trans_areas_clickable;
    Stage *stage = wxGetApp().GetStage();
	R(new Overlay(stage, name.c_str(), TToWxRect(bounds), dispatcher,
                  cursor, is_trans, are_trans_areas_clickable));
}

DEFINE_5L_PRIMITIVE(OverlaySetShape) {
    std::string name;
    TRect bounds;
    inArgs >> SymbolName(name) >> bounds;
    FIND_ELEMENT(Overlay, elem, name.c_str());
    elem->SetSize(TToWxRect(bounds).GetSize());
    wxGetApp().GetStage()->NotifyElementsChanged();
}

DEFINE_5L_PRIMITIVE(OverlayAnimated) {
	std::string name, cursor, state_path;
	TRect bounds;
	TCallbackPtr dispatcher;
    bool is_trans;
	TValue graphics;
	
	inArgs >> SymbolName(name) >> bounds >> dispatcher >> cursor
           >> is_trans >> SymbolName(state_path) >> graphics;
    Stage *stage = wxGetApp().GetStage();
	R(new AnimatedOverlay(stage, name.c_str(), TToWxRect(bounds), dispatcher, 
                          cursor, is_trans, state_path, TValueList(graphics)));
}

DEFINE_5L_PRIMITIVE(Screenshot) {
	std::string filename;
	inArgs >> filename;
	wxGetApp().GetStage()->Screenshot(filename.c_str());
}

DEFINE_5L_PRIMITIVE(RegisterCard) {
	std::string name;
	inArgs >> SymbolName(name);
	wxGetApp().GetStage()->RegisterCard(name.c_str());
	::SkipPrimitiveLogging();
}

DEFINE_5L_PRIMITIVE(RegisterCursor) {
	std::string name, path;
	TPoint hotspot;
	inArgs >> SymbolName(name) >> path >> hotspot;
	CursorManager *manager = wxGetApp().GetStage()->GetCursorManager();
	manager->RegisterImageCursor(name, path, hotspot.X(), hotspot.Y());
}

DEFINE_5L_PRIMITIVE(RegisterEventDispatcher) {
	TCallbackPtr callback;
	inArgs >> callback;
	wxGetApp().GetStage()->GetEventDispatcher()->SetDispatcher(callback);
}

/*---------------------------------------------------------------
    (SCREEN COLOR)

    A fast way to fill the entire screen with a particular color.
-----------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Screen) {
    Color color;
    inArgs >> color; 
	GetCurrentDrawingArea()->Clear(color);
}

DEFINE_5L_PRIMITIVE(SetImageCacheSize) {
	uint32 sz;
	inArgs >> sz;
	wxGetApp().GetStage()->GetImageCache()->SetMaxCacheSize(sz);
}

DEFINE_5L_PRIMITIVE(SetZoneCursor) {
	std::string name, cursor;
	inArgs >> SymbolName(name) >> SymbolName(cursor);

	FIND_ELEMENT(LightweightElement, elem, name.c_str());
	elem->SetCursorName(cursor);
}

DEFINE_5L_PRIMITIVE(TamaleExit) {
    // Force shutdown.
    wxGetApp().GetStageFrame()->Close(TRUE);
}

DEFINE_5L_PRIMITIVE(TextAA) {
	TRect		bounds;
	std::string style, text;

    inArgs >> SymbolName(style) >> bounds >> text;

	gStyleSheetManager.Draw(style, text,
							GraphicsTools::Point(bounds.Left(),
												 bounds.Top()),
							bounds.Right() - bounds.Left(),
							GetCurrentDrawingArea());
}

DEFINE_5L_PRIMITIVE(Wait) {
	std::string name;
	int32 frame = LAST_FRAME;

	inArgs >> SymbolName(name);
	if (inArgs.HasMoreArguments())
		inArgs >> frame;
    CHECK_SUSPEND_OK("WAIT");
	wxGetApp().GetStage()->Wait(name.c_str(), frame);
}

DEFINE_5L_PRIMITIVE(WakeUpIfNecessary) {
    // We get called way too often to actually be interesting, so don't log
    // this primitive.
	::SkipPrimitiveLogging(); 

    // We need to check if we still have a link to the stage.
    // StageFrame::OnClose may sever that link when we're doing event
    // processing in a call to FiveLApp::IdleProc, and when the engine
    // returns to Scheme, we'll be the next function called before
    // kernel.ss notices that the interpreter has been killed.
    if (wxGetApp().HaveStage())
        wxGetApp().GetStage()->InterpreterWakeUpIfNecessary();
}

DEFINE_5L_PRIMITIVE(WantsCursorGet) {
	std::string name;
	inArgs >> SymbolName(name);

	FIND_ELEMENT(LightweightElement, elem, name.c_str());
    ::SetPrimitiveResult(elem->WantsCursor());
}

DEFINE_5L_PRIMITIVE(WantsCursorSet) {
    std::string name;
    bool wants_cursor;
	inArgs >> SymbolName(name) >> wants_cursor;
    FIND_ELEMENT(LightweightElement, elem, name.c_str());
    elem->SetWantsCursor(wants_cursor);
}

DEFINE_5L_PRIMITIVE(Zone) {
	std::string name, cursor;
	TPolygon poly;
	TCallbackPtr dispatcher;
	
	inArgs >> SymbolName(name) >> poly >> dispatcher >> cursor;
	R(new Zone(wxGetApp().GetStage(), name.c_str(), poly, dispatcher, cursor));
}

DEFINE_5L_PRIMITIVE(ZoneSetShape) {
    std::string name;
    TPolygon bounds;
    inArgs >> SymbolName(name) >> bounds;
    FIND_ELEMENT(Zone, elem, name.c_str());
    elem->SetShape(bounds);
    wxGetApp().GetStage()->NotifyElementsChanged();
}
