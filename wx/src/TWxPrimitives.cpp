// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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
#include "AppHeaders.h"
#include <wx/image.h>

#include "TPrimitives.h"
#include "TWxPrimitives.h"

// Needed to implement the primitives.
#include "TCommonPrimitives.h"
#include "CrashReporter.h"
#include "AppConfig.h"
#include "TStyleSheet.h"
#include "HalyardApp.h"
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
#include "CommonWxConv.h"
#include "BrowserElement.h"
#include "EditBox.h"
#include "TStateDB.h"
#include "dlg/MultiButtonDlg.h"
#include "UrlRequest.h"
#include "Downloader.h"
#include "ProgramTree.h"

#if CONFIG_HAVE_AUDIOSTREAMS
#   include "AudioStream.h"
#   include "GeigerAudioStream.h"
#   include "VorbisAudioStream.h"
#   include "AudioStreamElement.h"
#   include "GeigerSynthElement.h"
#endif // CONFIG_HAVE_AUDIOSTREAMS

#if CONFIG_HAVE_ACTIVEX
#   include "ActiveXElement.h"
#endif // CONFIG_HAVE_ACTIVEX

using namespace Halyard;
using GraphicsTools::Color;
using FileSystem::Path;


//=========================================================================
//  Utility Functions
//=========================================================================

template <typename T>
shared_ptr<T> find_element(const char *inTypeName, const wxString &inName) {
    ElementPtr found = wxGetApp().GetStage()->FindElement(inName);
    if (!found) {
        std::string name(inName.mb_str());
        THROW("The element " + name + " does not exist.");
    }
    shared_ptr<T> elem = shared_ptr<T>(found, dynamic_cast_tag());
    if (!elem) {
        std::string name(inName.mb_str());
        THROW("The element " + name + " is not of type " + inTypeName);
    }
    return elem;
}

#define FIND_ELEMENT(TYPE, VAR, NAME) \
    shared_ptr<TYPE> VAR = find_element<TYPE>(#TYPE, NAME)

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


//=========================================================================
//  Implementation of wxWindows Primitives
//=========================================================================

#if CONFIG_HAVE_ACTIVEX

DEFINE_PRIMITIVE(ActiveX) {
    std::string name, control_name;
    TRect bounds;
    TCallbackPtr dispatcher;

    inArgs >> SymbolName(name) >> dispatcher >> bounds >> control_name;

    R(new ActiveXElement(wxGetApp().GetStage(), name.c_str(),
                         TToWxRect(bounds), dispatcher, control_name.c_str()));
}

DEFINE_PRIMITIVE(ActiveXPropGet) {
    std::string name, prop;
    inArgs >> SymbolName(name) >> prop;
    FIND_ELEMENT(ActiveXElement, element, name.c_str());
    ::SetPrimitiveResult(WxToTValue(element->Prop(prop.c_str())));
}

DEFINE_PRIMITIVE(ActiveXPropSet) {
    std::string name, prop;
    TValue value;
    inArgs >> SymbolName(name) >> prop >> value;
    FIND_ELEMENT(ActiveXElement, element, name.c_str());
    element->SetProp(prop.c_str(), TToWxValue(value));
}

#endif // CONFIG_HAVE_ACTIVEX

#if CONFIG_HAVE_AUDIOSTREAMS

DEFINE_PRIMITIVE(AudioStreamGeiger) {
    std::string name, path;
    double volume;
    TCallbackPtr dispatcher;
    inArgs >> SymbolName(name) >> dispatcher >> path >> volume;
    R(new AudioStreamElement(wxGetApp().GetStage(), name.c_str(),
                             new GeigerAudioStream(path.c_str(), volume),
                             dispatcher));
}

DEFINE_PRIMITIVE(AudioStreamGeigerSetCps) {
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

DEFINE_PRIMITIVE(AudioStreamSine) {
    std::string name;
    double volume;
    uint32 frequency;
    TCallbackPtr dispatcher;    
    inArgs >> SymbolName(name) >> dispatcher >> volume >> frequency;
    R(new AudioStreamElement(wxGetApp().GetStage(), name.c_str(),
                             new SineAudioStream(frequency, volume),
                             dispatcher));
}

DEFINE_PRIMITIVE(AudioStreamVorbis) {
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

DEFINE_PRIMITIVE(GeigerSynth) {
    std::string name, state_path, chirp_location, loop_location;
    double loop_cps, volume;
    uint32 buffer_size;

    inArgs >> SymbolName(name) >> SymbolName(state_path) >> chirp_location
           >> volume >> buffer_size;

    GeigerSynthElement *element =
        R(new GeigerSynthElement(wxGetApp().GetStage(), ToWxString(name),
                                 state_path, ToWxString(chirp_location.c_str()),
                                 1000, volume));

    while (inArgs.HasMoreArguments()) {
        inArgs >> loop_cps >> loop_location;
        element->AddLoop(loop_cps, ToWxString(loop_location.c_str()));
    }
    element->DoneAddingLoops();
}

#endif // CONFIG_HAVE_AUDIOSTREAMS

DEFINE_PRIMITIVE(Browser) {
    std::string name;
    bool want_builtin;
    TRect bounds;
    TCallbackPtr dispatcher;

    inArgs >> SymbolName(name) >> dispatcher >> bounds >> want_builtin;

    if (want_builtin)
        R(new BrowserElementWx(wxGetApp().GetStage(), ToWxString(name),
                               TToWxRect(bounds), dispatcher));
    else
        R(new BrowserElementNative(wxGetApp().GetStage(), ToWxString(name),
                                   TToWxRect(bounds), dispatcher));
}

DEFINE_PRIMITIVE(BrowserCanBack) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(BrowserElement, browser, ToWxString(name));
    ::SetPrimitiveResult(browser->CanGoBack());
}

DEFINE_PRIMITIVE(BrowserCanForward) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(BrowserElement, browser, ToWxString(name));
    ::SetPrimitiveResult(browser->CanGoForward());
}

DEFINE_PRIMITIVE(BrowserCanReload) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(BrowserElement, browser, ToWxString(name));
    ::SetPrimitiveResult(browser->CanRefresh());
}

DEFINE_PRIMITIVE(BrowserCanStop) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(BrowserElement, browser, ToWxString(name));
    ::SetPrimitiveResult(browser->CanStop());
}

DEFINE_PRIMITIVE(BrowserBack) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(BrowserElement, browser, ToWxString(name));
    ::SetPrimitiveResult(browser->GoBack());
}

DEFINE_PRIMITIVE(BrowserForward) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(BrowserElement, browser, ToWxString(name));
    ::SetPrimitiveResult(browser->GoForward());
}

DEFINE_PRIMITIVE(BrowserLoadPage) {
    std::string name, file_or_url;
    inArgs >> SymbolName(name) >> file_or_url;
    FIND_ELEMENT(BrowserElement, browser, ToWxString(name));
    browser->LoadPage(ToWxString(file_or_url.c_str()));
}

DEFINE_PRIMITIVE(BrowserReload) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(BrowserElement, browser, ToWxString(name));
    ::SetPrimitiveResult(browser->Refresh());
}

DEFINE_PRIMITIVE(BrowserStop) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(BrowserElement, browser, ToWxString(name));
    ::SetPrimitiveResult(browser->Stop());
}

DEFINE_PRIMITIVE(CancelDownload) {
    Downloader::GetInstance()->CancelDownload();
}

DEFINE_PRIMITIVE(ColorAt) {
    TPoint at;
    inArgs >> at;
    ::SetPrimitiveResult(GetCurrentDrawingArea()->GetPixel(at.X(), at.Y()));
}

DEFINE_PRIMITIVE(CopyStringToClipboard) {
    // This primitive is to allow people to write developer tools in scheme.
    // It isn't actually a necessary part of the API. 
    std::string string;
    inArgs >> string;
    wxGetApp().GetStage()->CopyStringToClipboard(ToWxString(string.c_str()));
}

DEFINE_PRIMITIVE(CursorElement) {
    std::string name, cursor_reg_name;
    TRect bounds;
    TCallbackPtr dispatcher;
    bool is_trans;
    
    inArgs >> SymbolName(name) >> bounds >> dispatcher >> is_trans
           >> SymbolName(cursor_reg_name);
    Stage *stage = wxGetApp().GetStage();

    CursorElement *cursor = new CursorElement(stage, ToWxString(name), 
                                              TToWxRect(bounds), dispatcher,
                                              is_trans, cursor_reg_name);
    R(cursor);
    cursor->RegisterWithCursorManager();
}

DEFINE_PRIMITIVE(DataPath) {
    // By the time we get here, our application name will actually be
    // our script name.
    FileSystem::Path dir = FileSystem::GetScriptDataDirectory();
    ::SetPrimitiveResult(dir.ToNativePathString());
}

DEFINE_PRIMITIVE(DataPathLocal) {
    FileSystem::Path dir = FileSystem::GetScriptLocalDataDirectory();
    ::SetPrimitiveResult(dir.ToNativePathString());
}

DEFINE_PRIMITIVE(DcPop) {
    std::string name;   
    inArgs >> SymbolName(name);
    FIND_ELEMENT(Element, elem, ToWxString(name));
    wxGetApp().GetStage()->PopDrawingContext(elem);
}

DEFINE_PRIMITIVE(DcPush) {
    std::string name;   
    inArgs >> SymbolName(name);
    FIND_ELEMENT(Element, elem, ToWxString(name));
    wxGetApp().GetStage()->PushDrawingContext(elem);
}

DEFINE_PRIMITIVE(DcRect) {
    wxRect bounds = GetCurrentDrawingArea()->GetBounds();
    ::SetPrimitiveResult(WxToTRect(wxRect(0, 0, bounds.width, bounds.height)));
}

DEFINE_PRIMITIVE(DebugReportAddFile) {
    std::string name;
    std::string description;
    inArgs >> name >> description;
    CrashReporter::GetInstance()->AddDiagnosticFile(name, description);
}

DEFINE_PRIMITIVE(DeleteElements) {
    if (!inArgs.HasMoreArguments()) {
        wxGetApp().GetStage()->DeleteElements();
    } else {
        while (inArgs.HasMoreArguments()) {
            std::string name;
            inArgs >> SymbolName(name);
            bool found =
                wxGetApp().GetStage()->DeleteElementByName(ToWxString(name));
            if (!found)
                gLog.Warn("halyard", "Deleting non-existant element '%s'.",
                                  name.c_str());
        }
    }
}

DEFINE_PRIMITIVE(Dialog) {
    std::string title, message, button1, button2, button3;
    inArgs >> title >> message >> button1;
    if (inArgs.HasMoreArguments())
        inArgs >> button2;
    if (inArgs.HasMoreArguments())
        inArgs >> button3;
    MultiButtonDlg dlg(wxGetApp().GetStage(),
                       ToWxString(title.c_str()), ToWxString(message.c_str()),
                       ToWxString(button1.c_str()), ToWxString(button2.c_str()),
                       ToWxString(button3.c_str()));
    ::SetPrimitiveResult(dlg.ShowModal());
}

DEFINE_PRIMITIVE(Download) {
    std::string URL, file;
    inArgs >> URL >> file;

    ::SetPrimitiveResult(Downloader::GetInstance()->Get(URL, file));
}

DEFINE_PRIMITIVE(DrawBoxFill) {
    TRect bounds;
    Color color;

    inArgs >> bounds >> color;
    GetCurrentDrawingArea()->FillBox(TToWxRect(bounds), color);
}

DEFINE_PRIMITIVE(DrawBoxOutline) {
    TRect bounds;
    Color color;
    int32 width;

    inArgs >> bounds >> color >> width;
    GetCurrentDrawingArea()->OutlineBox(TToWxRect(bounds), color, width);

}

DEFINE_PRIMITIVE(DrawOvalFill) {
    TRect bounds;
    Color color;

    inArgs >> bounds >> color;
    GetCurrentDrawingArea()->FillOval(TToWxRect(bounds), color);
}

DEFINE_PRIMITIVE(DrawOvalOutline) {
    TRect bounds;
    Color color;
    int32 width;

    inArgs >> bounds >> color >> width;
    GetCurrentDrawingArea()->OutlineOval(TToWxRect(bounds), color, width);

}

DEFINE_PRIMITIVE(DrawLine) {
    TPoint from, to;
    Color color;
    int32 width;

    inArgs >> from >> to >> color >> width;
    GetCurrentDrawingArea()->DrawLine(TToWxPoint(from), TToWxPoint(to),
                                      color, width);

}

DEFINE_PRIMITIVE(DrawLoadProgress) {
    wxGetApp().GetStage()->DrawLoadProgress();
}

DEFINE_PRIMITIVE(EditBox) {
    std::string name, text;
    TCallbackPtr dispatcher;
    TRect bounds;
    uint32 text_sz;
    bool multiline, send_enter_event;

    inArgs >> SymbolName(name) >> dispatcher >> bounds >> text >> text_sz
           >> multiline >> send_enter_event;

    R(new EditBox(wxGetApp().GetStage(), ToWxString(name), dispatcher,
                  TToWxRect(bounds), ToWxString(text.c_str()), text_sz,
                  multiline, send_enter_event));
}

DEFINE_PRIMITIVE(EditBoxGetValue) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(EditBox, elem, ToWxString(name));
    ::SetPrimitiveResult(std::string(elem->GetValue().mb_str()));
}

DEFINE_PRIMITIVE(EditBoxSetValue) {
    std::string name, value;
    inArgs >> SymbolName(name) >> value;
    FIND_ELEMENT(EditBox, elem, ToWxString(name));
    elem->SetValue(ToWxString(value.c_str()));
}

DEFINE_PRIMITIVE(EditBoxSetInsertionPoint) {
    std::string name;
    int32 pos;
    inArgs >> SymbolName(name) >> pos;
    FIND_ELEMENT(EditBox, elem, ToWxString(name));
    elem->SetInsertionPoint(pos);
}

DEFINE_PRIMITIVE(EditBoxSetSelection) {
    std::string name;
    int32 begin, end;
    inArgs >> SymbolName(name) >> begin >> end;
    FIND_ELEMENT(EditBox, elem, ToWxString(name));
    elem->SetSelection(begin, end);
}

DEFINE_PRIMITIVE(ElementExists) {
    std::string name;
    inArgs >> SymbolName(name);
    if (wxGetApp().GetStage()->FindElement(ToWxString(name)))
        ::SetPrimitiveResult(true);
    else
        ::SetPrimitiveResult(false);
}

DEFINE_PRIMITIVE(ElementIsShown) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(Element, element, ToWxString(name));
    ::SetPrimitiveResult(element->IsShown());
}

DEFINE_PRIMITIVE(ElementSetShown) {
    std::string name;
    bool show;
    inArgs >> SymbolName(name) >> show;
    FIND_ELEMENT(Element, element, ToWxString(name));
    element->Show(show);
    // TODO - Override MovieElement::Show for unshowable movies.
}

DEFINE_PRIMITIVE(ElementSetInDragLayer) {
    std::string name;
    bool in_drag_layer;
    inArgs >> SymbolName(name) >> in_drag_layer;
    FIND_ELEMENT(LightweightElement, element, ToWxString(name));
    element->SetInDragLayer(in_drag_layer);
}

DEFINE_PRIMITIVE(EnableDeveloperToolsInAllModes) {
    wxGetApp().GetStageFrame()->EnableDeveloperToolsInAllModes();
}

DEFINE_PRIMITIVE(EnableExpensiveEvents) {
    bool enable;
    inArgs >> enable;
    EventDispatcher::EnableExpensiveEvents(enable);
}

DEFINE_PRIMITIVE(ErrortraceCompileEnabled) {
    ::SetPrimitiveResult(wxGetApp().GetStage()->IsErrortraceCompileEnabled());
}

DEFINE_PRIMITIVE(Focus) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(Widget, element, ToWxString(name));
    element->SetFocus();
}

DEFINE_PRIMITIVE(HideCursorUntilMouseMoved) {
    wxGetApp().GetStage()->HideCursorUntilMouseMoved();
}

DEFINE_PRIMITIVE(Heartbeat) {
    wxGetApp().Heartbeat();
}

DEFINE_PRIMITIVE(UrlRequest) {
    std::string name, url;
    TCallbackPtr dispatcher;
    inArgs >> SymbolName(name) >> dispatcher >> url;
    R(new UrlRequest(wxGetApp().GetStage(), ToWxString(name.c_str()),
                     dispatcher, ToWxString(url.c_str())));
}

DEFINE_PRIMITIVE(UrlRequestConfigurePost) {
    std::string name, content_type, body;
    inArgs >> SymbolName(name) >> content_type >> body;
    FIND_ELEMENT(UrlRequest, request, ToWxString(name));
    request->ConfigurePost(content_type, body);
}

DEFINE_PRIMITIVE(UrlRequestConfigureSetHeader) {
    std::string name, header, value;
    inArgs >> SymbolName(name) >> header >> value;
    FIND_ELEMENT(UrlRequest, request, ToWxString(name));
    request->ConfigureSetHeader(header, value);
}

DEFINE_PRIMITIVE(UrlRequestGetResponseContentType) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(UrlRequest, request, ToWxString(name));
    std::string content_type(request->GetResponseContentType());
    if (content_type == "")
        ::SetPrimitiveResult(false);
    else
        ::SetPrimitiveResult(content_type);
}

DEFINE_PRIMITIVE(UrlRequestStart) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(UrlRequest, request, ToWxString(name));
    request->Start();
}

DEFINE_PRIMITIVE(IsVistaOrNewer) {
    // Return true if we're running Windows Vista or newer.  We need to
    // know this so that the updater can deal with the new security model.
    int major, minor;
    int family = ::wxGetOsVersion(&major, &minor);
    ::SetPrimitiveResult(family == wxOS_WINDOWS_NT && major >= 6);
}

DEFINE_PRIMITIVE(LaunchUpdateInstallerBeforeExiting) {
    wxGetApp().LaunchUpdateInstallerBeforeExiting();
}

/*---------------------------------------------------------------------
    (LOADPIC PICTURE X Y <FLAGS...>)

    Display the given picture at the given location (x, y).

    XXX - Flags not implemented!
-----------------------------------------------------------------------*/

static CairoSurfacePtr load_image(const std::string &inName) {
    // Load our image.
    CairoSurfacePtr image =
        wxGetApp().GetStage()->GetImageCache()->GetImage(ToWxString(inName));
    if (image.is_null())
        THROW("Can't load the specified image");
    return image;
}

static void draw_image(const std::string &inName, TPoint inLoc,
                         double scale_x, double scale_y,
                         wxRect *inClipRect = NULL)
{
    CairoSurfacePtr image(load_image(inName));
    wxRect bounds =
        GetCurrentDrawingArea()->DrawImage(image, inLoc.X(), inLoc.Y(),
                                           scale_x, scale_y, inClipRect);
    ::SetPrimitiveResult(WxToTRect(bounds));
}

DEFINE_PRIMITIVE(LoadGraphic) {
    std::string path;
    TPoint      loc;
    double      scale_x, scale_y;

    inArgs >> path >> loc >> scale_x >> scale_y;
    if (inArgs.HasMoreArguments()) {
        TRect clip_trect;
        inArgs >> clip_trect;
        wxRect clip_rect(TToWxRect(clip_trect));
        draw_image(path, loc, scale_x, scale_y, &clip_rect);
    } else {
        draw_image(path, loc, scale_x, scale_y);
    }
}

DEFINE_PRIMITIVE(MarkUnprocessedEventsAsStale) {
    EventDispatcher::UpdateMaxStaleTime();
}

DEFINE_PRIMITIVE(Mask) {
    std::string path;
    TPoint      loc;

    inArgs >> path >> loc;
    CairoSurfacePtr mask(load_image(path.c_str()));
    GetCurrentDrawingArea()->Mask(mask, loc.X(), loc.Y());
}

DEFINE_PRIMITIVE(MaybeLoadSplash) {
    std::string picname;
    inArgs >> picname;
    wxGetApp().GetStage()->MaybeDrawSplashGraphic(picname);
}

DEFINE_PRIMITIVE(MeasureGraphic) {
    std::string path;
    double      scale_x, scale_y;
    inArgs >> path >> scale_x >> scale_y;
    CairoSurfacePtr image(load_image(path));
    wxRect r(wxPoint(0, 0),
             DrawingArea::MeasureImage(image, scale_x, scale_y));
    ::SetPrimitiveResult(WxToTRect(r));
}

DEFINE_PRIMITIVE(NotifyEnterCard) {
    std::string name;
    inArgs >> SymbolName(name);
    wxGetApp().GetStage()->NotifyEnterCard(ToWxString(name));
}

DEFINE_PRIMITIVE(NotifyExitCard) {
    std::string name;
    inArgs >> SymbolName(name);
    wxGetApp().GetStage()->NotifyExitCard();
}

DEFINE_PRIMITIVE(MediaAttachCaptionFile) {
    std::string name, caption_file;
    inArgs >> SymbolName(name) >> caption_file;
    FIND_ELEMENT(MediaElement, media, ToWxString(name));
    media->AttachCaptionFile(caption_file);
}

DEFINE_PRIMITIVE(MediaSetVolume) {
    std::string name, channel_name;
    double volume;
    inArgs >> SymbolName(name) >> SymbolName(channel_name) >> volume;
    FIND_ELEMENT(MediaElement, media, ToWxString(name));
    media->SetVolume(channel_name, volume);
}

DEFINE_PRIMITIVE(MouseGrab) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(LightweightElement, elem, ToWxString(name));
    wxGetApp().GetStage()->MouseGrab(elem);
}

DEFINE_PRIMITIVE(MouseIsGrabbed) {
    ::SetPrimitiveResult(wxGetApp().GetStage()->MouseIsGrabbed());
}

DEFINE_PRIMITIVE(MouseIsGrabbedBy) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(LightweightElement, elem, ToWxString(name));
    ::SetPrimitiveResult(wxGetApp().GetStage()->MouseIsGrabbedBy(elem));
}

DEFINE_PRIMITIVE(MousePosition) {
    // XXX - Stop returning out-of-bounds co-ordinates.
    wxPoint p = wxGetApp().GetStage()->ScreenToClient(::wxGetMousePosition());
    ::SetPrimitiveResult(TPoint(p.x, p.y));
}

DEFINE_PRIMITIVE(MouseUngrab) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(LightweightElement, elem, ToWxString(name));
    wxGetApp().GetStage()->MouseUngrab(elem);
}

DEFINE_PRIMITIVE(Movie) {
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

    R(new MovieElement(wxGetApp().GetStage(), ToWxString(name), dispatcher,
                       TToWxRect(bounds), ToWxString(path), 0, style, volume));
}

DEFINE_PRIMITIVE(MovieEndPlayback) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(MediaElement, movie, ToWxString(name));
    movie->EndPlayback();
}

// Note: these primitives may not be happy if the underlying movie code 
// does not like to be paused.
DEFINE_PRIMITIVE(MoviePause) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(MediaElement, movie, ToWxString(name));
    movie->Pause();
}

DEFINE_PRIMITIVE(MovieResume) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(MediaElement, movie, ToWxString(name));
    movie->Resume();
}

DEFINE_PRIMITIVE(MovieSetTimeout) {
    std::string name;
    uint32 timeout;
    inArgs >> SymbolName(name) >> timeout;
    FIND_ELEMENT(MovieElement, movie, ToWxString(name));
    movie->SetTimeout(timeout);
}

DEFINE_PRIMITIVE(MovieSetPlaybackTimer) {
    std::string name;
    int32 frame;
    inArgs >> SymbolName(name) >> frame;
    FIND_ELEMENT(MediaElement, movie, ToWxString(name));
    movie->SetPlaybackTimer(frame);
}

DEFINE_PRIMITIVE(MovieClearPlaybackTimer) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(MediaElement, movie, ToWxString(name));
    movie->ClearPlaybackTimer();
}

DEFINE_PRIMITIVE(MoveElementTo) {
    std::string name;
    TPoint p;
    inArgs >> SymbolName(name) >> p;
    FIND_ELEMENT(Element, elem, ToWxString(name));
    elem->MoveTo(TToWxPoint(p));
}

DEFINE_PRIMITIVE(RaiseToTop) {
    std::string name;
    inArgs >> SymbolName(name);
    FIND_ELEMENT(Element, elem, ToWxString(name));
    wxGetApp().GetStage()->RaiseToTop(elem);
}

DEFINE_PRIMITIVE(Refresh) {
    std::string transition;
    int32 milliseconds;
    inArgs >> SymbolName(transition) >> milliseconds;
    wxGetApp().GetStage()->RefreshStage(transition, milliseconds);
}

DEFINE_PRIMITIVE(RefreshSplashScreen) {
    wxGetApp().GetStage()->RefreshSplashScreen();
}

DEFINE_PRIMITIVE(OpenInBrowser) {
    std::string url;
    inArgs >> url;
    ::SetPrimitiveResult(::wxLaunchDefaultBrowser(ToWxString(url.c_str())));
}

DEFINE_PRIMITIVE(Overlay) {
    std::string name, cursor;
    TRect bounds;
    TCallbackPtr dispatcher;
    bool is_trans, are_trans_areas_clickable;
    
    inArgs >> SymbolName(name) >> bounds >> dispatcher >> SymbolName(cursor)
           >> is_trans >> are_trans_areas_clickable;
    Stage *stage = wxGetApp().GetStage();
    R(new Overlay(stage, ToWxString(name), TToWxRect(bounds), dispatcher,
                  cursor, is_trans, are_trans_areas_clickable));
}

DEFINE_PRIMITIVE(OverlaySetShape) {
    std::string name;
    TRect bounds;
    inArgs >> SymbolName(name) >> bounds;
    FIND_ELEMENT(Overlay, elem, ToWxString(name));
    elem->SetSize(TToWxRect(bounds).GetSize());
    wxGetApp().GetStage()->NotifyElementsChanged();
}

DEFINE_PRIMITIVE(OverlayAnimated) {
    std::string name, cursor, state_path;
    TRect bounds;
    TCallbackPtr dispatcher;
    bool is_trans;
    TValue graphics;
    
    inArgs >> SymbolName(name) >> bounds >> dispatcher >> SymbolName(cursor)
           >> is_trans >> SymbolName(state_path) >> graphics;
    Stage *stage = wxGetApp().GetStage();
    R(new AnimatedOverlay(stage, ToWxString(name), TToWxRect(bounds),
                          dispatcher, cursor, is_trans, state_path,
                          tvalue_cast<TValueList>(graphics)));
}

DEFINE_PRIMITIVE(Screenshot) {
    std::string filename;
    inArgs >> filename;
    wxGetApp().GetStage()->Screenshot(ToWxString(filename));
}

DEFINE_PRIMITIVE(RegisterCursor) {
    std::string name, path;
    TPoint hotspot;
    inArgs >> SymbolName(name) >> path >> hotspot;
    CursorManager *manager = wxGetApp().GetStage()->GetCursorManager();
    manager->RegisterImageCursor(name, path, hotspot.X(), hotspot.Y());
}

DEFINE_PRIMITIVE(RegisterGroupMember) {
    std::string name;
    bool isCard, isLoaded;
    inArgs >> SymbolName(name) >> isCard >> isLoaded;
    ProgramTree *tree = wxGetApp().GetStageFrame()->GetProgramTree();
    tree->RegisterGroupMember(ToWxString(name), isCard, isLoaded);
}

DEFINE_PRIMITIVE(RegisterEventDispatcher) {
    TCallbackPtr callback;
    inArgs >> callback;
    wxGetApp().GetStage()->GetEventDispatcher()->SetDispatcher(callback);
}

/*---------------------------------------------------------------
    (SCREEN COLOR)

    A fast way to fill the entire screen with a particular color.
-----------------------------------------------------------------*/
DEFINE_PRIMITIVE(Screen) {
    Color color;
    inArgs >> color; 
    GetCurrentDrawingArea()->Clear(color);
}

DEFINE_PRIMITIVE(SetImageCacheSize) {
    uint32 sz;
    inArgs >> sz;
    wxGetApp().GetStage()->GetImageCache()->SetMaxCacheSize(sz);
}

DEFINE_PRIMITIVE(SetStatusText) {
    std::string text;
    inArgs >> text;
    wxGetApp().GetStageFrame()->SetStatusText(ToWxString(text));
}

DEFINE_PRIMITIVE(SetZoneCursor) {
    std::string name, cursor;
    inArgs >> SymbolName(name) >> SymbolName(cursor);

    FIND_ELEMENT(LightweightElement, elem, ToWxString(name));
    elem->SetCursorName(cursor);
}

DEFINE_PRIMITIVE(StateDBClear) {
    // Note that an equivalent process occurs during ReloadScript, but
    // spread out into two stages: One in TInterpreterManager.cpp and one
    // in Stage.cpp.
    gStateDB.Clear();
    wxGetApp().GetStage()->UpdateClockKeysInStateDB();
}

DEFINE_PRIMITIVE(MaybeExitScriptGui) {
    // Force shutdown if we're in runtime mode.
    bool force = TInterpreterManager::IsInRuntimeMode();
    wxGetApp().GetStageFrame()->Close(force);
}

DEFINE_PRIMITIVE(TextAA) {
    TRect       bounds;
    std::string style, text;

    inArgs >> SymbolName(style) >> bounds >> text;

    TRect bounds_used =
        gStyleSheetManager.Draw(style, text,
                                GraphicsTools::Point(bounds.Left(),
                                                     bounds.Top()),
                                bounds.Right() - bounds.Left(),
                                GetCurrentDrawingArea());
    ::SetPrimitiveResult(bounds_used);
}

DEFINE_PRIMITIVE(Wait) {
    std::string name;
    int32 frame = LAST_FRAME;

    inArgs >> SymbolName(name);
    if (inArgs.HasMoreArguments())
        inArgs >> frame;
    CHECK_SUSPEND_OK("WAIT");
    FIND_ELEMENT(MediaElement, elem, ToWxString(name));
    wxGetApp().GetStage()->Wait(elem, frame);
}

DEFINE_PRIMITIVE(WantsCursorGet) {
    std::string name;
    inArgs >> SymbolName(name);

    FIND_ELEMENT(LightweightElement, elem, ToWxString(name));
    ::SetPrimitiveResult(elem->WantsCursor());
}

DEFINE_PRIMITIVE(WantsCursorSet) {
    std::string name;
    bool wants_cursor;
    inArgs >> SymbolName(name) >> wants_cursor;
    FIND_ELEMENT(LightweightElement, elem, ToWxString(name));
    elem->SetWantsCursor(wants_cursor);
}

DEFINE_PRIMITIVE(Zone) {
    std::string name, cursor;
    TPolygon poly;
    TCallbackPtr dispatcher;
    
    inArgs >> SymbolName(name) >> poly >> dispatcher >> SymbolName(cursor);
    R(new Zone(wxGetApp().GetStage(), ToWxString(name), poly, dispatcher,
               cursor));
}

DEFINE_PRIMITIVE(ZoneSetShape) {
    std::string name;
    TPolygon bounds;
    inArgs >> SymbolName(name) >> bounds;
    FIND_ELEMENT(Zone, elem, ToWxString(name));
    elem->SetShape(bounds);
    wxGetApp().GetStage()->NotifyElementsChanged();
}


//=========================================================================
//  RegisterWxPrimitives
//=========================================================================
//  Install our wxWindows-specific primitives.  A lot of these are very
//  kludgy and should be replaced later on as the editing GUI improves.

void Halyard::RegisterWxPrimitives() {
#if CONFIG_HAVE_ACTIVEX
    REGISTER_PRIMITIVE(ActiveX);
    REGISTER_PRIMITIVE(ActiveXPropGet);
    REGISTER_PRIMITIVE(ActiveXPropSet);
#endif // CONFIG_HAVE_ACTIVEX

#if CONFIG_HAVE_AUDIOSTREAMS
    REGISTER_PRIMITIVE(AudioStreamGeiger);
    REGISTER_PRIMITIVE(AudioStreamGeigerSetCps);
    REGISTER_PRIMITIVE(AudioStreamSine);
    REGISTER_PRIMITIVE(AudioStreamVorbis);
    REGISTER_PRIMITIVE(GeigerSynth);
#endif // CONFIG_HAVE_AUDIOSTREAMS

    REGISTER_PRIMITIVE(Browser);
    REGISTER_PRIMITIVE(BrowserCanBack);
    REGISTER_PRIMITIVE(BrowserCanForward);
    REGISTER_PRIMITIVE(BrowserCanReload);
    REGISTER_PRIMITIVE(BrowserCanStop);
    REGISTER_PRIMITIVE(BrowserBack);
    REGISTER_PRIMITIVE(BrowserForward);
    REGISTER_PRIMITIVE(BrowserLoadPage);
    REGISTER_PRIMITIVE(BrowserReload);
    REGISTER_PRIMITIVE(BrowserStop);
    REGISTER_PRIMITIVE(CancelDownload);
    REGISTER_PRIMITIVE(ColorAt);
    REGISTER_PRIMITIVE(CopyStringToClipboard);
    REGISTER_PRIMITIVE(CursorElement);
    REGISTER_PRIMITIVE(DataPath);
    REGISTER_PRIMITIVE(DataPathLocal);
    REGISTER_PRIMITIVE(DcPop);
    REGISTER_PRIMITIVE(DcPush);
    REGISTER_PRIMITIVE(DcRect);
    REGISTER_PRIMITIVE(DebugReportAddFile);
    REGISTER_PRIMITIVE(DeleteElements);
    REGISTER_PRIMITIVE(Dialog);
    REGISTER_PRIMITIVE(Download);
    REGISTER_PRIMITIVE(DrawBoxFill);
    REGISTER_PRIMITIVE(DrawBoxOutline);
    REGISTER_PRIMITIVE(DrawOvalFill);
    REGISTER_PRIMITIVE(DrawOvalOutline);
    REGISTER_PRIMITIVE(DrawLine);
    REGISTER_PRIMITIVE(DrawLoadProgress);
    REGISTER_PRIMITIVE(EditBox);
    REGISTER_PRIMITIVE(EditBoxGetValue);
    REGISTER_PRIMITIVE(EditBoxSetValue);
    REGISTER_PRIMITIVE(EditBoxSetInsertionPoint);
    REGISTER_PRIMITIVE(EditBoxSetSelection);
    REGISTER_PRIMITIVE(ElementExists);
    REGISTER_PRIMITIVE(ElementIsShown);
    REGISTER_PRIMITIVE(ElementSetShown);
    REGISTER_PRIMITIVE(ElementSetInDragLayer);
    REGISTER_PRIMITIVE(EnableDeveloperToolsInAllModes);
    REGISTER_PRIMITIVE(EnableExpensiveEvents);
    REGISTER_PRIMITIVE(ErrortraceCompileEnabled);
    REGISTER_PRIMITIVE(Focus);
    REGISTER_PRIMITIVE(HideCursorUntilMouseMoved);
    REGISTER_PRIMITIVE(Heartbeat);
    REGISTER_PRIMITIVE(UrlRequest);
    REGISTER_PRIMITIVE(UrlRequestConfigurePost);
    REGISTER_PRIMITIVE(UrlRequestConfigureSetHeader);
    REGISTER_PRIMITIVE(UrlRequestGetResponseContentType);
    REGISTER_PRIMITIVE(UrlRequestStart);
    REGISTER_PRIMITIVE(IsVistaOrNewer);
    REGISTER_PRIMITIVE(LaunchUpdateInstallerBeforeExiting);
    REGISTER_PRIMITIVE(LoadGraphic);
    REGISTER_PRIMITIVE(MarkUnprocessedEventsAsStale);
    REGISTER_PRIMITIVE(Mask);
    REGISTER_PRIMITIVE(MaybeLoadSplash);
    REGISTER_PRIMITIVE(MeasureGraphic);
    REGISTER_PRIMITIVE(MediaAttachCaptionFile);
    REGISTER_PRIMITIVE(MediaSetVolume);
    REGISTER_PRIMITIVE(MouseGrab);
    REGISTER_PRIMITIVE(MouseIsGrabbed);
    REGISTER_PRIMITIVE(MouseIsGrabbedBy);
    REGISTER_PRIMITIVE(MousePosition);
    REGISTER_PRIMITIVE(MouseUngrab);
    REGISTER_PRIMITIVE(Movie);
    REGISTER_PRIMITIVE(MovieEndPlayback);
    REGISTER_PRIMITIVE(MoviePause);
    REGISTER_PRIMITIVE(MovieResume);
    REGISTER_PRIMITIVE(MovieSetTimeout);
    REGISTER_PRIMITIVE(MovieSetPlaybackTimer);
    REGISTER_PRIMITIVE(MovieClearPlaybackTimer);
    REGISTER_PRIMITIVE(MoveElementTo);
    REGISTER_PRIMITIVE(NotifyEnterCard);
    REGISTER_PRIMITIVE(NotifyExitCard);
    REGISTER_PRIMITIVE(OpenInBrowser);
    REGISTER_PRIMITIVE(Overlay);
    REGISTER_PRIMITIVE(OverlaySetShape);
    REGISTER_PRIMITIVE(OverlayAnimated);
    REGISTER_PRIMITIVE(RaiseToTop);
    REGISTER_PRIMITIVE(Refresh);
    REGISTER_PRIMITIVE(RefreshSplashScreen);
    REGISTER_PRIMITIVE(Screenshot);
    REGISTER_PRIMITIVE(RegisterCursor);
    REGISTER_PRIMITIVE(RegisterGroupMember);
    REGISTER_PRIMITIVE(RegisterEventDispatcher);
    REGISTER_PRIMITIVE(Screen);
    REGISTER_PRIMITIVE(SetImageCacheSize);
    REGISTER_PRIMITIVE(SetStatusText);
    REGISTER_PRIMITIVE(SetZoneCursor);
    REGISTER_PRIMITIVE(StateDBClear);
    REGISTER_PRIMITIVE(MaybeExitScriptGui);
    REGISTER_PRIMITIVE(TextAA);
    REGISTER_PRIMITIVE(Wait);
    REGISTER_PRIMITIVE(WantsCursorGet);
    REGISTER_PRIMITIVE(WantsCursorSet);
    REGISTER_PRIMITIVE(Zone);
    REGISTER_PRIMITIVE(ZoneSetShape);
}
