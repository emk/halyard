// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

// Needed for RegisterWxPrimitives.
#include <wx/wx.h>
#include <wx/image.h>
#include <wx/html/htmlwin.h> // TODO - Temporary?

#include "TCommon.h"
#include "TPrimitives.h"
#include "TWxPrimitives.h"

// Needed to implement the primitives.
#include "TCommonPrimitives.h"
#include "AppConfig.h"
#include "TStyleSheet.h"
#include "FiveLApp.h"
#include "Stage.h"
#include "Zone.h"
#include "MediaElement.h"
#include "MovieElement.h"
#include "Widget.h"
#include "FileSystem.h"
#include "EventDispatcher.h"
#include "ImageCache.h"
#include "CursorManager.h"
#include "AudioStream.h"
#include "VorbisAudioStream.h"
#include "AudioStreamElement.h"


USING_NAMESPACE_FIVEL
using GraphicsTools::Color;
using FileSystem::Path;


//=========================================================================
//  RegisterWxPrimitives
//=========================================================================
//  Install our wxWindows-specific primitives.  A lot of these are very
//  kludgy and should be replaced later on as the editing GUI improves.

void FIVEL_NS RegisterWxPrimitives()
{
	REGISTER_5L_PRIMITIVE(AudioStreamSine);
	REGISTER_5L_PRIMITIVE(AudioStreamVorbis);
    REGISTER_5L_PRIMITIVE(DeleteElements);
	REGISTER_5L_PRIMITIVE(DrawBoxFill);
	REGISTER_5L_PRIMITIVE(DrawBoxOutline);
	REGISTER_5L_PRIMITIVE(DrawLine);
	REGISTER_5L_PRIMITIVE(EditBox);
	REGISTER_5L_PRIMITIVE(ElementExists);
	REGISTER_5L_PRIMITIVE(ElementSetShown);
	REGISTER_5L_PRIMITIVE(EnableExpensiveEvents);
	REGISTER_5L_PRIMITIVE(HTML);
	REGISTER_5L_PRIMITIVE(Input);
	REGISTER_5L_PRIMITIVE(Loadpic);
	REGISTER_5L_PRIMITIVE(Loadsubpic);
	REGISTER_5L_PRIMITIVE(MediaSetVolume);
	REGISTER_5L_PRIMITIVE(MouseGrab);
	REGISTER_5L_PRIMITIVE(MouseIsGrabbed);
	REGISTER_5L_PRIMITIVE(MousePosition);
	REGISTER_5L_PRIMITIVE(MouseUngrab);
	REGISTER_5L_PRIMITIVE(Movie);
	REGISTER_5L_PRIMITIVE(MoviePause);
	REGISTER_5L_PRIMITIVE(MovieResume);
	REGISTER_5L_PRIMITIVE(Nap);
	REGISTER_5L_PRIMITIVE(NotifyEnterCard);
	REGISTER_5L_PRIMITIVE(NotifyExitCard);
	REGISTER_5L_PRIMITIVE(Refresh);
	REGISTER_5L_PRIMITIVE(SaveGraphics);
	REGISTER_5L_PRIMITIVE(RestoreGraphics);
	REGISTER_5L_PRIMITIVE(RegisterCard);
	REGISTER_5L_PRIMITIVE(RegisterCursor);
	REGISTER_5L_PRIMITIVE(RegisterEventDispatcher);
    REGISTER_5L_PRIMITIVE(Screen);
    REGISTER_5L_PRIMITIVE(SetImageCacheSize);
    REGISTER_5L_PRIMITIVE(SetZoneCursor);
	REGISTER_5L_PRIMITIVE(TextAA);
	REGISTER_5L_PRIMITIVE(Timeout);
    REGISTER_5L_PRIMITIVE(Wait);
    REGISTER_5L_PRIMITIVE(Zone);
}


//=========================================================================
//  Utility Functions
//=========================================================================

#define FIND_ELEMENT(TYPE, VAR, NAME) \
    Element *VAR##_temp = wxGetApp().GetStage()->FindElement(NAME); \
    if (VAR##_temp == NULL) \
        ::SetPrimitiveError("noelement", "The element does not exist."); \
    TYPE *VAR = dynamic_cast<TYPE *>(VAR##_temp); \
    if (VAR == NULL) \
        ::SetPrimitiveError("wrongelementtype", \
                            "The element is not of type " #TYPE)

static wxRect ConvRect(const TRect &inRect)
{
	return wxRect(wxPoint(inRect.Left(), inRect.Top()),
				  wxPoint(inRect.Right() - 1, inRect.Bottom() - 1));
}

static wxPoint ConvPoint(const TPoint &inPoint)
{
	return wxPoint(inPoint.X(), inPoint.Y());
}

static wxPoint GetPos(const TRect &inRect)
{
	return wxPoint(inRect.Left(), inRect.Top());
}

static wxSize GetSize(const TRect &inRect)
{
	return wxSize(1 + inRect.Right() - inRect.Left(),
				  1 + inRect.Bottom() - inRect.Top());
}

static wxColour ConvColor(GraphicsTools::Color inColor)
{
	// Translate a color using the official translator function.
	return wxGetApp().GetStage()->GetColor(inColor);
}


//=========================================================================
//  Implementation of wxWindows Primitives
//=========================================================================

DEFINE_5L_PRIMITIVE(AudioStreamSine)
{
	std::string name;
	uint32 frequency;
	inArgs >> name >> frequency;
	new AudioStreamElement(wxGetApp().GetStage(), name.c_str(),
						   new SineAudioStream(frequency));
}

DEFINE_5L_PRIMITIVE(AudioStreamVorbis)
{
	std::string name, path;
	uint32 buffer_size;
	bool should_loop;
	inArgs >> name >> path >> buffer_size >> should_loop;
	new AudioStreamElement(wxGetApp().GetStage(), name.c_str(),
						   new VorbisAudioStream(path.c_str(), buffer_size,
												 should_loop));
}

DEFINE_5L_PRIMITIVE(DeleteElements)
{
	if (!inArgs.HasMoreArguments())
		wxGetApp().GetStage()->DeleteElements();
	else
	{
		while (inArgs.HasMoreArguments())
		{
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

DEFINE_5L_PRIMITIVE(DrawBoxFill)
{
	TRect bounds;
	Color color;

	inArgs >> bounds >> color;
	wxGetApp().GetStage()->FillBox(ConvRect(bounds), color);
}

DEFINE_5L_PRIMITIVE(DrawBoxOutline)
{
	TRect bounds;
	Color color;
	int32 width;

	inArgs >> bounds >> color >> width;
	wxGetApp().GetStage()->OutlineBox(ConvRect(bounds), ConvColor(color),
									  width);

}

DEFINE_5L_PRIMITIVE(DrawLine)
{
	TPoint from, to;
	Color color;
	int32 width;

	inArgs >> from >> to >> color >> width;
	wxGetApp().GetStage()->DrawLine(ConvPoint(from), ConvPoint(to),
									ConvColor(color), width);

}

DEFINE_5L_PRIMITIVE(EditBox)
{
	std::string name, text;
	TRect bounds;

	inArgs >> SymbolName(name) >> bounds >> text;

	wxTextCtrl *edit =
		new wxTextCtrl(wxGetApp().GetStage(), -1, text.c_str(),
					   GetPos(bounds), GetSize(bounds),
					   wxBORDER | wxTE_MULTILINE);
	new Widget(wxGetApp().GetStage(), name.c_str(), edit);
}

DEFINE_5L_PRIMITIVE(ElementExists)
{
	std::string name;
	inArgs >> SymbolName(name);
	if (wxGetApp().GetStage()->FindElement(name.c_str()))
		::SetPrimitiveResult(true);
	else
		::SetPrimitiveResult(false);
}

DEFINE_5L_PRIMITIVE(ElementIsShown)
{
	std::string name;
	inArgs >> SymbolName(name);
	FIND_ELEMENT(Widget, element, name.c_str());
	::SetPrimitiveResult(element->IsShown());
}

DEFINE_5L_PRIMITIVE(ElementSetShown)
{
	std::string name;
	bool show;
	inArgs >> SymbolName(name) >> show;
	FIND_ELEMENT(Widget, element, name.c_str());
	element->Show(show);
	// TODO - Override MovieElement::Show for unshowable movies.
}

DEFINE_5L_PRIMITIVE(EnableExpensiveEvents)
{
	bool enable;
	inArgs >> enable;
	
	wxGetApp().GetStage()->GetEventDispatcher()->EnableExpensiveEvents(enable);
}

DEFINE_5L_PRIMITIVE(HTML)
{
	std::string name, file_or_url;
	TRect bounds;

	inArgs >> SymbolName(name) >> bounds >> file_or_url;

	wxHtmlWindow *html =
		new wxHtmlWindow(wxGetApp().GetStage(), -1,
						 GetPos(bounds), GetSize(bounds),
						 wxHW_SCROLLBAR_AUTO | wxBORDER);
	html->LoadPage(file_or_url.c_str());
	new Widget(wxGetApp().GetStage(), name.c_str(), html);
}

DEFINE_5L_PRIMITIVE(Input)
{
	TRect bounds;
	uint32 size;
	Color fore, back;

	inArgs >> bounds >> size >> fore >> back;
	wxGetApp().GetStage()->ModalTextInput(ConvRect(bounds), size,
										  ConvColor(fore),
										  ConvColor(back));
}

/*---------------------------------------------------------------------
    (LOADPIC PICTURE X Y <FLAGS...>)

	Display the given picture at the given location (x, y).

	XXX - Flags not implemented!
-----------------------------------------------------------------------*/

static void load_picture(const std::string &inName, TPoint inLoc,
						 TRect *inRect = NULL)
{
	// Load our image.
	wxBitmap bitmap = 
		wxGetApp().GetStage()->GetImageCache()->GetBitmap(inName.c_str());
	if (!bitmap.Ok())
	{
		::SetPrimitiveError("noimage", "Can't load the specified image");
		return;
	}

	// If we were given a sub-rectangle, try to extract it.
	if (inRect)
	{
		wxRect rect(inRect->Left(), inRect->Top(),
					inRect->Right() - inRect->Left(),
					inRect->Bottom() - inRect->Top());
		if (rect.GetX() < 0 || rect.GetY() < 0 ||
			rect.GetWidth() > bitmap.GetWidth() ||
			rect.GetHeight() > bitmap.GetHeight())
		{
			::SetPrimitiveError("outofbounds",
								"Sub-rectangle does not fit inside image");
			return;
		}
		bitmap = bitmap.GetSubBitmap(rect);
		inLoc.SetX(inLoc.X() + rect.GetX());
		inLoc.SetY(inLoc.Y() + rect.GetY());
	}

	// Draw our bitmap.
	wxGetApp().GetStage()->DrawBitmap(bitmap, inLoc.X(), inLoc.Y());

	// Update our special variables.
	// XXX - TRect constructor uses height/width order!  Ayiee!
	TRect bounds(TRect(inLoc.Y(), inLoc.X(),
					   inLoc.Y() + bitmap.GetHeight(),
					   inLoc.X() + bitmap.GetWidth()));
	UpdateSpecialVariablesForGraphic(bounds);	
}

DEFINE_5L_PRIMITIVE(Loadpic)
{
	std::string	picname;
    TPoint		loc;

	inArgs >> picname >> loc;

	// Process flags.
	// XXX - We're just throwing them away for now.
	if (inArgs.HasMoreArguments())
	{
		std::string junk;
		while (inArgs.HasMoreArguments())
			inArgs >> junk;

		::SetPrimitiveError("unimplemented",
							"loadpic flags are not implemented");
		return;
	}

	// Do the dirty work.
	load_picture(picname, loc);
}

DEFINE_5L_PRIMITIVE(Loadsubpic)
{
	std::string	picname;
    TPoint		loc;
	TRect		subrect;

	inArgs >> picname >> loc >> subrect;
	load_picture(picname, loc, &subrect);
}

DEFINE_5L_PRIMITIVE(NotifyEnterCard)
{
	wxGetApp().GetStage()->NotifyEnterCard();
	::SkipPrimitiveLogging();
}

DEFINE_5L_PRIMITIVE(NotifyExitCard)
{
	wxGetApp().GetStage()->NotifyExitCard();
	::SkipPrimitiveLogging();
}

/*------------------------------------------------
    (NAP TIME)

    Pause execution for TIME tenths of seconds.
    The user can abort a long nap via the ESC key.
--------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Nap)
{
    int32    tenths;

    inArgs >> tenths;

    //gCursorManager.CheckCursor();
    //gView->Draw();
    TInterpreter::GetInstance()->Nap(tenths);
}

DEFINE_5L_PRIMITIVE(MediaSetVolume)
{
	// Right now, this only works for media streams.
	std::string name, channel_name;
	double volume;
	inArgs >> SymbolName(name) >> SymbolName(channel_name) >> volume;
	FIND_ELEMENT(AudioStreamElement, stream, name.c_str());
	stream->SetVolume(channel_name, volume);
}

DEFINE_5L_PRIMITIVE(MouseGrab)
{
	std::string name;
	inArgs >> SymbolName(name);
	FIND_ELEMENT(Zone, zone, name.c_str());
	wxGetApp().GetStage()->MouseGrab(zone);
}

DEFINE_5L_PRIMITIVE(MouseIsGrabbed)
{
	::SetPrimitiveResult(wxGetApp().GetStage()->MouseIsGrabbed());
}

DEFINE_5L_PRIMITIVE(MousePosition)
{
	// XXX - Stop returning out-of-bounds co-ordinates.
	wxPoint p = wxGetApp().GetStage()->ScreenToClient(::wxGetMousePosition());
	::SetPrimitiveResult(TPoint(p.x, p.y));
}

DEFINE_5L_PRIMITIVE(MouseUngrab)
{
	std::string name;
	inArgs >> SymbolName(name);
	FIND_ELEMENT(Zone, zone, name.c_str());
	wxGetApp().GetStage()->MouseUngrab(zone);
}

DEFINE_5L_PRIMITIVE(Movie)
{
	std::string name, path;
	TRect bounds;
	bool controller, audio_only, loop, interaction;

	inArgs >> SymbolName(name) >> bounds >> path >> controller
		   >> audio_only >> loop >> interaction;

	MovieWindowStyle style = 0;
	if (controller)
		style |= MOVIE_CONTROLLER;
	if (audio_only)
		style |= MOVIE_AUDIO_ONLY;
	if (loop)
		style |= MOVIE_LOOP;
	if (interaction)
		style |= MOVIE_INTERACTION;

	new MovieElement(wxGetApp().GetStage(), name.c_str(), ConvRect(bounds),
					 path.c_str(), 0, style);
}

// Note: these primitives may not be happy if the underlying movie code 
// does not like to be paused.
DEFINE_5L_PRIMITIVE(MoviePause)
{
	std::string name;
	
	inArgs >> SymbolName(name);

	FIND_ELEMENT(IMediaElement, movie, name.c_str());

	movie->Pause();
}

DEFINE_5L_PRIMITIVE(MovieResume)
{
	std::string name;
	
	inArgs >> SymbolName(name);

	FIND_ELEMENT(IMediaElement, movie, name.c_str());

	movie->Resume();
}

DEFINE_5L_PRIMITIVE(Refresh)
{
	std::string transition;
	int32 milliseconds;
	inArgs >> SymbolName(transition) >> milliseconds;
	wxGetApp().GetStage()->RefreshStage(transition, milliseconds);
}

DEFINE_5L_PRIMITIVE(SaveGraphics)
{
	TRect bounds;
	inArgs >> bounds;
	wxGetApp().GetStage()->SaveGraphics(ConvRect(bounds));
}

DEFINE_5L_PRIMITIVE(RestoreGraphics)
{
	TRect bounds;
	inArgs >> bounds;
	wxGetApp().GetStage()->RestoreGraphics(ConvRect(bounds));
}

DEFINE_5L_PRIMITIVE(RegisterCard)
{
	std::string name;
	inArgs >> SymbolName(name);
	wxGetApp().GetStage()->RegisterCard(name.c_str());
	::SkipPrimitiveLogging();
}

DEFINE_5L_PRIMITIVE(RegisterCursor)
{
	std::string name, path;
	TPoint hotspot;
	inArgs >> SymbolName(name) >> path >> hotspot;
	CursorManager *manager = wxGetApp().GetStage()->GetCursorManager();
	manager->RegisterImageCursor(name, path, hotspot.X(), hotspot.Y());
}

DEFINE_5L_PRIMITIVE(RegisterEventDispatcher)
{
	TCallback *callback;
	inArgs >> callback;
	wxGetApp().GetStage()->GetEventDispatcher()->SetDispatcher(callback);
}

/*---------------------------------------------------------------
    (SCREEN COLOR)

    A fast way to fill the entire screen with a particular color.
-----------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Screen)
{
    Color color;
    inArgs >> color; 
	wxGetApp().GetStage()->ClearStage(ConvColor(color));
}

DEFINE_5L_PRIMITIVE(SetImageCacheSize)
{
	uint32 sz;
	inArgs >> sz;
	wxGetApp().GetStage()->GetImageCache()->SetMaxCacheSize(sz);
}

DEFINE_5L_PRIMITIVE(SetZoneCursor)
{
	std::string name, cursor;
	inArgs >> SymbolName(name) >> SymbolName(cursor);

	FIND_ELEMENT(Zone, zone, name.c_str());
	CursorManager *manager = wxGetApp().GetStage()->GetCursorManager();
	zone->SetCursor(manager->FindCursor(cursor));
}

DEFINE_5L_PRIMITIVE(TextAA)
{
	TRect		bounds;
	std::string style, text;

    inArgs >> SymbolName(style) >> bounds >> text;
    gOrigin.AdjustRect(&bounds);

	gStyleSheetManager.Draw(style, text,
							GraphicsTools::Point(bounds.Left(),
												 bounds.Top()),
							bounds.Right() - bounds.Left(),
							wxGetApp().GetStage());
}

/*-----------------------------------------------------------
    (TIMEOUT DELAY CARD)

    If the user doesn't respond in DELAY seconds, jump to the
    given card.
-------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Timeout)
{
	std::string cardName;
    int32     	secs;

    inArgs >> secs >> SymbolName(cardName);
    TInterpreter::GetInstance()->Timeout(cardName.c_str(), secs);
}

DEFINE_5L_PRIMITIVE(Wait)
{
	std::string name;
	int32 frame = LAST_FRAME;

	inArgs >> SymbolName(name);
	if (inArgs.HasMoreArguments())
		inArgs >> frame;

	wxGetApp().GetStage()->Wait(name.c_str(), frame);
}

DEFINE_5L_PRIMITIVE(Zone)
{
	std::string name, cursor;
	TRect bounds;
	TCallback *dispatcher;
	
	inArgs >> SymbolName(name) >> bounds >> dispatcher >> cursor;
	new Zone(wxGetApp().GetStage(), name.c_str(), ConvRect(bounds), dispatcher,
			 wxGetApp().GetStage()->GetCursorManager()->FindCursor(cursor));
}
