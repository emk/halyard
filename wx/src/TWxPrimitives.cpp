// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

// Needed for RegisterWxPrimitives.
#include <wx/wx.h>
#include <wx/image.h>
#include "TCommon.h"
#include "TPrimitives.h"
#include "TCommonPrimitives.h"
#include "TWxPrimitives.h"

// Needed to implement the primitives.
#include "FiveLApp.h"
#include "Stage.h"

USING_NAMESPACE_FIVEL
using GraphicsTools::Color;


//=========================================================================
//  RegisterWxPrimitives
//=========================================================================
//  Install our wxWindows-specific primitives.

void FIVEL_NS RegisterWxPrimitives()
{
	REGISTER_5L_PRIMITIVE(Loadpic);
    REGISTER_5L_PRIMITIVE(Screen);
    REGISTER_5L_PRIMITIVE(SetWindowTitle);
}


//=========================================================================
//  Utility Functions
//=========================================================================

static wxColour GetColor(GraphicsTools::Color inColor)
{
	// Translate a color using the official translator function.
	return wxGetApp().GetStage()->GetColor(inColor);
}


//=========================================================================
//  Implementation of wxWindows Primitives
//=========================================================================

/*---------------------------------------------------------------------
    (LOADPIC PICTURE X Y <FLAGS...>)

	Display the given picture at the given location (x, y).

	XXX - Flags not implemented!
-----------------------------------------------------------------------*/
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

	// Convert our image to a bitmap.
	wxImage image;
	image.LoadFile((".\\Graphics\\" + picname + ".png").c_str());
	wxBitmap bitmap(image);

	// Draw the bitmap.
	wxGetApp().GetStage()->DrawBitmap(bitmap, loc.X(), loc.Y());

	// Update our special variables.
	// XXX - TRect constructor uses height/width order!  Ayiee!
	TRect bounds(TRect(loc.Y(), loc.X(),
					   loc.Y() + bitmap.GetHeight(),
					   loc.X() + bitmap.GetWidth()));
	UpdateSpecialVariablesForGraphic(bounds);
}


/*---------------------------------------------------------------------
    (SETWINDOWTITLE TITLE)

    Set the application window title
 ---------------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(SetWindowTitle)
{
    std::string title;
    inArgs >> title;
    wxGetApp().GetStageFrame()->SetTitle(title.c_str());
}

/*---------------------------------------------------------------
    (SCREEN COLOR)

    A fast way to fill the entire screen with a particular color.
-----------------------------------------------------------------*/
DEFINE_5L_PRIMITIVE(Screen)
{
    Color color;
    inArgs >> color; 
	wxGetApp().GetStage()->ClearStage(GetColor(color));
}
