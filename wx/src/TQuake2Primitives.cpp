// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
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

// Needed for RegisterQuake2Primitives.
#include "TamaleHeaders.h"
#include "TPrimitives.h"
#include "TQuake2Primitives.h"

// Needed to implement the primitives.
#include "FiveLApp.h"
#include "Quake2Engine.h"

USING_NAMESPACE_FIVEL

bool Quake2Engine::sHasBeenCreated = false;
Quake2Engine *Quake2Engine::sInstance = NULL;

#if CONFIG_HAVE_QUAKE2

//=========================================================================
//  RegisterWxPrimitives
//=========================================================================
//  Install our wxWindows-specific primitives.

void FIVEL_NS RegisterQuake2Primitives()
{
	REGISTER_PRIMITIVE(Quake2Init);
	REGISTER_PRIMITIVE(Quake2Command);
	REGISTER_PRIMITIVE(Quake2BackgroundLoadCommand);
	REGISTER_PRIMITIVE(Quake2IsLoadingInBackground);
	REGISTER_PRIMITIVE(Quake2Print);
	REGISTER_PRIMITIVE(Quake2RegisterCommand);
	REGISTER_PRIMITIVE(Quake2IsShown);
	REGISTER_PRIMITIVE(Quake2Show);
	REGISTER_PRIMITIVE(Quake2Hide);
    REGISTER_PRIMITIVE(Quake2SetShouldRunInBackground);
    REGISTER_PRIMITIVE(Quake2GetShouldRunInBackground);
	REGISTER_PRIMITIVE(Quake2SetStringVar);
	REGISTER_PRIMITIVE(Quake2GetStringVar);
	REGISTER_PRIMITIVE(Quake2SetFloatVar);
	REGISTER_PRIMITIVE(Quake2GetFloatVar);
}


//=========================================================================
//  Implementation of Quake 2 Primitives
//=========================================================================

#define REQUIRE_QUAKE2_INITIALIZATION \
	if (!Quake2Engine::IsInitialized()) { \
		THROW("Quake 2 is not initialized."); \
	}

DEFINE_PRIMITIVE(Quake2Init)
{	
	std::string game, driver;

    // Default driver.
    driver = "soft";

	inArgs >> game;
    if (inArgs.HasMoreArguments())
        inArgs >> SymbolName(driver);

	if (!Quake2Engine::IsInitialized())
		Quake2Engine::Initialize(game, driver);
}

DEFINE_PRIMITIVE(Quake2Command)
{
	std::string cmd;
	inArgs >> cmd;
	REQUIRE_QUAKE2_INITIALIZATION;
	Quake2Engine::GetInstance()->ExecCommand(cmd.c_str());
}

DEFINE_PRIMITIVE(Quake2BackgroundLoadCommand)
{
	std::string cmd;
	inArgs >> cmd;
	REQUIRE_QUAKE2_INITIALIZATION;
	Quake2Engine::GetInstance()->ExecBackgroundLoadCommand(cmd.c_str());
}

DEFINE_PRIMITIVE(Quake2IsLoadingInBackground)
{
	REQUIRE_QUAKE2_INITIALIZATION;
	::SetPrimitiveResult(Quake2Engine::GetInstance()->IsLoadingInBackground());
}

DEFINE_PRIMITIVE(Quake2Print)
{
	std::string msg;
	inArgs >> msg;
	REQUIRE_QUAKE2_INITIALIZATION;
	Quake2Engine::GetInstance()->Print(msg.c_str());
}

DEFINE_PRIMITIVE(Quake2RegisterCommand)
{
	std::string name;
	TCallbackPtr callback;
	inArgs >> SymbolName(name) >> callback;
	REQUIRE_QUAKE2_INITIALIZATION; // FIXME - We may leak callback here.
	Quake2Engine::GetInstance()->RegisterCallback(name, callback);
}

DEFINE_PRIMITIVE(Quake2IsShown)
{
	REQUIRE_QUAKE2_INITIALIZATION;
	::SetPrimitiveResult(Quake2Engine::GetInstance()->IsShown());
}

DEFINE_PRIMITIVE(Quake2Show)
{
	REQUIRE_QUAKE2_INITIALIZATION;
	Quake2Engine::GetInstance()->Show();
}

DEFINE_PRIMITIVE(Quake2Hide)
{
	REQUIRE_QUAKE2_INITIALIZATION;
	Quake2Engine::GetInstance()->Hide();
}

DEFINE_PRIMITIVE(Quake2SetShouldRunInBackground)
{
    bool run;
    inArgs >> run;

    REQUIRE_QUAKE2_INITIALIZATION;
    Quake2Engine::GetInstance()->RunGameInBackground(run);
}

DEFINE_PRIMITIVE(Quake2GetShouldRunInBackground)
{
    REQUIRE_QUAKE2_INITIALIZATION;
    Quake2Engine *engine = Quake2Engine::GetInstance();
    ::SetPrimitiveResult(engine->ShouldRunGameInBackground());
}

DEFINE_PRIMITIVE(Quake2SetStringVar)
{
	std::string name, value;
	inArgs >> SymbolName(name) >> value;
	REQUIRE_QUAKE2_INITIALIZATION;
	Quake2Engine::GetInstance()->SetVariable(name.c_str(), value.c_str());
}

DEFINE_PRIMITIVE(Quake2GetStringVar)
{
	std::string name;
	inArgs >> SymbolName(name);

	REQUIRE_QUAKE2_INITIALIZATION;
	wxString value;
	Quake2Engine::GetInstance()->GetVariable(name.c_str(), value);
	::SetPrimitiveResult(value.mb_str());
}

DEFINE_PRIMITIVE(Quake2SetFloatVar)
{
	std::string name;
	double value;
	inArgs >> SymbolName(name) >> value;
	REQUIRE_QUAKE2_INITIALIZATION;
	Quake2Engine::GetInstance()->SetVariable(name.c_str(),
											 static_cast<float>(value));
}

DEFINE_PRIMITIVE(Quake2GetFloatVar)
{
	std::string name;
	inArgs >> SymbolName(name);

	REQUIRE_QUAKE2_INITIALIZATION;
	float value;
	Quake2Engine::GetInstance()->GetVariable(name.c_str(), value);
	::SetPrimitiveResult(static_cast<double>(value));
}

#endif // CONFIG_HAVE_QUAKE2
