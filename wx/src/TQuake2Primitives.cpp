// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

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
	REGISTER_5L_PRIMITIVE(Quake2Init);
	REGISTER_5L_PRIMITIVE(Quake2Command);
	REGISTER_5L_PRIMITIVE(Quake2BackgroundLoadCommand);
	REGISTER_5L_PRIMITIVE(Quake2IsLoadingInBackground);
	REGISTER_5L_PRIMITIVE(Quake2Print);
	REGISTER_5L_PRIMITIVE(Quake2RegisterCommand);
	REGISTER_5L_PRIMITIVE(Quake2IsShown);
	REGISTER_5L_PRIMITIVE(Quake2Show);
	REGISTER_5L_PRIMITIVE(Quake2Hide);
	REGISTER_5L_PRIMITIVE(Quake2SetStringVar);
	REGISTER_5L_PRIMITIVE(Quake2GetStringVar);
	REGISTER_5L_PRIMITIVE(Quake2SetFloatVar);
	REGISTER_5L_PRIMITIVE(Quake2GetFloatVar);
}


//=========================================================================
//  Implementation of Quake 2 Primitives
//=========================================================================

#define REQUIRE_QUAKE2_INITIALIZATION \
	if (!Quake2Engine::IsInitialized()) { \
		::SetPrimitiveError("noquake", "Quake 2 is not initialized."); \
		return; \
	}

DEFINE_5L_PRIMITIVE(Quake2Init)
{	
	std::string game;

	inArgs >> game;
	if (!Quake2Engine::IsInitialized())
		Quake2Engine::Initialize(game);
}

DEFINE_5L_PRIMITIVE(Quake2Command)
{
	std::string cmd;
	inArgs >> cmd;
	REQUIRE_QUAKE2_INITIALIZATION;
	Quake2Engine::GetInstance()->ExecCommand(cmd.c_str());
}

DEFINE_5L_PRIMITIVE(Quake2BackgroundLoadCommand)
{
	std::string cmd;
	inArgs >> cmd;
	REQUIRE_QUAKE2_INITIALIZATION;
	Quake2Engine::GetInstance()->ExecBackgroundLoadCommand(cmd.c_str());
}

DEFINE_5L_PRIMITIVE(Quake2IsLoadingInBackground)
{
	REQUIRE_QUAKE2_INITIALIZATION;
	::SetPrimitiveResult(Quake2Engine::GetInstance()->IsLoadingInBackground());
}

DEFINE_5L_PRIMITIVE(Quake2Print)
{
	std::string msg;
	inArgs >> msg;
	REQUIRE_QUAKE2_INITIALIZATION;
	Quake2Engine::GetInstance()->Print(msg.c_str());
}

DEFINE_5L_PRIMITIVE(Quake2RegisterCommand)
{
	std::string name;
	TCallbackPtr callback;
	inArgs >> SymbolName(name) >> callback;
	REQUIRE_QUAKE2_INITIALIZATION; // FIXME - We may leak callback here.
	Quake2Engine::GetInstance()->RegisterCallback(name, callback);
}

DEFINE_5L_PRIMITIVE(Quake2IsShown)
{
	REQUIRE_QUAKE2_INITIALIZATION;
	::SetPrimitiveResult(Quake2Engine::GetInstance()->IsShown());
}

DEFINE_5L_PRIMITIVE(Quake2Show)
{
	REQUIRE_QUAKE2_INITIALIZATION;
	Quake2Engine::GetInstance()->Show();
}

DEFINE_5L_PRIMITIVE(Quake2Hide)
{
	REQUIRE_QUAKE2_INITIALIZATION;
	Quake2Engine::GetInstance()->Hide();
}

DEFINE_5L_PRIMITIVE(Quake2SetStringVar)
{
	std::string name, value;
	inArgs >> SymbolName(name) >> value;
	REQUIRE_QUAKE2_INITIALIZATION;
	Quake2Engine::GetInstance()->SetVariable(name.c_str(), value.c_str());
}

DEFINE_5L_PRIMITIVE(Quake2GetStringVar)
{
	std::string name;
	inArgs >> SymbolName(name);

	REQUIRE_QUAKE2_INITIALIZATION;
	wxString value;
	Quake2Engine::GetInstance()->GetVariable(name.c_str(), value);
	::SetPrimitiveResult(value.mb_str());
}

DEFINE_5L_PRIMITIVE(Quake2SetFloatVar)
{
	std::string name;
	double value;
	inArgs >> SymbolName(name) >> value;
	REQUIRE_QUAKE2_INITIALIZATION;
	Quake2Engine::GetInstance()->SetVariable(name.c_str(),
											 static_cast<float>(value));
}

DEFINE_5L_PRIMITIVE(Quake2GetFloatVar)
{
	std::string name;
	inArgs >> SymbolName(name);

	REQUIRE_QUAKE2_INITIALIZATION;
	float value;
	Quake2Engine::GetInstance()->GetVariable(name.c_str(), value);
	::SetPrimitiveResult(static_cast<double>(value));
}

#endif // CONFIG_HAVE_QUAKE2
