// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

// Needed for RegisterQuake2Primitives.
#include <wx/wx.h>
#include "TCommon.h"
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
	REGISTER_5L_PRIMITIVE(Quake2Show);
	REGISTER_5L_PRIMITIVE(Quake2Hide);
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
	if (!Quake2Engine::IsInitialized())
		Quake2Engine::Initialize();
}

DEFINE_5L_PRIMITIVE(Quake2Command)
{
	std::string cmd;
	inArgs >> cmd;

	REQUIRE_QUAKE2_INITIALIZATION;
	Quake2Engine::GetInstance()->ExecCommand(cmd.c_str());
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

#endif // CONFIG_HAVE_QUAKE2
