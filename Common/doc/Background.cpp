// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "TCommon.h"
#include "Background.h"

USING_NAMESPACE_FIVEL
using namespace model;

IMPLEMENT_MODEL_CLASS(Background);

void Background::Initialize()
{
	SetString("name", "untitled");
}
