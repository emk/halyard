// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include "CommonHeaders.h"

#include <boost/lexical_cast.hpp>

#include <ctime>
#include "TamaleProgram.h"
#include "Background.h"

USING_NAMESPACE_FIVEL
using namespace model;

IMPLEMENT_MODEL_CLASS(TamaleProgram);

void TamaleProgram::Initialize()
{
	time_t t = time(0);
	std::string year =
		boost::lexical_cast<std::string>(localtime(&t)->tm_year + 1900);

	SetString("name", "");
	//SetSize("stageSize", ...);
	SetString("copyright",
			  "Copyright " + year + " ???.  All rights reserved.");
	SetString("version", "0.0");
	SetString("comments", "");

	Set("cards", new List());
	Set("backgrounds", new List());
}

List *TamaleProgram::GetBackgrounds()
{
	return cast<List>(Get("backgrounds"));
}

void TamaleProgram::InsertBackground()
{
	List *backgrounds = cast<List>(Get("backgrounds"));
	backgrounds->Append(new Background())->Initialize();
}
