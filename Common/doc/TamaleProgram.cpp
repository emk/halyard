// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <ctime>
#include "boost/lexical_cast.hpp"

#include "TCommon.h"
#include "TamaleProgram.h"

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
