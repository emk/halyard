// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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
